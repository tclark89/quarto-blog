# Libraries needed ####
library(dplyr)
library(ggplot2)
# Optional
# library(plotly)


# Data, Functions, Variables ####

# Normal data with 25 runs of 12 "units"
set.seed(1234)
dataSPC <- tibble::tibble(
  meas = rnorm(300, mean=90, sd=2),
  run = rep(c(1:12), 25)
) 
dataSPC <- dataSPC[order(dataSPC$run),]

# Color code
colorKey <- c("Beyond Limits"="red", 
              "Violating Run"="orange", 
              "Normal"="black")

# C4 Function
# used for S charts (when n>10)
c4 <- function(n) {
  sqrt(2/(n-1)) * (factorial(n/2-1) / factorial((n-1)/2-1))
}

# D2 Function
# used for R charts (n<=10)
d2 <- function (n){
  n[n == 1] <- 2
  
  fn <- quote(function(w){ptukey(w, n, Inf, lower.tail=FALSE)})
  
  vapply(
    n,
    function(n){
      stats::integrate(eval(fn), 0, Inf)[[1]]
    },
    numeric(1))
}

#A2, can be used for UCL and LCL on R charts
a2 <- function(n){3 / (d2(n)*sqrt(n))}


# Process/Summarise the data ####

# Find Run Mean and Std Dev
dataSPCSumm <- dataSPC |> 
  group_by(run) |> 
  # Mean, SD, and Count for each run
  summarise(measMean = mean(meas, na.rm=T),
            measSD = sd(meas, na.rm=T),
            measR = diff(range(meas, na.rm=T)),
            count = n()) |> 
  ungroup() |> 
  # Overall process Mean and SD
  mutate(
    processMean = mean(measMean, na.rm=T),
    processSD = mean(measSD, na.rm=T),
    processR = mean(measR, na.rm=T)
  )


# X-Bar Calculations
dataXBar <- dataSPCSumm |> 
  mutate(
    
    
    # for X-bar & S charts
    # Upper Limit
    UL = processMean + 3*processSD/(c4(count)*sqrt(count)),
    # Lower Limit
    LL = processMean - 3*processSD/(c4(count)*sqrt(count)),
    
    # For X-bar and R charts
    # UL = processMean + a2(count) * processR,
    # LL = processMean - a2(count) * processR,
    
    # Are we out of bounds (1=Yes, 0=No) ?
    beyondLimit = if_else(!is.na(measSD) & (measMean > UL | measMean < LL), 1, 0),
    # Violating runs: too many consecutive runs above or below process mean
    # First determine if above (1) or below (-1)
    posPoint = if_else(measMean > processMean, 1, 
                       if_else(measMean < processMean, -1, 0)),
    # Find the cumulative sum (2 in a row above is 1+1=2, etc.)
    posSum = cumsum((posPoint)),
    # Lag. make sure i know what this is doing....
    posLag = posSum - lag(posSum, 7, default = 0),
    # If 7 or more consecutive runs above or below then "violating run" 
    violatingRun = if_else(abs(posLag)>=7, 1, 0),
    # Classify; Beyond Limits overrides Violating Run
    measClass=if_else(beyondLimit == 1, "Beyond Limits", 
                  if_else(violatingRun==1, "Violating Run", "Normal"))
  )
    
    
    


# For S Chart
dataS <- dataSPCSumm |> 
  mutate(
    ULS = processSD + 3*(processSD/c4(count))*sqrt(1-c4(count)^2),
    LLS = pmax(processSD - 3*(processSD/c4(count))*sqrt(1-c4(count)^2), 0)
  ) 


# X-bar Chart ####
g <- ggplot(dataXBar, aes(x=run, y=measMean)) + 
  geom_line(aes(color="Normal")) +
  geom_point(aes(color=measClass)) +
  geom_step(aes(y=UL), lty=2) +
  geom_step(aes(y=LL), lty=2) +
  geom_hline(aes(yintercept = processMean, color="Normal")) +
  scale_color_manual(values=colorKey) +
  labs(x="Run", y="Run Mean Value", color="Class") +
  theme_bw()

g

plotly::ggplotly(g)


# S Chart ####
g <- ggplot(dataSPCSumm, aes(x=run, y=measSD)) + 
  geom_point() +
  geom_path() +
  geom_step(aes(y=ULS)) +
  geom_step(aes(y=LLS)) +
  geom_hline(aes(yintercept = processSD))

g

plotly::ggplotly(g)


# Original from RMD ####
if(F){
  dataSumm <- tibble(yVar = params$yVar,
                     xVar = params$xVar) %>% 
    group_by(xVar) %>% 
    summarise(yVarSD = sd(yVar, na.rm=T),
              yVarMean = mean(yVar, na.rm=T),
              count = n())
  
  processMean <- mean(dataSumm$yVarMean, na.rm=T)
  processSD <- mean(dataSumm$yVarSD, na.rm=T)
  
  
  dataXBar <- dataSumm %>%
    mutate(
      chartType = "X-Bar",
      yVar = yVarMean,
      chartMean = processMean,
      UL = pmin(processMean + 3*processSD/(c4(count)*sqrt(count)), params$uLimit),
      LL = pmax(processMean - 3*processSD/(c4(count)*sqrt(count)), params$lLimit),
      bl = if_else(!is.na(yVarSD) & (yVarMean > UL | yVarMean < LL), 1, 0),
      posPoint = if_else(yVarMean > processMean, 1, 
                         if_else(yVarMean < processMean, -1, 0)),
      posSum = cumsum((posPoint)),
      posLag = posSum - lag(posSum, 7, default = 0),
      vr = if_else(abs(posLag)>=7, 1, 0),
      color=if_else(bl == 1, "Beyond Limits", 
                    if_else(vr==1, "Violating Run", "Normal")))
  
  dataS <- dataSumm  %>% 
    filter(!is.na(yVarSD)) %>% 
    mutate(
      chartType = "S",
      yVar = yVarSD,
      chartMean = processSD,
      UL = processSD + 3*(processSD/c4(count))*sqrt(1-c4(count)^2),
      LL = pmax(processSD - 3*(processSD/c4(count))*sqrt(1-c4(count)^2), 0),
      bl = if_else(yVarSD > UL | yVarSD < LL, 1, 0),
      posPoint = if_else(yVarSD > processSD, 1, 
                         if_else(yVarSD < processSD, -1, 0)),
      posSum = cumsum((posPoint)),
      posLag = posSum - lag(posSum, 7, default = 0),
      vr = if_else(abs(posLag)>=7, 1, 0),
      color=if_else(bl == 1, "Beyond Limits", 
                    if_else(vr==1, "Violating Run", "Normal")))
  
  dataChart <- bind_rows(dataXBar, dataS) %>% 
    mutate(chartType = as_factor(chartType)) %>% 
    filter(chartType == "X-Bar")
  
  dataChart %>% 
    ggplot(aes(x=xVar, y=yVar)) +
    geom_line(aes(color="Normal")) +
    geom_point(aes(color=color)) +
    geom_line(aes(y=chartMean)) +
    geom_step(aes(y=UL), lty=2) +
    geom_step(aes(y=LL), lty=2) +
    scale_color_manual(values=colorKey) +
    labs(x=params$xVarName, y=params$yVarName) +
    scale_x_date(date_breaks = "2 months", 
                 date_labels = "%m/%Y",
                 limits=c(plotStart, plotEnd),
                 expand=c(0.01, 0)) +
    scale_y_continuous(n.breaks = 7) +
    theme_bw() +
    theme(legend.position = "bottom", 
          legend.title = element_blank()) # +
  # facet_grid(rows=vars(chartType), 
  # switch = "y", scales = "free_y")
}
