library(readr)
library(dplyr)
library(ggplot2)

# LITHOGRA.DAT ####
# The width of a line on a silicon wafer
fileLitho <- read_file("https://www.itl.nist.gov/div898/handbook/datasets/LITHOGRA.DAT")
# add blog link after publishing?

dataLitho <- read_fwf(
  fileLitho, 
  col_positions = fwf_widths(
    c(7, 7, 7, 12, 7, 12),
    col_names=c("CASSETTE", "WAFER", "SITE", "LINEWIDT", "RUNSEQ","LINEWIDT_2")
    ),
  skip=25
) |> 
  select(-LINEWIDT_2)

# Functions ####
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


# Initial Summarization ####
dataLithoSumm <- dataLitho |> 
  group_by(CASSETTE) |> 
  # Mean, SD, and Count for each run
  summarise(measMean = mean(LINEWIDT, na.rm=T),
            measSD = sd(LINEWIDT, na.rm=T),
            measR = diff(range(LINEWIDT, na.rm=T)),
            count = n()) |> 
  ungroup() |> 
  # Overall process Mean and SD
  mutate(
    processMean = mean(measMean, na.rm=T),
    processSD = mean(measSD, na.rm=T),
    processR = mean(measR, na.rm=T)
  ) 


# Examining the data ####
plotFunction <- function(data, yVar){
  data |> 
    ggplot(aes(x=CASSETTE, y={{ yVar }})) +
    geom_point() +
    geom_line() +
    geom_smooth(method="lm") +
    theme_bw()
}

# Group Means
dataLithoSumm |> 
  plotFunction(measMean)

# Group Standard Deviations
dataLithoSumm |> 
  plotFunction(measSD)

# Group Ranges
dataLithoSumm |> 
  plotFunction(measR)

# Group Counts
dataLithoSumm |> 
  plotFunction(count)

# Overall Stats
dataLithoSumm |> 
  select(starts_with("process")) |> 
  distinct()





# X-Bar Calculations ####
dataXBar <- dataLithoSumm |> 
  mutate(
    
    # for X-bar & S charts
    # Upper Limit
    UL = processMean + 3*processSD/(c4(count)*sqrt(count)),
    # Lower Limit
    LL = processMean - 3*processSD/(c4(count)*sqrt(count)),
    
    # For X-bar and R charts
    # UL = processMean + 3*processSD/(d2(count)*sqrt(count) * processR,
    # LL = processMean - 3*processSD/(d2(count)*sqrt(count) * processR,
    
    # Are we out of bounds (1=Yes, 0=No) ?
    beyondLimit = if_else(!is.na(measSD) & (measMean > UL | measMean < LL), 1, 0),
    # Violating runs: too many consecutive runs above or below process mean
    # First determine if above (1) or below (-1)
    posPoint = if_else(measMean > processMean, 1, 
                       if_else(measMean < processMean, -1, 0)),
    # Find the cumulative sum (2 in a row above is 1+1=2, etc.)
    posSum = cumsum((posPoint)),
    # Use lag to subtract the current score from the 7th previous.
    posLag = posSum - lag(posSum, 7, default = 0),
    # If 7 or more consecutive runs above or below then "violating run" 
    violatingRun = if_else(abs(posLag)>=7, 1, 0),
    # Classify; Beyond Limits overrides Violating Run
    measClass=if_else(beyondLimit == 1, "Beyond Limits", 
                      if_else(violatingRun==1, "Violating Run", "Normal"))
  )

# Color code
colorKey <- c("Beyond Limits"="red", 
              "Violating Run"="orange", 
              "Normal"="black")

dataXBar |> 
  ggplot(aes(x=CASSETTE, y=measMean)) + 
  geom_line(aes(color="Normal")) +
  geom_point(aes(color=measClass)) +
  geom_step(aes(y=UL), lty=2) +
  geom_step(aes(y=LL), lty=2) +
  geom_hline(aes(yintercept = processMean, color="Normal")) +
  scale_color_manual(values=colorKey) +
  labs(x="Cassette", y="Mean Line Width", color="Class") +
  theme_bw()
