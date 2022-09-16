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