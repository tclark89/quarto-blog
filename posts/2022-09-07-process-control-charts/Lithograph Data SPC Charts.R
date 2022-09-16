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