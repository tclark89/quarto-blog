dataLitho |> 
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
  ) |> 
  ggplot(aes(x=CASSETTE, y=measMean)) +
  geom_point() +
  geom_line() +
  geom_smooth(method="lm") +
  theme_bw()
