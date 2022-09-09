library(readr)
library(dplyr)

fileLitho <- read_file("https://www.itl.nist.gov/div898/handbook/datasets/LITHOGRA.DAT")


dataLitho <- read_fwf(fileLitho, 
         col_positions = fwf_empty(
           fileLith, 
           skip=25,
           col_names=c("CASSETTE", 
                       "WAFER", 
                       "SITE", 
                       "LINEWIDT", 
                       "RUNSEQ",
                       "LINEWIDT_2")),
         skip=25) |> 
  select(-LINEWIDT_2)
