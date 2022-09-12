library(readr)
library(dplyr)

# LITHOGRA.DAT ####
# The width of a line on a silicon wafer
fileLitho <- read_file("https://www.itl.nist.gov/div898/handbook/datasets/LITHOGRA.DAT")
# add blog link after publishing?

dataLitho <- read_fwf(fileLitho, 
         col_positions = fwf_empty(
           fileLitho, 
           skip=25,
           col_names=c("CASSETTE", 
                       "WAFER", 
                       "SITE", 
                       "LINEWIDT", 
                       "RUNSEQ",
                       "LINEWIDT_2")),
         skip=25) |> 
  select(-LINEWIDT_2)
dataLitho


# MONITOR-6_3_3_1.DAT ####
# Counts of defective chips per wafer
fileMonitor <- read_file("https://www.itl.nist.gov/div898/handbook/datasets/MONITOR-6_3_3_1.DAT")
# add blog link after publishing?

dataMonitor <- read_fwf(fileMonitor,
                        col_positions = fwf_empty(
                          fileMonitor,
                          skip=25,
                          col_names = c("waferid", "defects")
                        ),
                        skip=25)
dataMonitor


# I don't think I actually need this one
# NEGIZ4.DAT ####
# Particle size from aerosol mini-spray dryer
fileNegiz4 <- read_file("https://www.itl.nist.gov/div898/handbook/datasets/NEGIZ4.DAT")
# add blog link?

dataNegiz4 <- read_fwf(fileNegiz,
                      col_positions = fwf_empty(
                        fileNegiz,
                        skip=50,
                        col_names=c("junk1", "junk2", "y", "junk3", "junk4")
                      ),
                      skip=50) |> 
  select(y)
dataNegiz4