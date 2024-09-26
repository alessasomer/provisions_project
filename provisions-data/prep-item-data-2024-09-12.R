
# read in provisions item data 

#install.packages("tidyverse")

library(tidyverse)

# file path
username <- Sys.getenv("USERNAME")
mypath <- paste0("C:\\Users\\", username, "\\Dropbox\\provisions\\")

# load data
raw.items <- readxl::read_excel(paste0(mypath, "raw\\catalog-2024-08-18-2238.xlsx")) 

# clean data a little bit
item.data <- raw.items %>% 
  # remove columns with no variation
  select(where(~ n_distinct(.) != 1)) %>% 
  select(!`Default Vendor Name`)

# export data as a csv
write_csv(provisions, paste0(mypath, "ECON370-provisions-items.csv"))
