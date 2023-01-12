library(signal)
library(tidyverse)
library(patchwork)
source('aurora functions.R')
theme_set(theme_bw())
install.packages(rdrr)

dataFolder <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/DDF/02_ddf/'
DataFiles <- list.files(dataFolder, 'ddf', recursive = TRUE)

settingsFolder <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/settings_p02/'
allSettingFiles <- list.files(settingsFolder, 'settings', full.names = TRUE)

df <- tibble()

for (currentsettingfile in allSettingFiles) {
  currentsettingdata <- read_csv(currentsettingfile, col_names =  c("information", "data"))%>% 
    mutate(filename = currentsettingfile)
  df = rbind(df, currentsettingdata)
}

filtersetting <- tibble()
filtersetting <- df %>% dplyr::filter(information == "07. Intervention (film/shaved)")

datafilename <- filtersetting[,-1]
view(datafilename)


## format datafilename column 2 - ta bort onödig information i filnamnet så att du kan matcha dem
## format overlaydata column 6
#str_detect(datafilename[,2], DataFiles[,6])

#removes part of the setting file name to time stamp
datafilename$filename_date <- str_remove(datafilename$filename, "_P02_settings.csv")
datafilename$filename_date <- str_remove(datafilename$filename_date, "C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/settings_p02/film-shaved-aurora_")
datafilename <- datafilename[,-2]

view(datafilename)

#search for matches in pattern in the settings and data files

# conditionslist <- list()
# 
# new_table <- tibble()
# ctr <- 1
# for (this_setting_time in datafilename$filename_date){
#   temp_list <- list()
#   bools <- grepl(this_setting_time, DataFiles)
#   for (val in bools) {
#     if (val == TRUE) {
#       temp_list <-append(temp_list,datafilename[ctr,1])
#     }
#   }
#   if (length(temp_list) == 0) {print("OH NO!! FILES ARE MISSING")}
#   conditionslist <- append(conditionslist,temp_list)
#   ctr <- ctr +1
# }
# DataFiles <- cbind(DataFiles, conditionslist)

fixedconditions <- vector("list", length(DataFiles))
new_table <- tibble()
ctr <- 1
for (this_setting_time in datafilename$filename_date){
  bools <- grepl(this_setting_time, DataFiles)
  index_list <- which(bools==TRUE)
  condition <- datafilename[ctr,1]
  
  ctr <- ctr +1
}
DataFiles <- cbind(DataFiles, conditionslist)
  