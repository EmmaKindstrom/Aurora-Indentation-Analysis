library(signal)
library(tidyverse)
library(patchwork)
source('aurora functions.R')
theme_set(theme_bw())

dataFolder <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/DDF/02_ddf/'
DataFiles <- list.files(dataFolder, 'ddf', recursive = TRUE)

settingsFolder <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/settings_p02/'
allSettingFiles <- list.files(settingsFolder, 'settings', full.names = TRUE)

df <- tibble()

for (currentsettingfile in allSettingFiles) {
    currentsettingdata <- read_csv(currentsettingfile, col_names =  c("information", "data"), show_col_types = FALSE)%>% 
    mutate(filename = currentsettingfile)
    df = rbind(df, currentsettingdata)
}

filtersetting <- tibble()
filtersetting <- df %>% dplyr::filter(information == "07. Intervention (film/shaved)")

datafilename <- tibble(data = filtersetting$data, filename = filtersetting$filename)
datafilename <- filtersetting[,-1]
view(datafilename)

#removes part of the setting file name and leaves the time stamp
datafilename$filename_date <- str_remove(datafilename$filename, "_P02_settings.csv")
datafilename$filename_date <- str_remove(datafilename$filename_date, "C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/settings_p02/film-shaved-aurora_")
datafilename <- datafilename[,-2]

view(datafilename)

#create a list with the same length as DataFiles to later insert the condition to the DataFile
fixedconditions <- vector(mode = 'character',length(DataFiles))

#index to retrieve the condition from the setting files 
ctr <- 1
for (this_setting_time in datafilename$filename_date){
  #grepl returns list of booleans when it finds a match between the data file name and the timestamp from the settingsfilename
  #The bools list should be as long as the length of DataFiles
  bools <- grepl(this_setting_time, DataFiles)
  #Find indexes where a match was found 
  index_list <- which(bools==TRUE)
  #Get the condition from the settings file, ctr will be = amount of datafilename rows
  condition <- datafilename[ctr,1]
  # for each position in the index_list where a match was found, insert the right condition to fixedconditions list
  for (idx in index_list) {
    fixedconditions[idx] <- condition
  }
  ctr <- ctr +1
}
#The fixed conditions should be paired with the correct DataFiles positions (index)
#cbind will join the datafile name and the condition in the same table in two columns (coloumn bind = cbind)
DataFiles_conditions <- cbind(DataFiles, fixedconditions)

  