library(signal)
library(tidyverse)
library(patchwork)
source('aurora functions.R')
theme_set(theme_bw())


dataFolder <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/DDF/'
allDataFiles <- list.files(dataFolder, 'ddf', recursive = TRUE)

settingsFolder <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/settings/'
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
str_detect(datafilename[,2], overlayData[,6])
