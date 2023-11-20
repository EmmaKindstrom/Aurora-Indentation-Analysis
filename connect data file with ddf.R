library(tidyverse)
library(signal)
library(tidyverse)
library(patchwork)
library(stringr)

summaryHold <- read_tsv("20230220_120823_summaryInclusions.txt")


#Retrieve the time stamp from the source file
summaryHold$sourceFile_time <- str_extract(summaryHold$sourceFile, "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}")

#Retrieve the trail number form the source file 
summaryHold$trial_number <- str_remove(summaryHold$sourceFile, "[0-9]{2}_ddf/--film-shaved-aurora_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}_P[0-9]{2}_")
summaryHold$trial_number <- str_remove(summaryHold$trial_number, "shaved_")
summaryHold$trial_number <- str_remove(summaryHold$trial_number, "film_")
summaryHold$trial_number <- str_extract(summaryHold$trial_number, "_[0-9]{2,4}_")
summaryHold$trial_number <- str_remove(summaryHold$trial_number, "_")
summaryHold$trial_number <- str_remove(summaryHold$trial_number, "0_")

#Adjust data type to numeric
summaryHold$trial_number <- as.numeric(summaryHold$trial_number)

#divide trial number by two to match data.csv files
summaryHold$trial_number <- summaryHold$trial_number/2
summaryHold$trial_number <- ceiling(summaryHold$trial_number)

# read in data.csv
csvFolder <- 'C:/Users/emmku74/Documents/GitHub/Aurora-indentation-Analysis/Data_settings/'
allCsvFiles <- list.files(csvFolder, 'data', recursive = TRUE, full.names = TRUE)

csv_files <- tibble()

for (currentcsvfile in allCsvFiles) {
  currentcsvdata <- read_csv(currentcsvfile, show_col_types = FALSE)%>% 
    mutate(filename = currentcsvfile)
  csv_files = rbind(csv_files, currentcsvdata)
}

csv_files$filename <- str_remove(csv_files$filename, "C:/Users/emmku74/Documents/GitHub/Aurora-indentation-Analysis/Data_settings/film-shaved-aurora_")

csv_files$sourceFile_time <- str_extract(csv_files$filename, "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}")
csv_files$PID <- str_extract(csv_files$filename, "P[0-9]{2}")

#Change name of the column to match summaryHold
colnames(csv_files)[1] <- "trial_number"

#merge lists (.ddf and .csv)
summaryHoldCsv <- tibble()
summaryHoldCsv <- full_join(summaryHold, csv_files)


