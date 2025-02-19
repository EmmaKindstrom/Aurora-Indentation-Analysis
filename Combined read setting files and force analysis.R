library(signal)
library(tidyverse)
library(patchwork)
library(stringr)
source('aurora functions.R')
theme_set(theme_bw())
#### this section specific to the data set ####

# Reading in all ddf files
dataFolder <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/DDF/'
#dataFolder <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/DDF/12_ddf/'
allDataFiles <- list.files(dataFolder, 'ddf', recursive = TRUE)

forceDataFiles <- allDataFiles

### This section reads in all setting files and matches the condition with corresponding ddf file ###
settingsFolder <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/settings/'
#settingsFolder <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/settings_p12/'
allSettingFiles <- list.files(settingsFolder, 'settings', full.names = TRUE)

df <- tibble()

for (currentsettingfile in allSettingFiles) {
  currentsettingdata <- read_csv(currentsettingfile, col_names =  c("information", "data"), show_col_types = FALSE)%>% 
    mutate(filename = currentsettingfile)
  df = rbind(df, currentsettingdata)
}

filtersetting <- tibble()
filtersetting <- df %>% dplyr::filter(information == "07. Intervention (film/shaved)")

datafilename <- filtersetting[,-1]
#view(datafilename)

#removes part of the setting file name and leaves the time stamp
datafilename$filename_date <- str_remove(datafilename$filename, "_P[0-9]{2}_settings.csv")
datafilename$filename_date <- str_remove(datafilename$filename_date, "_P[0-9]{2}_film_settings.csv")
datafilename$filename_date <- str_remove(datafilename$filename_date, "_P[0-9]{2}_shaved_settings.csv")
datafilename$filename_date <- str_remove(datafilename$filename_date, "C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/settings/film-shaved-aurora_")
datafilename <- datafilename[,-2]

#view(datafilename)


#create a list with the same length as DataFiles to later insert the condition to the DataFile
fixedconditions <- vector(mode = 'character',length(forceDataFiles))

#index to retrieve the condition from the setting files 
ctr <- 1
for (this_setting_time in datafilename$filename_date){
  #grepl returns list of Booleans when it finds a match between the data file name and the timestamp from the settingsfilename
  #The bools list should be as long as the length of DataFiles
  bools <- grepl(this_setting_time, forceDataFiles)
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
DataFiles_conditions <- cbind(forceDataFiles, fixedconditions)

view(DataFiles_conditions)

#### ---- ####

outputFolder <- './Processed Data/'
timenow <- format(Sys.time(), '%Y%m%d_%H%M%S')
summaryDataFile <- paste0(outputFolder, 'force_summary_data_',timenow,'.txt')
overlayDataFile <- paste0(outputFolder, 'force_overlay_data_',timenow,'.txt')
outputPlotFolder <- paste0(outputFolder,'Force Plots ',timenow,'/')


# Plot raw data frrom a single file (force trace)
# weirddataFolder <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/Excluded_ddf/'
# ddfFile <- paste0(dataFolder,"06_ddf/--film-shaved-aurora_2022-02-25_15-59-16_P06_800.0_10_25.ddf")
# ddfFile <- paste0(weirddataFolder,"--film-shaved-aurora_2022-03-10_10-09-27_P17_film_400.0_20_73.ddf")
# ddfFile <- paste0(weirddataFolder,"--film-shaved-aurora_2022-03-10_10-09-27_P17_film_600.0_10_73.ddf")
# checkData <- ddfFile %>% read_delim('\t', skip = 25, col_types = paste0(rep('d',12), collapse = ''))
# checkData %>%
#   ggplot(aes(x = Sample, y = AI1)) +
#   geom_line() +
#   labs(x = NULL, y = NULL)


overlayData <- tibble()
# for (n in 1:1000) {
for (n in seq_along(forceDataFiles)) {
  ddfFile <- paste0(dataFolder,forceDataFiles[n])
  
  
  print(paste(n, 'of', length(forceDataFiles), ':', ddfFile))
  
  # read the protocol info
  ptcl <- ddfFile %>% read_force_protocol(skip = 16, n_max = 5)
  
  # read the raw data
  ddfData <- ddfFile %>% read_delim('\t', skip = 25, col_types = paste0(rep('d',12), collapse = ''))
  #ddfData %>% plot_ddf_data()
  
  # make noise filter
  SamplingRate <- read_lines(ddfFile, skip = 1, n_max = 1) %>% parse_number()
  FilterFreqCutOff <- ceiling(10^(1.4 + 0.6*log10( 1000/ptcl$rampOnDuration.ms))) 
  butterworthFilter <- butter(n = 2, W = FilterFreqCutOff/SamplingRate, type = "low")
  
  # data in real units, filtered data, and time derivative
  scaleUnits <- read_lines(ddfFile, skip = 8, n_max = 1) %>% 
    str_split('\t') %>% .[[1]] %>% .[-1] %>% parse_double()
  
  scaledData <- ddfData %>% 
    scale_and_filter(scaleUnits, SamplingRate, butterworthFilter) %>% 
    dplyr::filter(Time.ms %>% between(ptcl$stimWindowStart, ptcl$stimWindowEnd))
  
  # automatically find where the force ramps are
  forceRateThreshold <- 0.1*(ptcl$targetForce.mN/ptcl$rampOnDuration.ms)
  forceRamps <- find_ramps(scaledData, ptcl, ForceDeriv.Nps, forceRateThreshold)
  
  # save summary data
  summaryData <- scaledData %>%
    summarise_data(ramps = forceRamps) %>% 
    mutate(condition = fixedconditions[[n]],
      targetForce.mN = ptcl$targetForce.mN,
           targetRampTime.ms = ptcl$rampOnDuration.ms,
           positionLimit.mm = ptcl$lengthLimit.mm,
           sourceFile = ddfFile %>% str_replace(dataFolder,'')
    ) 
  
   
  tare <- summaryData %>% 
    dplyr::filter(phase == 'pre-stim') %>% 
    select(meanForce.mN, meanPosition.mm)
  
  overlayData <- bind_rows(overlayData,
                           scaledData %>% 
                             mutate(
                               condition = fixedconditions[[n]],
                               Force.mN = ForceFiltered.mN - tare$meanForce.mN,
                               Length.mm = LengthFiltered.mm - tare$meanPosition.mm
                               ) %>% 
                             select(Time.ms, Force.mN, Length.mm, condition) %>% 
                             mutate(targetForce.mN = ptcl$targetForce.mN,
                                    targetRampTime.ms = ptcl$rampOnDuration.ms,
                                    sourceFile = ddfFile %>% 
                                      str_replace(dataFolder,'')
                                    
                             )
  )
  

  summaryData %>%
    write_delim(paste0(summaryDataFile), '\t', append = n>1)
  print(paste("added data to", summaryDataFile))
  
  #####-#####

  # windows()
  # 
  # scaledData %>%
  #   ggplot(aes(x = Time.ms)) +
  #   geom_vline(xintercept = c(ptcl$rampOn,ptcl$hold,ptcl$rampOff,ptcl$endStim), colour = 'grey') +
  #   geom_vline(xintercept = unlist(forceRamps), colour = 'red') +
  #   geom_point(aes(y = ForceMeasured.mN), shape = 21, fill = 'black', alpha = 0.1, size = 3) +
  #   geom_point(aes(y = ForceFiltered.mN), colour = 'blue', size = 1) +
  #   labs(title = 'Force trace', x = 'Time (ms)', y = 'Force (mN)') -> force.trace
  # 
  # scaledData %>%
  #   ggplot(aes(x = Time.ms)) +
  #   geom_vline(xintercept = unlist(forceRamps), colour = 'red') +
  #   geom_point(aes(y = ForceDeriv.Nps), colour = 'blue', size = 1) +
  #   labs(title = 'Force derivative', x = 'Time (ms)', y = 'Force rate (N/s)') -> force.deriv
  # 
  # scaledData %>%
  #   ggplot(aes(x = Time.ms)) +
  #   geom_vline(xintercept = unlist(forceRamps), colour = 'red') +
  #   geom_point(aes(y = LengthMeasued.mm), shape = 21, fill = 'black', alpha = 0.1, size = 3) +
  #   geom_point(aes(y = LengthFiltered.mm), colour = 'purple', size = 1) +
  #   labs(title = paste('Displacement trace, limit =', ptcl$lengthLimit.mm, 'mm'),
  #        x = 'Time (ms)', y = 'Displacement (mm)') -> disp.trace
  # 
  # scaledData %>%
  #   ggplot(aes(x = Time.ms)) +
  #   geom_vline(xintercept = unlist(forceRamps), colour = 'red') +
  #   geom_point(aes(y = LengthDeriv.mps), colour = 'purple', size = 1) +
  #   labs(title = 'Displacement derivative', x = 'Time (ms)', y = 'Velocity (m/s)') -> disp.deriv
  # 
  # force.trace / force.deriv / disp.trace / disp.deriv +
  #   plot_annotation(title = paste('Target =', ptcl$targetForce.mN,'mN.',
  #                                 ptcl$rampOnDuration.ms,'ms ramp.',
  #                                 'Low pass butterworth filter',FilterFreqCutOff,'Hz'))
  # 
  # plotFile <- ddfFile %>%
  #   str_replace(dataFolder,'') %>%
  #   str_replace_all('/','_') %>%
  #   paste0(outputPlotFolder,.) %>%
  #   str_replace('\\.ddf', '\\.tiff')
  # 
  # if (!dir.exists(file.path(dirname(plotFile))) ) dir.create(file.path(dirname(plotFile)), recursive = TRUE)
  # ggsave(plotFile)
  # print(paste('saved figure:',plotFile))
  # 
  # dev.off()
}


overlayData %>%
  write_delim(paste0(overlayDataFile), '\t')
print(paste("saved", overlayDataFile))
