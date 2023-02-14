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

# this excludes bad data files and also sorts into length and force based on text in the file names
sortedDataFiles <- tibble(filename = allDataFiles,
                          type = case_when(
                            !str_detect(filename,'(length)|(Velocity)|(bad)|(dont use)|([0-9]v)') ~ 'force',
                            !str_detect(filename,'(force)|(Force)|(bad)|(dont use)') ~ 'length'
                          )
)

# if you edit the above, you need to end up with something called forceDataFiles, which is the list of data files you want to process
forceDataFiles <- sortedDataFiles %>% 
  dplyr::filter(type == 'force') %>% pull(filename)

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
outputDataFile <- paste0(outputFolder, 'force_data_',timenow,'.txt')
outputPlotFolder <- paste0(outputFolder,'Force Plots ',timenow,'/')
outputTracesFolder <- paste0(outputFolder,'Force Overlay ',timenow,'/')


overlayData <- tibble()
# for (n in seq_along(forceDataFiles)) {
for (n in 1:1000) {
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
  
  #Sets the threshold for the values that should be excluded from the plot (define outliers)
  UpperThreshold <- (ptcl$targetForce.mN*1.1)
  LowerThreshold <- (ptcl$targetForce.mN*0.9)
  
  peakhold <- summaryData$peakForce.mN[3]
  minhold  <- summaryData$minimumForce.mN[3]
  
  outliers = if_else(
    between(peakhold, LowerThreshold, UpperThreshold) & 
      between(minhold, LowerThreshold, UpperThreshold), 
    "include", 
    "exclude", 
    "missing"
    )
  
  tare <- summaryData %>% 
    dplyr::filter(phase == 'pre-stim') %>% 
    select(meanForce.mN, meanPosition.mm)
  
  overlayData <- bind_rows(overlayData,
                           scaledData %>% 
                             mutate(
                               condition = fixedconditions[[n]],
                               Force.mN = ForceFiltered.mN - tare$meanForce.mN,
                               Length.mm = LengthFiltered.mm - tare$meanPosition.mm,
                               outliers = outliers
                               ) %>% 
                             select(Time.ms, Force.mN, Length.mm, condition, outliers) %>% 
                             mutate(targetForce.mN = ptcl$targetForce.mN,
                                    targetRampTime.ms = ptcl$rampOnDuration.ms,
                                    sourceFile = ddfFile %>% 
                                      str_replace(dataFolder,'')
                                    
                             )
  )
  

  summaryData %>%
    write_delim(paste0(outputDataFile), '\t', append = n>1)
  print(paste("added data to", outputDataFile))
  
  
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
  #   str_replace('ddf', 'tiff')
  # 
  # if (!dir.exists(file.path(dirname(plotFile))) ) dir.create(file.path(dirname(plotFile)), recursive = TRUE)
  # ggsave(plotFile)
  # print(paste('saved figure:',plotFile))
  # 
  # dev.off()
}

# see what files we read in
overlayData %>% 
  group_by(sourceFile) %>% tally()

unique(overlayData$sourceFile)

rampforcecombo <- overlayData %>% 
  group_by(targetForce.mN,targetRampTime.ms,sourceFile) %>%
  tally() %>% 
  xtabs( ~ targetForce.mN + targetRampTime.ms, .)

ramps <- sort_unique(overlayData$targetRampTime.ms)
forces <- sort_unique(overlayData$targetForce.mN)
# PIDs <- overlayData$sourceFile %>%
#   str_extract("_P[0-9]+_") %>% 
#   str_remove_all("_") %>% 
#   sort_unique()

#change include to exclude to plot both datasets (compare to find a good upper and lower threshold)
for (ramp_n in seq_along(ramps)) {
  # ramp_n <- 2
  # plotlist = list()
  for (force_n in seq_along(forces)) {
    plotData <- overlayData %>% 
      dplyr::filter(targetRampTime.ms == ramps[ramp_n] & targetForce.mN == forces[force_n] & outliers == "include") %>% 
      mutate(
        nfiles = n_distinct(sourceFile),
        targetForceLabel = paste0('t=',targetForce.mN,'mN, n=',nfiles),
        
        PID = sourceFile %>% 
          str_extract("_P[0-9]+_") %>% 
          str_remove_all("_")
      )
    
    current_force_str <- toString(forces[force_n])
    current_ramp_str <- toString(ramps[ramp_n])
    print("...")
    print(paste0("force", current_force_str))
    print(paste0("ramp", current_ramp_str))
    if (rampforcecombo[current_force_str, current_ramp_str] > 0) {
      
      force.trace <- plotData %>%
        ggplot(aes(x = Time.ms/1000, y = Force.mN)) +
        facet_wrap(PID ~ targetForceLabel, ncol=9) +
        geom_line(aes(group = sourceFile, colour = condition), size = 0.5, alpha = 0.4) +
        labs(x = NULL, y = NULL)
      if (force_n==1) force.trace <- force.trace + labs(y = 'Force (mN)') 
      
      pos.trace <- plotData %>%
        ggplot(aes(x = Time.ms/1000, y = Length.mm)) +
        facet_wrap(PID ~ targetForceLabel, ncol=9) +
        geom_line(aes(group = sourceFile, colour = condition), size = 0.5, alpha = 0.4) +
        labs(x = 'Time (sec)', y = NULL)
      if (force_n==1) pos.trace <- pos.trace + labs(y = 'Position (mm)')
      
      print("adding to list")
      windows(20, 11)
      (force.trace / pos.trace)  + 
        plot_annotation(title = paste0('Ramp = ',ramps[ramp_n],'ms'),
                        caption = paste0('Overlaid force traces (top) and position traces (bottom) with ramp time = ',ramps[ramp_n],'ms'))
      
      plotFile <- paste0(outputTracesFolder,'Force overlay ',forces[force_n],'mN.tiff')
      if (!dir.exists(file.path(dirname(plotFile))) ) dir.create(file.path(dirname(plotFile)), recursive = TRUE)
      ggsave(plotFile)
      dev.off()
    }
  }
}

