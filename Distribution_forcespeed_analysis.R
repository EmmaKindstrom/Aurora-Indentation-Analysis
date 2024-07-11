### distribution of maximum force/force rate ### 

library(rlang)
library(quickpsy)
library(dplyr)
library(stringr)
library(ggplot2)
source('aurora functions.R')


DistributionDataBad <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/Excluded_ddf/bad/'
DistributionDataGood <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/DDF/'

DataFiles <-  c(DistributionDataBad, DistributionDataGood)
AllDistributionDataFiles <- list.files(DataFiles, 'ddf', recursive = TRUE)

  
# this sorts into length and force based on text in the file names 
sortedDataFiles <- tibble(filename = AllDistributionDataFiles,
       type = case_when(
          !str_detect(filename,'(length)|(Velocity)|(bad)|(dont use)|([0-9]v)') ~ 'force',
          !str_detect(filename,'(force)|(Force)|(bad)|(dont use)') ~ 'length'
    )
)

 
# sortedDataFiles - list of data files you want to process
forceDdfFiles <- sortedDataFiles %>% 
  dplyr::filter(type == 'force') %>% pull(filename)
  
#### ---- ####


outputFolder <- './Processed SD Data/'
timenow <- format(Sys.time(), '%Y%m%d_%H%M%S')
outputDataFile <- paste0(outputFolder, 'SD_force_data_',timenow,'.txt')
outputPlotFolder <- paste0(outputFolder,'SD_Force Plots ',timenow,'/')
outputTracesFolder <- paste0(outputFolder,'SD_Force Overlay ',timenow,'/')
  
  
overlaySDData <- tibble()
for (n in seq_along(forceDdfFiles)) {
  # for (n in 211:356) {
  ddfFile <- paste0(DataFiles,forceDdfFiles[n])
  
  print(paste(n, 'of', length(forceDdfFiles), ':', ddfFile))
  
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
    mutate(targetForce.mN = ptcl$targetForce.mN,
           targetRampTime.ms = ptcl$rampOnDuration.ms,
           positionLimit.mm = ptcl$lengthLimit.mm,
           sourceFile = ddfFile %>% str_replace(DataFiles,'')
    ) 
  
  tare <- summaryData %>% 
    dplyr::filter(phase == 'pre-stim') %>% 
    select(meanForce.mN, meanPosition.mm)
  
  overlaySDData <- bind_rows(overlaySDData,
                           scaledData %>% 
                             mutate(Force.mN = ForceFiltered.mN - tare$meanForce.mN,
                                    Length.mm = LengthFiltered.mm - tare$meanPosition.mm) %>% 
                             select(Time.ms, Force.mN, Length.mm) %>% 
                             mutate(targetForce.mN = ptcl$targetForce.mN,
                                    targetRampTime.ms = ptcl$rampOnDuration.ms,
                                    sourceFile = ddfFile %>% 
                                      str_replace(DataFiles,'')
                             )
  )
  
  summaryData %>%
    write_delim(paste0(outputDataFile), '\t', append = n>1)
  print(paste("added data to", outputDataFile))  
}  

##################

rampforcecombo <- overlaySDData %>% 
  group_by(targetForce.mN,targetRampTime.ms,sourceFile) %>%
  tally() %>% 
  xtabs( ~ targetForce.mN + targetRampTime.ms, .)
  
ramps <- sort_unique(overlaySDData$targetRampTime.ms)
forces <- sort_unique(overlaySDData$targetForce.mN)
# PIDs <- overlaySDData$sourceFile %>%
#   str_extract("_P[0-9]+_") %>% 
#   str_remove_all("_") %>% 
#   sort_unique()
  
for (ramp_n in seq_along(ramps)) {
  # ramp_n <- 2
  # plotlist = list()
  for (force_n in seq_along(forces)) {
    plotData <- overlaySDData %>% 
      dplyr::filter(targetRampTime.ms == ramps[ramp_n] & targetForce.mN == forces[force_n]) %>% 
      mutate(
        nfiles = n_distinct(sourceFile),
        targetForceLabel = paste0('t=',targetForce.mN,'mN, n=',nfiles),
        condition = sourceFile %>% 
          str_extract("_(film)|(shaved)_") %>% 
          str_remove_all("_"),
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
  
  
  











#### kladd ####

  
data <- c()
for (currentfile in AllDistributionDataFiles) {
  print(currentfile)
  
  s
}
  
  
  
  
  Distributiongood.list <- list.files(
  'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/DDF/',
  pattern = 'ddf', 
  recursive = TRUE,
  full.names = TRUE)

DistributionAll.list <- c(Distributionbad.list, Distributiongood.list)


DistributionData <- c()
for(currentfile in DistributionAll.list) {
  print(currentfile)
  
  read.csv(DistributionAll.list)
}








data <- c()
for(currentfile in filelist) {
  # which one are we up to
  print(currentfile)
  
  # find settings file
  settingsfile <- str_replace(currentfile,"data.csv","settings.csv")
  
  # extract condition from settings file
  extracted.condition <- read.csv(settingsfile)[7,2]
  
  # extract info from filename
  extracted.PID <- str_extract(currentfile, "(P[0-9]{2})")
  
  # read data file and add columns with extracted info
  currentdata <- read.csv(currentfile) %>% 
    mutate(PID = extracted.PID,
           condition = extracted.condition,
           filename = currentfile)
  # add the new data to the end of the data frame
  data <- rbind(data, currentdata)
  rm(currentdata)
}
