library(signal)
library(tidyverse)
library(patchwork)
library(stringr)
source('aurora functions.R')
theme_set(theme_bw())

#### this section specific to the data set ####

#dataFolder <- 'C:/Users/sarmc72/OneDrive - Linköpings universitet/projects - in progress/Peripheral speed force/MNG experiment/Aurora data/'

dataFolder <- 'C:/Users/emmku74/Desktop/01_ddf/'
dataFolder <- 'C:/Experiments/DDF/01_ddf/'
dataFolder <- 'C:/Users/emmku74/Desktop/New folder/'
dataFolder <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/DDF/'
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

#### ---- ####

outputFolder <- './Processed Data/'
timenow <- format(Sys.time(), '%Y%m%d_%H%M%S')
outputDataFile <- paste0(outputFolder, 'force_data_',timenow,'.txt')
outputPlotFolder <- paste0(outputFolder,'Force Plots ',timenow,'/')
outputTracesFolder <- paste0(outputFolder,'Force Overlay ',timenow,'/')

overlayData <- tibble()
for (n in seq_along(forceDataFiles)) {
  # for (n in 211:356) {
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
    mutate(targetForce.mN = ptcl$targetForce.mN,
           targetRampTime.ms = ptcl$rampOnDuration.ms,
           positionLimit.mm = ptcl$lengthLimit.mm,
           sourceFile = ddfFile %>% str_replace(dataFolder,'')
    ) 
  
  tare <- summaryData %>% 
    dplyr::filter(phase == 'pre-stim') %>% 
    select(meanForce.mN, meanPosition.mm)
  
  overlayData <- bind_rows(overlayData,
                           scaledData %>% 
                             mutate(Force.mN = ForceFiltered.mN - tare$meanForce.mN,
                                    Length.mm = LengthFiltered.mm - tare$meanPosition.mm) %>% 
                             select(Time.ms, Force.mN, Length.mm) %>% 
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
  #   str_replace('\\.ddf', '\\.tiff')
  # 
  # if (!dir.exists(file.path(dirname(plotFile))) ) dir.create(file.path(dirname(plotFile)), recursive = TRUE)
  # ggsave(plotFile)
  # print(paste('saved figure:',plotFile))
  # 
  # dev.off()
}

#read about group_by and see if you can  
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

for (ramp_n in seq_along(ramps)) {
  # ramp_n <- 2
  # plotlist = list()
  for (force_n in seq_along(forces)) {
    plotData <- overlayData %>% 
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

