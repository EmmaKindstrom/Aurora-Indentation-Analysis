library(signal)
library(tidyverse)
library(patchwork)
source('aurora functions.R')
theme_set(theme_bw())

# dataFolder <- "~/Library/CloudStorage/OneDrive-SharedLibraries-Linköpingsuniversitet/Emma Kindström - DDF/"
dataFolder <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/DDF/18_ddf/'
allDataFiles <- list.files(dataFolder, 'ddf', recursive = TRUE)
forceDataFiles <- tibble(filename = allDataFiles) %>% pull(filename)

outputFolder <- 'Processed Data/'
timenow <- format(Sys.time(), '%Y%m%d_%H%M%S')
outputDataFile <- paste0(outputFolder, 'force_data_',timenow,'.txt')
outputPlotFolder <- paste0(outputFolder,'Force Plots ',timenow,'/')
outputTracesFolder <- paste0(outputFolder,'Force Overlay ',timenow,'/')


overlayData <- tibble()
for (n in seq_along(forceDataFiles)) {
# for (n in 21:30) {
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
  
  # use the protocol times to divide into different phases
  protocolPhases <- list(
    OnStart = ptcl$rampOn,
    OnStop = ptcl$hold,
    OffStart = ptcl$rampOff,
    OffStop = ptcl$endStim
  )
    
  # save summary data
  summaryData <- scaledData %>%
    summarise_data(ramps = protocolPhases) %>% 
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
    write_delim(outputDataFile, '\t', append = n>1)
  print(paste("added data to", outputDataFile))
  
    
  windows()
  #quartz()
  
  scaledData %>%
    ggplot(aes(x = Time.ms)) +
    geom_vline(xintercept = unlist(protocolPhases), colour = 'orange') +
    geom_point(aes(y = ForceMeasured.mN), shape = 21, fill = 'black', alpha = 0.1, size = 3) +
    geom_point(aes(y = ForceFiltered.mN), colour = 'blue', size = 1) +
    labs(title = 'Force trace', x = NULL, y = 'Force (mN)') -> force.trace
 
  scaledData %>%
    ggplot(aes(x = Time.ms)) +
    geom_vline(xintercept = unlist(protocolPhases), colour = 'orange') +
    geom_point(aes(y = ForceDeriv.Nps), colour = 'blue', size = 1) +
    labs(title = 'Force derivative', x = NULL, y = 'Force rate (N/s)') -> force.deriv
  
  scaledData %>%
    ggplot(aes(x = Time.ms)) +
    geom_vline(xintercept = unlist(protocolPhases), colour = 'orange') +
    geom_point(aes(y = ForceDeriv2.Npss), colour = 'blue', size = 1) +
    labs(title = 'Force 2nd derivative', x = NULL, y = 'Force acceleration (N/s^2)') -> force.deriv2

  scaledData %>%
    ggplot(aes(x = Time.ms)) +
    geom_vline(xintercept = unlist(protocolPhases), colour = 'orange') +
    geom_point(aes(y = LengthMeasued.mm), shape = 21, fill = 'black', alpha = 0.1, size = 3) +
    geom_point(aes(y = LengthFiltered.mm), colour = 'purple', size = 1) +
    labs(title = paste('Displacement trace, limit =', ptcl$lengthLimit.mm, 'mm'),
         x = NULL, y = 'Displacement (mm)') -> disp.trace
  
  scaledData %>%
    ggplot(aes(x = Time.ms)) +
    geom_vline(xintercept = unlist(protocolPhases), colour = 'orange') +
    geom_point(aes(y = LengthDeriv.mps), colour = 'purple', size = 1) +
    labs(title = 'Displacement derivative', x = NULL, y = 'Velocity (m/s)') -> disp.deriv
  
  scaledData %>%
    ggplot(aes(x = Time.ms)) +
    geom_vline(xintercept = unlist(protocolPhases), colour = 'orange') +
    geom_point(aes(y = LengthDeriv2.mpss), colour = 'purple', size = 1) +
    labs(title = 'Displacement 2nd derivative', x = 'Time (ms)', y = 'Acceleration (m/s^2)') -> disp.deriv2
  
  force.trace / force.deriv / force.deriv2 / disp.trace / disp.deriv / disp.deriv2 +
    plot_annotation(title = paste('Target =', ptcl$targetForce.mN,'mN.',
                                  ptcl$rampOnDuration.ms,'ms ramp.',
                                  'Low pass butterworth filter',FilterFreqCutOff,'Hz'))
  
  plotFile <- ddfFile %>%
    str_replace(dataFolder,'') %>%
    str_replace_all('/','_') %>%
    paste0(outputPlotFolder,.) %>%
    str_replace('\\.ddf', '\\.tiff')
  
  if (!dir.exists(file.path(dirname(plotFile))) ) dir.create(file.path(dirname(plotFile)), recursive = TRUE)
  ggsave(plotFile, width = 10, height = 10)
  print(paste('saved figure:',plotFile))
  
  dev.off()
}

### have this v ####

overlayData %>% 
  group_by(targetForce.mN,targetRampTime.ms,sourceFile) %>% 
  tally() %>% 
  xtabs( ~ targetForce.mN + targetRampTime.ms, .)

ramps <- sort_unique(overlayData$targetRampTime.ms)
forces <- sort_unique(overlayData$targetForce.mN)


for (rampN in seq_along(ramps)) {
  plotlist = list()
  for (forceN in seq_along(forces)) {
    plotData <- overlayData %>% 
      dplyr::filter(targetRampTime.ms == ramps[rampN] & targetForce.mN == forces[forceN]) %>% 
      mutate(nfiles = n_distinct(sourceFile),
             targetForceLabel = paste0('t=',targetForce.mN,'mN, n=',nfiles))
    
    force.trace <- plotData %>%
      ggplot(aes(x = Time.ms/1000, y = Force.mN)) +
      facet_wrap( ~ targetForceLabel) +
      geom_line(aes(group = sourceFile), size = 0.5, alpha = 0.4) +
      labs(x = NULL, y = NULL)
    if (forceN==1) force.trace <- force.trace + labs(y = 'Force (mN)') 
    
    pos.trace <- plotData %>%
      ggplot(aes(x = Time.ms/1000, y = Length.mm)) +
      facet_wrap( ~ targetForceLabel) +
      geom_line(aes(group = sourceFile), size = 0.5, alpha = 0.4) +
      labs(x = 'Time (sec)', y = NULL)
    if (forceN==1) pos.trace <- pos.trace + labs(y = 'Position (mm)')
    
    plotlist[[forceN]] = (force.trace / pos.trace)
  }
  
  windows(17.5,4.5)
  wrap_plots(plotlist, ncol = length(forces)) + 
    plot_annotation(title = paste0('Ramp = ',ramps[rampN],'ms'),
                    caption = paste0('Overlaid force traces (top) and position traces (bottom) for different target forces (columns) with ramp time = ',ramps[rampN],'ms'))
  
  plotFile <- paste0(outputTracesFolder,'Force overlay ',ramps[rampN],'ms ramp.tiff')
  if (!dir.exists(file.path(dirname(plotFile))) ) dir.create(file.path(dirname(plotFile)), recursive = TRUE)
  ggsave(plotFile)
  dev.off()
}

