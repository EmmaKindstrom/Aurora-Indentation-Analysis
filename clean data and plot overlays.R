library(signal)
library(tidyverse)
library(patchwork)
library(stringr)
source('aurora functions.R')
theme_set(theme_bw())


processedDataFolder <- './Processed Data/'
processTime <- "20230220_120823"
overlayData <- read_tsv(paste0(processedDataFolder, "force_overlay_data_",processTime,".txt"))
summaryData <- read_tsv(paste0(processedDataFolder, "force_summary_data_",processTime,".txt"))

#combinedData <- full_join(overlayData, summaryData) 

outputTracesFolder <- paste0(processedDataFolder,'Force Overlay ',processTime,'/')

# see what files we read in
overlayData %>% 
  group_by(sourceFile) %>% tally()

unique(overlayData$sourceFile)

summaryData %>%
  filter(phase == "hold") %>% 
  ggplot(mapping = aes(x = heldForce.mN/targetForce.mN)) +
  geom_histogram(binwidth = 0.1)

summaryData %>%
  filter(phase == "hold") %>% 
  filter(heldForce.mN/targetForce.mN > 1.1)

#Sets the threshold for the values that should be excluded from the plot (define outliers)

summaryHold <- summaryData %>% 
  filter(phase == "hold") %>% 
  mutate(
    UpperThreshold = targetForce.mN*1.1,
    LowerThreshold = targetForce.mN*0.9
  ) %>% 
  mutate(
    outliers = if_else(
      peakForce.mN > LowerThreshold & peakForce.mN < UpperThreshold & 
      minimumForce.mN > LowerThreshold & minimumForce.mN < UpperThreshold &
      heldForce.mN > LowerThreshold & heldForce.mN < UpperThreshold,
    # between(heldForce.mN, LowerThreshold, UpperThreshold), 
  "include", 
  "exclude", 
  "missing"
    )
  )

#Change includeTraces/excludeTraces depending on what to plot
includeTraces <- summaryHold %>% 
  filter(outliers == "include") %>% 
  pull(sourceFile)


overlayData_include <- overlayData %>% 
  filter( !sourceFile %in% includeTraces )

overlayData_include <- overlayData %>% 
  filter( sourceFile %in% includeTraces )

FilesToCheck <- summaryHold %>% 
  filter(outliers == "include" & targetForce.mN == 1200) %>%  pull(sourceFile)

summaryHold%>% 
  dplyr::filter(sourceFile == FilesToCheck[4]) %>% 
  View()

checkData <- overlayData_include %>% 
  dplyr::filter(sourceFile == FilesToCheck[4]) %>% 
  mutate(
    nfiles = n_distinct(sourceFile),
    targetForceLabel = paste0('t=',targetForce.mN,'mN, n=',nfiles),
    
    PID = sourceFile %>% 
      str_extract("_P[0-9]+_") %>% 
      str_remove_all("_")
  )

  
checkData %>%
    ggplot(aes(x = Time.ms/1000, y = Force.mN)) +
    #facet_wrap(PID ~ targetForceLabel, ncol=9) +
    geom_line(aes(group = sourceFile, colour = condition), size = 0.5, alpha = 0.4) +
    labs(x = NULL, y = NULL)



#define if you want to plot included or excluded values
rampforcecombo <- overlayData_include %>% 
  group_by(targetForce.mN,targetRampTime.ms,sourceFile) %>%
  tally() %>% 
  xtabs( ~ targetForce.mN + targetRampTime.ms, .)

ramps <- sort_unique(overlayData_include$targetRampTime.ms)
forces <- sort_unique(overlayData_include$targetForce.mN)
# PIDs <- overlayData_exclude$sourceFile %>%
#   str_extract("_P[0-9]+_") %>% 
#   str_remove_all("_") %>% 
#   sort_unique()

#change include to exclude to plot both datasets (compare to find a good upper and lower threshold)
for (ramp_n in seq_along(ramps)) {
  # ramp_n <- 2
  # plotlist = list()
  for (force_n in seq_along(forces)) {
    plotData <- overlayData_include %>% 
      dplyr::filter(targetRampTime.ms == ramps[ramp_n] & targetForce.mN == forces[force_n]) %>% 
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

