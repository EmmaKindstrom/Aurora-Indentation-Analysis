scale_and_filter <- function(ddfData, scaleUnits, SamplingRate, butterworthFilter) {
  ddfData %>% 
    mutate(Time.ms = 1000*Sample/SamplingRate,
           LengthMeasued.mm = AI0*scaleUnits[1], #V/mm AI0
           ForceMeasured.mN = AI1*scaleUnits[2], #V/mN AI1
           LengthCommanded.mm = AO0*scaleUnits[9], #V/mm AO0
           ForceCommanded.mN = AO1*scaleUnits[10], #V/mN AO0
           LengthFiltered.mm = filtfilt(butterworthFilter, AI0)*scaleUnits[1],
           ForceFiltered.mN = filtfilt(butterworthFilter, AI1)*scaleUnits[2],
           LengthDeriv.mps = c(NA,diff(LengthFiltered.mm)/diff(Time.ms)),
           ForceDeriv.Nps = c(NA,diff(ForceFiltered.mN)/diff(Time.ms))
    ) %>% 
    select(c(Time.ms, LengthFiltered.mm, ForceFiltered.mN, 
             LengthMeasued.mm, ForceMeasured.mN, 
             LengthCommanded.mm, ForceCommanded.mN,
             LengthDeriv.mps, ForceDeriv.Nps))
}

plot_ddf_data <- function(ddfData, channels = c('AI0', 'AI1', 'AO0', 'AO1')) {
  ddfData %>%
    select(c('Sample',channels)) %>%
    pivot_longer(cols = -c(Sample), names_to = c('Channel')) %>%
    ggplot(aes(x = Sample, y = value, colour = Channel)) +
    facet_grid(Channel ~., scales = 'free_y') +
    geom_jitter()  
}

read_force_protocol <- function(file, skip, n_max) { 
  protocolData <- read_lines(file, skip = skip, n_max = n_max) %>% 
    str_split('\t|,', simplify = T) %>% 
    as_tibble() %>% 
    set_names(c('Wait.sec', 'Action', 'Type', 'Target', 'Window.sec', 'X')) %>% 
    select(-X) %>% 
    mutate(Wait.sec = parse_number(Wait.sec),
           Target = parse_number(Target),
           Window.sec = parse_number(Window.sec))
  
  output <- tibble(
    targetForce.mN = protocolData$Target[2],
    lengthLimit.mm = protocolData$Target[1],
    preStim = (protocolData$Wait.sec[1] + protocolData$Window.sec[1])*1000,
    rampOn = preStim + protocolData$Wait.sec[2]*1000, #ms
    hold = rampOn + protocolData$Window.sec[2]*1000,
    rampOff = hold + protocolData$Wait.sec[3]*1000,
    endStim = rampOff + protocolData$Window.sec[3]*1000,
    endRecording = endStim + (protocolData$Wait.sec[4]+protocolData$Window.sec[4]+protocolData$Wait.sec[5])*1000,
    rampOnDuration.ms = hold - rampOn,
    holdDuration.ms = rampOff - hold,
    rampOffDuration.ms = endStim - rampOff,
    padding = min(500, max(50,0.2*(rampOnDuration.ms + holdDuration.ms + rampOffDuration.ms))),
    stimWindowStart = max( preStim*0.7, rampOn-padding ),
    stimWindowEnd = min( endRecording-(endRecording-endStim)*0.7, endStim+padding )
  ) %>% 
    as.list()
  
  return(update_list(output, protocolData = protocolData))
}

read_length_protocol <- function(file, skip, n_max) { 
  protocolData <- read_lines(file, skip = skip, n_max = n_max) %>% 
    str_split('\t|,', simplify = T) %>% 
    as_tibble() %>% 
    set_names(c('Wait.sec', 'Action', 'Type', 'Target', 'Window.sec', 'X')) %>% 
    select(-X) %>% 
    mutate(Wait.sec = parse_number(Wait.sec),
           Target = parse_number(Target),
           Window.sec = parse_number(Window.sec))
  
  output <- tibble(
    targetLength.mm = protocolData$Target[1],
    preStim = 0,
    rampOn = preStim + protocolData$Wait.sec[1]*1000, #ms
    hold = rampOn + protocolData$Window.sec[1]*1000,
    rampOff = hold + protocolData$Wait.sec[2]*1000,
    endStim = rampOff + protocolData$Window.sec[2]*1000,
    endRecording = endStim + protocolData$Wait.sec[3]*1000,
    rampOnDuration.ms = hold - rampOn,
    holdDuration.ms = rampOff - hold,
    rampOffDuration.ms = endStim - rampOff,
    padding = min(500, max(50,0.2*(rampOnDuration.ms + holdDuration.ms + rampOffDuration.ms))),
    stimWindowStart = max( preStim*0.7, rampOn-padding ),
    stimWindowEnd = min( endRecording-(endRecording-endStim)*0.7, endStim+padding )
  ) %>% 
    as.list()
  
  return(update_list(output, protocolData = protocolData))
}

find_ramps <- function(scaledData, ptcl, channel, threshold) {
  
  find_window <- function(scaledData,channel,threshold,direction) {
    win <- tibble()
    tr <- threshold*0.5
    while (nrow(win) == 0 & abs(tr) <= abs(threshold*3)) {
      tr <- tr + threshold*0.5
      if (direction == 'g') {
        win <- scaledData %>% 
          dplyr::filter({{channel}} > tr)
      } else {
        win <- scaledData %>% 
          dplyr::filter({{channel}} < tr)
      }
    }
    if (abs(tr) > abs(threshold)) {
      print(paste0('increased threshold ', tr/threshold/10))}
    return(win)
  }
  
  OnStartWindow <- scaledData %>% 
    dplyr::filter(
      Time.ms %>% between(ptcl$rampOn, ptcl$rampOff)) %>% 
    find_window({{channel}}, threshold, direction = 'g')
  if (nrow(OnStartWindow) == 0) {
    print('couldn\'t find start of on ramp')
    OnStartWindow <- OnStartWindow %>% 
      mutate(Time.ms = as.numeric(NA)) }
  
  OnStart <- OnStartWindow %>% 
    pull(Time.ms) %>% min()
  
  OnStopWindow <- scaledData %>% 
    dplyr::filter(
      Time.ms %>% between(OnStart, ptcl$rampOff)) %>% 
    find_window({{channel}}, threshold, direction = 'l')
  if (nrow(OnStopWindow) == 0) {
    print('couldn\'t find end of on ramp')
    OnStopWindow <- OnStopWindow %>% 
      mutate(Time.ms = as.numeric(NA)) }
  
  OnStop <- OnStopWindow %>% 
    pull(Time.ms) %>% min()
  
  OffStartWindow <- scaledData %>% 
    dplyr::filter(
      Time.ms %>% between(ptcl$rampOff, ptcl$stimWindowEnd)) %>% 
    find_window({{channel}}, -threshold, direction = 'l')
  if (nrow(OffStartWindow) == 0) {
    print('couldn\'t find start of off ramp')
    OffStartWindow <- OffStartWindow %>% 
      mutate(Time.ms = as.numeric(NA)) }
  
  OffStart <- OffStartWindow %>% 
    pull(Time.ms) %>% min()
  
  OffStopWindow <- scaledData %>% 
    dplyr::filter(
      Time.ms %>% between(OffStart, ptcl$stimWindowEnd)) %>% 
    find_window({{channel}}, -threshold, direction = 'g')
  if (nrow(OffStopWindow) == 0) {
    print('couldn\'t find end of on ramp')
    OffStopWindow <- OffStopWindow %>% 
      mutate(Time.ms = as.numeric(NA)) }
  
  OffStop <- OffStopWindow %>% 
    pull(Time.ms) %>% min()
  
  list(OnStart=OnStart, OnStop=OnStop, 
       OffStart=OffStart, OffStop=OffStop) %>% 
    return()
}

summarise_data <- function(df, ramps) {
  output <- df %>% 
    mutate(phase = case_when(
      Time.ms %>% between(0, ramps$OnStart) ~ 'pre-stim', 
      Time.ms %>% between(ramps$OnStart, ramps$OnStop) ~ 'ramp on',
      Time.ms %>% between(ramps$OnStop, ramps$OffStart) ~ 'hold',
      Time.ms %>% between(ramps$OffStart, ramps$OffStop) ~ 'ramp off',
      Time.ms %>% between(ramps$OffStop, max(Time.ms)) ~ 'post-stim',
      #ramps$OnStart %>% is.infinite ~ 'ramp not detected'
    ) %>% factor(levels = c('pre-stim', 'ramp on', 'hold', 'ramp off', 'post-stim'))) %>% 
    group_by(phase) %>% 
    summarise(
      phaseDuration.ms = max(Time.ms) - min(Time.ms),
      
      peakForce.mN = max(ForceFiltered.mN),
      minimumForce.mN = min(ForceFiltered.mN),
      forceChange.mN = max(ForceFiltered.mN) - min(ForceFiltered.mN),
      meanForce.mN = mean(ForceFiltered.mN),
      sdForce.mN = sd(ForceFiltered.mN),
      meanForceRate.mNps = 1000*mean(ForceDeriv.Nps, na.rm = TRUE),
      peakForceRate.mNps = 1000*ForceDeriv.Nps[which.max(abs(ForceDeriv.Nps))],
      
      peakPosition.mm = max(LengthFiltered.mm),
      positionChange.mm = max(LengthFiltered.mm) - min(LengthFiltered.mm),
      meanPosition.mm = mean(LengthFiltered.mm),
      sdPosition.mm = mean(LengthFiltered.mm),
      meanVelocity.mmps = 1000*mean(LengthDeriv.mps, na.rm = TRUE),
      peakVelocity.mmps = 1000*LengthDeriv.mps[which.max(abs(LengthDeriv.mps))]
    )
  
  if ("hold" %in% output$phase) {
    output <- output %>% 
      mutate (
        heldForce.mN = meanForce.mN[phase=='hold'] - meanForce.mN[phase=='pre-stim'],
        heldPosition.mm = meanPosition.mm[phase=='hold'] - meanPosition.mm[phase=='pre-stim']
      )
  }
  
  return(output)
  
}

sort_unique <- function(x) {
  y <- unique(x)
  y[order(y)] }

