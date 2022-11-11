install.packages("tidyverse")
install.packages("signal")
install.packages("patchwork")
install.packages('aurora functions.R')
library(signal)
library(tidyverse)
library(patchwork)
source('aurora functions.R')
theme_set(theme_bw())

# ddfFile <- '../Raw Data/Force Data/Ful data sorted by row/Row 1 - 0.013/Trial 1/--100_1.ddf'
# ddfFile <- '../Raw Data/Force Data/Ful data sorted by row/Row 1 - 0.013/Trial 2/--30000_1.ddf'
# ddfFile <- '../Raw Data/Force Data/Ful data sorted by row/Row 10 - 4/Trial 1/--100_1.ddf'
# ddfFile <- '../Raw Data/Force Data/Ful data sorted by row/Row 10 - 4/Trial 1/--30000_1.ddf'
# ddfFile <- 'C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/DDF/01_ddf'
ddfFile <- 'C:/Users/emmku74/Desktop/01_ddf/--film-shaved-aurora_2022-02-21_16-25-19_P01_250.0_50_1.ddf'
ddfFile <- 'C:/Experiments/DDF/03_ddf/--film-shaved-aurora_2022-02-22_16-43-47_P03_1000.0_100_8.ddf'
# ddfFile <- '../Raw Data/Force Data/Ful data sorted by row/Row 3 - 0.04/Trial 5/--30000_1.ddf'
# ddfFile <- '../Raw Data/Force Data/Ful data sorted by row/Row 5 - 0.133/Trial 7/--100_1.ddf'

# read the protocol info
ptcl <- ddfFile %>% read_force_protocol(skip = 16, n_max = 5)

# read the raw data
ddfData <- ddfFile %>% read_delim('\t', skip = 25, 
                                  col_types = paste0(rep('d',12), collapse = ''))
#ddfFile %>% plot_ddf_data()

# make noise filter
SamplingRate <- read_lines(ddfFile, skip = 1, n_max = 1) %>% parse_number()
# FreqCutOff <- ceiling(2*1000/min(ptcl$rampOnDuration.ms, ptcl$holdDuration.ms, ptcl$rampOffDuration.ms))
FilterFreqCutOff <- ceiling(10^(1.4 + 0.6*log10( 1000/ptcl$rampOnDuration.ms))) 
# FilterFreqCutOff <- ceiling(3.948 + 4.013 * 1000/ptcl$rampOnDuration.ms) #20*1000/ptcl$rampOnDuration.ms 

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
scaledData %>%
  summarise_data(ramps = forceRamps) %>% 
  mutate(sourceFile = str_extract(ddfFile, 'Row.*\\.ddf')) %>% glimpse()
#write_delim(paste0('../Processed Data/force_data_',format(Sys.time(), '%Y%m%d_%H%M%S.txt')), '\t')

windows()

scaledData %>% 
  ggplot(aes(x = Time.ms)) +
  geom_vline(xintercept = unlist(forceRamps), colour = 'red') +
  geom_point(aes(y = ForceMeasured.mN), shape = 21, fill = 'black', alpha = 0.1, size = 3) +
  geom_point(aes(y = ForceFiltered.mN), colour = 'blue', size = 1) +
  labs(title = 'Force trace', x = 'Time (ms)', y = 'Force (mN)') -> force.trace

scaledData %>% 
  ggplot(aes(x = Time.ms)) +
  geom_vline(xintercept = unlist(forceRamps), colour = 'red') +
  geom_point(aes(y = ForceDeriv.Nps), colour = 'blue', size = 1) +
  labs(title = 'Force derivative', x = 'Time (ms)', y = 'Force rate (N/s)') -> force.deriv

scaledData %>% 
  ggplot(aes(x = Time.ms)) +
  geom_vline(xintercept = unlist(forceRamps), colour = 'red') +
  geom_point(aes(y = LengthMeasued.mm), shape = 21, fill = 'black', alpha = 0.1, size = 3) +
  geom_point(aes(y = LengthFiltered.mm), colour = 'purple', size = 1) +
  labs(title = paste('Displacement trace, limit =', ptcl$lengthLimit.mm, 'mm'), 
       x = 'Time (ms)', y = 'Displacement (mm)') -> disp.trace

scaledData %>% 
  ggplot(aes(x = Time.ms)) +
  geom_vline(xintercept = unlist(forceRamps), colour = 'red') +
  geom_point(aes(y = LengthDeriv.mps), colour = 'purple', size = 1) +
  labs(title = 'Displacement derivative', x = 'Time (ms)', y = 'Velocity (m/s)') -> disp.deriv

(force.trace / force.deriv / disp.trace / disp.deriv)  + 
  plot_annotation(title = paste('Target =', ptcl$targetForce.mN,'mN.',
                                ptcl$rampOnDuration.ms,'ms ramp.', 
                                'Low pass butterworth filter',FilterFreqCutOff,'Hz')) 

# outputFile <- ddfFile %>% 
#   str_replace('../Raw Data/Force Data/Ful data sorted by row/', '../Processed Data/Force Plots - update/') %>% 
#   str_replace('ddf', 'tiff')
# if (!dir.exists(file.path(dirname(outputFile))) ) dir.create(file.path(dirname(outputFile)), recursive = TRUE)
# ggsave(outputFile)