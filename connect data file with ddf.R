library(tidyverse)
library(signal)
library(tidyverse)
library(patchwork)
library(stringr)
library(dplyr)
library(rlang)
library(quickpsy)
library(ggplot2)

summaryHold <- read_tsv("force_summary_data_20240704_163126.txt")


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

#Select only the 'hold' phase
ConnectedData <- summaryHoldCsv %>%
  dplyr::filter(phase == 'hold') %>% 
  select(-one_of('phaseDuration.ms', 'forceChange.mN', 'meanForceRate.mNps', 'peakForceRate.mNps', 'peakPosition.mm',
                 'positionChange.mm', 'sdPosition.mm', 'meanVelocity.mmps', 'peakVelocity.mmps', 'heldPosition.mm', 'meanPosition.mm',
                 'targetRampTime.ms', 'positionLimit.mm')) 


### paste - from Data Analysis.R ###
ggplot(ConnectedData, aes(x = comparison, y = comparison.more.intense, colour = condition)) +
  stat_summary(geom = 'point', fun = 'mean') +
  facet_wrap(. ~ PID, scales = 'free') + # comment out to create a graph that contains all the participants together (mean of all trials)
  scale_y_continuous(limits = c(0,1))

# fit psychometric functions
#shaved_only_data <- filter(ConnectedData, condition == "shaved")
fit <- quickpsy(
  d = ConnectedData, 
  x = comparison, 
  k = comparison.more.intense,
  grouping = .(condition, PID),
  # parini = c(250, 2000),
  log = TRUE,
  fun = cum_normal_fun,
  B = 10000, # 10 or 100 for testing code, 10000 once everything is working, it will take time
  ci = 0.95
)
# plot  psychometric functions
theme_set(theme_bw(base_size = 14))

xbreaks <- unique(ConnectedData$comparison)

# quartz() #mac os
windows() # windows
plot(fit) + 
  scale_x_continuous(breaks = xbreaks) +
  geom_vline(xintercept = 600, linetype = 'dotted') +
  labs(x = "comparison force (mN)", y = "Proportion called more intense")

ggsave("filtereddata_psychfuns.pdf")

# Plot the PSE
plotthresholds(fit)

# plot the PSE and the slope
plotpar(fit)

#convert from log
PSEfilm_for_stats <- fit$par %>% 
  dplyr::filter(parn == 'p1' & condition == 'film') %>% 
  mutate(
    PSE.mN = exp(par),
    CIupper = exp(parsup),
    CIlowe = exp(parinf)
  )

#convert from log
PSEshaved_for_stats <- fit$par %>% 
  dplyr::filter(parn == 'p1' & condition == 'shaved') %>% 
  mutate(
    PSE.mN = exp(par),
    CIupper = exp(parsup),
    CIlowe = exp(parinf)
  )

#t-test
t.test(
  x = PSEfilm_for_stats$PSE.mN,
  mu = 600,
  var.equal = TRUE
)

t.test(
  x = PSEshaved_for_stats$PSE.mN,
  mu = 600,
  var.equal = TRUE
  )


