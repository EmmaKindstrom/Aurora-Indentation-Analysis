install.packages('quickpsy')

#install.packages('devtools')
devtools::install_github('danilinares/quickpsy')
1

library(quickpsy)

data1 <- read.csv('C:/Users/emma_/OneDrive/Skrivbord/R course/pilot2/pilot1_2022-01-24_14-35-33_P02_data.csv')

data2 <- read.csv('C:/Users/emma_/OneDrive/Skrivbord/R course/pilot2/pilot2_2022-01-24_14-43-41_P02_data.csv')

rbind(data1, data2)

filelist <- list.files('C:/Users/emma_/OneDrive/Skrivbord/R course/pilot2', pattern = 'data', full.names = TRUE)


data <- c()

for(currentfile in filelist){
  currentdata <- read.csv(currentfile)
  data <- rbind(data, currentdata)
  print(currentfile)
}


fit <- quickpsy(qpdat, phase, resp, grouping = c("participant", "cond"))

plot(fit, color = cond)

