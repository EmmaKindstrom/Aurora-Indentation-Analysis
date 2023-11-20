Outliersmat <- matrix(summaryHold$outliers & )

overlayData_include

View(Outliersmat)


ViewsummaryHold <- summaryData %>% 
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

M1<-rep(c("A","B","C","D","E"),each=5)
M1

02_ddf/--film-shaved-aurora_2022-02-22_13-21-25_P02_1000.0_400_2.ddf

datafilename$filename_date <- str_remove(datafilename$filename, "_P[0-9]{2}_settings.csv")
datafilename$filename_date <- str_remove(datafilename$filename_date, "_P[0-9]{2}_film_settings.csv")
datafilename$filename_date <- str_remove(datafilename$filename_date, "_P[0-9]{2}_shaved_settings.csv")
datafilename$filename_date <- str_remove(datafilename$filename_date, "C:/Users/emmku74/OneDrive - Linköpings universitet/film-shaved-aurora/settings/film-shaved-aurora_")
datafilename <- datafilename[,-2]

summaryData$sourceFile_date <- str_remove(summaryData$sourceFile, "[0-9][0-9]_ddf/--film-shaved-aurora_")
summaryData$sourceFile_date <- str_remove(summaryData$sourceFile_date, "_P[0-9]{2}_[0-9]{4}.0")
summaryData$sourceFile_date <- str_remove(summaryData$sourceFile_date, "_P[0-9]{2}_[0-9]{3}.0")
summaryData$sourceFile_date <- str_remove(summaryData$sourceFile_date, "0_[0-9].ddf")



View(summaryData$sourceFile_date)

