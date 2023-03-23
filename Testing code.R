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