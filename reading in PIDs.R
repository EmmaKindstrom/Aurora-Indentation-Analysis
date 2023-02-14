f <- plotData$sourceFile[1]

PID <- f %>% 
  str_extract("_P[0-9]+_") %>% 
  str_remove_all("_")

f %>% 
  str_extract("_(film)|(shaved)_") %>% 
  str_remove_all("_")

plotData %>% 
  mutate(
    condition = sourceFile %>% 
      str_extract("_(film)|(shaved)_") %>% 
      str_remove_all("_")
  ) %>% 
  group_by(condition) %>% 
  tally()

