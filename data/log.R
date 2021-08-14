log=list()

log[['mh_nationwide']]=
  "Create a sparse binary data frame of diagnosis/procedure code
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=16s  
Split into batches of subject per healthcare provider
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=01m 24s
Compute day interval of a medical history to each visit
Started: 2021-02-21 04:45:58 
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=07h 44m 28s
End: 2021-02-21 12:31:22"

log[['mh_provider']]=
  "Create a sparse binary data frame of diagnosis/procedure code
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=11s  
Split into batches of subject per healthcare provider
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=47s  
Compute day interval of a medical history to each visit
Started: 2021-02-21 12:33:08 
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=07h 59m 34s
End: 2021-02-21 20:33:18"

log[['cf_nationwide']]=
  "Create a sparse binary data frame of diagnosis/procedure code
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=01s  
Split into batches of subject per healthcare provider
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=12s  
Compute day interval of a medical history to each visit
Started: 2021-02-21 20:35:38 
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=01h 01m 56s
End: 2021-02-21 21:37:42"

log[['cf_provider']]=
  "Create a sparse binary data frame of diagnosis/procedure code
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=01s  
Split into batches of subject per healthcare provider
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=12s  
Compute day interval of a medical history to each visit
Started: 2021-02-21 21:40:00 
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=01h 02m 34s
End: 2021-02-21 22:42:43"

log[['ipw']]=
  "Conduct IPW for causal inference based on the causal diagram
Started: 2021-04-20 01:06:27 
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=08m 05s
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=11m 31s
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=11m 48s
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=08m 17s
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=07m 37s
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=08m 05s
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=07m 52s
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=07m 56s
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=06m 58s
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=06m 34s
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=06m 31s
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=05m 47s
End: 2021-04-20 02:43:28"

log[['causal_ridge']]=
  "Conduct causal ridge regression by parallel computing 
Started: 2021-08-13 18:04:34 
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=04m 60s
End: 2021-08-13 18:09:35"

saveRDS(log,'data/log.rds')
rm(log)