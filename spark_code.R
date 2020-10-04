library(tidyverse)
library(sparklyr)
library(here)

local_spark <- spark_connect(master = "local")


IPC <- spark_read_csv(sc = local_spark, 
                      name = "IPC", 
                      path = here("01-Data", "IPC_csv.csv"),
                      memory = FALSE)

ts_signature_spark <- spark_read_csv(sc = local_spark,
                                     name = "ts_signature",
                                     path = here("01-Data", "ts_signature.csv"),
                                     memory = FALSE)


joined_tbl <- full_join(x = ts_signature_spark, 
                        y = IPC, 
                        by = c("index_date" = "tidy_date",
                               "index_time" = "tidy_time")) %>% 
  arrange(index_date, index_time)


joined_tbl %>% spark_write_csv(path = here("01-Data", "joined.csv"))
