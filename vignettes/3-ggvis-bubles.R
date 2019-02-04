
library(tidyverse)
# install.packages("ggvis")
library(ggvis)


gviz_data <-
  read_rds("clean-data/analysis_data.rds") %>% 
  filter(year > 1995) %>% 
  mutate(
    `Variable group` = ifelse(is.na(`Variable group`), "", `Variable group`),
    var = str_c(`Source`, " - ", ifelse(`Variable group` == '', '', str_c(`Variable group`, " - ")), var)) %>% 
  select(-var_code, - Source, -`Variable group`, -flag) %>%
  # slice(c(329624, 329625))
  spread(var, val) 

# write_rds(gviz_data, "clean-data/google_vis_data.rds", compress = "gz")
