#
librarian::shelf(tidyverse, readxl, tidyverse/lubridate)

### Load & Rename cols

c('year0', 'month0', 'day0', 'time0',
  'year1', 'month1', 'day1', 'time1',
  'loc0', 'loc1', 'loc2', 'loc3', 'loc4', 'loc5', 'loc6',
  'origin_cat', 'origin_desc1', 'origin_desc2',
  'damage') -> col_new



read_xls(here::here('data', '2003_2018.xls'), skip=2) -> tdf0


tdf0 %>%
  rename_all(
    ~col_new
  ) %>%
  mutate(
    origin_cat = as_factor(origin_cat),
    damage = as.numeric(damage)
  ) %>%

