# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

if (!exists("pop")) source("01_load.R")


# format recycling volume data --------------------------------------------

all.regions <- read_csv(file.path(data.dir,'all.regions.csv'))

all.regions <- all.regions %>%
  filter(!regional_district == "Provincial Total",
         !regional_district == "BC Average") %>%
  gather("year", "n", 5:length(.)) %>%
  mutate(n.kg = ifelse(str_detect(measure,"onnes"), n * 1000, n)) %>%
  filter(!n == 0) %>%
  filter(!measure == "Population-") %>%
  left_join(pop, by = c("regional_district","year")) %>%
  mutate(n.kg.pp = ifelse(str_detect(measure, "pp"), n, n.kg / pop))


# calcaulate the total recycling (all types combined) ---------

 measures.to.exclude <- c("Absolute collection - smoke and CO alarms",
                          "Absolute collection - number of thermostats",
                          "Absolute collection - number of loose vessels",
                          "Absolute collection - adjusted number of thermostats",
                          "Absolute Collection- Total Tubskids",
                          "Absolute Collection-Units Collected-",
                          "Absolute Collection-Number of Containers-",
                          "Absolute collection - estimate of units",
                          "Estimated Tonnes Collected")


# calculate totals (all years, and per year)
region <- all.regions %>%
  filter(!measure %in% measures.to.exclude)

saveRDS(region , file.path("data","region.rds"))

# format financial dataset ------------------------------------------------

to.keep = c("Expenditure-Total", "Expenditures", "Revenue-Total","Revenues",
            "Revenue-Total (Including Deposits Charged)",
            "Expenditure-Total (Including Deposits Returned)",
            "Balance (Including Deposits Charged/Returned)" )

fdata <- all.finance %>%
  gather("year", "n", 4:length(.)) %>%
  mutate(n.m = n/1000000) %>%
  group_by(organization, type, measure, year) %>%
  summarise(total.m = sum(n.m, na.rm = TRUE)) %>%
  filter(measure %in% to.keep) %>%
  mutate(measure_consol = ifelse(measure %in%
                                   c("Expenditure-Total","Expenditures",
                                     "Expenditure-Total (Including Deposits Returned)"),"Expenditure",
                                 ifelse(measure %in% c("Revenue-Total","Revenues",
                                                       "Revenue-Total (Including Deposits Charged)"),"Revenue",NA) )) %>%
  filter(measure_consol == "Expenditure") %>%
  ungroup() %>%
  select(-c(measure)) %>%
  filter(!total.m == 0)

saveRDS(fdata , file.path("data","fdata.rds"))

# format spatial dataset --------------------------------------------------

reg_dist$ADMIN_AREA_NAME[which(reg_dist$ADMIN_AREA_NAME %in%
                                 c("Comox Valley Regional District", "Strathcona Regional District"))] <- "Comox-Strathcona"

reg_dist %<>%
  group_by(ADMIN_AREA_NAME) %>%
  summarise(do_union = FALSE) %>%
  ungroup() %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON")

### match district names by removing words and hyphenating
reg_dist$ADMIN_AREA_NAME %<>%
  str_replace(" Regional District", "") %>%
  str_replace("Regional District of ", "") %>%
  str_replace("-", " ") %>%
  str_replace("qathet", "Qathet")

reg_dist <- reg_dist %>%
  mutate(regional_district = ADMIN_AREA_NAME)


saveRDS(reg_dist , file.path("data","reg_dist.rds"))
