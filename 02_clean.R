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

# format regional dataset --------------------

tonnes <- c("Absolute Collection-Weight Collected (Tonnes)-",
            "Absolute collection - Regular products -Weight (kg)-",
            "Absolute collection - batteries (kg)",
            "Estimated Tonnes Collected", "tonnes of ppp",
            "Absolute Collection-Weight Collected (kg)-")

region <- all.regions %>%
  filter(measure %in% tonnes) %>%
  select(-c(organization)) %>%
  group_by(type, measure, regional_district) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  gather("year", "n", 4:length(.)) %>%
  filter(!n == 0) %>%
  mutate(n.kg = ifelse(str_detect(measure,"onnes"), n * 1000, n)) %>%
  group_by(regional_district, year) %>%
  summarise(n.kg.sum = sum(n.kg)) %>%
  left_join(pop, by = c("regional_district","year")) %>%
  filter(!regional_district == "Provincial Total") %>%
  mutate(n.kg.pop = n.kg.sum / pop)

saveRDS(region, file.path("out","region.R"))


# get tonnes per recycling type

tot.region <- all.regions %>%
  filter(measure %in% tonnes) %>%
  filter(!regional_district == "Provincial Total") %>%
  group_by(organization, type, measure, regional_district) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  gather("year", "n", 5:length(.)) %>%
  filter(!n == 0) %>%
  mutate(n.kg = ifelse(str_detect(measure,"onnes"), n * 1000, n)) %>%
  group_by(organization, type,year) %>%
  summarise(n.kg.sum = sum(n.kg))

saveRDS(region, file.path("out","region.R"))


## format financial dataset --------------------


to.keep = c("Expenditure-Total", "Expenditures", "Revenue-Total","Revenues",
            "Revenue-Total (Including Deposits Charged)",
            "Expenditure-Total (Including Deposits Returned)",
            "Balance (Including Deposits Charged/Returned)" )

fdata <- all.finance %>%
  gather("year", "n", 4:length(.)) %>%
  mutate(n.m = n/1000000) %>%
  group_by(organization, type, measure, year) %>%
  summarise(total.m = sum(n.m,na.rm = TRUE)) %>%
  filter(measure %in% to.keep) %>%
  mutate(measure_consol = ifelse(measure %in%
                                   c("Expenditure-Total","Expenditures",
                                     "Expenditure-Total (Including Deposits Returned)"),"Expenditure",
                                 ifelse(measure %in% c("Revenue-Total","Revenues",
                                                       "Revenue-Total (Including Deposits Charged)"),"Revenue",NA) )) %>%
  filter(measure_consol == "Expenditure")



# format spatial data : reg_dist

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

# might need more changes ?? as no population data for regional districts

reg_dist <- reg_dist %>%
  left_join(region, by = c("ADMIN_AREA_NAME" = "regional_district")) %>%
  mutate(regional_district = ADMIN_AREA_NAME)

mapview::mapview(reg_dist)
