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

#summarise the data per region and all BC totals

if (!exists("region")) load("data/region.rds")


# Summaries for BC ---------------------------------------------------

# 1) all years by type
region.type <- region %>%
  group_by(type, regional_district) %>%
  summarise(n.kg.pp =  sum(n.kg.pp))

# 2) per year by type
time.type <- region %>%
  group_by(type, year) %>%
  summarise(n.kg.pp =  sum(n.kg.pp))



# Regional Summary --------------------------------------------------------

reg.sum <- region %>%
  group_by(regional_district, year, type) %>%
  summarise(n.kg.pp = sum(n.kg.pp))

reg.sum.wide <- reg.sum %>%
  spread(., type, n.kg.pp) %>%
  group_by(regional_district) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

reg.total <- reg.sum %>%
  group_by(regional_district) %>%
  summarise(n.kg.pp.total = sum(n.kg.pp))

reg.sum.wide <- left_join(reg.sum.wide, reg.total)

# get BC average per year and type

bc.ave <- reg.sum %>%
  group_by(type, year) %>%
  summarise(bc_ave = mean(n.kg.pp))

# join the regional and prov. ave data and calculate the difference
 diff.df <- reg.sum %>%
   left_join(bc.ave, by = c('type',"year")) %>%
   mutate(delta = n.kg.pp - bc_ave) %>%
   mutate(response = ifelse(delta < 0,"below", "above")) %>%
   mutate(response = ifelse(delta == 0,"No data", response))

 saveRDS( diff.df, file.path("data","reg_diff.rds"))

# Finance calculations ----------------------------------------------------

cost.per.tonne <- region %>%
  group_by(type, organization, year) %>%
  summarise(n.kg.sum = sum(n.kg)) %>%
  ungroup() %>%
  left_join(fdata) %>%
  select(c(organization, type, year, n.kg.sum, total.m)) %>%
  filter(!is.na(total.m)) %>%
  filter(! total.m == 0) %>%
  mutate(c.p.tonne = total.m * 1000 /(n.kg.sum * 0.001),
         organization = ifelse(type == "ppp", "ppp",
                               ifelse(type == "oil", "oil", organization)))


# Spatial_regional dataset --------------------------------------------------------

# set up a temporal data set for spatial data
reg_dist <- read_rds(file.path("data","reg_dist.rds"))

reg_dist <- reg_dist %>%
  left_join(reg.sum.wide) %>%
  rename(Beverage = bev,
         Electric = elect,
         Pharmaceuticals = pharm,
         `Paper and Pulp` = ppp,
         Oil = oil)


saveRDS(reg_dist, file.path("data","reg_dist_sum.rds"))
