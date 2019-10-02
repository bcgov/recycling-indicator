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


#summarise the data per region and all BC totals


if (!exists("region")) load("data/region.rds")


# calculate the provincial average per kg
bc.kg.per.cap <- region %>%
  na.omit() %>%
  group_by(year) %>%
  summarise(bc_ave = mean(n.kg.pop))


# join the regional and prov. ave data and calculate the difference
diff.df <- region %>%
  left_join(bc.kg.per.cap, by = 'year') %>%
  mutate(delta = n.kg.pop - bc_ave) %>%
  mutate(response = ifelse(delta < 0,"below", "above")) %>%
  mutate(response = ifelse(delta == 0,"No data", response))


# calculate cost per tonne fo recycling
cost.per.tonne <- tot.region %>%
  left_join(fdata) %>%
  select(c(organization, type, year, n.kg.sum, total.m)) %>%
  filter(!is.na(total.m)) %>%
  filter(! total.m == 0) %>%
  mutate(c.p.tonne = total.m * 1000 /(n.kg.sum * 0.001))




