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


## Basic plot one off plots for weight per capita per year
reg.time.kg.cap <-
  ggplot(region, aes(measure, n.kg.pp)) +
  facet_wrap(~ type) +
  geom_bar(stat = "identity", position="dodge") +
  theme_soe_facet() +
  theme(axis.text.x = element_text(angle = 90))

# note ppp is disproportional amount collected

reg.time.kg.cap.zoom <-
  ggplot(region, aes(type, n.kg.pp)) +
  facet_wrap(~ regional_district) +
  geom_bar(stat = "identity", position="dodge") +
  coord_cartesian(ylim = c(0,2000)) +
  theme_soe_facet() +
  theme(axis.text.x = element_text(angle = 90))


region1 <- region %>%
  filter(!type =="ppp")


reg.type <-
  ggplot(region1 , aes(type, n.kg.pp)) +
  facet_wrap(~ regional_district) +
  geom_bar(stat = "identity", position="dodge") +
  theme_soe_facet() +
  theme(axis.text.x = element_text(angle = 90))



# calculate the provincial average per type of recycling

bc.kg.ave <- region %>%
  na.omit() %>%
  group_by(type) %>%
  summarise(bc_ave = mean(n.kg.pp))


# join the regional and prov. ave data and calculate the difference
diff.df <- region %>%
  left_join(bc.kg.ave, by = 'type') %>%
  mutate(delta = n.kg.pp - bc_ave) %>%
  mutate(response = ifelse(delta < 0,"below", "above")) %>%
  mutate(response = ifelse(delta == 0,"No data", response))




# calculate cost per tonne fo recycling
cost.per.tonne <- region %>%
  group_by(type, measure, year) %>%
  summarise(n.kg.sum = sum(n.kg)) %>%
  ungroup() %>%
  left_join(fdata) %>%
  select(c(organization, type, year, n.kg.sum, total.m)) %>%
  filter(!is.na(total.m)) %>%
  filter(! total.m == 0) %>%
  mutate(c.p.tonne = total.m * 1000 /(n.kg.sum * 0.001))


# oil has a very high cost to recycle
data.check <- cost.per.tonne %>%
  group_by(organization, type) %>%
  summarise(ave = mean(c.p.tonne))
