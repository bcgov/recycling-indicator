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

## Load in libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(bcmaps)
library(sf)
library(envreportutils)
library(leaflet)
library(bcmaps)
library(dplyr)
library(rmapshaper)
library(mapview)
library(purrr)
library(readr)
library(here)


## Load  data files

data.dir <- soe_path("Operations ORCS/Data - Working/sustainability/EPR/")# to run on O:/
pop.dir <- file.path("C:/Temp/Github/recycling-indicator/data")

# Read in population data -------------------------------
# BC Pop Stats (ignore and get directly from Stats Can)
# https://www.bcstats.gov.bc.ca/apps/PopulationEstimates.aspx
# manual export of population per regional district (2000 - 2018) and store in data folder


pop.0 <- read_csv(file.path("C:/Temp/Github/recycling-indicator/data",'Population_Estimates.csv'))

pop <- pop.0 %>%
  mutate(regional_district = gsub("-", " ", `Regional District`),
         pop = Total,
         year = as.character(Year)) %>%
  select(-c('X1','Gender','Regional District','Total',"Year")) %>%
  mutate(regional_district = ifelse(regional_district == "Powell River",
                                    "Qathet",
                                    ifelse(regional_district == "Stikine",
                                           "Kitimat Stikine",
                                           ifelse(regional_district == "Comox Valley",
                                                  "Comox Strathcona",
                                                  ifelse(regional_district == "Strathcona",
                                                         "Comox Strathcona",
                                                         regional_district)))))


pop <- pop %>%
  group_by(regional_district, year) %>%
  summarize(pop = sum(pop))

#######################################################################

# source(paste('scratch','clean_readxl.R',sep = '/'))

##or

all.regions <- read_csv(file.path('data','all.regions.csv'))

#all.finance <- read_csv(file.path('data','all.finance.csv'))
#all.units <- read_csv(file.path('data','all.units.csv'))

# Finance consolidated:

desc <- all.finance %>%
  dplyr::select(type, measure)

to.keep = c("Expenditure-Total", "Expenditures", "Revenue-Total","Revenues",
            "Revenue-Total (Including Deposits Charged)",
            "Expenditure-Total (Including Deposits Returned)",
            "Balance (Including Deposits Charged/Returned)" )

fdata <- all.finance %>%
  gather("year", "n", 4:length(.)) %>%
  mutate(n.m = n/1000000) %>%
  group_by(type, measure, year) %>%
  summarise(total = sum(n.m,na.rm = TRUE)) %>%
  filter(measure %in% to.keep) %>%
  mutate(measure_consol = ifelse(measure %in%
      c("Expenditure-Total","Expenditures",
        "Expenditure-Total (Including Deposits Returned)"),"Expenditure",
      ifelse(measure %in% c("Revenue-Total","Revenues",
                            "Revenue-Total (Including Deposits Charged)"),"Revenue",NA) ))

# basic bar chart
ggplot(fdata, aes(year, total, fill = measure_consol)) +
  facet_wrap(~ type) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Revenue and expenditure per year",
       x = "Year", y = "Amount ($1,000,000)") +
  theme(axis.text.x = element_text(angle = 90))

# filter for revenue only
rdata <- fdata %>%
  filter(measure_consol == "Revenue") %>%
  filter(total > 0)

ggplot(rdata, aes(year, total, group = type)) +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = factor(type)))+
  labs(title="Annual Revenue per recycling type",
       x = "Year", y = " Amount ($1,000,000)") +
  theme(axis.text.x = element_text(angle = 90)) #+
  scale_y_log10()

# Add a balance data set
bdata <- fdata  %>%
    group_by(type) %>%
    filter(!is.na(measure_consol)) %>%
    select(- measure)%>%
    filter(total > 0)%>%
    spread(., measure_consol, total) %>%
    mutate(Balance = Revenue - Expenditure)

# Balance and line type
ggplot(bdata, aes(year, Balance, group = type)) +
  geom_hline(yintercept =  0, colour = "dark grey", linetype="dashed") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = factor(type)))+
  labs(title="Annual Balance per recycling type",
       x = "Year", y = " Amount ($1,000,000)") +
  theme(axis.text.x = element_text(angle = 90))

# bar chart
ggplot(bdata, aes(year, Balance, fill = type)) +
  facet_wrap(~ type) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Annual Balance",
       x = "Year", y = " Amount ($1,000,000)") +
  theme(axis.text.x = element_text(angle = 90))

# bar chart
ggplot(bdata, aes(year, Balance, fill = type)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Annual Balance",
       x = "Year", y = " Amount ($1,000,000)") +
  theme(axis.text.x = element_text(angle = 90))

# regions consolidated -----------------------------------------
all.regions <- read_csv(file.path('data','all.regions.csv'))

#unique(all.regions$measure)
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

## Basic plot one off plots for weight per capita per year
ggplot(region, aes(year, n.kg.pop)) +
  facet_wrap(~ regional_district) +
  geom_bar(stat = "identity", position="dodge") +
  labs(title = "Regional weight of recycling (tonnes) per capita",
       x = "Year", y = "weight per cap (kg)") +
  theme(axis.text.x = element_text(angle = 90))

# calculate the provincial average
bc.kg.per.cap <- region %>%
  na.omit() %>%
  group_by(year) %>%
  summarise(bc_ave = mean(n.kg.pop))


# write a function to plot each of the regions
temp_plots <- function(rdata, district) {

  make_plot <- ggplot () +
    geom_bar(data = rdata, aes(x = year, y = n.kg.pop),stat = "identity") +
    geom_point(data = bc.kg.per.cap, aes(x = year, y = bc_ave), colour = "red") +
    labs(x = "Year", y = "kg per capital") + # Legend text
    ggtitle(paste("Reported Recycling for "
                  , district
                  ,sep = "")) +
    theme_soe() + theme(plot.title = element_text(hjust = 0.5), # Centre title
                        legend.position = "bottom",
                        plot.caption = element_text(hjust = 0)) # L-align caption
    make_plot
}

# Create ggplot graph loop
names <- unique(region$regional_district)
<<<<<<< HEAD
temp_plot_list <- vector(length = length(names), mode = "list")
=======
>>>>>>> 1f2e3d7a1d411967a51fee20c3b454a554bdb142

plots <- for (i in 1:length(names)) {
  district <- names[i]
  rdata <- region %>% filter(regional_district == district)
  p <- temp_plots(rdata, district)
<<<<<<< HEAD
  temp_plot_list[[i]] <- p
=======
>>>>>>> 1f2e3d7a1d411967a51fee20c3b454a554bdb142
  ggsave(p, file = paste0("out/", district, ".svg"))
}


## other plot types # Diverging barcharts

# join the regional and prov. ave data and calculate the difference
diff.df <- region %>%
  left_join(bc.kg.per.cap, by = 'year') %>%
  mutate(delta = n.kg.pop.sum - bc_ave) %>%
  mutate(response = ifelse(delta < 0,"below", "above")) %>%
  mutate(response = ifelse(delta == 0,"No data", response))

## something not quite right about this plot (2007/2008/and 2009 all look identical? )
## may also want to split out the consumables and non-consumables.

ggplot(diff.df, aes(x = regional_district,
                    y = delta,
                    label = delta)) +
  facet_wrap(~year) +
  geom_bar(stat ='identity', aes(fill = response), width = .5)  +
  scale_fill_manual(name ="Total Recycling Quantity",
                    labels = c("Above Average", "Below Average", "No Data"),
                    values = c("above" = "#008000", "below"="#FF0000", "no data" = 'grey')) +
  labs(title= "Regional difference from BC Ave",
       subtitle = " Bev units per capita") +
  coord_flip()


# Create a leaflet map ------------------------------
#x <- region
#rdkey <- read_csv(file.path('data','RD_KEY.csv'))
#region <-  left_join(region, rdkey, by = c("regional_district" = "Region"))
library(lwgeom)

reg_dist <- combine_nr_rd() %>%
  rmapshaper::ms_simplify(0.005) %>%
  st_intersection(bc_bound()) %>%
  st_transform(4326) %>%
  group_by(ADMIN_AREA_NAME,ADMIN_AREA_ABBREVIATION) %>%
  summarize()

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
<<<<<<< HEAD

mapview(reg_dist)

# might need more changes ??


reg_dist <- reg_dist %>%
  left_join(region, by = c("ADMIN_AREA_NAME" = "regional_district")) %>%
  mutate(regional_district = ADMIN_AREA_NAME)
=======

mapview(reg_dist)

# might need more changes ??


reg_dist <- reg_dist %>%
  left_join(region, by = c("ADMIN_AREA_NAME" = "regional_district"))
>>>>>>> 1f2e3d7a1d411967a51fee20c3b454a554bdb142

mapview(reg_dist)

# need to adjust these labels to kg.per.cap (not %)
labels <- sprintf(
  "<strong>%s (%s%%)</strong>",
  tools::toTitleCase(tolower(reg_dist$regional_district)),
  round(reg_dist$n.kg.pop, 0)
) %>% lapply(htmltools::HTML)

<<<<<<< HEAD
pal <- colorNumeric(palette = "YlGn", domain = reg_dist$n.kg.pop)

# set up popup list
temp_popups <-  leafpop::popupGraph(temp_plot_list, type = "svg")
saveRDS(temp_popups, "out/temp_popups.rds")
=======
pal <- colorNumeric(palette = "YlGn", domain = reg_dist$n.kg.pop.sum)
>>>>>>> 1f2e3d7a1d411967a51fee20c3b454a554bdb142

leaflet(reg_dist, width = "900px", height = "550px") %>%
  setView(lng = -126.5, lat = 54.5, zoom = 4) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite",
                   options = providerTileOptions(minZoom = 5, maxZoom = 10)) %>%
  addPolygons(color = "#7f7f7f", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
<<<<<<< HEAD
              fillColor = ~ pal(n.kg.pop),
=======
              fillColor = ~ pal(n.kg.pop.sum),
>>>>>>> 1f2e3d7a1d411967a51fee20c3b454a554bdb142
              #fillColor = ~ colorQuantile("YlOrRd", n.pop.sum)(n.pop.sum),
              highlightOptions = highlightOptions(fillOpacity = 0.9,
                                    weight = 2,
                                    bringToFront = FALSE),
              label = labels,
              labelOptions = labelOptions(direction = "auto",
                                          textsize = "12px")#,
              #popup = popups,
              #popupOptions = popup_options
 ) %>%
  addEasyButton(easyButton(
    icon = htmltools::span('Reset Map'),
    onClick = JS("function(btn, map) {
                     map.closePopup();
                     map.setView({lon: -126.5, lat: 54.5}, 5);
                     // Close labels - they stay stuck open on mobile
                     map.eachLayer(function (layer) {
                         if (layer instanceof L.Polygon) {
                           layer.label.close();
                         }
                     });
                  }"),
    position = "bottomleft", id = "reset-button")) %>%
  addLegend(position = "bottomleft", pal = pal, values = ~n.kg.pop,
            title = htmltools::HTML("Recycled<br/>material<br/>kg per<br/>capita"),
            labFormat = labelFormat(suffix = , between = "", digits = 3))


# add pop up ?

# if (params$add_popups) {
#   # get plot_list with ecoregions in same order as ecoregions in ecoreg
#   plot_list <- readRDS(here("tmp/plotlist.rds"))[ecoreg$ECOREGION_NAME]
#
#   popupGraph_list <- imap(plot_list, ~ {
#     .x$barchart +
#       .x$map +
#       plot_layout(widths = c(1, 1.5)) +
#       plot_annotation(title = tools::toTitleCase(tolower(.y)),
#                       theme = theme(title = element_text(size = 18)))
#   })
#
#   popups <-  popupGraph(popupGraph_list, type = "svg", width = 700,
#                         height = 400)
#   popup_options <-  popupOptions(maxWidth = "100%", autoPan = TRUE,
#                                  keepInView = TRUE,
#                                  zoomAnimation = FALSE,
#                                  closeOnClick = TRUE,
#                                  autoPanPaddingTopLeft = c(120, 10),
#                                  autoPanPaddingBottomRight = c(10,10))
# } else {
#   popups <- popup_options <- NULL
# }


# this needs some work still.




