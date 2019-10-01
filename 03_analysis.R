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


#
# ggplot(diff.df, aes(x = regional_district,
#                      y = delta,
#                      label = delta)) +
#      facet_wrap(~year) +
#    geom_bar(stat ='identity', aes(fill = response), width = .5)  +
#    scale_fill_manual(name ="Total Recycling Quantity",
#                      labels = c("Above Average", "Below Average", "No Data"),
#                      values = c("above" = "#008000", "below"="#FF0000", "no data" = 'grey')) +
#    labs(title= "Regional difference from BC Ave",
#         subtitle = " Bev units per capita") +
#    coord_flip()

# create a pop-up map per region based on difference to BC average value


region_list <- unique(region$regional_district)
region_plot_list <- vector(length = length(region_list), mode = "list")
names(region_plot_list) <- region_list


# Create plotting function
temp_plots <- function(data, name) {
    make_plot <- ggplot(data) +
      geom_bar(aes(x = year , y = n.kg.pop), stat = "identity") +
      geom_point(aes(x = year, y = bc_ave), colour = "red") +
      labs(x = "Year", y = "kg per capital") + # Legend text
      ggtitle(paste("Reported Recycling for "
                    , n
                    ,sep = "")) +
      theme_soe() + theme(plot.title = element_text(hjust = 0.5), # Centre title
                          legend.position = "bottom",
                          plot.caption = element_text(hjust = 0)) # L-align caption
    make_plot
  }

  # Lopo through the regional districts

plots <- for (n in region_list) {
   print(n)
   #n <- region_list [1]
   data <- filter(diff.df, regional_district == n)
   #rdata <- type.region %>% filter(type == "bev", regional_district == district)
    p <- temp_plots(data, n)
    region_plot_list [[n]] <- p
    ggsave(p, file = paste0("out/", n, ".svg"))
  }












# calculate cost per tonne fo recycling
cost.per.tonne <- tot.region %>%
  left_join(fdata) %>%
  select(c(organization, type, year, n.kg.sum, total.m)) %>%
  filter(!is.na(total.m)) %>%
  filter(! total.m == 0) %>%
  mutate(c.p.tonne = total.m * 1000 /(n.kg.sum * 0.001))

ggplot(cost.per.tonne, aes(x = year, y = c.p.tonne, group = organization))+
  geom_point(aes(colour = type))+
  geom_line(aes(colour = factor(type))) +
  labs(title="Cost per tonne of recycled material",
       x = "Year", y = "Cost ($1,000) per tonne of recycling") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_soe()


# basic bar chart
ggplot(fdata, aes(year, total.m, fill = measure_consol)) +
  facet_wrap(~ type) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Expenditure per year",
       x = "Year", y = "Amount ($1,000,000)") +
  theme(axis.text.x = element_text(angle = 90))+
  theme_soe_facet()

## region



 ## Basic plot one off plots for weight per capita per year
 ggplot(region, aes(year, n.kg.pop)) +
   facet_wrap(~ regional_district) +
   geom_bar(stat = "identity", position="dodge") +
   labs(title = "Regional of recycling (tonnes) per capita",
        x = "Year", y = "weight per cap (kg)") +
   theme(axis.text.x = element_text(angle = 90)) +
   theme_soe_facet()



# ## something not quite right about this plot (2007/2008/and 2009 all look identical? )
# ## may also want to split out the consumables and non-consumables.
#
# ggplot(diff.df, aes(x = regional_district,
#                     y = delta,
#                     label = delta)) +
#   facet_wrap(~year) +
#   geom_bar(stat ='identity', aes(fill = response), width = .5)  +
#   scale_fill_manual(name ="Total Recycling Quantity",
#                     labels = c("Above Average", "Below Average", "No Data"),
#                     values = c("above" = "#008000", "below"="#FF0000", "no data" = 'grey')) +
#   labs(title= "Regional difference from BC Ave",
#        subtitle = " Bev units per capita") +
#   coord_flip()






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



















# need to adjust these labels to kg.per.cap (not %)
labels <- sprintf(
  "<strong>%s (%s%%)</strong>",
  tools::toTitleCase(tolower(reg_dist$regional_district)),
  round(reg_dist$n.kg.pop, 0)
) %>% lapply(htmltools::HTML)
pal <- colorNumeric(palette = "YlGn", domain = reg_dist$n.kg.pop)

# set up popup list
temp_popups <-  leafpop::popupGraph(temp_plot_list, type = "svg")
saveRDS(temp_popups, "out/temp_popups.rds")

popup_options <-  popupOptions(autoPan = TRUE,
                               keepInView = TRUE,
                               closeOnClick = TRUE,
                               autoPanPaddingTopLeft = c(120, 20),
                               autoPanPaddingBottomRight = c(120,20))


leaflet(reg_dist, width = "900px", height = "550px") %>%
  setView(lng = -126.5, lat = 54.5, zoom = 4) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite",
                   options = providerTileOptions(minZoom = 5, maxZoom = 10)) %>%
  addPolygons(color = "#7f7f7f", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = ~ pal(n.kg.pop),
              highlightOptions = highlightOptions(fillOpacity = 0.9,
                                                  weight = 2,
                                                  bringToFront = FALSE),
              label = labels,
              labelOptions = labelOptions(direction = "auto",
                                          textsize = "12px"),
              popup = temp_popups,
              popupOptions = popup_options
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


