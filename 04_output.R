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


# create the static mapping outputs

library(RColorBrewer)
library(ggplot2) #plotting
library(envreportutils)


# create a function for outputting plots
multi_plot <- function(plotdata, filename) {
  svg_px( paste0(filename,".svg"), width = 500, height = 400)
  plot(plotdata)
  dev.off()
  png_retina(paste0(filename,".png"), width = 500, height = 400,
             units = "px", type = "cairo-png", antialias = "default")
  plot(plotdata)
  dev.off()
}

#multi_plot(overall_threat_plot, "./print_ver/othreat_plot")

# Set plotting parameters common to many plots:
x_scale <- scale_x_continuous(limits = c(2007-1, 2018+1),
                              breaks = seq(2007, 2018, 5),
                              expand = c(0,0))



## Basic plot one off plots for weight per capita per year
reg.time.kg.cap <- ggplot(region, aes(year, n.kg.pop)) +
  facet_wrap(~ regional_district) +
  geom_bar(stat = "identity", position="dodge") +
  labs(title = "Regional of recycling (tonnes) per capita",
       x = "Year", y = "weight per cap (kg)") +
  x_scale +
  theme_soe_facet()
 # theme(axis.text.x = element_text(angle = 90))

plot(reg.time.kg.cap)

multi_plot(reg.time.kg.cap, "out/regional.kg.cap.facet")


# financial cost per tonne

cost_plot <- ggplot(cost.per.tonne,
                    aes(x = year,
                        y = c.p.tonne, group = organization))+
  geom_line(aes(colour = type), size = 1.5) +
  xlab(NULL) + ylab ("Cost per tonne of recycling ($1,000)")+
  ggtitle("Cost per tonne of recycled material")+
  #theme(axis.text.x = element_text(angle = 90)) +
  #scale_colour_manual(values = normpal, guide = FALSE) +
  theme_soe()

  plot(cost_plot)

  multi_plot(cost_plot, "out/cost.per.tonne")

#  norm <- norm_base +
#    annotate("text", label = "GDP", colour = "#e41a1c",
#             x = 2004.1, y = 1.56, size = 5) +
#    annotate("text", label = "GHG", colour = "#377eb8",
#             x = 2010, y = 1.14, size = 5) +
#    annotate("text", label = "Population", colour = "#4daf4a",
#             x = 2009, y = 1.41, size = 5)
#  plot(norm)



# basic bar chart
ggplot(fdata, aes(year, total.m, fill = measure_consol)) +
  facet_wrap(~ type) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Expenditure per year",
       x = "Year", y = "Amount ($1,000,000)") +
  theme(axis.text.x = element_text(angle = 90))+
  theme_soe_facet()




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



# create the graphics for leaflet map

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
  data <- filter(diff.df, regional_district == n)
  p <- temp_plots(data, n)
  region_plot_list [[n]] <- p
  ggsave(p, file = paste0("dataviz/trend_plots/", n, ".svg"))
}

