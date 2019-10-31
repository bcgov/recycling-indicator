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

if (!exists("region")) source("03_analysis.R")

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


# Set plotting parameters common to many plots:
x_scale <- scale_x_continuous(limits = c(2007-1, 2018+1),
                              breaks = seq(2007, 2018, 5),
                              expand = c(0,0))

# Summaries for BC ---------------------------------------------------

## 1) all years by type
region.type.plot <-
  ggplot(region.type, aes(regional_district, n.kg.pp)) +
  facet_wrap(~ type, scales = "free_y") +
  geom_bar(stat = "identity", position="dodge") +
  theme_soe_facet() +
  theme(axis.text.x = element_text(angle = 90))

multi_plot(region.type.plot, "print_ver/regional.type.facet")

# 2) per year by type

time.type.plot <-
  ggplot(time.type, aes(year, n.kg.pp)) +
  facet_wrap(~ type, scales = "free_y") +
  geom_bar(stat = "identity", position="dodge") +
  theme_soe_facet() +
  theme(axis.text.x = element_text(angle = 90))

multi_plot(time.type.plot, "print_ver/time.type.facet")


# Cost Finance plots ------------------------------------------------------

cost.per.tonne <- cost.per.tonne %>%
  filter(! type == "oil")

cost_plot <- ggplot(cost.per.tonne,
                    aes(x = as.numeric(year),
                        y = c.p.tonne, group = organization)) +
  geom_line(aes(colour = type), size = 1.5) +
  xlab(NULL) + ylab ("Cost per tonne of recycling ($1,000)")+
  # ggtitle("Cost per tonne of recycled material") +
  scale_x_continuous(limits = c(2007, 2017),
                     breaks = seq(2007, 2018, 1)) +
  scale_y_continuous(limits = c(0, 2.5))+
  #expand = c(0,0)) +
  theme_soe() +
  theme(legend.position = "none")

cost_plot <- cost_plot +
  annotate("text", label = "Beverage", colour = "#e41a1c",
           x = 2008, y = 2, size = 5) +
  annotate("text", label = "Pulp & Paper", colour = "#377eb8",
           x = 2015, y = 0.6, size = 5) +
  annotate("text", label = "Electrical", colour = "#4daf4a",
           x = 2013.5, y = 2.2, size = 5)
plot(cost_plot)


multi_plot(cost_plot, "print_ver/cost.per.tonne")


# Create Pop-ups for leaflet plots  ---------------------------------------

region_list <- unique(region$regional_district)
region_plot_list <- vector(length = length(region_list), mode = "list")
names(region_plot_list) <- region_list

# Create plotting function
temp_plots <- function(pdata, name) {
  make_plot <- ggplot() +
    geom_bar(data = pdata,  aes(x = year, y = n.kg.pp), fill = "grey", stat ="identity", position="dodge") +
    geom_point(data = pdata, aes(x = year, y = bc_ave), size = 2, color = "blue") +
    geom_line(data = pdata, aes(x = year, y = bc_ave), group = "regional_district", color = "blue") +
    labs(title = n, x = "Year", y = "kg per person") +
    theme_soe() + theme(plot.title = element_text(hjust = 0.5), # Centre title
                legend.position = "bottom",
                plot.caption = element_text(hjust = 0)) # L-align caption
  make_plot
}

# loop through each type and generate the plots per

types <- unique(region$type)

for (t in types) {
  print(t)

  # Lopo through the regional districts : bev
  plots <- for (n in region_list) {
    # n = region_list[2]
    print(n)
    pdata <- filter(diff.df, regional_district == n & type == t)
    p <- temp_plots(pdata, n)
    region_plot_list [[n]] <- p
    ggsave(p, file = paste0("dataviz/trend_plots/", n, "_",t,".svg"))
  }

}

# Create combined plots for print version  ---------------------------------------

for (t in types) {
  print(t)

  pdata <- filter(diff.df, type == t)

  make_plot <- ggplot() +
    facet_wrap(~ regional_district) +
    geom_bar(data = pdata ,  aes(x = year, y = n.kg.pp), fill = "grey", stat ="identity", position="dodge") +
    geom_point(data =  pdata , aes(x = year, y = bc_ave), size = 2, color = "blue") +
    geom_line(data =  pdata , aes(x = year, y = bc_ave), group = "regional_district", color = "blue") +
    #labs(title = n, x = "Year", y = "kg per person") +
    theme(axis.text.x = element_blank()) +
    theme_soe() + theme(plot.title = element_text(hjust = 0.5), # Centre title
                        legend.position = "bottom",
                        plot.caption = element_text(hjust = 0),
                        axis.text.x = element_blank()) # L-align caption

  ggsave(make_plot, file = paste0("dataviz/trend_plots/","facet_time_",t,".png"))

}


