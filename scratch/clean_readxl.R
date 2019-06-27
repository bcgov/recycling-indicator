library(readxl)
library(cellranger) # letter_to_num
library(dplyr)
library(tidyr)
library(envreportutils)

excel_file <- file.path(
  soe_path("Operations ORCS/Data - Working/sustainability/EPR"),
  "EPR annual report info roll up 2017.xlsb.xlsx"
)

# Or use local copy to test
excel_file <- file.path("C:/Temp/Github/recycling-indicator/data",
          "EPR annual report info roll up 2017.xlsb.xlsx"
)

list.files("C:/Temp/Github/recycling-indicator/data")

rename_by_pos <- function(df, index, new_name){
  colnames(df)[index] = new_name
  df
}

ncols_from_range <- function(range) {
  diff(letter_to_num(strsplit(gsub("[0-9]", "", range), ":")[[1]])) + 1
}


# Beverage functions and data extract -------------------------

read_bevs_recovery <- function(file, range, org) {
  read_excel(excel_file, sheet = "Bevs(2000-2017)",
             range = range, col_names = TRUE) %>%
    #rename_at(1:2, ~ c("measure", "regional_district")) %>%
    rename_by_pos(1,"measure") %>%
    rename_by_pos(2,"regional_district") %>%
    filter(measure != "Priority Measures") %>%
    mutate(organization = org) %>%
    select(organization, everything())
}


read_bevs_financial <- function(file, range, org) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Bevs(2000-2017)",
             range = range,
             col_types = c("text", "skip", rep("numeric", cols - 2)),
             col_names = c("measure", "foo",
                           seq(2000, length.out = cols - 2))) %>%
    mutate(organization = org) %>%
    select(organization, everything())
}


read_bevs_units <- function(file, range, org) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Bevs(2000-2017)",
             range = range,
             col_types = c("text", "skip", rep("numeric", cols - 2)),
             col_names = c("measure", "foo",
                           seq(2000, length.out = cols - 2))) %>%
    mutate(organization = org) %>%
    select(organization, everything())
}

read_bevs_recovery_raw <- function(file, range,org) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Bevs(2000-2017)",
             range = range,
             col_names = c("measure", "regional_district",
                           seq(2000, length.out = cols - 2))) %>%
    filter(measure != "Priority Measures") %>%
    mutate(organization = org) %>%
    select(organization, everything())
}

#--------------------------------------------------------------
# extract Bev data

encorp_priority <- read_bevs_recovery(excel_file, "B5:U38", "Encorp Pacific")
bc_brewers_priority <- read_bevs_recovery(excel_file, range = "B138:U170", "BC Brewers")

encorp_financial <- read_bevs_financial(excel_file, "B45:U53", "Encorp Pacific")
bc_brewers_financial <- read_bevs_financial(excel_file, "B185:U193", "BC Brewers")

encorp_units <- read_bevs_units(excel_file, "B55:U56", "Encorp Pacific")
bc_brewers_units <- read_bevs_units(excel_file, "B195:U196", "BC Brewers")

encorp_priority_raw <- read_bevs_recovery_raw(excel_file, "B65:U122", "Encorp Pacific")
bc_brewers_priority_raw <- read_bevs_recovery_raw(excel_file, "B209:U293", "BC Brewers")


# combine data sets into single file per metric (financial/units/priority)

financial <- rbind(encorp_financial, bc_brewers_financial)
units <- rbind(encorp_units, bc_brewers_units)
priority_raw <- rbind(encorp_priority_raw, bc_brewers_priority_raw )

# Functions to get Oil data from xlxs.--------------------------------

read_oil_recovery <- function(file, range,type) {
  cols <- ncols_from_range(range)
read_excel(excel_file, sheet = "Oil(2003-2017) ",
               range = range,
               col_types = c("text", rep("numeric", cols - 1)),
               col_names = c("regional_district",
                   seq(2003, length.out = cols - 1))) %>%
          mutate(measure = type) %>%
          select(measure, everything()
  )
}

read_oil_financial <- function(file, range) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Oil(2003-2017) ",
             range = range,
             col_types = c("text", rep("numeric", cols - 1)),
             col_names = c("measure",
                           seq(2003, length.out = cols - 1))) %>%
    select(measure, everything())
}


read_oil_units <- function(file, range, org) {
  cols <- ncols_from_range(range)
read_excel(excel_file, sheet = "Oil(2003-2017) ",
             range = range,
             col_types = c("text", rep("numeric", cols - 1)),
             col_names = c("measure",
                seq(2003, length.out = cols - 1))) %>%
        select(measure, everything())
}


# Extract oil data -------------------------------

oil_recovery <- read_oil_recovery(excel_file, "B12:Q40","oil_lt_pp")
filter_recovery <- read_oil_recovery(excel_file, "B42:Q70","filters_kg_pp")
contain_recovery <- read_oil_recovery(excel_file, "B72:Q100", "containers_kg_pp")
anti_recovery <- read_oil_recovery(excel_file, "B102:Q130", "antifreeze_kg_pp")

recovery_pp <- rbind(oil_recovery,
                     filter_recovery,
                     contain_recovery,
                     anti_recovery)

oil_financial <- read_oil_financial(excel_file, "B147:Q152")

oil_units <- read_oil_units(excel_file, "B154:Q163")

#------------------------------------------------------
oil_units
oil_financial
recovery_pp
