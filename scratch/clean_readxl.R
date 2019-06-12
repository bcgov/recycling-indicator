library(readxl)
library(cellranger) # letter_to_num
library(dplyr)
library(tidyr)
library(envreportutils)

excel_file <- file.path(
  soe_path("Operations ORCS/Data - Working/sustainability/EPR"),
  "EPR annual report info roll up 2017.xlsb.xlsx"
)

read_bevs_recovery <- function(file, range, org) {
  read_excel(excel_file, sheet = "Bevs(2000-2017)",
             range = range, col_names = TRUE) %>%
    rename_at(1:2, ~ c("measure", "regional_district")) %>%
    filter(measure != "Priority Measures") %>%
    mutate(organiziation = org) %>%
    select(organiziation, everything())
}

read_bevs_financial <- function(file, range, org) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Bevs(2000-2017)",
             range = range,
             col_types = c("text", "skip", rep("numeric", cols - 2)),
             col_names = c("measure", "foo",
                           seq(2000, length.out = cols - 2))) %>%
    mutate(organiziation = org) %>%
    select(organiziation, everything())
}

ncols_from_range <- function(range) {
  diff(letter_to_num(strsplit(gsub("[0-9]", "", range), ":")[[1]])) + 1
}

encorp_priority <- read_bevs_recovery(excel_file, "B5:U38", "Encorp Pacific")

bc_brewers_priority <- read_bevs_recovery(excel_file, range = "B138:U170", "BC Brewers")

encorp_financial <- read_bevs_financial(excel_file, "B45:U53", "Encorp Pacific")
