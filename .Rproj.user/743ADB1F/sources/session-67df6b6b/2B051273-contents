library(officer)
library(readxl)
library(tidyverse)
library(readr)
library(scales)

# This script searches the word document for these elements and replaces them with information from the grant data
# The information is replaced excacly how it appears in the excel document
# {{GRANTEE}}
# {{PROJECT_TITLE}}
# {{AMOUNT}}

{{AGREEMENTSTART}} <- "July 15, 2025"
{{AGREEMENTEND}} <- "October 31, 2026"


template <- "C:/Users/dburruss/Documents/GitHub/NMDA/scripts/Prgm_mgmt/Auto_MOA/Templates/MOA_Template.docx"
output_dir <- "C:/Users/dburruss/Documents/GitHub/NMDA/scripts/Prgm_mgmt/Auto_MOA/Draft_MOAs/"

# Read in the grantee data to write to the MOA template (Grant_data Excel file)
grant_dat <- readxl::read_xlsx(
  "C:/Users/dburruss/Documents/GitHub/NMDA/scripts/Prgm_mgmt/Auto_MOA/Templates/Grant_data.xlsx"
) %>%
  as_tibble()

dir.create(output_dir, showWarnings = FALSE) #creates a folder specified by output_dir only if it doesn't exist

# Loop through the find and replace arguments for each Grantee
for (i in seq_len(nrow(grant_dat))) {
  
  doc <- read_docx(template)
  
  doc <- doc %>%
    body_replace_all_text("{{GRANTEE}}", grant_dat$GRANTEE[i], fixed = TRUE) %>%
    body_replace_all_text("{{PROJECT_TITLE}}", grant_dat$TITLE[i], fixed = TRUE) %>%
    body_replace_all_text("{{AMOUNT}}", scales::dollar(grant_dat$AMOUNT[i]), fixed = TRUE)
  
  # Write out new MOA, replacing blanks spaces in file name
  out_file <- paste0(
    "MOA_",
    grant_dat$ID[i], "_",
    gsub("[^A-Za-z0-9]+", "_", grant_dat$GRANTEE[i]),
    ".docx"
  )
  
  print(doc, target = file.path(output_dir, out_file))
}
