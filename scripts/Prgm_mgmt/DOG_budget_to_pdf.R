library(stringr)
library(officer)
library(readxl)
library(tidyverse)
library(readr)
library(scales)
library(RDCOMClient)
library(qpdf)

grant_table <- "//nmda2/DATA/APR/FY27_MOA_Working/DOG/Grant_data_2026.xlsx"    #location of grant data table

# Read in DOG MOA Table data (uses readxl and tidyverse)
grant_dat <- readxl::read_xlsx(
  grant_table,
  col_types = "text"
) %>%
  as_tibble()

dir.create(output_dir, showWarnings = FALSE)



budgetdir <- "//nmda2/DATA/APR/APR Programs/LOE/FY27 DO Grant/FY27 Applications/" 
files <- list.files(budgetdir, recursive = TRUE, pattern = "\\.xlsx$")
files <- files[!grepl("INELIGIBLE", files, ignore.case = TRUE)]


base <- gsub("REVISED-", "", files)
result <- files[!duplicated(base, fromLast = TRUE)]
files <- result

files2 <- str_replace(
  files,
  "^[0-9]+",
  ~ sprintf("%04d", as.integer(.x))
)

pattern <- paste(grant_dat$ID, collapse = "|")
files_filtered <- files2[grepl(pattern, files2)]

Files_filtered2 <- sub("^0+([0-9]+)", "\\1", files_filtered)


final_files <- paste0("//nmda2/DATA/APR/APR Programs/LOE/FY27 DO Grant/FY27 Applications/", Files_filtered2)
rm(files_filtered, Files_filtered2, files2, pattern, result, base)

# Budget PDF conversion loop

ps_script <- "C:/Users/acolin/OneDrive - New Mexico State University/Documents/NMDA/scripts/Prgm_mgmt/Auto_MOA/PowerShell/excel_to_pdf.ps1"

output_dir <- "//nmda2/DATA/APR/FY27_MOA_Working/DOG/BudgetPDFs"

# create directory if needed
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

for (input_file in final_files) {
  
  # get filename without extension
  base_name <- tools::file_path_sans_ext(basename(input_file))
  
  # create full output PDF path
  output_file <- file.path(output_dir, paste0(base_name, ".pdf"))
  
  # run PowerShell script
  system2(
    command = "powershell",
    args = c(
      "-ExecutionPolicy", "Bypass",
      "-File", shQuote(ps_script),
      "-inputFile", shQuote(input_file),
      "-outputFile", shQuote(output_file)
    )
  )
}

# version of above that attempts to append app number

for (input_file in final_files) {
  
  # extract folder name like "32 - Hagerman-Dexter SWCD"
  parent_folder <- basename(dirname(input_file))
  
  # extract leading number before " - "
  folder_num <- sub("^([0-9]+) - .*", "\\1", parent_folder)
  
  # original filename without extension
  base_name <- tools::file_path_sans_ext(basename(input_file))
  
  # prepend number to output filename
  output_file <- file.path(
    output_dir,
    paste0(folder_num, "-", base_name, ".pdf")
  )
  
  system2(
    command = "powershell",
    args = c(
      "-ExecutionPolicy", "Bypass",
      "-File", shQuote(ps_script),
      "-inputFile", shQuote(input_file),
      "-outputFile", shQuote(output_file)
    )
  )
}