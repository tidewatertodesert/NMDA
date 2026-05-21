library(officer)
library(readxl)
library(tidyverse)
library(readr)
library(scales)
library(RDCOMClient)
library(qpdf)

#USE THIS TO INSTALL RDCOMClient
# install.packages("remotes")
# remotes::install_github("omegahat/RDCOMClient")

# UPDATE TEMPLATE AND OUTPUT DIRECTORY LOCATION HERE
template <- "//nmda2/DATA/APR/FY27_MOA_Working/DOG/APR_MOA_Template_20260512_NWP.docx"     #MOA template 
grant_table <- "//nmda2/DATA/APR/FY27_MOA_Working/DOG/Grant_data_2026.xlsx"    #location of grant data table
output_dir <- "//nmda2/DATA/APR/FY27_MOA_Working/DOG/Draft_MOA/"    #where MOA's will be written to
budget_dir <- "//nmda2/DATA/APR/FY27_MOA_Working/DOG/BudgetPDFs"    #location of Budgets
exhibitb <- "//nmda2/DATA/APR/FY27_MOA_Working/DOG/DOG_Exhibit_B.pdf"    #Exhibit B with file path

# Read in grant data
grant_dat <- readxl::read_xlsx(
  grant_table,
  col_types = "text"
) %>%
  as_tibble()

dir.create(output_dir, showWarnings = FALSE)

# Open Word once
word_app <- COMCreate("Word.Application")
word_app[["Visible"]] <- FALSE

# Loop through records
for (i in seq_len(nrow(grant_dat))) {
  
  doc <- read_docx(template)
  
  doc <- doc %>%
    body_replace_all_text("{{CONTRACTOR}}", grant_dat$CONTRACTOR[i], fixed = TRUE) %>%
    body_replace_all_text("{{CONTRNAME}}", grant_dat$CONTRNAME[i], fixed = TRUE) %>%
    body_replace_all_text("{{CONTRADDRESS}}", grant_dat$CONTRADDRESS[i], fixed = TRUE) %>%
    body_replace_all_text("{{CONTRTEL}}", grant_dat$CONTRTEL[i], fixed = TRUE) %>%
    body_replace_all_text("{{CONTREMAIL}}", grant_dat$CONTREMAIL[i], fixed = TRUE) %>%
    body_replace_all_text("{{PROJTITLE}}", grant_dat$PROJTITLE[i], fixed = TRUE) %>%
    body_replace_all_text(
      "{{AMOUNT}}",
      scales::dollar(as.numeric(grant_dat$AMOUNT[i]), accuracy = 0.01),
      fixed = TRUE
    )
  
  # Safe filename
  base_name <- paste0(
    grant_dat$ID[i], "_",
    "MOA_",
    gsub("[^A-Za-z0-9]+", "_", grant_dat$CONTRACTOR[i])
  )
  
  docx_file <- file.path(paste0(output_dir,"temp"), paste0(base_name, ".docx"))
  pdf_file  <- file.path(paste0(output_dir,"temp"), paste0(base_name, ".pdf"))
  
  # Save DOCX
  print(doc, target = docx_file)
  
  # Open DOCX in Word
  word_doc <- word_app$Documents()$Open(normalizePath(docx_file))
  
  # Export as PDF
  word_doc$SaveAs(normalizePath(pdf_file), FileFormat = 17)
  
  # Close document
  word_doc$Close(FALSE)
}

# Quit Word
word_app$Quit()

# APPEND BUDGET PDFS and 

drafts <- list.files(paste0(output_dir,"temp"), pattern = "\\.pdf$")
budgets <- list.files(budget_dir, pattern = "\\.pdf$")

# Loop through draft PDFs
for (draft_file in drafts) {
  
  # Extract 4-digit ID from draft filename
  id <- str_extract(draft_file, "^\\d{4}")

  # Find matching budget file
  budget_file <- budgets[str_detect(budgets, paste0("^", id))]

  # Skip if no match found
  if (length(budget_file) == 0) {
    message("No budget match for: ", draft_file)
    next
  }

  # Full paths
  draft_path  <- file.path(paste0(output_dir,"temp"), draft_file)
  budget_path <- file.path(budget_dir, budget_file[1])

  # Final output path
  output_path <- file.path(output_dir, draft_file)
  
  # Combine PDFs directly into output directory
  pdf_combine(
    input = c(draft_path, budget_path, exhibitb),
    output = output_path
  )
  
  message("Created: ", output_path)
}

  
  