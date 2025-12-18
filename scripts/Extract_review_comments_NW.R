#collate reviewer comments and write out document for each applicant
#ndb 4-Jun-25

library(tidyverse)
library(officer)
library(readxl)
library(stringr)

output_dir <- "C:/Users/dburruss/Documents/Nox_Weeds/Noxious_weed_grant_program/FY26-grants/Comments/"

data <- readxl::read_xlsx("C:/Users/dburruss/Documents/Nox_Weeds/Noxious_weed_grant_program/FY26-grants/Reviews/Review_noxweedgrant_fy26_2025-04-25.xlsx", 
                  sheet="submissions-report-fy26-nmdas-n") %>%
  as_tibble() %>%
  #identify fields that you want to keep
  select(ID,
         `Project Title`,
         `Review 1: Scoring: What areas of the application could have been most improved? I.e. which elements of the application lowered your scoring, and how could they have been improved?`,
         `Review 2: Scoring: What areas of the application could have been most improved? I.e. which elements of the application lowered your scoring, and how could they have been improved?`,
         `Review 3: Scoring: What areas of the application could have been most improved? I.e. which elements of the application lowered your scoring, and how could they have been improved?`,
         `Review 1: Scoring: What were the strongest aspects of this application? I.e. of the application's elements, what most made you want to recommend it for funding?`,
         `Review 2: Scoring: What were the strongest aspects of this application? I.e. of the application's elements, what most made you want to recommend it for funding?`,
         `Review 3: Scoring: What were the strongest aspects of this application? I.e. of the application's elements, what most made you want to recommend it for funding?`) %>%
  #rename the columns so they are manageable
  rename_with(
    ~ c(
      "improved_R1", 
      "improved_R2", 
      "improved_R3",
      "strongest_R1", 
      "strongest_R2", 
      "strongest_R3"
    ),
    .cols = 3:8
  )

# Define questions
questions <- list(
  strongest = "What were the strongest aspects of this application? I.e. of the application's elements, what most made you want to recommend it for funding?",
  improved = "What areas of the application could have been most improved? I.e. which elements of the application lowered your scoring, and how could they have been improved?"
)

# Loop through all rows in the dataset
for (row_index in 1:nrow(data)) {
  
  # Get the row's data
  app_data <- data[row_index, ]
  target_id <- app_data$ID
  
  # Create a new Word document
  doc <- read_docx()
  
  # Add strongest question heading and blank line
  doc <- doc %>%
    body_add_par("What were the strongest aspects of this application? I.e. of the application's elements, what most made you want to recommend it for funding?", style = "heading 1") %>%
    body_add_par("", style = "Normal")
  
  # Add responses to strongest aspects
  for (i in 1:3) {
    col_name <- paste0("strongest_R", i)
    response <- app_data[[col_name]]
    text <- ifelse(is.na(response) || str_trim(response) == "", "NA", response)
    doc <- doc %>%
      body_add_par(paste0("R", i, ": ", text), style = "Normal") %>%
      body_add_par("", style = "Normal")
  }
  
  # Add improved question heading and blank line
  doc <- doc %>%
    body_add_par("What areas of the application could have been most improved? I.e. which elements of the application lowered your scoring, and how could they have been improved?", style = "heading 1") %>%
    body_add_par("", style = "Normal")
  
  # Add responses to improved aspects
  for (i in 1:3) {
    col_name <- paste0("improved_R", i)
    response <- app_data[[col_name]]
    text <- ifelse(is.na(response) || str_trim(response) == "", "NA", response)
    doc <- doc %>%
      body_add_par(paste0("R", i, ": ", text), style = "Normal") %>%
      body_add_par("", style = "Normal")
  }
  
  # Save the document
  output_path <- file.path(output_dir, paste0(target_id, "_comments.docx"))
  print(doc, target = output_path)
}
