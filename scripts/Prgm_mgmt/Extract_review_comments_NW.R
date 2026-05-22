# collate reviewer comments and write out document for each applicant
# ndb 4-Jun-25

library(tidyverse)
library(officer)
library(readxl)
library(stringr)

output_dir <- "C:/Users/dburruss/Documents/Nox_Weeds/Noxious_weed_grant_program/FY27-grants/Comments/"

data <- readxl::read_xlsx(
  "C:/Users/dburruss/Documents/Nox_Weeds/Noxious_weed_grant_program/FY27-grants/Reviews/NWMG_app_review_30APR26_AC.xlsx",
  sheet = "All_data"
) %>%
  as_tibble() %>%
  
  # identify fields that you want to keep
  select(
    ID,
    `Provide your Project Title`,
    
    # Title / Summary
    `Review 1: Provide feedback on the Title and Summary below for the applicant. (Scoring)`,
    `Review 2: Provide feedback on the Title and Summary below for the applicant. (Scoring)`,
    `Review 3: Provide feedback on the Title and Summary below for the applicant. (Scoring)`,
    
    # Justification
    `Review 1: Provide feedback on the Project Justification below for the applicant. (Scoring)`,
    `Review 2: Provide feedback on the Project Justification below for the applicant. (Scoring)`,
    `Review 3: Provide feedback on the Project Justification below for the applicant. (Scoring)`,
    
    # Timeline
    `Review 1: Provide feedback on the Project Timeline below for the applicant. (Scoring)`,
    `Review 2: Provide feedback on the Project Timeline below for the applicant. (Scoring)`,
    `Review 3: Provide feedback on the Project Timeline below for the applicant. (Scoring)`,
    
    # Budget
    `Review 1: Provide feedback on the Budget Template below for the applicant. (Scoring)`,
    `Review 2: Provide feedback on the Budget Template below for the applicant. (Scoring)`,
    `Review 3: Provide feedback on the Budget Template below for the applicant. (Scoring)`,
    
    # General
    `Review 1: Provide any general feedback, praise, or advice for applicants below if you have any beyond what was shared in the section-specific prompts above. (Scoring)`,
    `Review 2: Provide any general feedback, praise, or advice for applicants below if you have any beyond what was shared in the section-specific prompts above. (Scoring)`,
    `Review 3: Provide any general feedback, praise, or advice for applicants below if you have any beyond what was shared in the section-specific prompts above. (Scoring)`
  ) %>%
  
  # rename columns to manageable names
  rename(
    project_title = `Provide your Project Title`
  ) %>%
  rename_with(~ paste0("title_R", 1:3),          .cols = 3:5) %>%
  rename_with(~ paste0("justification_R", 1:3),  .cols = 6:8) %>%
  rename_with(~ paste0("timeline_R", 1:3),       .cols = 9:11) %>%
  rename_with(~ paste0("budget_R", 1:3),         .cols = 12:14) %>%
  rename_with(~ paste0("general_R", 1:3),        .cols = 15:17)

# Define sections and headings
sections <- list(
  title         = "Title and Summary Feedback",
  justification = "Project Justification Feedback",
  timeline      = "Project Timeline Feedback",
  budget        = "Budget Feedback",
  general       = "General Feedback"
)

# Loop through all rows in the dataset
for (row_index in 1:nrow(data)) {
  
  # Get row data
  app_data <- data[row_index, ]
  
  target_id     <- app_data$ID
  project_title <- app_data$project_title
  
  # Create new Word document
  doc <- read_docx()
  
  # Add document title (bold Normal to avoid auto-numbering from heading styles)
  doc <- doc %>%
    body_add_fpar(
      fpar(ftext(paste0("Reviewer Feedback for ", project_title),
                 prop = fp_text(bold = TRUE, font.size = 16))),
      style = "Normal"
    ) %>%
    body_add_par("", style = "Normal")
  
  # Loop through sections
  for (section_name in names(sections)) {
    
    # Add section heading (bold Normal to avoid auto-numbering from heading styles)
    doc <- doc %>%
      body_add_fpar(
        fpar(ftext(sections[[section_name]],
                   prop = fp_text(bold = TRUE, font.size = 13))),
        style = "Normal"
      ) %>%
      body_add_par("", style = "Normal")
    
    # Add reviewer comments (skip blank responses)
    for (i in 1:3) {
      
      col_name <- paste0(section_name, "_R", i)
      response <- app_data[[col_name]]
      
      text <- ifelse(
        is.na(response) || str_trim(response) == "",
        NA,
        response
      )
      
      # Only add paragraph if there is actual content
      # Bold "Reviewer N:" label inline with normal comment text
      if (!is.na(text)) {
        doc <- doc %>%
          body_add_fpar(
            fpar(
              ftext(paste0("Reviewer ", i, ": "), prop = fp_text(bold = FALSE, font.size = 11)),
              ftext(text,                          prop = fp_text(bold = FALSE, font.size = 11))
            ),
            style = "Normal"
          ) %>%
          body_add_par("", style = "Normal")
      }
    }
  }
  
  # Save document
  output_path <- file.path(
    output_dir,
    paste0(target_id, "_comments.docx")
  )
  
  print(doc, target = output_path)
}

