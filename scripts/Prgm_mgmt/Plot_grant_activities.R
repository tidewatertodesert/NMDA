library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

# Replace with your actual file path and sheet name
raw <- read_excel("C:/Users/dburruss/Documents/NMDA_admin/Grant_organization/Grant_schedule.xlsx", sheet = 1)

# parse date
parse_dates <- function(x, year = 2025) {
  
  # Missing / N-A-like values
  if (is.na(x) || str_detect(tolower(x), "n/a|same as")) {
    return(tibble(start = NA_Date_, end = NA_Date_))
  }
  
  x <- as.character(x)
  
  # Excel serial dates stored as text or numeric
  if (str_detect(x, "^\\d{4,5}$")) {
    d <- as.Date(as.numeric(x), origin = "1899-12-30")
    return(tibble(start = d, end = d))
  }
  
  # Remove annotations and normalize text
  x <- str_replace_all(x, "\\(.*?\\)", "")
  x <- str_replace_all(x, "~", "")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  
  # Handle multiple dates joined by &
  if (str_detect(x, "&")) {
    parts <- str_split(x, "&", simplify = TRUE)
    d <- suppressWarnings(mdy(paste(parts[1], year)))
    return(tibble(start = d, end = d))
  }
  
  # Normalize dash characters
  x <- str_replace_all(x, "–|—", "-")
  
  # Date ranges
  if (str_detect(x, "-")) {
    parts <- str_split(x, "-", simplify = TRUE)
    
    start <- suppressWarnings(mdy(paste(parts[1], year)))
    end   <- suppressWarnings(mdy(paste(parts[2], year)))
    
    return(tibble(start = start, end = end))
  }
  
  # Single dates
  d <- suppressWarnings(mdy(paste(x, year)))
  tibble(start = d, end = d)
}

# ---- Reshape and parse dates ----
timeline <- raw %>%
  pivot_longer(
    cols = -Step,
    names_to = "Program",
    values_to = "DateText"
  ) %>%
  rowwise() %>%
  mutate(parsed = list(parse_dates(DateText))) %>%
  unnest(parsed) %>%
  ungroup() %>%
  filter(!is.na(start))

timeline <- timeline %>%
  mutate(
    end_plot = if_else(start == end, start + days(1), end)
  )

# ---- Activity color palette (matches previous figure) ----
activity_colors <- c(
  "Application Period"   = "#1f77b4",
  "Eligibility Screening"= "#ff7f0e",
  "Reviewer Training"    = "#2ca02c",
  "Technical Review"     = "#d62728",
  "Review Meeting"       = "#9467bd",
  "Awards Meeting"       = "#018b8b",
  "Award Notifications"  = "#e377c2",
  "MOAs Due"             = "#7f7f7f",
  "Projects Start"       = "#bcbd22"
)

# ---- Enforce step order ----
step_order <- c(
  "Application Period",
  "Eligibility Screening",
  "Reviewer Training",
  "Technical Review",
  "Review Meeting",
  "Awards Meeting",
  "Award Notifications",
  "MOAs Due",
  "Projects Start"
)

timeline <- timeline %>%
  mutate(Step = factor(Step, levels = step_order))

# ---- Square-ended Gantt bars ----
ggplot(timeline) +
  geom_segment(
    aes(
      y = Program,
      yend = Program,
      x = start,
      xend = end_plot,
      color = Step
    ),
    linewidth = 15,
    lineend = "butt"
  ) +
  scale_color_manual(values = activity_colors) +
  scale_x_date(
    date_labels = "%b",            # show only months
    date_breaks = "1 month",       # one tick per month
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  labs(
    title = "Grant Program Timelines by Activity",
    x = "Month",
    y = "Grant Program",
    color = "Activity"
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 5))) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),   # remove horizontal lines
    panel.grid.major.x = element_blank(),   # remove vertical lines
    panel.grid.minor = element_blank(),     # remove minor lines
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.ticks.x = element_line(color = "black"),  # show tick marks
    axis.ticks.length = unit(5, "pt")             # control tick length
  )
