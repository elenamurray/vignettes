
## load packages and functions ------------------
library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)
library(janitor)
library(stringi)


## functions to process vignettes data ---------

## vignettes: content

#Function to wrap text
str_wrap_string <- function(string, width = 65) {
  out <- stringi::stri_wrap(string, width = width, whitespace_only = TRUE) 
  out <- paste(out, collapse = "\n")
  return(out)
}

#Function to create vignette text for each scenario
import_vignettes_content <- function(scenario = "park") {
  vignettes_content_df <- read_xlsx("messages/vignettes_content.xlsx", sheet = paste0("scenario_", scenario))
  vignettes_content_df$scenario <- scenario
  vignettes_content_df$title_message_split <- map_chr(vignettes_content_df$title, str_wrap_string, width = 40)
  vignettes_content_df$actor_message_split <- map_chr(vignettes_content_df$actor_message, str_wrap_string, width = 40)
  vignettes_content_df$type_message_split <- map_chr(vignettes_content_df$type_message, str_wrap_string, width = 42)
  vignettes_content_df$scale_message_split <- map_chr(vignettes_content_df$scale_message, str_wrap_string, width = 42)
  
  vignettes_content_df$title_lines <- str_count(vignettes_content_df$title_message_split, fixed("\n")) + 1
  
  vignettes_content_df$actor_lines <- str_count(vignettes_content_df$actor_message_split, fixed("\n")) + 1

  vignettes_content_df$type_lines <- str_count(vignettes_content_df$type_message_split, fixed("\n")) + 1
  
  vignettes_content_df$scale_lines <- str_count(vignettes_content_df$scale_message_split, fixed("\n")) + 1
  
  vignettes_content_df$message_combination <- str_c(vignettes_content_df$scenario,
                                             vignettes_content_df$actor,
                                             vignettes_content_df$type,
                                             vignettes_content_df$scale,
                                                 sep = "-")
  vignettes_content_df$vig_id <- paste(scenario, "vig", seq_len(nrow(vignettes_content_df)), sep = "-")
  vignettes_content_df %<>% relocate(scenario, vig_id, message_combination)
  return(vignettes_content_df)
}

#Set up park vignettes
park_vignettes_text <- import_vignettes_content(scenario = "park")

#Set up stadium vignettes
stadium_vignettes_text <- import_vignettes_content(scenario = "stadium")

#Set up shopping vignettes
shopping_vignettes_text <- import_vignettes_content(scenario = "shopping")


## generate full vignette data frames ---------------------------------
vignettes_universe <- rbind.data.frame(park_vignettes_text, shopping_vignettes_text, stadium_vignettes_text)

#Save
save(park_vignettes_text, shopping_vignettes_text, stadium_vignettes_text,  
     vignettes_universe, 
     file = "vignettes/vignettes_df.RData")


