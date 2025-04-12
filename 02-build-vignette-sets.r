
## load packages and functions ------------------

library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)
library(janitor)
library(magick)
library(summarytools)
library(ragg)
library(magick)
library(dplyr)
library(writexl)

### load image data ---------------------

# import interface pictures
park_img <- image_read("images/park.png")
shopping_img <- image_read("images/shopping.png")
stadium_img <- image_read("images/stadium.png")


## load prepared vignette data -----------------------

load("vignettes/vignettes_df.RData")

#vignettes_universe df already contains all possible combinations of vignettes (8 per scenario)

#Add text to images 
# Function to create vignette images
create_vignette_images <- function(df, output_dir = "vignettes_images") {
  
  # Create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Iterate over each row in the dataframe to create images
  for (i in seq_len(nrow(df))) {
    # Extract row data
    scenario <- df$scenario[i]
    vig_id <- df$vig_id[i]
    actor_message <- df$actor_message_split[i]
    type_message <- df$type_message_split[i]
    scale_message <- df$scale_message_split[i]
    
    # Select the correct image for the scenario
    base_img <- switch(scenario,
                       park = park_img,
                       shopping = shopping_img,
                       stadium = stadium_img)

    
    # Compose the text for the vignette
    vignette_text <- paste(actor_message, "\n",
                           type_message, "\n",
                           scale_message, sep = "")
    
    # Define vertical spacing between lines (in pixels)
    line_spacing <- 50
    
    # Apply annotations one by one with spacing
    vignette_img <- base_img %>%
      # Line 1: Bold actor message
      image_annotate(actor_message, size = 28, color = "black",
                     location = "+502+290",
                     font = "Arial Rounded MT Bold") %>%
      # Line 2: Type message
      image_annotate(type_message, size = 28, color = "black",
                     location = "+502+410",
                     font = "Arial") %>%
      # Line 3: Scale message
      image_annotate(scale_message, size = 28, color = "black",
                     location = "+502+650",
                     font = "Arial")
    
    # Create a unique file name
    file_name <- paste0(output_dir, "/", vig_id, ".png")
    
    # Save the vignette image
    image_write(vignette_img, path = file_name, format = "png")
  }
  
  message("Vignette images created successfully in: ", output_dir)
}

# Run the function
create_vignette_images(vignettes_universe)


### prepare decks / vignette ids data frame for qualtrics ---------------------------------------
# Filter by scenarios to get vignette IDs for each scenario
park_vignettes <- vignettes_universe %>% filter(scenario == "park") %>% pull(vig_id)
shopping_vignettes <- vignettes_universe %>% filter(scenario == "shopping") %>% pull(vig_id)
stadium_vignettes <- vignettes_universe %>% filter(scenario == "stadium") %>% pull(vig_id)

# Generate all combinations of vignettes (one from each scenario)
deck_combinations <- expand.grid(
  park = park_vignettes,
  shopping = shopping_vignettes,
  stadium = stadium_vignettes,
  stringsAsFactors = FALSE
)

# Generate image paths and Qualtrics links
generate_link <- function(scenario, vig_id) {
  paste0("https://raw.githubusercontent.com/elenamurray/vignettes/refs/heads/main/vignettes_images", "/", vig_id, ".png")
}


#Create all possible combinations of text+images 
deck_combinations <- deck_combinations %>%
  mutate(
    park_image = paste0(park, ".png"),
    shopping_image = paste0(shopping, ".png"),
    stadium_image = paste0(stadium, ".png"),
    park_link = generate_link("park", park),
    shopping_link = generate_link("shopping", shopping),
    stadium_link = generate_link("stadium", stadium),
    deck_id = paste0("deck_", row_number())
  )

# Reorder columns for clarity
deck_combinations <- deck_combinations %>%
  select(deck_id, park, park_image, park_link, shopping, shopping_image, shopping_link, stadium, stadium_image, stadium_link)

### set up Qualtrics image links deck spreadsheet  -----------------------------------------

# Transform the deck_combinations dataframe
qualtrics_links_df <- deck_combinations %>%
  select(park_link, shopping_link, stadium_link) %>%
  rename(
    `vig_1 (link)` = park_link,
    `vig_2 (link)` = shopping_link,
    `vig_3 (link)` = stadium_link
  )

# Save the transformed dataframe to Excel
#export
write_xlsx(qualtrics_links_df, "vignettes/deck_vignette_list.xlsx")

#shorter test version
write_xlsx(head(qualtrics_links_df, 150), "vignettes/deck_vignette_list_150.xlsx")







magick_fonts()
magick_options()

magick::font_config()






