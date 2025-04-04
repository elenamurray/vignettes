
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

load("vignettes/rdata/vignettes_df.RData")

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
    title <- df$title[i]
    actor_message <- df$actor_message_split[i]
    type_message <- df$type_message_split[i]
    scale_message <- df$scale_message_split[i]
    
    # Select the correct image for the scenario
    base_img <- switch(scenario,
                       park = park_img,
                       shopping = shopping_img,
                       stadium = stadium_img)
    
    # Define specific text locations for each scenario
    text_locations <- list(
      park = "+500+180",       # Position for park image
      shopping = "+650+190",   # Position for shopping image
      stadium = "+500+200"     # Position for stadium image
    )
    
    # Get the specific text location for the current scenario
    text_location <- text_locations[[scenario]]

    
    # Compose the text for the vignette
    vignette_text <- paste(title, "\n",
                           actor_message, "\n",
                           type_message, "\n",
                           scale_message, sep = "")
    
    # Create the image with annotated text
    vignette_img <- base_img %>%
      image_annotate(vignette_text, size = 32, color = "black", location = text_location, font = "Helvetica")
    
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
  paste0("https://github.com/elenamurray/vignettes", scenario, "/", vig_id, ".jpg")
}

deck_combinations <- deck_combinations %>%
  mutate(
    park_image = paste0(park, ".jpg"),
    shopping_image = paste0(shopping, ".jpg"),
    stadium_image = paste0(stadium, ".jpg"),
    park_link = generate_link("park", park),
    shopping_link = generate_link("shopping", shopping),
    stadium_link = generate_link("stadium", stadium),
    deck_id = paste0("deck_", row_number())
  )

# Reorder columns for clarity
deck_combinations <- deck_combinations %>%
  select(deck_id, park, park_image, park_link, shopping, shopping_image, shopping_link, stadium, stadium_image, stadium_link)

#export
save(deck_combinations,
     file = "vignettes/deck_vignette_list.RDa")


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
save(deck_combinations,
     file = "vignettes/deck_vignette_list.xlsx")

write_xlsx(qualtrics_links_df, "vignettes/rdata/qualtrics_links_flat.xlsx")

# Display the first few rows to confirm
print(head(qualtrics_links_df))














