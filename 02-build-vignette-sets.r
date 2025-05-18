
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
library(AlgDesign)
library(gt)

### load image data ---------------------

# import interface pictures
park_img <- image_read("images/park.png")
shopping_img <- image_read("images/shopping.png")
stadium_img <- image_read("images/stadium.png")


### load prepared vignette data -----------------------
load("vignettes/vignettes_df.RData")

# vignettes_universe df already contains all possible combinations of vignettes (8 per scenario)

###Add text to images------------------------------------

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
  paste0("https://github.com/elenamurray/vignettes/blob/main/vignettes_images", "/", vig_id, ".png", "?raw=true")
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

#---------------------------
#Set up design matrix

# Only keep relevant variables
vigs_clean <- vignettes_universe %>%
  select(vig_id, actor, type, scale) %>%
  rename(vignette_id = vig_id)

# Long format for deck lookups
decks_long <- deck_combinations %>%
  pivot_longer(cols = c(park, shopping, stadium)) %>% 
  rename(vignette_id = value) %>% 
  rename(setting = name)

#Merge vignette attributes into deck data
decks_with_attrs <- decks_long %>%
  left_join(vigs_clean, by = "vignette_id") %>%
  mutate(setting = paste0(setting, "_"))  # so we can prefix columns

#Reshape to wide format (deck-level design matrix)
design_matrix <- decks_with_attrs %>%
  pivot_wider(
    id_cols = deck_id,
    names_from = setting,
    values_from = c(vignette_id, actor, type, scale),
    names_glue = "{setting}{.value}"
  ) %>%
  select(deck_id, park_vignette_id, park_actor, park_type, park_scale, shopping_vignette_id, shopping_actor, 
         shopping_type, shopping_scale, stadium_vignette_id, stadium_actor, stadium_type, stadium_scale)

## Save this as a file for next steps 
save(design_matrix, 
     file = "vignettes/design_matrix.RData")

#---------------------
#Try d-efficient sampling to get 150 deck combinations

#Create a binary code for each attribute (0 and 1 as each one only has two levels). 
#Combine them to create a binary profile for each vignette. 
vignette_profiles <- vignettes_universe %>%
  select(vig_id, actor, type, scale) %>%
  mutate(
    actor_code = ifelse(actor == "Private", 1, 0),
    type_code = ifelse(type == "video analysis", 1, 0),
    scale_code = ifelse(scale == "Low", 1, 0)) %>% 
  mutate(profile_binary = paste0(actor_code, type_code, scale_code))

#add to vignette universe
vignettes_universe_binary <- vignettes_universe %>%
  left_join(vignette_profiles %>% select(vig_id, profile_binary), by = "vig_id") %>% 
  select(vig_id, profile_binary, scenario, message_combination, actor,
         actor_message, type, type_message, scale, scale_message)

# Define the 8 binary profile codes (as strings)
binary_profiles <- c("000", "001", "010", "011", "100", "101", "110", "111")

# Create all possible combinations of profile codes across three scenarios
profile_combinations <- expand.grid(
  park_profile     = binary_profiles,
  shopping_profile = binary_profiles,
  stadium_profile  = binary_profiles,
  stringsAsFactors = FALSE
)

# Confirm total number of combinations
nrow(profile_combinations)  # Should be 512 (8 x 8 x 8)

#Add deck ID
profile_combinations <- profile_combinations %>% 
  mutate(deck_id = paste0("deck_", row_number())) %>% 
  select(deck_id, park_profile, shopping_profile, stadium_profile)

# Select decks using D-efficient sampling
#130 decks is the minimum to get d-efficiency (Ge value) over 90
set.seed(42)
d_opt <- optFederov(
  data = profile_combinations %>% select(-deck_id),
  nTrials = 130
)

# Create d-efficient decks
#Access selected row indices
selected_rows <- d_opt$rows

#Use those rows to extract from original design
selected_decks <- profile_combinations[selected_rows, ]

# Filter df2 to only rows where deck_id is in df1
deck_combinations_defficient <- deck_combinations %>%
  filter(deck_id %in% selected_decks$deck_id)

#Set up for qualtrics:
qualtrics_links_d_eff <- deck_combinations_defficient %>%
  select(park_link, shopping_link, stadium_link) %>%
  rename(
    `vig_1 (link)` = park_link,
    `vig_2 (link)` = shopping_link,
    `vig_3 (link)` = stadium_link
  )

#Save and export
write_xlsx(qualtrics_links_d_eff, "vignettes/deck_vignette_list_deff.xlsx")

#--------------------------
#Try random sampling instead of d-efficient sampling 
set.seed(42)  # for reproducibility
deck_combinations_random_sampling <- deck_combinations %>%
  sample_n(130)


#Set up for qualtrics:
qualtrics_links_random <- deck_combinations_random_sampling %>%
  select(park_link, shopping_link, stadium_link) %>%
  rename(
    `vig_1 (link)` = park_link,
    `vig_2 (link)` = shopping_link,
    `vig_3 (link)` = stadium_link
  )

#Save and export
write_xlsx(qualtrics_links_random, "vignettes/deck_vignette_list_random.xlsx")


#-----------------------
#If I need to create balanced decks 
#Each deck includes: 	
#•	1 Public, 1 Private, 1 random actor
#•	1 FRT (facial recognition), 1 AI (video analysis), 1 random type
#•	1 High scale, 1 Low scale, 1 random scale

# Step 1: Define your deck-level data frame
df_constraints <- design_matrix 

# Step 2: Function to test constraints for one row
meets_constraints <- function(row) {
  actor_vals <- c(row$park_actor, row$shopping_actor, row$stadium_actor)
  type_vals <- c(row$park_type, row$shopping_type, row$stadium_type)
  scale_vals <- c(row$park_scale, row$shopping_scale, row$stadium_scale)
  
  actor_check <- all(c("Public", "Private") %in% actor_vals)
  type_check <- all(c("facial recognition", "video analysis") %in% type_vals)
  scale_check <- all(c("High", "Low") %in% scale_vals)
  
  return(actor_check & type_check & scale_check)
}

# Step 3: Apply to all rows
df_constrained <- df_constraints %>%
  rowwise() %>%
  filter(meets_constraints(pick(everything()))) %>%
  ungroup()

# Step 4: Randomly select 130 of the qualifying decks
set.seed(42)
selected_decks_constrained <- df_constrained %>%
  sample_n(100)

# Filter df2 to only rows where deck_id is in df1
deck_combinations_constrained_random <- deck_combinations %>%
  filter(deck_id %in% selected_decks_constrained$deck_id)

#Set up for qualtrics:
qualtrics_links_constrained_random <- deck_combinations_constrained_random %>%
  select(park_link, shopping_link, stadium_link) %>%
  rename(
    `vig_1 (link)` = park_link,
    `vig_2 (link)` = shopping_link,
    `vig_3 (link)` = stadium_link
  )

#Save and export
write_xlsx(qualtrics_links_constrained_random, "vignettes/deck_vignette_list_constrained_random.xlsx")


#-------- d-efficient constrained decks
# Filter df2 to only rows where deck_id is in df1
deck_combinations_constrained_deff <- profile_combinations %>%
  filter(deck_id %in% df_constrained$deck_id)

# Select decks using D-efficient sampling
#100 decks is the minimum to get d-efficiency (Ge value) over 90
set.seed(42)
d_opt_2 <- optFederov(
  data = deck_combinations_constrained_deff %>% select(-deck_id),
  nTrials = 100
)

# Create d-efficient decks
#Access selected row indices
selected_rows_constrained_deff <- d_opt_2$rows

#Use those rows to extract from original design
selected_decks_constrained_deff <- deck_combinations_constrained_deff[selected_rows_constrained_deff, ]

# Filter df2 to only rows where deck_id is in df1
deck_combinations_constrained_deff_100 <- deck_combinations %>%
  filter(deck_id %in% selected_decks_constrained_deff$deck_id)

#Set up for qualtrics:
qualtrics_links_d_eff_constrained <- deck_combinations_constrained_deff_100 %>%
  select(park_link, shopping_link, stadium_link) %>%
  rename(
    `vig_1 (link)` = park_link,
    `vig_2 (link)` = shopping_link,
    `vig_3 (link)` = stadium_link
  )

#Save and export
write_xlsx(qualtrics_links_d_eff_constrained, "vignettes/deck_vignette_list_deff_constrained.xlsx")


#WITH DECK ID FOR QUALTRICS
#Set up for qualtrics:
qualtrics_links_d_eff_constrained_deckid <- deck_combinations_constrained_deff_100 %>%
  select(park_link, shopping_link, stadium_link, deck_id) %>%
  rename(
    `vig_1 (link)` = park_link,
    `vig_2 (link)` = shopping_link,
    `vig_3 (link)` = stadium_link
  )

#Save and export
write_xlsx(qualtrics_links_d_eff_constrained_deckid, "vignettes/deck_vignette_list_deff_constrained_id.xlsx")



