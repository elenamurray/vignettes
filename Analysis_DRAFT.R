#Test Data Run 
library(dplyr)
library(stringr)
library(tidyr)
library(readr)

#Import 
raw_data <- read.csv("/Users/elenamurray/Documents/Documents/HERTIE MASTERS/Semester 4/Thesis/build-vignettes-elena/Results/Thesis+Survey_May+8,+2025_15.27.csv")

test_data_wide <- raw_data %>% 
  filter(str_detect(StartDate, "2025-05-08")) 

transposed_df <- as.data.frame(t(test_data_wide))
write_csv(transposed_df, "transposed_df.csv")


### load prepared vignette data -----------------------
load("/Users/elenamurray/Documents/Documents/HERTIE MASTERS/Semester 4/Thesis/build-vignettes-elena/vignettes/design_matrix.RData")

write_csv(design_matrix, "design_matrix.csv")


