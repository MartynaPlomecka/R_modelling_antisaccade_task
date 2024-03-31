#libraries
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
#####
df <- na.omit(read_csv("data/table.csv"))

# add "A" to each value in the 'sbj_id' column
df$sbj_id <- paste0("A", df$sbj_id)
write_csv(df, "data/df.csv")

# additional tables
old_long <- read_excel("data/old_long.xlsx")
yng_long <- read_excel("data/yng_long.xlsx")


# Create a vector of old and young subjects for efficient lookup
old_subjects <- old_long$Subjects
young_subjects <- yng_long$Subjects

# Function to determine age based on subject ID
determine_age <- function(subject_id) {
  if (subject_id %in% old_subjects) {
    return(1)
  } else if (subject_id %in% young_subjects) {
    return(0)
  } else {
    return(NA) # Return NA if subject ID doesn't match either group
  }
}

# Apply the function to the 'sbj_id' column to create the 'age' column
df$age <- sapply(df$sbj_id, determine_age)

# Display the first few rows to verify the addition
head(df)

####
library(dplyr)


num_trials_old_subjects <- sum(df$age == 1)
num_trials_young_subjects <- sum(df$age == 0)

# Define correct trial execution based on condition
df <- df %>%
  mutate(correct_trial = if_else((type == 1 & stim_dir == sacc_dir) | (type == 0 & stim_dir != sacc_dir), TRUE, FALSE))

# Calculate mean RT and error rates for old and young subjects across conditions
mean_rt <- df %>%
  group_by(age, type) %>%
  summarize(mean_rt = mean(sacc_time), .groups = 'drop')

error_rates <- df %>%
  group_by(age, type) %>%
  summarize(error_rate = mean(!correct_trial), .groups = 'drop')

# Displaying the results
print(paste("Number of trials for old subjects:", num_trials_old_subjects))
print(paste("Number of trails for young subjects:", num_trials_young_subjects))

print("Mean Reaction Times (RT) and Error Rates:")
print(mean_rt)
print(error_rates)


###
# Update dataframe with "correctness" and "condition" columns
df <- df %>%
  mutate(correctness = if_else(correct_trial == TRUE, 1, 0),
         condition = if_else(type == 1, "pro", "anti"))
head(df)

#### selecting relevant columns onky
df_selected <- df %>% 
  select(sbj_id, sacc_time, correctness, condition)

df_selected$condition <- as.character(df_selected$condition)

head(df_selected)

write_csv(df, "data/df_ddm.csv")
