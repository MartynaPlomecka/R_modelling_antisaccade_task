setwd("~/dev/modelling_antisaccades")

#libraries
library(dplyr)
library(readr)
library(ggplot2)
table <- read_csv("data/table.csv")

table_clean <- na.omit(table)

# Calculate whether each trial was an error or not
table_clean$error <- with(table_clean, (type == 1 & stim_dir != sacc_dir) | (type == 0 & stim_dir == sacc_dir))

# Calculate mean reaction times and error rates for each subject and condition type
statistics <- table_clean %>%
  group_by(sbj, type) %>%
  summarise(
    MeanReactionTime = mean(sacc_time, na.rm = TRUE),
    MeanErrorRate = mean(error, na.rm = TRUE),
    .groups = 'drop'
  )

print(statistics)



###


# Violin plot for Mean Reaction Times
ggplot(statistics, aes(x = factor(type), y = MeanReactionTime, color = factor(type))) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2) +
  labs(title = "Mean Reaction Times by Condition",
       x = "Condition Type",
       y = "Mean Reaction Time",
       color = "Condition") +
  scale_color_discrete(labels = c("Antisaccade", "Prosaccade")) +
  theme_minimal()

# Violin plot for Mean Error Rates
ggplot(statistics, aes(x = factor(type), y = MeanErrorRate, color = factor(type))) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2) +
  labs(title = "Mean Error Rates by Condition",
       x = "Condition Type",
       y = "Mean Error Rate",
       color = "Condition") +
  scale_color_discrete(labels = c("Antisaccade", "Prosaccade")) +
  theme_minimal()
