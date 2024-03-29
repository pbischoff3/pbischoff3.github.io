# reading in excel files ####
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
## names of labels
# combined_data$Question <- fct_recode(combined_data$Question,
#"Science Identity" = "Sci_Ident_AVE_post",
#"Career Motivation" = "Sci_Career_Mot_AVE_post",
#"Science Interest" = "Sci_Interest_Ave_post",
#"Self Determination" = "Self_Determin_Ave_post",
#"Self Efficacy" = "Self_Efficacy_Ave_post",
#"Value of Peers" = "Value_Peers_Ave_post",
#"Sense of Belonging" = "Sci_SenseOfBelong_Ave_post",
#"Science Engagement" = "Sci_Engagement_Ave_post",
#"Community in Science" = "Communal_Sci_Ave_post",
#"Treatment Outcomes" = "Treatment outcomes",
#"Doctor Interactions" = "Interactions with medical prof…",
#"Interactions with Caregivers" = "Interactions with caregivers",
#"Engagement with Science" = "Engagement with science")
## by school year####
### first year
chronic_1 <- read_excel("data/1_chronic.xlsx")
no_1 <- read_excel("data/1_no.xlsx")

### second year
chronic_2 <- read_excel("data/2_chronic.xlsx")
no_2 <- read_excel("data/2_no.xlsx")

### third year
chronic_3 <- read_excel("data/3_chronic.xlsx")
no_3 <- read_excel("data/3_no.xlsx")

### fourth year
chronic_4 <- read_excel("data/4_chronic.xlsx")
no_4 <- read_excel("data/4_no.xlsx")



### 1st year ####
# Load the necessary libraries if not already installed
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(forcats)) {
  install.packages("forcats")
  library(forcats)
}
# Combine the data frames into one for easier plotting
combined_data <- rbind(mutate(chronic_1, Condition = "Chronic"), mutate(no_1, Condition = "No Chronic"))

# Recode the levels of the Question variable
combined_data$Question <- fct_recode(combined_data$Question,
                                     "Science Identity" = "Sci_Ident_AVE_post",
                                     "Career Motivation" = "Sci_Career_Mot_AVE_post",
                                     "Science Interest" = "Sci_Interest_Ave_post",
                                     "Self Determination" = "Self_Determin_Ave_post",
                                     "Self Efficacy" = "Self_Efficacy_Ave_post",
                                     "Value of Peers**" = "Value_Peers_Ave_post",
                                     "Sense of Belonging" = "Sci_SenseOfBelong_Ave_post",
                                     "Science Engagement" = "Sci_Engagement_Ave_post",
                                     "Community in Science" = "Communal_Sci_Ave_post",
                                     "Treatment Outcomes**" = "Treatment outcomes",
                                     "Doctor Interactions" = "Interactions with medical prof…",
                                     "Interactions with Caregivers" = "Interactions with caregivers",
                                     "Engagement with Science" = "Engagement with science")

# Reorder the levels of the Question variable alphabetically
combined_data$Question <- fct_relevel(combined_data$Question, sort(levels(combined_data$Question)))

# Define custom colors
custom_colors <- c("Chronic" = "lightpink", "No Chronic" = "lightgreen")

# Plot the bar chart with error bars based on SEM and custom colors
ggplot(combined_data, aes(x = Question, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Adjust the width of the bars
  geom_errorbar(aes(ymin = Mean - `Std. Error Mean`, ymax = Mean + `Std. Error Mean`), position = position_dodge(width = 0.7), width = 0.25) +  # Add error bars based on SEM
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  labs(x = "Question", y = "Mean", fill = "Condition") +
  ggtitle("Freshman - Mean STEM Motivation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


### 2nd year ####
# Load the necessary libraries if not already installed
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(forcats)) {
  install.packages("forcats")
  library(forcats)
}
# Combine the data frames into one for easier plotting
combined_data <- rbind(mutate(chronic_2, Condition = "Chronic"), mutate(no_2, Condition = "No Chronic"))

# Recode the levels of the Question variable
combined_data$Question <- fct_recode(combined_data$Question,
                                     "Science Identity" = "Sci_Ident_AVE_post",
                                     "Career Motivation" = "Sci_Career_Mot_AVE_post",
                                     "Science Interest" = "Sci_Interest_Ave_post",
                                     "Self Determination" = "Self_Determin_Ave_post",
                                     "Self Efficacy" = "Self_Efficacy_Ave_post",
                                     "Value of Peers" = "Value_Peers_Ave_post",
                                     "Sense of Belonging" = "Sci_SenseOfBelong_Ave_post",
                                     "Science Engagement**" = "Sci_Engagement_Ave_post",
                                     "Community in Science" = "Communal_Sci_Ave_post",
                                     "Treatment Outcomes" = "Treatment outcomes",
                                     "Doctor Interactions" = "Interactions with medical prof…",
                                     "Interactions with Caregivers" = "Interactions with caregivers",
                                     "Engagement with Science" = "Engagement with science")

# Reorder the levels of the Question variable alphabetically
combined_data$Question <- fct_relevel(combined_data$Question, sort(levels(combined_data$Question)))

# Define custom colors
custom_colors <- c("Chronic" = "yellow", "No Chronic" = "lightgreen")

# Plot the bar chart with error bars based on SEM and custom colors
ggplot(combined_data, aes(x = Question, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Adjust the width of the bars
  geom_errorbar(aes(ymin = Mean - `Std. Error Mean`, ymax = Mean + `Std. Error Mean`), position = position_dodge(width = 0.7), width = 0.25) +  # Add error bars based on SEM
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  labs(x = "Question", y = "Mean", fill = "Condition") +
  ggtitle("Sophomore - Mean STEM Motivation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
### 3rd year ####
# Load the necessary libraries if not already installed
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(forcats)) {
  install.packages("forcats")
  library(forcats)
}
# Combine the data frames into one for easier plotting
combined_data <- rbind(mutate(chronic_3, Condition = "Chronic"), mutate(no_3, Condition = "No Chronic"))

# Recode the levels of the Question variable
combined_data$Question <- fct_recode(combined_data$Question,
                                     "Science Identity" = "Sci_Ident_AVE_post",
                                     "Career Motivation" = "Sci_Career_Mot_AVE_post",
                                     "Science Interest" = "Sci_Interest_Ave_post",
                                     "Self Determination**" = "Self_Determin_Ave_post",
                                     "Self Efficacy**" = "Self_Efficacy_Ave_post",
                                     "Value of Peers" = "Value_Peers_Ave_post",
                                     "Sense of Belonging" = "Sci_SenseOfBelong_Ave_post",
                                     "Science Engagement" = "Sci_Engagement_Ave_post",
                                     "Community in Science" = "Communal_Sci_Ave_post",
                                     "Treatment Outcomes" = "Treatment outcomes",
                                     "Doctor Interactions" = "Interactions with medical prof…",
                                     "Interactions with Caregivers**" = "Interactions with caregivers",
                                     "Engagement with Science" = "Engagement with science")

# Reorder the levels of the Question variable alphabetically
combined_data$Question <- fct_relevel(combined_data$Question, sort(levels(combined_data$Question)))

# Define custom colors
custom_colors <- c("Chronic" = "blue", "No Chronic" = "lightgreen")

# Plot the bar chart with error bars based on SEM and custom colors
ggplot(combined_data, aes(x = Question, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Adjust the width of the bars
  geom_errorbar(aes(ymin = Mean - `Std. Error Mean`, ymax = Mean + `Std. Error Mean`), position = position_dodge(width = 0.7), width = 0.25) +  # Add error bars based on SEM
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  labs(x = "Question", y = "Mean", fill = "Condition") +
  ggtitle("Junior - Mean STEM Motivation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

### 4th year ####
# Load the necessary libraries if not already installed
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(forcats)) {
  install.packages("forcats")
  library(forcats)
}
# Combine the data frames into one for easier plotting
combined_data <- rbind(mutate(chronic_1, Condition = "Chronic"), mutate(no_1, Condition = "No Chronic"))

# Recode the levels of the Question variable
combined_data$Question <- fct_recode(combined_data$Question,
                                     "Science Identity" = "Sci_Ident_AVE_post",
                                     "Career Motivation" = "Sci_Career_Mot_AVE_post",
                                     "Science Interest" = "Sci_Interest_Ave_post",
                                     "Self Determination" = "Self_Determin_Ave_post",
                                     "Self Efficacy" = "Self_Efficacy_Ave_post",
                                     "Value of Peers" = "Value_Peers_Ave_post",
                                     "Sense of Belonging" = "Sci_SenseOfBelong_Ave_post",
                                     "Science Engagement" = "Sci_Engagement_Ave_post",
                                     "Community in Science" = "Communal_Sci_Ave_post",
                                     "Treatment Outcomes" = "Treatment outcomes",
                                     "Doctor Interactions" = "Interactions with medical prof…",
                                     "Interactions with Caregivers" = "Interactions with caregivers",
                                     "Engagement with Science" = "Engagement with science")

# Reorder the levels of the Question variable alphabetically
combined_data$Question <- fct_relevel(combined_data$Question, sort(levels(combined_data$Question)))

# Define custom colors
custom_colors <- c("Chronic" = "orange", "No Chronic" = "lightgreen")

# Plot the bar chart with error bars based on SEM and custom colors
ggplot(combined_data, aes(x = Question, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Adjust the width of the bars
  geom_errorbar(aes(ymin = Mean - `Std. Error Mean`, ymax = Mean + `Std. Error Mean`), position = position_dodge(width = 0.7), width = 0.25) +  # Add error bars based on SEM
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  labs(x = "Question", y = "Mean", fill = "Condition") +
  ggtitle("Senior - Mean STEM Motivation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))




# spider plots ####

## freshman ####
library(dplyr)

# Combine the datasets and add a column to identify the groups
chronic_1$Group <- "Chronic"
no_1$Group <- "No Chronic"

# Combine the datasets into one data frame
fresh <- rbind(chronic_1, no_1)

# Select only the columns you want to keep
fresh <- fresh %>%
  select(-N, -`Std. Deviation`, -`Std. Error Mean`, -Chronic_cond_code)

# Reshape the data frame so that each question becomes a column
fresh_wide <- spread(fresh, key = Question, value = Mean)

# Remove any NA values
fresh_wide <- na.omit(fresh_wide)

# Convert numeric columns to numeric
fresh_wide <- fresh_wide %>%
  mutate_if(is.numeric, as.numeric)

fresh_wide <- fresh_wide %>%
  rename("Science Identity" = Sci_Ident_AVE_post,
         "Career Motivation" = Sci_Career_Mot_AVE_post,
         "Science Interest" = Sci_Interest_Ave_post,
         "Self Determination" = Self_Determin_Ave_post,
         "Self Efficacy" = Self_Efficacy_Ave_post,
         "Value of Peers**" = Value_Peers_Ave_post,
         "Sense of Belonging" = Sci_SenseOfBelong_Ave_post,
         "Science Engagement" = Sci_Engagement_Ave_post,
         "Community in Science" = Communal_Sci_Ave_post,
         "Treatment Outcomes**" = `Treatment outcomes`,
         "Doctor Interactions" = `Interactions with medical professionals`,
         "Interactions with Caregivers" = `Interactions with caregivers`,
         "Engagement with Science" = `Engagement with science`)

# Load the required library
library(fmsb)

# Set the colors for customization
colors_border <- c("#8B4513", "#3baed4")
colors_in <- adjustcolor(c("#8B4513", "#3baed4"), alpha.f = 0.4)

# Define your data
data_wide <- fresh_wide

# To use the fmsb package, add max and min values for each measure
data_wide <- rbind(rep(5, ncol(data_wide) - 1), rep(1, ncol(data_wide) - 1), data_wide[, -1])

# Create the PNG file
png("images/school_year/freshman.png", width = 3000, height = 2800, bg = "#DCF5E3")

# Create the radar chart with customization options
radarchart(data_wide,
           axistype = 1,
           pcol = colors_border,
           pfcol = colors_in,
           plwd = 2,
           plty = 1,
           cglcol = "black",
           cglty = 1,
           axislabcol = "black",
           caxislabels = seq(1, 5, 1),  # Adjust axis labels as needed
           cglwd = 1.3,
           vlcex = 3) 

# Add legend
legend("bottomleft", legend = unique(fresh_wide$Group), fill = colors_in, border = colors_border, bty = "o", cex = 5)

# Add title
title(main = "Freshman: Chronic v. Not", col.main = "black", font.main = 2, cex.main = 6.5)

# Close the PNG file
dev.off()

## sophomore ####
library(dplyr)

# Combine the datasets and add a column to identify the groups
chronic_2$Group <- "Chronic"
no_2$Group <- "No Chronic"

# Combine the datasets into one data frame
soph <- rbind(chronic_2, no_2)

# Select only the columns you want to keep
soph <- soph %>%
  select(-N, -`Std. Deviation`, -`Std. Error Mean`, -Chronic_cond_code)

# Reshape the data frame so that each question becomes a column
soph_wide <- spread(soph, key = Question, value = Mean)

# Remove any NA values
soph_wide <- na.omit(soph_wide)

# Convert numeric columns to numeric
soph_wide <- soph_wide %>%
  mutate_if(is.numeric, as.numeric)

soph_wide <- soph_wide %>%
  rename("Science Identity" = Sci_Ident_AVE_post,
         "Career Motivation" = Sci_Career_Mot_AVE_post,
         "Science Interest" = Sci_Interest_Ave_post,
         "Self Determination" = Self_Determin_Ave_post,
         "Self Efficacy" = Self_Efficacy_Ave_post,
         "Value of Peers" = Value_Peers_Ave_post,
         "Sense of Belonging" = Sci_SenseOfBelong_Ave_post,
         "Science Engagement**" = Sci_Engagement_Ave_post,
         "Community in Science" = Communal_Sci_Ave_post,
         "Treatment Outcomes" = `Treatment outcomes`,
         "Doctor Interactions" = `Interactions with medical professionals`,
         "Interactions with Caregivers" = `Interactions with caregivers`,
         "Engagement with Science" = `Engagement with science`)

# Load the required library
library(fmsb)

# Set the colors for customization
colors_border <- c("#8B4513", "#3baed4")
colors_in <- adjustcolor(c("#8B4513", "#3baed4"), alpha.f = 0.4)

# Define your data
data_wide <- soph_wide

# To use the fmsb package, add max and min values for each measure
data_wide <- rbind(rep(5, ncol(data_wide) - 1), rep(1, ncol(data_wide) - 1), data_wide[, -1])

# Create the PNG file
png("images/school_year/sophomore.png", width = 3000, height = 2800, bg = "#DCF5E3")

# Create the radar chart with customization options
radarchart(data_wide,
           axistype = 1,
           pcol = colors_border,
           pfcol = colors_in,
           plwd = 2,
           plty = 1,
           cglcol = "black",
           cglty = 1,
           axislabcol = "black",
           caxislabels = seq(1, 5, 1),  # Adjust axis labels as needed
           cglwd = 1.3,
           vlcex = 3) 

# Add legend
legend("bottomleft", legend = unique(soph_wide$Group), fill = colors_in, border = colors_border, bty = "o", cex = 5)

# Add title
title(main = "Sophomore: Chronic v. Not", col.main = "black", font.main = 2, cex.main = 6.5)

# Close the PNG file
dev.off()

## Junior ####
library(dplyr)

# Combine the datasets and add a column to identify the groups
chronic_3$Group <- "Chronic"
no_3$Group <- "No Chronic"

# Combine the datasets into one data frame
jun <- rbind(chronic_3, no_3)

# Select only the columns you want to keep
jun <- jun %>%
  select(-N, -`Std. Deviation`, -`Std. Error Mean`, -Chronic_cond_code)

# Reshape the data frame so that each question becomes a column
jun_wide <- spread(jun, key = Question, value = Mean)

# Remove any NA values
jun_wide <- na.omit(jun_wide)

# Convert numeric columns to numeric
jun_wide <- jun_wide %>%
  mutate_if(is.numeric, as.numeric)

jun_wide <- jun_wide %>%
  rename("Science Identity" = Sci_Ident_AVE_post,
         "Career Motivation" = Sci_Career_Mot_AVE_post,
         "Science Interest" = Sci_Interest_Ave_post,
         "Self Determination**" = Self_Determin_Ave_post,
         "Self Efficacy**" = Self_Efficacy_Ave_post,
         "Value of Peers" = Value_Peers_Ave_post,
         "Sense of Belonging" = Sci_SenseOfBelong_Ave_post,
         "Science Engagement" = Sci_Engagement_Ave_post,
         "Community in Science" = Communal_Sci_Ave_post,
         "Treatment Outcomes" = `Treatment outcomes`,
         "Doctor Interactions" = `Interactions with medical professionals`,
         "Interactions with Caregivers**" = `Interactions with caregivers`,
         "Engagement with Science" = `Engagement with science`)

# Load the required library
library(fmsb)

# Set the colors for customization
colors_border <- c("#8B4513", "#3baed4")
colors_in <- adjustcolor(c("#8B4513", "#3baed4"), alpha.f = 0.4)

# Define your data
data_wide <- jun_wide

# To use the fmsb package, add max and min values for each measure
data_wide <- rbind(rep(5, ncol(data_wide) - 1), rep(1, ncol(data_wide) - 1), data_wide[, -1])

# Create the PNG file
png("images/school_year/junior.png", width = 3000, height = 2800, bg = "#DCF5E3")

# Create the radar chart with customization options
radarchart(data_wide,
           axistype = 1,
           pcol = colors_border,
           pfcol = colors_in,
           plwd = 2,
           plty = 1,
           cglcol = "black",
           cglty = 1,
           axislabcol = "black",
           caxislabels = seq(1, 5, 1),  # Adjust axis labels as needed
           cglwd = 1.3,
           vlcex = 3) 

# Add legend
legend("bottomleft", legend = unique(jun_wide$Group), fill = colors_in, border = colors_border, bty = "o", cex = 5)

# Add title
title(main = "Junior: Chronic v. Not", col.main = "black", font.main = 2, cex.main = 6.5)

# Close the PNG file
dev.off()

## senior ####
library(dplyr)

# Combine the datasets and add a column to identify the groups
chronic_4$Group <- "Chronic"
no_4$Group <- "No Chronic"

# Combine the datasets into one data frame
senior <- rbind(chronic_4, no_4)

# Select only the columns you want to keep
senior <- senior %>%
  select(-N, -`Std. Deviation`, -`Std. Error Mean`, -Chronic_cond_code)

# Reshape the data frame so that each question becomes a column
senior_wide <- spread(senior, key = Question, value = Mean)

# Remove any NA values
senior_wide <- na.omit(senior_wide)

# Convert numeric columns to numeric
senior_wide <- senior_wide %>%
  mutate_if(is.numeric, as.numeric)

senior_wide <- senior_wide %>%
  rename("Science Identity" = Sci_Ident_AVE_post,
         "Career Motivation" = Sci_Career_Mot_AVE_post,
         "Science Interest" = Sci_Interest_Ave_post,
         "Self Determination" = Self_Determin_Ave_post,
         "Self Efficacy" = Self_Efficacy_Ave_post,
         "Value of Peers" = Value_Peers_Ave_post,
         "Sense of Belonging" = Sci_SenseOfBelong_Ave_post,
         "Science Engagement" = Sci_Engagement_Ave_post,
         "Community in Science" = Communal_Sci_Ave_post,
         "Treatment Outcomes" = `Treatment outcomes`,
         "Doctor Interactions" = `Interactions with medical professionals`,
         "Interactions with Caregivers" = `Interactions with caregivers`,
         "Engagement with Science" = `Engagement with science`)

# Load the required library
library(fmsb)

# Set the colors for customization
colors_border <- c("#8B4513", "#3baed4")
colors_in <- adjustcolor(c("#8B4513", "#3baed4"), alpha.f = 0.4)

# Define your data
data_wide <- senior_wide

# To use the fmsb package, add max and min values for each measure
data_wide <- rbind(rep(5, ncol(data_wide) - 1), rep(1, ncol(data_wide) - 1), data_wide[, -1])

# Create the PNG file
png("images/school_year/senior.png", width = 3000, height = 2800, bg = "#DCF5E3")

# Create the radar chart with customization options
radarchart(data_wide,
           axistype = 1,
           pcol = colors_border,
           pfcol = colors_in,
           plwd = 2,
           plty = 1,
           cglcol = "black",
           cglty = 1,
           axislabcol = "black",
           caxislabels = seq(1, 5, 1),  # Adjust axis labels as needed
           cglwd = 1.3,
           vlcex = 3) 

# Add legend
legend("bottomleft", legend = unique(senior_wide$Group), fill = colors_in, border = colors_border, bty = "o", cex = 5)

# Add title
title(main = "Senior: Chronic v. Not", col.main = "black", font.main = 2, cex.main = 6.5)

# Close the PNG file
dev.off()