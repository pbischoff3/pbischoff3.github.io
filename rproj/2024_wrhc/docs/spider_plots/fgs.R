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

# all chronic or no####
fgs_chronic <- read_excel("data/fgs_chronic.xlsx")

fgs_no <- read_excel("data/fgs_no.xlsx")

cgs_chronic <- read_excel("data/cgs_chronic.xlsx")

cgs_no <- read_excel("data/cgs_no.xlsx")

# spider plot ####
## fgs ####
library(dplyr)

# Combine the datasets and add a column to identify the groups
fgs_chronic$Group <- "Chronic"
fgs_no$Group <- "No Chronic"

# Combine the datasets into one data frame
fgs_data <- rbind(fgs_chronic, fgs_no)

# Select only the columns you want to keep
fgs_data <- fgs_data %>%
  select(-`Std. Deviation`, -`Std. Error Mean`, -N, -Chronic_cond_code)

# Reshape the data frame so that each question becomes a column
fgs_data_wide <- spread(fgs_data, key = Question, value = Mean)

# Remove any NA values
fgs_data_wide <- na.omit(fgs_data_wide)

# Convert numeric columns to numeric
fgs_data_wide <- fgs_data_wide %>%
  mutate_if(is.numeric, as.numeric)

fgs_data_wide <- fgs_data_wide %>%
  rename("Science Identity" = Sci_Ident_AVE_post,
         "Career Motivation" = Sci_Career_Mot_AVE_post,
         "Science Interest" = Sci_Interest_Ave_post,
         "Self Determination" = Self_Determin_Ave_post,
         "Self Efficacy" = Self_Efficacy_Ave_post,
         "Value of Peers**" = Value_Peers_Ave_post,
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

# Define your data (using the fgs_data_wide name)
data_wide <- fgs_data_wide

# To use the fmsb package, add max and min values for each measure
data_wide <- rbind(rep(5, ncol(data_wide) - 1), rep(1, ncol(data_wide) - 1), data_wide[, -1])

# Create the PNG file
png("images/fgs/fgs.png", width = 3000, height = 2800, bg = "#DCF5E3")

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
legend("bottomleft", legend = unique(fgs_data_wide$Group), fill = colors_in, border = colors_border, bty = "o", cex = 5)

# Add title
title(main = "First-Generation: Chronic v. Not", col.main = "black", font.main = 2, cex.main = 6.5)

# Close the PNG file
dev.off()

## cgs ####
library(dplyr)

# Combine the datasets and add a column to identify the groups
cgs_chronic$Group <- "Chronic"
cgs_no$Group <- "No Chronic"

# Combine the datasets into one data frame
cgs_data <- rbind(cgs_chronic, cgs_no)

# Select only the columns you want to keep
cgs_data <- cgs_data %>%
  select(-`Std. Deviation`, -`Std. Error Mean`, -N, -Chronic_cond_code)

# Reshape the data frame so that each question becomes a column
cgs_data_wide <- spread(cgs_data, key = Question, value = Mean)

# Remove any NA values
cgs_data_wide <- na.omit(cgs_data_wide)

# Convert numeric columns to numeric
cgs_data_wide <- cgs_data_wide %>%
  mutate_if(is.numeric, as.numeric)

cgs_data_wide <- cgs_data_wide %>%
  rename("Science Identity" = Sci_Ident_AVE_post,
         "Career Motivation" = Sci_Career_Mot_AVE_post,
         "Science Interest" = Sci_Interest_Ave_post,
         "Self Determination" = Self_Determin_Ave_post,
         "Self Efficacy" = Self_Efficacy_Ave_post,
         "Value of Peers" = Value_Peers_Ave_post,
         "Sense of Belonging" = Sci_SenseOfBelong_Ave_post,
         "Science Engagement" = Sci_Engagement_Ave_post,
         "Community in Science" = Communal_Sci_Ave_post,
         "Treatment Outcomes**" = `Treatment outcomes`,
         "Doctor Interactions" = `Interactions with medical professionals`,
         "Interactions with Caregivers**" = `Interactions with caregivers`,
         "Engagement with Science" = `Engagement with science`)

# Load the required library
library(fmsb)

# Set the colors for customization
colors_border <- c("#8B4513", "#3baed4")
colors_in <- adjustcolor(c("#8B4513", "#3baed4"), alpha.f = 0.4)

# Define your data (using the cgs_data_wide name)
data_wide <- cgs_data_wide

# To use the fmsb package, add max and min values for each measure
data_wide <- rbind(rep(5, ncol(data_wide) - 1), rep(1, ncol(data_wide) - 1), data_wide[, -1])

# Create the PNG file
png("images/fgs/cgs.png", width = 3000, height = 2800, bg = "#DCF5E3")

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
legend("bottomleft", legend = unique(cgs_data_wide$Group), fill = colors_in, border = colors_border, bty = "o", cex = 5)

# Add title
title(main = "Continuing-Generation: Chronic v. Not", col.main = "black", font.main = 2, cex.main = 6.5)

# Close the PNG file
dev.off()