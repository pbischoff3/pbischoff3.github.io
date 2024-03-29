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
#"Interactions with Medical Professionals" = "Interactions with medical prof…",
#"Interactions with Caregivers" = "Interactions with caregivers",
#"Engagement with Science" = "Engagement with science")

# all chronic or no####
all_chronic <- read_excel("data/all_chronic.xlsx")

all_no <- read_excel("data/4_no.xlsx")

# spider plot ####
library(dplyr)

# Combine the datasets and add a column to identify the groups
all_chronic$Group <- "Chronic"
all_no$Group <- "No Chronic"
all_no <- all_no %>% 
  select(-N, -Chronic_cond_code)

# Combine the datasets into one data frame
all_data <- rbind(all_chronic, all_no)

# Select only the columns you want to keep
all_data <- all_data %>%
  select(-`Std. Deviation`, -`Std. Error Mean`)

# Reshape the data frame so that each question becomes a column
all_data_wide <- spread(all_data, key = Question, value = Mean)

# Remove any NA values
all_data_wide <- na.omit(all_data_wide)

# Convert numeric columns to numeric
all_data_wide <- all_data_wide %>%
  mutate_if(is.numeric, as.numeric)

all_data_wide <- all_data_wide %>%
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

# Define your data (using the all_data_wide name)
data_wide <- all_data_wide

# To use the fmsb package, add max and min values for each measure
data_wide <- rbind(rep(5, ncol(data_wide) - 1), rep(1, ncol(data_wide) - 1), data_wide[, -1])

# Create the PNG file
png("images/all/all.png", width = 3000, height = 2800, bg = "#DCF5E3")

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
legend("bottomleft", legend = unique(all_data_wide$Group), fill = colors_in, border = colors_border, bty = "o", cex = 5)

# Add title
title(main = "Chronic v. Non-Chronic", col.main = "black", font.main = 2, cex.main = 6.5)

# Close the PNG file
dev.off()