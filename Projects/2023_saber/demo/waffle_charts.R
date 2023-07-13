# color pal####
"#275d38"
"#8BD8F0"
"#A6D19F"
"#A391CF"
"#074EAB"
"#BA56CC"
"#25B81D"
"#76A398"


# medical experience waffle#### 
library(waffle)

# Define the data
medical_experience <- c(
  "No" = 167,
  "Yes" = 223
)

# Calculate the counts based on a 10x10 grid
counts <- round(medical_experience / sum(medical_experience) * 100)

# Define colors for each category
experience_colors <- c(
  "No" = "#8BD8F0",
  "Yes" = "#275d38"
)

# Create the waffle chart
chart <- waffle(counts,
                rows = 5,  # Number of rows in the chart
                colors = experience_colors,  # Colors for each category
                size = 0.7,  # Size of each waffle block
                title = "Medical Experience") + # Chart title
  theme(legend.position = "bottom")  # Move legend to the bottom of the graph

chart

# Set the file path
file_path <- "Projects/2023_saber/plots/medical.png"
width <- 1050
height <- 330

# Save the chart as a PNG file with specified dimensions
png(file_path, width = width, height = height, res = 96)
print(chart)
dev.off()

# career waffle####
library(waffle)

# Define the data
career_aspiration <- c(
  "Doctor" = 51,
  "Dentist" = 28,
  "Health Care Professional" = 80,
  "STEM Professional" = 77,
  "Non-STEM" = 154
)

# Calculate the adjusted counts based on a 10x10 grid
counts <- round(career_aspiration / sum(career_aspiration) * 100)

# Define colors for each career aspiration
career_colors <- c(
  "Doctor" = "#275d38",
  "Dentist" = "#8BD8F0",
  "Health Care Professional" = "#A6D19F",
  "STEM Professional" = "#A391CF",
  "Non-STEM" = "#074EAB"
)

# Create the waffle chart with distinct shades of green
chart <- waffle(counts,
                rows = 10,  # Number of rows in the chart
                colors = career_colors,  # Colors for each category
                size = 0.7,  # Size of each waffle block
                title = "Career Aspiration") + 
  theme(legend.position = "bottom")  # Move legend to the bottom of the graph

chart

# Set the file path
file_path <- "Projects/2023_saber/plots/career.png"
width <- 600
height <- 600

# Save the chart as a PNG file with pixel dimensions
png(file_path, width = width, height = height, res = 96)
print(chart)
dev.off()


# ethnicity waffle####
library(waffle)

# Define the data
ethnicity <- c(
  "Black" = 2.051282051,
  "Native American" = 0.769230769,
  "Middle Eastern" = 0.256410256,
  "Asian" = 4.871794872,
  "Latina/o" = 8.205128205,
  "Multiracial" = 2.051282051,
  "Pacific Islander" = 1.282051282,
  "White" = 80.51282051
)

# Calculate the counts based on a 10x10 grid
counts <- round(ethnicity / 100 * 100)

# Define colors for each ethnicity
ethnicity_colors <- c(
  "Black" = "#275d38",
  "Native American" = "#8BD8F0",
  "Middle Eastern" = "#76A398",
  "Asian" = "#A391CF",
  "Latina/o" = "#074EAB",
  "Multiracial" = "#BA56CC",
  "Pacific Islander" = "#25B81D",
  "White" = "#A6D19F"
)

# Create the waffle chart with distinct shades of green
chart <- waffle(counts,
       rows = 10,  # Number of rows in the chart
       colors = ethnicity_colors,  # Colors for each category
       size = 0.7,  # Size of each waffle block
       title = "Ethnicity") + # Chart title
theme(legend.position = "bottom")  # Move legend to the bottom of the graph

chart

# Set the file path
file_path <- "Projects/2023_saber/plots/ethnicity.png"
width <- 600
height <- 600

# Save the chart as a PNG file with pixel dimensions
png(file_path, width = width, height = height, res = 96)
print(chart)
dev.off()

