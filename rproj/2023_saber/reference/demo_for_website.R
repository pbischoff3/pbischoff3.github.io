# major####
# Load required library
library(waffle)

# Define the data
career_aspiration <- c(
  "Biology" = 109,
  "Non-biology STEM" = 59,
  "Pre-Professional" = 80,
  "Non-STEM" = 142
)

# Calculate the adjusted counts based on a 10x10 grid
counts <- round(career_aspiration / sum(career_aspiration) * 100)

# Define the color palette for each career aspiration
pal <- c(
  "Biology" = "#275d38",
  "Non-biology STEM" = "#8BD8F0",
  "Pre-Professional" = "#A6D19F",
  "Non-STEM" = "#074EAB"
)

# Create the waffle chart with distinct colors
chart <- waffle(counts,
                rows = 10,  # Number of rows in the chart
                colors = pal,  # Colors for each category
                size = 0.7) +  # Size of each waffle block
  theme(legend.position = "bottom")

chart

# year####
# Load required library
library(waffle)

# Define the data
class_year <- c(
  "Freshman" = 132,
  "Sophomore" = 112,
  "Junior" = 66,
  "Senior" = 80
)

# Calculate the adjusted percentages to have a total of 100 squares
total_squares <- 100
adjusted_percentages <- class_year / sum(class_year) * total_squares

# Round down the adjusted percentages to integers
counts <- floor(adjusted_percentages)

# Calculate the difference between the desired total squares and the current count
remaining_squares <- total_squares - sum(counts)

# Distribute the remaining squares to the categories with the highest fractional part
frac_parts <- adjusted_percentages - counts
sorted_indices <- order(frac_parts, decreasing = TRUE)
counts[sorted_indices[1:remaining_squares]] <- counts[sorted_indices[1:remaining_squares]] + 1

# Define the color palette for each class year
pal <- c(
  "Freshman" = "#275d38",
  "Sophomore" = "#8BD8F0",
  "Junior" = "#A6D19F",
  "Senior" = "#074EAB"
)

# Create the waffle chart with distinct colors
chart <- waffle(counts,
                rows = 10,  # Number of rows in the chart
                colors = pal,  # Colors for each category
                size = 0.7) +  # Size of each waffle block
              theme(legend.position = "bottom")  # Move legend to the bottom of the graph

chart



# career aspiration####
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
                size = 0.7) +  # Size of each waffle block
  theme(legend.position = "bottom")  # Move legend to the bottom of the graph

chart

# ethnicity ####
# Load required library
library(waffle)

# Define the data
ethnicity_data <- c(
  "Black" = 8,
  "Native American" = 3,
  "Middle Eastern" = 1,
  "Asian" = 19,
  "Latina/o" = 32,
  "Multiracial" = 8,
  "Pacific Islander" = 5,
  "White" = 314
)

# Calculate the adjusted percentages to have a total of 100 squares
total_squares <- 100
adjusted_percentages <- ethnicity_data / sum(ethnicity_data) * total_squares

# Round down the adjusted percentages to integers for categories with more than 1%
counts <- floor(adjusted_percentages[adjusted_percentages >= 1])

# Calculate the remaining squares for categories with less than 1%
remaining_squares <- total_squares - sum(counts)

# Identify categories with less than 1% representation and allocate one square to each
counts_below_1_percent <- adjusted_percentages[adjusted_percentages < 1]
counts_below_1_percent <- ifelse(counts_below_1_percent > 0, 1, 0)
remaining_squares <- remaining_squares - sum(counts_below_1_percent)

# Distribute the remaining squares to categories with more than 1% representation
counts <- c(counts, counts_below_1_percent)
frac_parts <- adjusted_percentages[adjusted_percentages >= 1] - counts[adjusted_percentages >= 1]
sorted_indices <- order(frac_parts, decreasing = TRUE)
counts[sorted_indices[1:remaining_squares]] <- counts[sorted_indices[1:remaining_squares]] + 1

# Define the color palette for each ethnicity category
pal <- c(
  "Black" = "#275d38",
  "Native American" = "#8BD8F0",
  "Middle Eastern" = "#A6D19F",
  "Asian" = "#A391CF",
  "Latina/o" = "#074EAB",
  "Multiracial" = "#BA56CC",
  "Pacific Islander" = "#25B81D",
  "White" = "#76A398"
)

# Create the waffle chart with distinct colors
chart <- waffle(counts,
                rows = 10,  # Number of rows in the chart
                colors = pal,  # Colors for each category
                size = 0.7) +  # Size of each waffle block
  theme(legend.position = "bottom")  # Move legend to the bottom of the graph

chart



# gender####
# Load required library
library(waffle)

# Define the data
gender_data <- c(
  "Men" = 201,
  "Transgender" = 2,
  "Women" = 182,
  "Fluid / Gender Non-Conforming" = 5
)

# Calculate the adjusted percentages to have a total of 100 squares
total_squares <- 100
adjusted_percentages <- gender_data / sum(gender_data) * total_squares

# Round down the adjusted percentages to integers for categories with more than 1%
counts <- floor(adjusted_percentages[adjusted_percentages >= 1])

# Calculate the remaining squares for categories with less than 1%
remaining_squares <- total_squares - sum(counts)

# Identify categories with less than 1% representation and allocate one square to each
counts_below_1_percent <- adjusted_percentages[adjusted_percentages < 1]
counts_below_1_percent <- ifelse(counts_below_1_percent > 0, 1, 0)
remaining_squares <- remaining_squares - sum(counts_below_1_percent)

# Distribute the remaining squares to categories with more than 1% representation
counts <- c(counts, counts_below_1_percent)
frac_parts <- adjusted_percentages[adjusted_percentages >= 1] - counts[adjusted_percentages >= 1]
sorted_indices <- order(frac_parts, decreasing = TRUE)
counts[sorted_indices[1:remaining_squares]] <- counts[sorted_indices[1:remaining_squares]] + 1

# Define the color palette for each gender category
pal <- c(
  "Men" = "#275d38",
  "Transgender" = "#8BD8F0",
  "Women" = "#A6D19F",
  "Fluid / Gender Non-Conforming" = "#074EAB"
)

# Create the waffle chart with distinct colors
chart <- waffle(counts,
                rows = 10,  # Number of rows in the chart
                colors = pal,  # Colors for each category
                size = 0.7) +  # Size of each waffle block
  theme(legend.position = "bottom")  # Move legend to the bottom of the graph

chart

# religious #####
# Load required library
library(waffle)

# Define the data
religion_data <- c(
  "Non-Religious" = 106,
  "Religious" = 284
)

# Calculate the adjusted percentages to have a total of 100 squares
total_squares <- 100
adjusted_percentages <- religion_data / sum(religion_data) * total_squares

# Round down the adjusted percentages to integers for categories with more than 1%
counts <- floor(adjusted_percentages[adjusted_percentages >= 1])

# Calculate the remaining squares for categories with less than 1%
remaining_squares <- total_squares - sum(counts)

# Identify categories with less than 1% representation and allocate one square to each
counts_below_1_percent <- adjusted_percentages[adjusted_percentages < 1]
counts_below_1_percent <- ifelse(counts_below_1_percent > 0, 1, 0)
remaining_squares <- remaining_squares - sum(counts_below_1_percent)

# Distribute the remaining squares to categories with more than 1% representation
counts <- c(counts, counts_below_1_percent)
frac_parts <- adjusted_percentages[adjusted_percentages >= 1] - counts[adjusted_percentages >= 1]
sorted_indices <- order(frac_parts, decreasing = TRUE)
counts[sorted_indices[1:remaining_squares]] <- counts[sorted_indices[1:remaining_squares]] + 1

# Define the color palette for each religion category
pal <- c(
  "Non-Religious" = "#275d38",
  "Religious" = "#8BD8F0"
)

# Create the waffle chart with distinct colors
chart <- waffle(counts,
                rows = 10,  # Number of rows in the chart
                colors = pal,  # Colors for each category
                size = 0.7) +  # Size of each waffle block
  theme(legend.position = "bottom")  # Move legend to the bottom of the graph

chart

# fgs####
# Load required library
library(waffle)

# Define the data
generation_data <- c(
  "Continuing Generation Student" = 290,
  "First Generation Student" = 93
)

# Calculate the adjusted percentages to have a total of 100 squares
total_squares <- 100
adjusted_percentages <- generation_data / sum(generation_data) * total_squares

# Round down the adjusted percentages to integers for categories with more than 1%
counts <- floor(adjusted_percentages[adjusted_percentages >= 1])

# Calculate the remaining squares for categories with less than 1%
remaining_squares <- total_squares - sum(counts)

# Identify categories with less than 1% representation and allocate one square to each
counts_below_1_percent <- adjusted_percentages[adjusted_percentages < 1]
counts_below_1_percent <- ifelse(counts_below_1_percent > 0, 1, 0)
remaining_squares <- remaining_squares - sum(counts_below_1_percent)

# Distribute the remaining squares to categories with more than 1% representation
counts <- c(counts, counts_below_1_percent)
frac_parts <- adjusted_percentages[adjusted_percentages >= 1] - counts[adjusted_percentages >= 1]
sorted_indices <- order(frac_parts, decreasing = TRUE)
counts[sorted_indices[1:remaining_squares]] <- counts[sorted_indices[1:remaining_squares]] + 1

# Define the color palette for each generation category
pal <- c(
  "Continuing Generation Student" = "#275d38",
  "First Generation Student" = "#8BD8F0"
)

# Create the waffle chart with distinct colors
chart <- waffle(counts,
                rows = 10,  # Number of rows in the chart
                colors = pal,  # Colors for each category
                size = 0.7) +  # Size of each waffle block
  theme(legend.position = "bottom")  # Move legend to the bottom of the graph

chart

# medical experience####
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
                rows = 10,  # Number of rows in the chart
                colors = experience_colors,  # Colors for each category
                size = 0.7) +  # Size of each waffle block
  theme(legend.position = "bottom")  # Move legend to the bottom of the graph

chart

# chronic condition####
# Load required library
library(waffle)

# Define the data
response_data <- c(
  "No" = 127,
  "Yes" = 86
)

# Calculate the adjusted percentages to have a total of 100 squares
total_squares <- 100
adjusted_percentages <- response_data / sum(response_data) * total_squares

# Round down the adjusted percentages to integers for categories with more than 1%
counts <- floor(adjusted_percentages[adjusted_percentages >= 1])

# Calculate the remaining squares for categories with less than 1%
remaining_squares <- total_squares - sum(counts)

# Identify categories with less than 1% representation and allocate one square to each
counts_below_1_percent <- adjusted_percentages[adjusted_percentages < 1]
counts_below_1_percent <- ifelse(counts_below_1_percent > 0, 1, 0)
remaining_squares <- remaining_squares - sum(counts_below_1_percent)

# Distribute the remaining squares to categories with more than 1% representation
counts <- c(counts, counts_below_1_percent)
frac_parts <- adjusted_percentages[adjusted_percentages >= 1] - counts[adjusted_percentages >= 1]
sorted_indices <- order(frac_parts, decreasing = TRUE)
counts[sorted_indices[1:remaining_squares]] <- counts[sorted_indices[1:remaining_squares]] + 1

# Define the color palette for each response category
pal <- c(
  "No" = "#8BD8F0",
  "Yes" = "#275d38"
)

# Create the waffle chart with distinct colors
chart <- waffle(counts,
                rows = 10,  # Number of rows in the chart
                colors = pal,  # Colors for each category
                size = 0.7) +  # Size of each waffle block
  theme(legend.position = "bottom")  # Move legend to the bottom of the graph

chart
