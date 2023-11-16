# heatmaps ####
## pre ####
library(ggplot2)

pre <- read.csv("ABRCMS_2023/pre.csv")

# Assuming 'post' is your data frame
# If not, replace 'post' with the actual name of your data frame

# Convert the 'variable' column to a factor to ensure correct ordering
pre$variable <- factor(pre$variable, levels = unique(pre$variable))

# Create a heatmap with borders
pre_heatmap <- ggplot(pre, aes(x = group, y = variable, fill = mean)) +
  geom_tile(color = "black", size = 0.5, height = 0.75) +  # Add borders and adjust position
  geom_text(aes(label = sprintf("%.2f", mean)), vjust = .5, size = 4, color = "white") +  # Add mean values
  scale_fill_gradient(low = "yellow", high = "blue", limits = c(1, 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        legend.position = "bottom") +
  labs(title = "Pre-Semester", x = "", y = "", fill = "Mean")

# Print the heatmap
print(pre_heatmap)

## post ####
library(ggplot2)

post <- read.csv("ABRCMS_2023/post.csv")

# Assuming 'post' is your data frame
# If not, replace 'post' with the actual name of your data frame

# Convert the 'variable' column to a factor to ensure correct ordering
post$variable <- factor(post$variable, levels = unique(post$variable))

# Create a heatmap with borders
post_heatmap <- ggplot(post, aes(x = group, y = variable, fill = mean)) +
  geom_tile(color = "black", size = 0.5, height = 0.75) +  # Add borders and adjust position
  geom_text(aes(label = sprintf("%.2f", mean)), vjust = .5, size = 4, color = "white") +  # Add mean values
  scale_fill_gradient(low = "magenta", high = "seagreen3", limits = c(1, 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        legend.position = "bottom") +
  labs(title = "Post-Semester", x = "", y = "", fill = "Mean")

# Print the heatmap
print(post_heatmap)

## pre/comp comparison####
library(ggplot2)

comp <- read.csv("ABRCMS_2023/comp.csv")

# Assuming 'comp' is your data frame
# If not, replace 'comp' with the actual name of your data frame

# Convert the 'variable' column to a factor to ensure correct ordering
comp$variable <- factor(comp$variable, levels = unique(comp$variable))

# Create a heatmap with borders
comp_heatmap <- ggplot(comp, aes(x = group, y = variable, fill = mean)) +
  geom_tile(color = "black", size = 0.5, height = 0.75) +  # Add borders and adjust position
  geom_text(aes(label = sprintf("%.2f", mean)), vjust = .5, size = 4, color = "white") +  # Add mean values
  scale_fill_gradient(low = "tomato", high = "deepskyblue", limits = c(1, 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        legend.position = "bottom") +
  labs(title = "Comparison Pre/Post", x = "", y = "", fill = "Mean")

# Print the heatmap
print(comp_heatmap)

# spider maps ####

## pre ####
# Load the required libraries
library(fmsb)

# Read your data from the CSV file
file_path <- "ABRCMS_2023/pre.csv"
data <- read.csv(file_path)

# Select only the relevant columns (group, variable, mean)
data_selected <- data[, c("group", "variable", "mean")]

# Reshape the data for spider chart
data_wide <- spread(data_selected, key = "variable", value = "mean")

# To use the fmsb package, add max and min values for each measure
data_wide <- rbind(rep(5, ncol(data_wide) - 1), rep(1, ncol(data_wide) - 1), data_wide[, -1])

# Color vectors for customization
colors_border <- c("#FFD133", "#3372FF")
colors_in <- adjustcolor(c("#FFD133", "#3372FF"), alpha.f = 0.4)

# Create the radar chart with customization options
radarchart(data_wide,
           axistype = 1,
           pcol = colors_border,
           pfcol = colors_in,
           plwd = 2,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(1, 5, 1),  # Adjust the axis labels as needed
           cglwd = 0.8,
           vlcex = 0.8,
           vlcol = rep("gray", ncol(data_wide) - 1)
) 

# Add legend
legend("bottomright", legend = c("With Medical Experiences", "No Medical Conditions"), fill = colors_in, border = colors_border, bty = "o", cex = .8)

# Add title
title(main = "Pre-Semester", col.main = "black", font.main = 3, cex.main = 1.2)

## post ####
# Load the required libraries
library(fmsb)

# Read your data from the CSV file
file_path <- "ABRCMS_2023/post.csv"
data <- read.csv(file_path)

# Select only the relevant columns (group, variable, mean)
data_selected <- data[, c("group", "variable", "mean")]

# Reshape the data for spider chart
data_wide <- spread(data_selected, key = "variable", value = "mean")

# To use the fmsb package, add max and min values for each measure
data_wide <- rbind(rep(5, ncol(data_wide) - 1), rep(1, ncol(data_wide) - 1), data_wide[, -1])

# Color vectors for customization
colors_border <- c("#23b083", "#CC33FF")
colors_in <- adjustcolor(c("#23b083", "#CC33FF"), alpha.f = 0.4)

# Create the radar chart with customization options
radarchart(data_wide,
           axistype = 1,
           pcol = colors_border,
           pfcol = colors_in,
           plwd = 2,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(1, 5, 1),  # Adjust the axis labels as needed
           cglwd = 0.8,
           vlcex = 0.8,
           vlcol = rep("gray", ncol(data_wide) - 1)
) 

# Add legend
legend("bottomright", legend = c("With Medical Experiences", "No Medical Conditions"), fill = colors_in, border = colors_border, bty = "o", cex = .8)

# Add title
title(main = "Post-Semester", col.main = "black", font.main = 3, cex.main = 1.2)

## comp ####
# Load the required libraries
library(fmsb)

# Read your data from the CSV file
file_path <- "ABRCMS_2023/comp.csv"
data <- read.csv(file_path)

# Select only the relevant columns (group, variable, mean)
data_selected <- data[, c("group", "variable", "mean")]

# Reshape the data for spider chart
data_wide <- spread(data_selected, key = "variable", value = "mean")

# To use the fmsb package, add max and min values for each measure
data_wide <- rbind(rep(5, ncol(data_wide) - 1), rep(1, ncol(data_wide) - 1), data_wide[, -1])

# Color vectors for customization
colors_border <- c("#20839e", "#FF3347")
colors_in <- adjustcolor(c("#20839e", "#FF3347"), alpha.f = 0.4)

# Create the radar chart with customization options
radarchart(data_wide,
           axistype = 1,
           pcol = colors_border,
           pfcol = colors_in,
           plwd = 2,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(1, 5, 1),  # Adjust the axis labels as needed
           cglwd = 0.8,
           vlcex = 0.8,
           vlcol = rep("gray", ncol(data_wide) - 1)
) 

# Add legend
legend("bottomright", legend = c("Post-Semester", "Pre-Semester"), fill = colors_in, border = colors_border, bty = "o", cex = .8)

# Add title
title(main = "Pre/Post-Semester Comparison", col.main = "black", font.main = 3, cex.main = 1.2)