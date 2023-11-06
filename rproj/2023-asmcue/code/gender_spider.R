# Gender####
# Load the fmsb package
library(fmsb)

# Create your data frame with shorter labels
data <- data.frame(
  "Identity" = c(3.125, 3.574),
  "Career Value" = c(4.02247191, 4.185585586),
  "Interest" = c(4.179775281, 4.401801802),
  "Actions to Promote Success" = c(3.970037453, 3.981981982),
  "Belief in One's Ability" = c(4.020599251, 4.209009009),
  "Value of Peers" = c(4.071161049, 3.936936937),
  "Sense of Belonging" = c(3.618, 3.8405),
  "Integration" = c(3.29588015, 3.727927928),
  "Communal View of Science" = c(4.556179775, 4.497297297)
)

# To use the fmsb package, add max and min values for each measure
data <- rbind(rep(5, 9), rep(1, 9), data)

# Color vectors for customization
colors_border <- c("#7a225a", "#1b1f54")
colors_in <- adjustcolor(c("#c486ad", "#424285"), alpha.f = 0.4)

# Save the plot as a PNG with specified width, height, and resolution
png("ASMCUE2023/plots/gender.png", width = 3000, height = 2200, res = 350, bg = 'transparent')

# Create the radar chart with customization options
radarchart(data,
           axistype = 1,
           pcol = colors_border,
           pfcol = colors_in,
           plwd = 4,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(1, 5, 1),  # Adjust the axis labels as needed
           cglwd = 0.8,
           vlcex = 0.8,
           vlcol = c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray")
)

# Add a legend
legend(x = 1.25, y = 1.25, legend = c("Women", "Men"),
       bty = "n", pch = 20, col = adjustcolor(colors_in, alpha.f = 1), text.col = "black", cex = 1.2, pt.cex = 3)

# Add the title at the top
title("Women v. Men Science Motivation")

# Add the subtitle below the title
mtext("Women: n=178. Men: n=185", side = 1, line = 2, cex = 0.8)
mtext("Identity, Interest, Belief in One's Ability, Sense of Belonging, and Integration significant at p<.05", side = 1, line = 3, cex = 0.8)

dev.off()


# No Medical Gender ####
# Load the fmsb package
library(fmsb)

# Create your data frame with shorter labels
data <- data.frame(
  "Identity" = c(3.083, 3.324),
  "Career Value" = c(3.882051282, 4.042145594),
  "Interest" = c(4.148717949, 4.295019157),
  "Actions to Promote Success" = c(3.815384615, 3.881226054),
  "Belief in One's Ability" = c(4.076923077, 4.095785441),
  "Value of Peers" = c(4.107692308, 3.904214559),
  "Sense of Belonging" = c(3.6154, 3.796),
  "Integration" = c(3.215384615, 3.586206897),
  "Communal View of Science" = c(4.58974359, 4.45210728)
)

# To use the fmsb package, add max and min values for each measure
data <- rbind(rep(5, 9), rep(1, 9), data)

# Color vectors for customization
colors_border <- c("#7a225a", "#1b1f54")
colors_in <- adjustcolor(c("#c486ad", "#424285"), alpha.f = 0.4)

# Save the plot as a PNG with specified width, height, and resolution
png("ASMCUE2023/plots/nomed_gender.png", width = 3000, height = 2200, res = 350, bg = 'transparent')

# Create the radar chart with customization options
radarchart(data,
           axistype = 1,
           pcol = colors_border,
           pfcol = colors_in,
           plwd = 4,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(1, 5, 1),  # Adjust the axis labels as needed
           cglwd = 0.8,
           vlcex = 0.8,
           vlcol = c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray")
)

# Add a legend
legend(x = 1.25, y = 1.25, legend = c("Women", "Men"),
       bty = "n", pch = 20, col = adjustcolor(colors_in, alpha.f = 1), text.col = "black", cex = 1.2, pt.cex = 3)

# Add the title at the top
title("No Medical Experiences: Women v. Men Science Motivation")

# Add the subtitle below the title
mtext("Women: n=65. Men: n=87", side = 1, line = 2, cex = 0.8)
mtext("Integration significant at p<.05", side = 1, line = 3, cex = 0.8)

dev.off()


# Medical Gender####
# Load the fmsb package
library(fmsb)

# Create your data frame with shorter labels
data <- data.frame(
  "Identity" = c(3.149, 3.796),
  "Career Value" = c(4.103244838, 4.31292517),
  "Interest" = c(4.197640118, 4.496598639),
  "Actions to Promote Success" = c(4.05899705, 4.071428571),
  "Belief in One's Ability" = c(3.98820059, 4.30952381),
  "Value of Peers" = c(4.050147493, 3.965986395),
  "Sense of Belonging" = c(3.6195, 3.8801),
  "Integration" = c(3.342182891, 3.853741497),
  "Communal View of Science" = c(4.536873156, 4.537414966)
)

# To use the fmsb package, add max and min values for each measure
data <- rbind(rep(5, 9), rep(1, 9), data)

# Color vectors for customization
colors_border <- c("#7a225a", "#1b1f54")
colors_in <- adjustcolor(c("#c486ad", "#424285"), alpha.f = 0.4)

# Save the plot as a PNG with specified width, height, and resolution
png("ASMCUE2023/plots/med_gender.png", width = 3000, height = 2200, res = 350, bg = 'transparent')

# Create the radar chart with customization options
radarchart(data,
           axistype = 1,
           pcol = colors_border,
           pfcol = colors_in,
           plwd = 4,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(1, 5, 1),  # Adjust the axis labels as needed
           cglwd = 0.8,
           vlcex = 0.8,
           vlcol = c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray")
)

# Add a legend
legend(x = 1.25, y = 1.25, legend = c("Women", "Men"),
       bty = "n", pch = 20, col = adjustcolor(colors_in, alpha.f = 1), text.col = "black", cex = 1.2, pt.cex = 3)

# Add the title at the top
title("Medical Experiences: Women v. Men Science Motivation")

# Add the subtitle below the title
mtext("Women: n=113. Men: n=98", side = 1, line = 2, cex = 0.8)
mtext("Identity, Interest, Belief in One's Ability, Sense of Belonging, and Integration significant at p<.05", side = 1, line = 3, cex = 0.8)

dev.off()

