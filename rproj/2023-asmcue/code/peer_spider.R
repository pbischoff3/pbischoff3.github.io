# PEER 1st####
# Load the fmsb package
library(fmsb)

# Create your data frame with shorter labels
data <- data.frame(
  "Identity" = c(3.382, 3.223),
  "Career Value" = c(4.093881857, 4.220125786),
  "Interest" = c(4.283755274, 4.371069182),
  "Actions to Promote Success" = c(3.988396624, 3.880503145),
  "Belief in One's Ability" = c(4.151898734, 3.86163522),
  "Value of Peers" = c(3.982067511, 4.106918239),
  "Sense of Belonging" = c(3.7302, 3.6887),
  "Integration" = c(3.569620253, 3.188679245),
  "Communal View of Science" = c(4.512658228, 4.559748428)
)

# To use the fmsb package, add max and min values for each measure
data <- rbind(rep(5, 9), rep(1, 9), data)

# Color vectors for customization
colors_border <- c("#9c940e", "#134f14")
colors_in <- adjustcolor(c("#bab559", "#7dad8b"), alpha.f = 0.4)

# Save the plot as a PNG with specified width, height, and resolution
png("ASMCUE2023/plots/peer_v_nonpeer.png", width = 3000, height = 2200, res = 350, bg = 'transparent')

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
legend(x = 1.25, y = 1.25, legend = c("Non-PEER", "PEER"),
       bty = "n", pch = 20, col = adjustcolor(colors_in, alpha.f = 1), text.col = "black", cex = 1.2, pt.cex = 3)

# Add the title at the top
title("PEER v. Non-PEER Science Motivation")

# Add the subtitle below the title
mtext("PEER: n=53. Non-PEER: n=316", side = 1, line = 2, cex = 0.8)
mtext("Belief in One's Ability and Integration significant at p<.05", side = 1, line = 3, cex = 0.8)


dev.off()



# No Med PEER/Non-Peer ####
# Load the fmsb package
library(fmsb)

# Create your data frame with shorter labels
data <- data.frame(
  "Identity" = c(3.211, 3.267),
  "Career Value" = c(3.920765027, 4.202020202),
  "Interest" = c(4.199453552, 4.343434343),
  "Actions to Promote Success" = c(3.855191257, 3.787878788),
  "Belief in One's Ability" = c(4.098360656, 4.0),
  "Value of Peers" = c(3.972677596, 4.01010101),
  "Sense of Belonging" = c(3.6803, 3.7879),
  "Integration" = c(3.461748634, 3.242424242),
  "Communal View of Science" = c(4.461748634, 4.575757576)
)

# To use the fmsb package, add max and min values for each measure
data <- rbind(rep(5, 9), rep(1, 9), data)

# Color vectors for customization
colors_border <- c("#9c940e", "#134f14")
colors_in <- adjustcolor(c("#bab559", "#7dad8b"), alpha.f = 0.4)

# Save the plot as a PNG with specified width, height, and resolution
png("ASMCUE2023/plots/nomed_peer_v_nonpeer.png", width = 3000, height = 2200, res = 350, bg = 'transparent')

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
legend(x = 1.25, y = 1.25, legend = c("Non-PEER", "PEER"),
       bty = "n", pch = 20, col = adjustcolor(colors_in, alpha.f = 1), text.col = "black", cex = 1.2, pt.cex = 3)

# Add the title at the top
title("No Medical Conditions: PEER v. Non-PEER Science Motivation")

# Add the subtitle below the title
mtext("PEER: n=33. Non-PEER: n=122", side = 1, line = 2, cex = 0.8)
mtext("Career Value significant at p<.05", side = 1, line = 3, cex = 0.8)

dev.off()

# Med PEER/Non-Peer ####
# Load the fmsb package
library(fmsb)

# Create your data frame with shorter labels
data <- data.frame(
  "Identity" = c(3.489, 3.15),
  "Career Value" = c(4.202749141, 4.25),
  "Interest" = c(4.336769759, 4.416666667),
  "Actions to Promote Success" = c(4.072164948, 4.033333333),
  "Belief in One's Ability" = c(4.18556701, 3.633333333),
  "Value of Peers" = c(3.987972509, 4.266666667),
  "Sense of Belonging" = c(3.7616, 3.525),
  "Integration" = c(3.637457045, 3.1),
  "Communal View of Science" = c(4.54467354, 4.533333333)
)

# To use the fmsb package, add max and min values for each measure
data <- rbind(rep(5, 9), rep(1, 9), data)

# Color vectors for customization
colors_border <- c("#9c940e", "#134f14")
colors_in <- adjustcolor(c("#bab559", "#7dad8b"), alpha.f = 0.4)

# Save the plot as a PNG with specified width, height, and resolution
png("ASMCUE2023/plots/med_peer_v_nonpeer.png", width = 3000, height = 2200, res = 350, bg = 'transparent')

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
legend(x = 1.25, y = 1.25, legend = c("Non-PEER", "PEER"),
       bty = "n", pch = 20, col = adjustcolor(colors_in, alpha.f = 1), text.col = "black", cex = 1.2, pt.cex = 3)

# Add the title at the top
title("Medical Conditions: PEER v. Non-PEER Science Motivation")

# Add the subtitle below the title
mtext("PEER: n=20. Non-PEER: n=194", side = 1, line = 2, cex = 0.8)
mtext("Belief in One's Ability and Integration significant at p<.05", side = 1, line = 3, cex = 0.8)

dev.off()
