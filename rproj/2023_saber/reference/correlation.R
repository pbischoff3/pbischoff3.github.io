# Install and load the 'readr' package for reading the data
# install.packages("readr")
library(readr)
library(corrplot)

# Create a variable with the dataset
data <- "Variable,Sci_Ident_AVE,Sci_Engagement_Ave,Sci_SenseOfBelong_Ave,Sci_Career_Mot_AVE,Sci_Interest_Ave,Self_Determin_Ave,Self_Efficacy_Ave,Value_Peers_Ave,Communal_Sci_Ave,Med_interrupt_academics
Sci_Ident_AVE,1,.664**,.602**,.579**,.676**,.452**,.523**,.110*,.261**,-0.001
Sci_Engagement_Ave,.664**,1,.601**,.507**,.609**,.448**,.454**,.158**,.285**,0.102
Sci_SenseOfBelong_Ave,.602**,.601**,1,.471**,.583**,.497**,.532**,.312**,.315**,-.150*
Sci_Career_Mot_AVE,.579**,.507**,.471**,1,.581**,.425**,.440**,.247**,.257**,0.006
Sci_Interest_Ave,.676**,.609**,.583**,.581**,1,.520**,.500**,.189**,.343**,0.029
Self_Determin_Ave,.452**,.448**,.497**,.425**,.520**,1,.509**,.260**,.221**,-0.095
Self_Efficacy_Ave,.523**,.454**,.532**,.440**,.500**,.509**,1,.201**,.216**,-.164*
Value_Peers_Ave,.110*,.158**,.312**,.247**,.189**,.260**,.201**,1,.214**,0.004
Communal_Sci_Ave,.261**,.285**,.315**,.257**,.343**,.221**,.216**,.214**,1,-0.013
Med_interrupt_academics,-0.001,0.102,-.150*,0.006,0.029,-0.095,-.164*,0.004,-0.013,1"

# Read the data into a dataframe
df <- read.csv(text = data, check.names = FALSE)

# Remove the asterisks and convert the columns to numeric
df[-1] <- sapply(df[-1], function(x) as.numeric(gsub("\\**", "", x)))

# Create a subset of the dataframe without the first row and column
df_subset <- df[-1, -1]

# Extract the labels for the columns from the first row of df
col_labels <- unname(df[1, -1])

# Extract the labels for the rows from the first column of df
row_labels <- unname(df[-1, 1])

# Convert the subset to a correlation matrix
corr_matrix <- cor(df_subset)

# Define the bottom and top colors for the gradient
bottom_color <- "#074EAB"
top_color <- "#275d38"

# Create a gradient of colors from bottom to top, passing through white
n_colors <- 100  # Number of colors in the gradient
colors <- c(colorRampPalette(c(bottom_color, "white"))(n_colors / 2), colorRampPalette(c("white", top_color))(n_colors / 2))

# Create the correlation plot using corrplot with the gradient colors
corrplot(corr_matrix, type = 'lower', method = 'color', tl.col = "black", tl.srt = 45, col = colors)