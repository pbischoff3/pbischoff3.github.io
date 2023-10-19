# Create an empty data frame to store the results
chi_squared_results <- data.frame(Species = character(0), ChiSquaredStatistic = numeric(0), pValue = numeric(0))

# Define the model organisms
model_organisms <- c("Arabidopsis thaliana Present (Y/N)", 
                     "Zea mays Present (Y/N)", 
                     "Taraxacum officinale Present (Y/N)", 
                     "Brachypodium distachyon (Y/N)", 
                     "Nicotania sp. (Y/N)")

# Loop through each model organism and perform the chi-squared test
for (organism in model_organisms) {
  # Create a contingency table
  contingency_table <- table(df$`Research Greenhouse (Y/N)`, df[[organism]])
  
  # Perform the chi-squared test
  chi_squared_test <- chisq.test(contingency_table)
  
  # Append the results to the data frame
  chi_squared_results <- rbind(chi_squared_results, data.frame(Species = organism, ChiSquaredStatistic = chi_squared_test$statistic, pValue = chi_squared_test$p.value))
}

# Print the table of chi-squared statistics for all species
print(chi_squared_results)

# Original p-value plot
p_value_plot <- ggplot(chi_squared_results, aes(x = Species, y = pValue)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Chi-Squared Test Results for Species",
       x = "Species",
       y = "p-Value") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add horizontal line at p=.05 
p_value_plot + 
  geom_hline(yintercept=.05, linetype="dashed", color = "red") +
  annotate("text", x = 3, y = .04, label = ".05", color="red", size=4)