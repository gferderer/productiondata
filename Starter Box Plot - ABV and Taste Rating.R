## Exploring relationships between Starter Identity (categorical) and ABV / Taste Ratings
##
## First Uploading and Data Cleaning
#Search for file in finder, copy file path, add additional backslashes to single one, as they will otherwise throw an error
# header =  TRUE tells us that there is a header and not to include it in the data set 
# stringsAsFactors = FALSE .

brew_data8.18 <- read.csv("C:\\Users\\HMKom\\Downloads\\Production_Brewing_Database - Cleaned Production Brewing Data (2).csv", header=TRUE, stringsAsFactors=FALSE)

#install data cleaning packagees
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)

#Inspect the Data: Explore the dataset to understand its structure, variable names, missing values, and potential issues.
# View the first few rows of the data
head(brew_data8.18)

# Check summary statistics
summary(brew_data8.18)

# Removes duplicates
brew_data8.18 <- brew_data8.18 %>% distinct()

#Lists names of columns
colnames(brew_data8.18)

# Removes columns listed, depending on your analysis you may want to remove certain columns, this list includes target values and is a good default. 
brew_data8.18 <- brew_data8.18[, !(names(brew_data8.18) %in% c("Flavor","Batch.Code","Date.Brewed","Starter.Group","Starter.Refresh.Date","Batch.Code","Fp.UCL","Fp.LCL","Diff.in.pH.from.Previous.Starter.Use","Diff.in.Sp.Gr.from.Previous.Starter.Use","Fp.Target","Helper.Column..Starter...Refresh.Date.","IS.Target","IS.UCL","IS.LCL","Ip.Target","Ip.UCL","Ip.LCL","FA.Target","FA.UCL","FA.LCL","Final.Specific.Gravity","FS.Target","FS.UCL","FS.LCL","TA.Target","TA.UCL","TA.LCL"))]

# Assuming "dependent_var" is your continuous dependent variable
# and "independent_var1" and "independent_var2" are your two categorical independent variables
# this is a 2-way ANOVA
anova_result <- aov(Starter.ABV ~ Starter * Taste.Rating.out.of.10, data = brew_data8.18)
summary(anova_result)

#ANOVA results suggest that both the "Starter" factor and the "Taste.Rating.out.of.10" factor, as well as their interaction, have statistically significant effects on the "Starter.ABV." 

install.packages("ggplot2")
library(ggplot2)

## filters out starters with less than 3 data points
counta3 <- brew_data8.18 %>% 
  group_by(Starter) %>% 
  filter(n() >= 3) %>%
  ungroup()

counta4 <- brew_data8.18 %>% 
  group_by(Starter) %>% 
  filter(n() >= 4) %>%
  ungroup()

# Create a vector of labels for each Starter category
starter_labels <- unique(counta4$Starter)

# Create the boxplot with custom labels
boxplot(Taste.Rating.out.of.10 ~ Starter, data = counta4, names = starter_labels)

# Create the boxplot with custom labels
boxplot(Starter.ABV ~ Starter, data = counta4, names = starter_labels)

install.packages("ggplot2")
library(ggplot2)

# Combined Boxplot 
# Taste Rating out of 10 in blue, Starter ABV in green. 
# While this combines them but the scale makes it difficult to tell how they relate.
combined_boxplots <- ggplot(counta4, aes(x = Starter)) +
  geom_boxplot(aes(y = Taste.Rating.out.of.10), fill = "lightblue", width = 0.5) +
  geom_boxplot(aes(y = Starter.ABV), fill = "lightgreen", width = 0.5) +
  labs(x = "Starter", y = "Values") +
  theme_minimal()

print(combined_boxplots)

# Rescale the variables
counta4$Taste.Rating.rescaled <- scale(counta4$Taste.Rating.out.of.10)
counta4$Starter.ABV.rescaled <- scale(counta4$Starter.ABV)

# Create combined scaled boxplots with legend and title
# Taste Rating out of 10 in blue, Starter ABV in green. 
# We want to select for starters with high taste rating and low ABV.
combined_scaled_boxplots <- ggplot(counta4, aes(x = Starter)) +
  geom_boxplot(aes(y = Taste.Rating.rescaled, fill = "Taste Rating"), width = 0.5) +
  geom_boxplot(aes(y = Starter.ABV.rescaled, fill = "Starter ABV"), width = 0.5) +
  labs(
    x = "Starter",
    y = "Rescaled Values",
    title = "Combined Scaled Boxplots of Taste Rating and Starter ABV"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("Taste Rating" = "lightblue", "Starter ABV" = "lightgreen"),
    name = "Variables"  # Set the legend title
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# Display the plot
print(combined_scaled_boxplots)

library(ggplot2)

# Create combined scaled boxplots with colored outliers
combined_scaled_boxplots <- ggplot(counta4, aes(x = Starter)) +
  geom_boxplot(aes(y = Taste.Rating.rescaled, fill = "Taste Rating"), width = 0.5) +
  geom_point(aes(y = Taste.Rating.rescaled, color = "Taste Rating"), position = position_jitterdodge(jitter.width = 0.1), size = 3) +
  geom_boxplot(aes(y = Starter.ABV.rescaled, fill = "Starter ABV"), width = 0.5) +
  geom_point(aes(y = Starter.ABV.rescaled, color = "Starter ABV"), position = position_jitterdodge(jitter.width = 0.1), size = 3) +
  labs(
    x = "Starter",
    y = "Rescaled Values",
    title = "Combined Scaled Boxplots of Taste Rating and Starter ABV"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("Taste Rating" = "lightblue", "Starter ABV" = "lightgreen"),
    name = "Variables"  # Set the legend title
  ) +
  scale_color_manual(
    values = c("Taste Rating" = "blue", "Starter ABV" = "green"),
    name = "Variables"  # Set the legend title
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# Display the plot
print(combined_scaled_boxplots)

install.packages("plotly")
library(plotly)

# Create combined scaled boxplots with colored outliers
combined_scaled_boxplots <- ggplot(counta4, aes(x = Starter)) +
  geom_boxplot(aes(y = Taste.Rating.rescaled, fill = "Taste Rating"), width = 0.5) +
  geom_point(aes(y = Taste.Rating.rescaled, color = "Taste Rating"), position = position_jitterdodge(jitter.width = 0.05), size = 2) +
  geom_boxplot(aes(y = Starter.ABV.rescaled, fill = "Starter ABV"), width = 0.5) +
  geom_point(aes(y = Starter.ABV.rescaled, color = "Starter ABV"), position = position_jitterdodge(jitter.width = 0.05), size = 2) +
  labs(
    x = "Starter",
    y = "Rescaled Values",
    title = "Combined Scaled Boxplots of Taste Rating and Starter ABV"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("Taste Rating" = "lightblue", "Starter ABV" = "lightgreen"),
    name = "Variables"  # Set the legend title
  ) +
  scale_color_manual(
    values = c("Taste Rating" = "blue", "Starter ABV" = "green"),
    name = "Variables"  # Set the legend title
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# Make the plot interactive using plotly
interactive_plot <- ggplotly(combined_scaled_boxplots)

# Display the interactive plot
interactive_plot










           