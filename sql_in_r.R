#SQL in R

install.packages("sqldf")
library(sqldf)





## Creates a data frame that isolates starters that are **decreasing** in ABV output over time

dec.abv <- sqldf('
  SELECT a.`Starter`, `Starter.ABV`, `Date.Brewed`
  FROM `brew_data8.18` AS a
  WHERE NOT EXISTS (
    SELECT 1
    FROM `brew_data8.18` AS b
    WHERE a.`Starter` = b.`Starter` AND a.`Date.Brewed` > b.`Date.Brewed`
      AND a.`Starter.ABV` >= b.`Starter.ABV`
  )
')

## Creates a data frame that isolates starters that are **increasing** in ABV output over time

inc.abv <- sqldf('
  SELECT a.`Starter`, `Starter.ABV`, `Date.Brewed`
  FROM `brew_data8.18` AS a
  WHERE NOT EXISTS (
    SELECT 1
    FROM `brew_data8.18` AS b
    WHERE a.`Starter` = b.`Starter` AND a.`Date.Brewed` > b.`Date.Brewed`
      AND a.`Starter.ABV` < b.`Starter.ABV`
  )
')
## Creates a box plot of the dec.abv dataframe
library(ggplot2)


ggplot(data = dec.abv, aes(x = Date.Brewed, y = Starter.ABV, color = Starter)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression lines
  labs(
    title = "Scatter Plot of Date Brewed vs. ABV Over Time",
    x = "Date Brewed",
    y = "Starter ABV"
  ) +
  theme_minimal()

## Creates a box plot of the inc.abv dataframe
library(ggplot2)


ggplot(data = inc.abv, aes(x = Date.Brewed, y = Starter.ABV, color = Starter)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression lines
  labs(
    title = "Scatter Plot of Date Brewed vs. ABV Over Time",
    x = "Date Brewed",
    y = "Starter ABV"
  ) +
  theme_minimal()



