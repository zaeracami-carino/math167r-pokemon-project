# Pokemon Health Multiple Linear Regression Analysis
# Cami Carino

# Load the packages needed for data manipulation and VIF analysis
# install.packages("car") # run only once if needed
library(tidyverse)
library(car)

# Import cleaned dataset
pokemon <- read_csv(
  "data/pokemon_cleaned.csv",
  show_col_types = FALSE
)

# Convert column names with spaces and parentheses into names that are
# easier to use in R formulas -> "Special Attack" becomes "Special.Attack"
names(pokemon) <- make.names(names(pokemon))

# Check that the data is imported properly
dim(pokemon)           # shows number of rows and columns
names(pokemon)         # displays column names
head(pokemon)          # displays the first six rows

# Prepare variables

# Convert categorical variables into factors
pokemon <- pokemon |>
  mutate(
    # Primary Typing is converted into categories such as grass, fire, water
    Primary.Typing = factor(Primary.Typing),
    # Generation is converted into categorical groups
    Generation = factor(Generation),
    # Legendary Status is converted into TRUE and FALSE categories
    Legendary.Status = factor(Legendary.Status),
    # Evolution stage is converted into categorical stages
    Evolution.Stage = factor(Evolution.Stage),
    # Alternate-form status is converted into TRUE and FALSE categories
    Alt.Form.Flag = factor(Alt.Form.Flag)
  ) |>
  # Remove rows with missing values in variables used by the model
  drop_na(
    Health,
    Attack,
    Defense,
    Special.Attack,
    Special.Defense,
    Speed,
    Height..in.,
    Weight..lbs.,
    Primary.Typing,
    Generation,
    Legendary.Status,
    Evolution.Stage,
    Alt.Form.Flag
  )

# Create the multiple linear regression model

# Health is the response variable; other variables are explanatory variables
# Base.Stat.Total is not included because it already combines Health w/ other stats,
# which would make the model redundant.
full_model <- lm(
  Health ~ Attack +
    Defense +
    Special.Attack +
    Special.Defense +
    Speed +
    Height..in. +
    Weight..lbs. +
    Primary.Typing +
    Generation +
    Legendary.Status +
    Evolution.Stage +
    Alt.Form.Flag,
  data = pokemon
)

# Examine regression results

summary(full_model) 
confint(full_model)  # calculates 95% confidence interval for each coefficient
vif(full_model)      # checks for multicollinearity among the explanatory variables

# Create regression diagnostic plots

# Open a png file in the figures folder with stated dimensions
png(
  "figures/regression_diagnostics.png",
  width = 1600,
  height = 1200,
  res = 200
)

# Arrange 4 regression diagnostic plots in a 2x2 layout
par(mfrow = c(2, 2))

# Generate diagnostic plots used to evaluate model assumptions:
# 1. Residuals vs. Fitted checks for patterns in the residuals
# 2. Normal Q-Q checks whether residuals are approximately normal
# 3. Scale-Location checks whether residual variance is consistent
# 4. Residuals vs. Leverage identifies potentially influential observations
plot(full_model)

# Close the PNG device and save figure
dev.off()

# Reset the plotting layout to one graph at a time for any plots created later
par(mfrow = c(1, 1))