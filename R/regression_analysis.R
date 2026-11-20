library(tidyverse)
install.packages(car)
library(car)

pokemon <- read_csv(
  "data/pokemon_cleaned.csv",
  show_col_types = FALSE
)

# Convert column names into R-friendly names
names(pokemon) <- make.names(names(pokemon))

# Check the new names
names(pokemon)

pokemon <- pokemon |>
  mutate(
    Primary.Typing = factor(Primary.Typing),
    Generation = factor(Generation),
    Legendary.Status = factor(Legendary.Status),
    Evolution.Stage = factor(Evolution.Stage),
    Alt.Form.Flag = factor(Alt.Form.Flag)
  ) |>
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

summary(full_model)
confint(full_model)
vif(full_model)

png(
  "figures/regression_diagnostics.png",
  width = 1600,
  height = 1200,
  res = 200
)

par(mfrow = c(2, 2))
plot(full_model)

dev.off()
par(mfrow = c(1, 1))