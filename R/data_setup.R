library(tidyverse)
# Original Data Set
pokemon <- read_csv("data/all_pokemon_data.csv")

dim(pokemon)
names(pokemon)
head(pokemon)

# ===============================
# Data Cleaning - Zaider Pimentel
# ===============================
# Fixed two non legendary Pokemon into FALSE Legendary Status
pokemon$`Legendary Status`[
  pokemon$Name %in% 
    c("meganium","yanmega")
] <- FALSE

# Fixed non legendary and non legendary mega-evolution Pokemon into 
# FALSE Legendary Status
pokemon$`Legendary Status`[
  pokemon$Name %in% 
    c("venusaur-mega", "charizard-mega-x", "charizard-mega-y", "blastoise-mega",
      "beedrill-mega", "pidgeot-mega", "alakazam-mega", "slowbro-mega", 
      "gengar-mega", "kangaskhan-mega", "pinsir-mega", "gyarados-mega", 
      "aerodactyl-mega", "ampharos-mega", "steelix-mega", "scizor-mega", 
      "heracross-mega", "houndoom-mega", "tyranitar-mega", "sceptile-mega",
      "blaziken-mega", "swampert-mega", "gardevoir-mega", "sableye-mega",
      "mawile-mega", "aggron-mega", "medicham-mega", "manectric-mega", 
      "sharpedo-mega", "camerupt-mega", "altaria-mega", "absol-mega",
      "glalie-mega", "salamence-mega", "metagross-mega", "lopunny-mega",
      "garchomp-mega", "lucario-mega", "abomasnow-mega", "gallade-mega",
      "audino-mega", "great-tusk", "scream-tail", "brute-bonnet", "flutter-mane",
      "lither-wing", "sandy-shocks", "iron-treads", "iron-bundle", "iron-hands",
      "iron-jugulis", "iron-moth", "iron-thorns")
] <- FALSE

# Fixed Legendary Pokemon to legendary status
pokemon$`Legendary Status`[
  pokemon$Name %in% 
    c("iron-boulder","gouging-fire")
] <- TRUE

# Use this code to look at the cleaned data set
head(pokemon)
str(pokemon)

# New CSV File for the Cleaned Pokemon Data Set
write.csv(pokemon, "data/pokemon_cleaned.csv", row.names = FALSE)

# USE THIS CODE TO LOAD CLEAN POKEMON DATA SET
cleaned_pokemon <- read.csv("data/pokemon_cleaned.csv")

# =============================================
# Descriptive Data Processing - Zaider Pimentel
# =============================================

# Summary of Entire Data set
summary(cleaned_pokemon)

# Copy of All Pokemon
all_pokemon <- cleaned_pokemon

# Only Legendary Pokemon
legendary <- cleaned_pokemon[cleaned_pokemon$Legendary.Status == "TRUE",]

# Only Non-Legendary Pokemon
regular <- cleaned_pokemon[cleaned_pokemon$Legendary.Status == "FALSE",]

battleStatsCate <- c("Base.Stat.Total", "Health", "Attack", "Defense", 
                 "Special.Attack", "Special.Defense", "Speed", 
                 "Height..in.", "Weight..lbs.", "Catch.Rate")

# ==========================================================
# Summary of Battle Stats from All Pokemon - Zaider Pimentel
# ==========================================================

# This function create a data frames based on Pokemon status (Legendary, Not, & All)
descriptive_funct <- function(data, class) {
  data.frame(
    pokemonGroup = class,
    BattleStatsCate = battleStatsCate,
    Mean = sapply(data[battleStatsCate], mean, na.rm = TRUE),
    Median = sapply(data[battleStatsCate], median, na.rm = TRUE),
    Minimum = sapply(data[battleStatsCate], min, na.rm = TRUE),
    
    First_Quartile = sapply(data[battleStatsCate], quantile, 
                            prob = 0.25, na.rm = TRUE),
    
    Third_Quartile = sapply(data[battleStatsCate], quantile, 
                            prob = 0.75, na.rm = TRUE),
    
    Maximum = sapply(data[battleStatsCate], max, na.rm = TRUE),
    IQR = sapply(data[battleStatsCate], IQR, na.rm = TRUE),
    Standard.Deviation = sapply(data[battleStatsCate], sd, na.rm = TRUE)
  )
}

# using function to create data frames for stats
all_stats <- descriptive_funct(all_pokemon, "All Pokemon")
legendary_stats <- descriptive_funct(legendary, "Legendary Pokemon")
non_legendary_stats <- descriptive_funct(regular, "Non-Lengendary Pokemon")

# Viewing the Summarized Data for all, only legendary, and non-legendary Pokemon
print(head(all_stats))
print(head(legendary_stats))
print(head(non_legendary_stats))

# =====================================
# Correlation Testing - Zaider Pimentel
# =====================================
# IMPORTANT NOTE: Health statistic will primary comparison with another statistic

# Correlation Function
cor_function <- function(data1, data2) {
  # Calculates Correlation Coefficient
  correlation_coe <- cor(data1, data2, use = "complete.obs")
  
  # Correlation Testing
  correlation_testing <- cor.test(data1, data2)
  
  # Extract P value
  p_val <- correlation_testing$p.value
  
  print(paste("Correlation:", correlation_coe))
  print(paste("P Value: ", p_val))
  print("=======================================================================")
  print(correlation_testing)
  print("=======================================================================")
}

# Correlation Between Health & Attack
all_health_v_attk <- cor_function(all_pokemon$Health, all_pokemon$Attack)
legendary_health_v_attk <- cor_function(legendary$Health, legendary$Attack)
regular_health_v_attk <- cor_function(regular$Health, regular$Attack)

# Correlation Between Health & Defense
all_health_v_def <- cor_function(all_pokemon$Health, all_pokemon$Defense)
legendary_health_v_def <- cor_function(legendary$Health, legendary$Defense)
regular_health_v_def <- cor_function(regular$Health, regular$Defense)

# Correlation Between Health & Special Attack
all_health_v_SpAttk <- cor_function(all_pokemon$Health, all_pokemon$Special.Attack)
legendary_health_v_SpAttk <- cor_function(legendary$Health, legendary$Special.Attack)
regular_health_v_SpAttk <- cor_function(regular$Health, regular$Special.Attack)

# Correlation Between Health & Special Defense
all_health_v_SpDef <- cor_function(all_pokemon$Health, all_pokemon$Special.Defense)
legendary_health_v_SpDef <- cor_function(legendary$Health, legendary$Special.Defense)
regular_health_v_SpDef <- cor_function(regular$Health, regular$Special.Defense)

# Correlation Between Health & Speed
all_health_v_Speed <- cor_function(all_pokemon$Health, all_pokemon$Speed)
legendary_health_v_Speed <- cor_function(legendary$Health, legendary$Speed)
regular_health_v_Speed <- cor_function(regular$Health, regular$Speed)

# Correlation Between Health & Catch Rate (CR)
all_health_v_CR <- cor_function(all_pokemon$Health, all_pokemon$Catch.Rate)
legendary_health_v_CR <- cor_function(legendary$Health, legendary$Catch.Rate)
regular_health_v_CR <- cor_function(regular$Health, regular$Catch.Rate)

# Correlation Between Health & Weight (lbs)
all_health_v_weight <- cor_function(all_pokemon$Health, all_pokemon$Weight..lbs.)
legendary_health_v_weight <- cor_function(legendary$Health, legendary$Weight..lbs.)
regular_health_v_weight <- cor_function(regular$Health, regular$Weight..lbs.)

# Correlation Between Health & Height (in)
all_health_v_height <- cor_function(all_pokemon$Health, all_pokemon$Height..in.)
legendary_health_v_height <- cor_function(legendary$Health, legendary$Height..in.)
regular_health_v_height <- cor_function(regular$Health, regular$Height..in.)

# =============================
# Basic Plots 
# =============================