library(tidyverse)
# Orginal Data Set
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

# Fixed non legendary mega-evolution Pokemon into FALSE Legendary Status
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
      "audino-mega")
] <- FALSE

# Changing Generation from objects/word into integers (1 to 9)
gen_number <- c("i"=1, "ii"=2, "iii"=3, "iv"=4, "v"=5,
                "vi"=6, "vii"=7, "viii"=8, "ix"=9)
pokemon$Generation <- as.integer(gen_number[tolower(
  gsub("generation-","",pokemon$Generation))]
)

cleaned_pokemon <- pokemon
# New CSV File for the Cleaned Pokemon Data Set
write.csv(cleaned_pokemon, "pokemon_cleaned.csv", row.names = FALSE)

# ==========================
# Summary of Data set
# ==========================
# Summary of Entire Data Set
summary(cleaned_pokemon)

# Total Battle Stats
summary(cleaned_pokemon$`Base Stat Total`)
