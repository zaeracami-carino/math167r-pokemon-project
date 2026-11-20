#visuals 

#importing cleaned data set
cleaned_pokemon <- read.csv("pokemon_cleaned.csv")
all_pokemon <- cleaned_pokemon

#checking to see if data imported correctly
head(all_pokemon)
names(all_pokemon)
str(all_pokemon)

#health histogram
#checks health values & entries
#counts total num health entries
length(all_pokemon$Health)
#counts missing values
sum(is.na(all_pokemon$Health))
#counts usuable values
sum(!is.na(all_pokemon$Health))
#adds all counts from bins
sum(hist(
  all_pokemon$Health,
  breaks = 10,
  plot = FALSE)
  $counts)

#histogram graph
hist(
  all_pokemon$Health,
  breaks = 10,
  col = "gold",
  border = "black",
  main = "Distribution of Pokemon Health",
  xlab = "Health", 
  ylab = "Frequency",
)

#health vs attack scatterplot
#checks health and attack data
#checks data type of attack
class(all_pokemon$Attack)
#checks data type of health
class(all_pokemon$Health)
#counts missing attack values
sum(is.na(all_pokemon$Attack))
#counts missing health values
sum(is.na(all_pokemon$Health))
#counts rows with both attack and health value
sum(complete.cases(all_pokemon$Attack, all_pokemon$Health))
#shows min and max attack values
range(all_pokemon$Attack, na.rm = TRUE)
#shows min and max health values
range(all_pokemon$Health, na.rm = TRUE)

#scatterplot graph
transparent_darkgrey <- adjustcolor("darkgrey", alpha.f = 0.6)
plot(
  all_pokemon$Attack,
  all_pokemon$Health,
  col = transparent_purple,
  pch = 16,
  cex = 0.50,
  main = "Pokemon Health vs Attack",
  xlab = "Attack",
  ylab = "Health",
)

#health vs weight scatterplot
#check weight variable
#checks data type
class(all_pokemon$Weight..lbs.)
#counts missing weight values
sum(is.na(all_pokemon$Weight..lbs.))
#shows smallest and largest weight values
range(all_pokemon$Weight..lbs., na.rm = TRUE)

#scatterplot graph
trasnparent_darkgrey <- adjustcolor("darkgrey", alpha.f = 0.6 )
plot(
  all_pokemon$Weight..lbs.,
  all_pokemon$Health,
  col = transparent_purple,
  pch = 16,
  cex = 0.50,
  main = "Pokemon Health vs. Weight",
  xlab = "Weight (lbs)",
  ylab = "Health"
)

#health by legendary status boxplot
#verifying legendary status
table(all_pokemon$Legendary.Status)
levels(factor(all_pokemon$Legendary.Status))

#boxplot graph
boxplot(
  all_pokemon$Health ~ all_pokemon$Legendary.Status,
  #category values gray = non legend, gold = legend
  col = c("red", "gold"), 
  border = "black",
  outcex = 0.5,
  names = c("Non-Legendary", "Legendary"),
  horizontal = TRUE,
  main = "Pokemon Health by Legendary Status",
  xlab = "Health",
  ylab = "Legendary Status"
)

#health by evolution stage boxplot
#verifying evolution stages
#checks data type
class(all_pokemon$Evolution.Stage)
#counts missing values
sum(is.na(all_pokemon$Evolution.Stage))
#counts how many for each stage
table(all_pokemon$Evolution.Stage)

#converts evolution stage into grapable categories
evolution_stage <- factor(
  all_pokemon$Evolution.Stage,
  levels = c(1, 2, 3),
  labels = c("Stage 1", "Stage 2", "Stage 3")
)

#boxplot graph
boxplot(
  all_pokemon$Health ~ evolution_stage,
  col = c("red", "blue", "gold"),
  border = "black",
  outcex = 0.5,
  horizontal = TRUE,
  main = "Pokemon Health by Evolution Stage",
  xlab = "Health",
  ylab = "Evolution Stage"
)

#health across generations boxplot
#checks generation variables
#checks data type
class(all_pokemon$Generation)
#counts pokemon per generation
table(all_pokemon$Generation)
#counts missing generations
sum(is.na(all_pokemon$Generation))

#converts generation names in labels
generation <- factor(
  all_pokemon$Generation,
  levels = c(
    "generation-i", "generation-ii", "generation-iii",
    "generation-iv", "generation-v", "generation-vi",
    "generation-vii", "generation-viii", "generation-ix"
  ),
  labels = c("I", "II", "III", "IV", "V", "VI",
             "VII", "VIII", "IX")
)

#boxplot graph
boxplot(
  all_pokemon$Health ~ generation,
  col = "blue",
  border = "black",
  outcex = 0.5,
  main = "Pokemon Health by Generation",
  horizontal = TRUE,
  xlab = "Health",
  ylab = "Generation"
)

#mean health by primary type bar graph
#checking variables types 
class(all_pokemon$Health)
class(all_pokemon$Primary.Typing)
#checking missing values
sum(is.na(all_pokemon$Health))
sum(is.na(all_pokemon$Primary.Typing))
#checks num of primary types
length(unique(all_pokemon$Primary.Typing))
#displays primary type categories/frequencies
table(all_pokemon$Primary.Typing)

#calculates mean health
mean_health <- tapply(
  all_pokemon$Health,
  all_pokemon$Primary.Typing,
  mean,
  na.rm = TRUE
)
#sorts type from high to low mean health
mean_health <- sort(mean_health, decreasing = TRUE)

#bargraph 
barplot(
  mean_health,
  col = "red",
  border = "black",
  las = 2,
  cex.names = 0.75,
  #extends y axis
  ylim = c(0, max(mean_health) * 1.20),
  main = "Mean Pokemon Health by Primary Type",
  xlab = "Primary Type",
  ylab = "Mean Health"
)


