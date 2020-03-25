setwd("https://github.com/JBuffa10/clustering-physiology.git")
library(tidyverse)

# Scoring Function
custom.scoring <- function(x){
  scale(x)*10+50
}

# Tables
athleteInfo <- read.csv("AthleteInformation1.csv")
allData <- read.csv("FINAL TABLE.csv")

###################
## ATHLETE TYPES ##
###################

powerAthletes <- c('Cody Klotz','Isaiah Williams','Cruz Valencia','Cade Mahn','Derek Stanley','Drake Lubin','Jordan Johnson','Kyle Fowler','Matthew Nolan',
'Clayton Giorgio','Mycole Pruitt','Tyler Reichenborn','Zach Bryce','Ryan Hoshaw','Markel Smith','Max Datoli','Robert Ford 3','Wyatt Brockman','Robby Manor')

athleticAthletes <- c('Alex Hoff','Ben Borgann','Ben Smith','Braden Barnard','Jacob Buffa','Brett Fischer','Andrew Buescher','Anthony Green','Arturo Romero','Braxton Martinez',
'Bryce Horstman','Cade Schares','Cameron Haegele','Cody Creed','Darian Crisp','Dennis Jordan','DJ Miller','Evan Martin','Jehu','Josh Bunselmeyer','Matt Rehder',
'Taylor Robinson','Tony Adams','Zeb Roos','Dexter Swims','Eric Loomis','Ian Lohse','Nathaniel Sems')

momentumAthletes <- c('Alex Henagean','Andrew Ness','Braden Burcham','Blaise Matheny','Brady Cook','Bryce Grossius','Charlie Bourneuf','Cody Siebenberger','Elias Stevens',
'Matthew Kaiser','Hunter Counton','Jake Matheny','Keaton Greiwe','Landon Johnson','Matthew Arnold','Nathan Beaton','Nicholas Harms','Paul Noel','Ryan Malzahn', 
'Ryan Miller', 'Tate Matheny', 'Tommy Shriver','Troy Schneider','Tyson James','Zach Brasier','Zach Voss','Carson Maloney','Isaac Parks')

trainingAthletes <- c('Aaron Schromm','Adam Lackey','Adam Rakowiecki','Aidan McNamee','Alex Gladson','Anthony Altobella','Andrew Hartle','Avery Himes','Ben Axelrod',
'Ben Kennebeck','Ben Sems','Brady Heinzmann','Brandon Murphy','Brayden Arnold','Brandon Odehnal','Cannon Hritz','Cherokee Boynton','Cole Bornhop','Craig Mcgee',
'Daniel Covert','Denton McNamee','DeAngelo Sommers','Derek Archer','Drake Kanallakan','Dylan Mooney','Erik Keiweit','Erich Dodge','Evan Clawson','Evan Fufaro',
'Gavin Oswald','Gavyn Dockery','Ian Funk','Greg Hamilton','Grant Middendorf','Grant Miller','Jack Hunke','Jackson Hogan','James Bradley','James Murray','Jonathon Bonner',
'Josh Falke','JT Mabry','Louis Niemerg','Michael Long','Mike Fuller','Mitchell Davis','Nick Alberico','Nick Heine','Noah Reichman','Patrick Deemer','Tanner Marshall',
'Tommy Woods','Tripp Johns','Ty Stauss','Xavier Flores','Zane Roos')


########################
## DATA ORGANIZATIONS ##
########################

athleteInfo <-
  athleteInfo %>%
  transform(Birthday = as.Date(Birthday, format = "%m/%d/%Y")) %>%
  mutate(Gender = ifelse(Gender == "F", "F","M"))

masterData <- 
  allData %>%
  mutate(Avg..Relative.Braking.Force = (Avg..Braking.Force/System.Weight)*100,
         Avg..Relative.Braking.Power = Avg..Braking.Power/System.Weight,
         Avg..Relative.Propulsive.Power = Avg..Propulsive.Power/System.Weight) %>%
  transform(Date = as.Date(Date, format = "%m/%d/%Y"),
            Name = as.character(Name)) %>%
  left_join(athleteInfo, by = "Name") %>%
  filter(Gender == "M")

variables <- colnames(masterData)
brakingMetrics <- variables[c(5:7,12,13,16,17,19)]
propulsiveMetrics <- variables[c(9:11,14,15,47,53:55)]
balanceMetrics <- variables[c(25:34)]

clusterData <- masterData %>% 
  mutate_at(vars(brakingMetrics,propulsiveMetrics), custom.scoring) %>%
  group_by(Name) %>%
  summarise_at(vars(brakingMetrics, propulsiveMetrics), mean, na.rm = TRUE)
# remove NA
clusterData <- na.omit(clusterData)



#### View Different Logic Structures For Labelling ###

# Momentum
clusterData %>%
  filter(Avg..Relative.Propulsive.Force <= Takeoff.Velocity - 5,
         Avg..Propulsive.Velocity < Takeoff.Velocity,
         Avg..Relative.Propulsive.Force > 40) %>% view()
# No Power
clusterData %>%
  filter(Avg..Relative.Propulsive.Force < 40) %>% view()

# Power
clusterData %>%
  filter(Avg..Relative.Propulsive.Force >= Avg..Propulsive.Velocity + 5,
         Avg..Relative.Propulsive.Force >= Takeoff.Velocity,
         Avg..Relative.Propulsive.Force > 50) %>% view()


