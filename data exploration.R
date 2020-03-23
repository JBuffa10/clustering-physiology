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

powerAthletes <- c('Aaron Schromm','Marcus Greer','Jordan Johnson','Evan Martin','Ben Sems','Derek Stanley','Drake Lubin','Jackson Burrell',
                   'Clayton Giorgio','Mike Fuller','Tony Adams','Mycole Pruitt','Josh Sutton','Ian Lohse','Eric Loomis','Jack Hunke','Tyler Reichenborn',
                   'Robby Manor','Matthew Nolan','Darian Crisp','Evan Fufaro')
momentumAthletes <- c('Zach Brasier','Tripp Johns','Blaise Matheny','Bryce Grossius','Zeb Roos','Jake Matheny','Ian Funk','Matthew Arnold',
                      'Ryan Malzahn','Tate Matheny','James Murray','Jonathon Bonner','Dennis Jordan' ,'Zane Roos','Tommy Woods','Elias Stevens','Zach Voss',
                      'Paul Noel','Nicholas Harms','Craig Mcgee','Derek Archer','Gavyn Dockery','Troy Schneider','Quinton Dacus','Braden Burcham','Tom McPherson')
athleticAthletes <- c('Nathaniel Sems','Braxton Martinez','Tyson James','JT Mabry','Arturo Romero','Cameron Haegele','Bryce Horstman',
                      'Taylor Robinson','Isaiah Williams','Brett Fischer','Jacob Buffa','Robert Ford 3','DJ Miller','Josh Bunselmeyer','Cody Klotz',
                      'Brady Cook','Jehu','Alex Henagean','Anthony Green','Ben Smith','Matt Rehder','Cody Creed','Andrew Buescher','Mitchell Davis','Dexter Swims')


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

### VIEW THE AVERAGES OR CENTERS FOR EACH GROUP
clusterData %>%
  filter(Name %in% powerAthletes) %>%
  summarise_at(vars(brakingMetrics,propulsiveMetrics), mean) %>% view()


clusterData %>%
  filter(Name %in% momentumAthletes) %>%
  summarise_at(vars(brakingMetrics, propulsiveMetrics), mean) %>% view()


clusterData %>%
  filter(Name %in% athleticAthletes) %>%
  summarise_at(vars(brakingMetrics, propulsiveMetrics), mean) %>% view()


clusterData %>%
  mutate(Avg.Braking.Total = (Avg..Braking.Force + Avg..Relative.Braking.Force + Braking.RFD)/3,
         Avg.Propulsive.Total = (Avg..Propulsive.Velocity + Avg..Propulsive.Force + Avg..Relative.Propulsive.Force)/3) %>%
  filter(Takeoff.Velocity >= Avg..Propulsive.Velocity+2) %>%
  select(Name, Avg..Propulsive.Force, Avg..Propulsive.Velocity, Takeoff.Velocity, Avg..Propulsive.Power) %>% view()

clusterData %>%
  filter(Avg..Braking.Velocity > Avg..Braking.Force,
         Braking.RFD > Avg..Braking.Velocity) %>%
  select(Name) %>% view()

# View individual athletes
clusterData %>%
  filter(Name %in% c('Ian Funk')) %>% view()
