library(extrafont)
library(tidyverse)
library(plotly)


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


############################
## SAMPLE VELOCITY GRAPHS ##
############################

# Same Average & Lower Takeoff
VelocityCurve <- 
  ggplot(data = data.frame(x = c(0,1)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(.6,.12), size = 2, aes(color = "Good")) +
  stat_function(fun = dnorm, args = list(.6,.125), size = 2, aes(color = 'Bad')) +
  scale_y_continuous(name = "Velocity", breaks = seq(0,4,.5), limits = c(0,4)) +
  scale_x_continuous(name = "Time", breaks = seq(0,1,.2)) +
  scale_colour_brewer(palette="Accent") +
  labs(colour = "Takeoff Velocity") +
  ggtitle("Same Avg. & Lower Takeoff Velocity") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.title.y = element_text(size = 14, face = "bold.italic"),
        axis.line = element_line(size=1, colour = "black"),
        text=element_text(family="Tahoma"),
        plot.title = element_text(size = 18, family = "Tahoma", face = "bold", hjust = .5),
        legend.position = "bottom")


# Lower Average & Lower Takeoff
VelocityCurve.1 <- 
  ggplot(data = data.frame(x = c(0,1)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(.6,.12), size = 2, aes(color = "Good")) +
  stat_function(fun = dnorm, args = list(.6,.16), size = 2, aes(color = 'Bad')) +
  scale_y_continuous(name = "Velocity", breaks = seq(0,4,.5), limits = c(0,4)) +
  scale_x_continuous(name = "Time", breaks = seq(0,1,.2)) +
  scale_colour_brewer(palette="Accent") +
  labs(colour = "Takeoff Velocity") +
  ggtitle("Lower Avg. & Takeoff Velocity") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.title.y = element_text(size = 14, face = "bold.italic"),
        axis.line = element_line(size=1, colour = "black"),
        text=element_text(family="Tahoma"),
        plot.title = element_text(size = 18, family = "Tahoma", face = "bold", hjust = .5),
        legend.position = "bottom")

# Higher Average & Higher Takeoff
VelocityCurve.2 <- 
  ggplot(data = data.frame(x = c(0,1)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(.6,.13), size = 2, aes(color = "Better")) +
  stat_function(fun = dnorm, args = list(.6,.1), size = 2, aes(color = 'Good')) +
  scale_y_continuous(name = "Velocity", breaks = seq(0,4,.5), limits = c(0,4)) +
  scale_x_continuous(name = "Time", breaks = seq(0,1,.2)) +
  scale_colour_brewer(palette="Accent") +
  labs(colour = "Takeoff Velocity") +
  ggtitle("Velocity and Time Curves") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.title.y = element_text(size = 14, face = "bold.italic"),
        axis.line = element_line(size=1, colour = "black"),
        text=element_text(family="Tahoma"),
        plot.title = element_text(size = 18, family = "Tahoma", face = "bold", hjust = .5),
        legend.position = "bottom")

# Lower Average & Same Takeoff
VelocityCurve.3 <- 
  ggplot(data = data.frame(x = c(0,1)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(.6,.12), size = 2, aes(color = "Good")) +
  stat_function(fun = dnorm, args = list(.6,.13), size = 2, aes(color = 'Bad')) +
  scale_y_continuous(name = "Velocity", breaks = seq(0,4,.5), limits = c(0,4)) +
  scale_x_continuous(name = "Time", breaks = seq(0,1,.2)) +
  scale_colour_brewer(palette="Accent") +
  labs(colour = "Takeoff Velocity") +
  ggtitle("Velocity and Time Curves") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.title.y = element_text(size = 14, face = "bold.italic"),
        axis.line = element_line(size=1, colour = "black"),
        text=element_text(family="Tahoma"),
        plot.title = element_text(size = 18, family = "Tahoma", face = "bold", hjust = .5),
        legend.position = "bottom")


####### SAMPLE CURVES USING dnorm

ggplot(data = data.frame(x=x.norm,y=curve.x.norm)) + 
  geom_line(aes(x=x, y=y, color = "Normal"), size = 2) +
  geom_line(data = data.frame(x=x.norm,y=curve.x.slow), aes(x=x, y=y, color = "Slow"), size = 2) +
  scale_y_continuous(name = "Velocity", breaks = seq(0,4,.5), limits = c(0,4)) +
  scale_x_continuous(name = "Time", breaks = seq(0,1,.2)) +
  scale_colour_brewer(palette="Accent") +
  labs(colour = "Takeoff Velocity") +
  ggtitle("Velocity and Time Curves") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.title.y = element_text(size = 14, face = "bold.italic"),
        axis.line = element_line(size=1, colour = "black"),
        text=element_text(family="Tahoma"),
        plot.title = element_text(size = 18, family = "Tahoma", face = "bold", hjust = .5),
        legend.position = "bottom")

curve.x.slow = dnorm(x.norm,mean, sd.slow)
df = data.frame(x=x.norm,y=curve.x.slow) %>% mutate(y = ifelse(y >= quantile(y,.9,na.rm = TRUE), y+1,y))
df.1 = data.frame(x=x.norm,y=curve.x.norm)

ggplot(data = df) + 
  geom_smooth(aes(x=x, y=y), color = 'purple', se = FALSE, size = 2) + 
  geom_smooth(data = df.1, aes(x=x, y=y), color = 'green', se = FALSE, size = 2) +
  scale_y_continuous(name = "Velocity", breaks = seq(0,4,.5), limits = c(0,4)) +
  scale_x_continuous(name = "Time", breaks = seq(0,1,.2)) +
  scale_colour_brewer(palette="Accent") +
  labs(colour = "Takeoff Velocity") +
  ggtitle("Velocity and Time Curves") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.title.y = element_text(size = 14, face = "bold.italic"),
        axis.line = element_line(size=1, colour = "black"),
        text=element_text(family="Tahoma"),
        plot.title = element_text(size = 18, family = "Tahoma", face = "bold", hjust = .5),
        legend.position = "bottom")

########################################
## DATA FRAME OF DIFFERENT JUMP TYPES ##
########################################
mean = .6
sd.fast = .08
sd.slow = .16
sd.normal = .12
x = seq(0,1,length=100)
velo.norm = dnorm(x,mean, sd.normal)
velo.slow = dnorm(x, mean, sd.slow)
velo.fast = dnorm(x, mean, sd.fast)
df = data.frame(x = x, y.norm = velo.norm, y.slow = velo.slow, y.fast = velo.fast)

df.edited = df %>% mutate(y.slow = ifelse(y.slow >= quantile(y.slow,.78,na.rm = TRUE), y.norm,y.slow),
                          y.fast = ifelse(y.fast >= quantile(y.fast,.8,na.rm = TRUE), y.norm,y.fast))


df.edited %>%
  ggplot() + 
  geom_line(aes(x=x,y=y.slow, color = 'Slow'), size = 2) +
  geom_line(aes(x=x,y=y.norm, color='Normal'), size = 2) +
  #geom_smooth(aes(x=x, y=y.norm, color = 'Normal'), method = 'loess', se = FALSE, size = 1.5, alpha = .3) + 
  #geom_smooth(aes(x=x, y=y.slow, color = 'Slow'), method = 'loess', se = FALSE, size = 1.5, alpha = .3) +
  #geom_smooth(aes(x=x, y=y.fast, color = 'Fast'), method = 'loess', se = FALSE, size = 1.5, alpha = .3) +
  scale_y_continuous(name = "Velocity", breaks = seq(0,4,.5), limits = c(0,4)) +
  scale_x_continuous(name = "Time", breaks = seq(0,1,.2)) +
  scale_colour_brewer(palette="Accent") +
  labs(colour = "Avg. Velocities") +
  ggtitle("Assessing Different Average Velocities") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.title.y = element_text(size = 14, face = "bold.italic"),
        axis.line = element_line(size=1, colour = "black"),
        text=element_text(family="Tahoma"),
        plot.title = element_text(size = 18, family = "Tahoma", face = "bold", hjust = .5),
        legend.position = "bottom")


######################
## CUSTOM FUNCTIONS ##
######################
set.seed(123)
x = seq(0,1,.01) # plotting x and x would make velocity and acceleration the same
y = dnorm(x,.4,.1) # plotting x and y would make higher acceleration curve
y_2 = qnorm(x) # average this with Y to get bad acceleration
data = data.frame(x=x, y=(y+y_2),y_2=(y+y_2)/1.5)

# TWITCH
data[1:45,] %>%
  ggplot() +
  geom_line(aes(x=x,y=y), color = 'black', size = 2) +
  geom_line(aes(x=x,y=(x*12)-1), color = 'dark red', size = 1.5, linetype = 'dashed', alpha = .4) +
  scale_x_continuous(name = "Time", limits = c(-.1,.6)) +
  scale_y_continuous(name = "Velocity", limits = c(-1,4)) +
  ggtitle("Acceleration > Velocity") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.text = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold.italic"),
        axis.line = element_line(size=1, colour = "black"),
        text=element_text(family="Tahoma"),
        plot.title = element_text(size = 18, face = "bold", hjust = .5),
        legend.position = "bottom")

# MOMENTUM
data[1:45,] %>%
  ggplot() +
  geom_line(aes(x=x,y=y_2), color = 'black', size = 2) +
  geom_line(aes(x=x,y=(x*9)-1.4), color = 'dark red', size = 1.5, linetype = 'dashed', alpha = .4) +
  scale_x_continuous(name = "Time", limits = c(-.1,.6)) +
  scale_y_continuous(name = "Velocity", limits = c(-1.4,3)) +
  ggtitle("Acceleration < Velocity") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.text = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold.italic"),
        axis.line = element_line(size=1, colour = "black"),
        text=element_text(family="Tahoma"),
        plot.title = element_text(size = 18, face = "bold", hjust = .5),
        legend.position = "bottom")

# SAME ACCEL & VELOCITY
data %>%
  ggplot() +
  geom_line(aes(x=x,y=(x*9)-1.4), color = 'dark red', size = 2) +
  scale_x_continuous(name = "Time", breaks = seq(0,1,.1), limits = c(-.1,.6)) +
  scale_y_continuous(name = "Velocity", limits = c(-1.4,3)) +
  ggtitle("Acceleration = Velocity") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.text = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold.italic"),
        axis.line = element_line(size=1, colour = "black"),
        text=element_text(family="Tahoma"),
        plot.title = element_text(size = 18, family = "Tahoma", face = "bold", hjust = .5),
        legend.position = "bottom")



