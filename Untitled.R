#Install Packages
install.packages("tidyverse")
install.packages("devtools")
devtools::install_github("statsbomb/StatsBombR")
devtools::install_github("FCrSTATS/SBpitch")

#Pulling StatsBomb Free Data Into R

library(tidyverse) #basic functions to work with data 
library(StatsBombR) 

Comps <- FreeCompetitions()

Comps <- FreeCompetitions()

Comps = Comps %>%
  filter(competition_name=="UEFA Euro" & season_name=="2024")

Matches <- FreeMatches(Comps)

StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)

StatsBombData = allclean(StatsBombData)

##Filtering to Team Shots and Goals

#Totals

#how many shots did each team take?
#how mnay goals did each team score? 

#total goals by season
#team total goals 
shots_goals = StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE), #summarise data, sum every row of data for shot and goal, ignore na.rm values 
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE))

#total goals per game by team 
#per match goals 
#Per Match
shots_goals = StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE)/n_distinct(match_id), #DIVIDE BY THE NUMBER OF DISTINCT MATCH_ID
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)/n_distinct(match_id))

#####
##Filter to Player Shots and Key Passes
#looking at the most influential players in the tournament 

#total shots and key passes by players 
#Total
player_shots_keypasses = StatsBombData %>%
  group_by(player.name, player.id) %>% 
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            keypasses = sum(pass.shot_assist==TRUE, na.rm = TRUE))

#per 90- 
#data for each player, in each match, time coming on and off the picth, time game ended, and player which played  
#Get Minutes Data - match by match minutes player data 
player_minutes = get.minutesplayed(StatsBombData) #get.minutesplayed()- sum all the minutes played by each player 

#all minutes each player played across the whole tournament
player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed))

#Join Minutes Played Data To Player Shots Dataframe
player_shots_keypasses = left_join(player_shots_keypasses, player_minutes)
player_shots_keypasses = player_shots_keypasses %>% mutate(nineties = minutes/90) #mutate- add a column to the existing data 
player_shots_keypasses = player_shots_keypasses %>% mutate(shots_per90 = shots/nineties, #shots per 90s
                                                           kp_per90 = keypasses/nineties, #key passes by 90s
    
                                                                                                                  shots_kp_per90 = shots_per90+kp_per90) #shots and key passes per 90s
#filter minutes 
#filters players that played at least 360 mins 
player_shots_keypasses = player_shots_keypasses %>% filter(minutes>360)


#####
#Passes To The Final Third

#which players made the most passes into the final third

passes = StatsBombData %>%
  filter(type.name=="Pass" & is.na(pass.outcome.name)) %>%  #if a pass is complete it has an NA value thats why we are filtering is.na()
  filter(location.x<80 & pass.end_location.x>=80) %>% #filter passes that went into the final third , location.x for events that started with <80, pass.end_location to be higher than 80
  group_by(player.name, player.id) %>%
  summarise(f3_passes = sum(type.name=="Pass"))

unique(StatsBombData$pass.outcome.name)
#statsbombdata has an location x coordinate running from 0 to 120- lentgh of the picth 
#y coordinate runs from 0 to 80 - width of the picth 

#final third passes ending inside the final third have an x coordinate >80 and all passes or events that started outside the final third a location of x value of <80


#####
#Plotting Team Shots and Goals

#Totals
shots_goals = StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE))

#Per Match
shots_goals = StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE)/n_distinct(match_id),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)/n_distinct(match_id))

#Plotting the data

library(ggplot2)

ggplot(data = shots_goals, #data we want ggplot to use is shots_goals
       aes(x = reorder(team.name, shots), y = shots)) + #aesthethic functions, x axis- use function reorder, show team name and reorder by number of shots, y axis put the number of shots
  geom_bar(stat = "identity", width = 0.5) + #geom_bar for barchart, stat for identity (want to look at values) width how wide you want bars to be 
  labs(y="Shots") + #labs is labels
  theme(axis.title.y = element_blank()) + #theme - aesthethic elements -title is blank 
  scale_y_continuous( expand = c(0,0)) + # tighten the plot around the values 
  coord_flip() #flipping bar chart so is horizontal bar chart 

#####
#Plotting Player Shots and Key Passes

#Total
player_shots_keypasses = StatsBombData %>%
  group_by(player.name, player.id) %>% 
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            keypasses = sum(pass.shot_assist==TRUE, na.rm = TRUE))

#Get Minutes Data
player_minutes = get.minutesplayed(StatsBombData)
player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed))

#Join Minutes Played Data To Player Shots Dataframe
player_shots_keypasses = left_join(player_shots_keypasses, player_minutes)
player_shots_keypasses = player_shots_keypasses %>% mutate(nineties = minutes/90)
player_shots_keypasses = player_shots_keypasses %>% mutate(shots_per90 = shots/nineties,
                                                           kp_per90 = keypasses/nineties,
                                                           shots_kp_per90 = shots_per90+kp_per90)

#filter minutes
player_shots_keypasses = player_shots_keypasses %>% filter(minutes>360)%>%
  arrange(desc(shots_kp_per90))%>%
  head(n=20)

#Creating A Scatter Plot

ggplot(player_shots_keypasses, aes(x = shots_per90, y = kp_per90, #call ggplot, specify x and y axis 
                                   colour = shots_kp_per90, alpha = 0.9)) + #colour each node by value , alpha sets a transparency aesthethic  
  labs(x = "Shots per 90", y = "Key Passes per 90") + #labels 
  geom_point(size = 5, show.legend = FALSE) + #adding points in for scatter plot, no showing legend 
  geom_text(data = player_shots_keypasses, aes(label = player.name), #text data , from player_shots_key_passes and a aestheic label 
            colour = "black", size = 4, vjust = -0.5) + #text to be black , vjust adjust vertical placement of the text 
  guides(alpha = "none") #removes the legends for alpha 

#####
#Plotting things on a pitch

library(SBpitch) #visualize the pitch 

passes = StatsBombData %>%
  filter(type.name=="Pass" & is.na(pass.outcome.name)) %>%
  filter(location.x<80 & pass.end_location.x>=80) %>%
  group_by(player.id, player.name) %>%
  summarise(f3_passes = sum(type.name=="Pass"))

#Get Minutes Data
player_minutes = get.minutesplayed(StatsBombData)
player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed))

#Join Minutes Played Data To Player Shots Dataframe
passes = left_join(passes, player_minutes)
passes = passes %>% mutate(nineties = minutes/90)
passes = passes %>% mutate(f3passes_per90 = f3_passes/nineties)#- final third passes by 90

#filter minutes
passes = passes %>% filter(minutes>360) %>%
  arrange(desc(f3passes_per90))

player_passes = StatsBombData %>%
  filter(type.name=="Pass" & is.na(pass.outcome.name) & player.name=="Toni Kroos") %>% #filter to the player you want to plot 
  filter(location.x<80 & pass.end_location.x>=80)

create_Pitch() +
  geom_segment(data = player_passes, aes(x = location.x, y = location.y,
                                         xend = pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", linewidth = 0.5, colour = "#000000",
               arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + # arrow direction where pass was made 
  labs(title = "Toni Kroos, Passes to the final third", subtitle = "UEFA Euro 2024") +
  scale_y_reverse() + # reverse values in y axis, plot descending - we want 0 values on left hand side 
  coord_fixed(ratio = 105/100) #for every pixel value 

