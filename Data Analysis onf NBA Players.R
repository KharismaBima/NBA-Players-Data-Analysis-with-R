library(tidyverse)

# Importing the dataset
NBA_Players <- read_csv("NBA Players.csv", show_col_types = 'FALSE')

# Let's look trough the data
head(NBA_Players)
summary(NBA_Players)

# Check the data if any records are missing
apply(NBA_Players, 2, function(x) any(is.na(x)))
sum(is.na(NBA_Players))
NBA_Players <- NBA_Players %>% drop_na(college)

###### DRAFT STATUS ANALYSIS ######

# Make new draft variable with boolean logic 
NBA_Players$draft <- ifelse(NBA_Players$draft_year == "Undrafted", "No", "Yes")

# Make dataframe based on player draft status with only three variables (year, drafted, undrafted)
# Change season variable format to numeric value
Draft_Yes <- NBA_Players %>% group_by(season) %>% filter(draft == "Yes") %>% count()
Draft_No <- NBA_Players %>% group_by(season) %>% filter(draft == "No") %>% count()
Draft_vars <- data.frame(year = c(substr(Draft_Yes$season, 1, 4)), 
                         drafted = c(Draft_Yes$n), 
                         undrafted = c(Draft_No$n))
Draft_vars$year <- as.numeric(Draft_vars$year) 
head(Draft_vars)

# Visualize Player's draft status over the years

D <- ggplot(Draft_vars, aes(year, drafted, group = 1)) +
  geom_point() +
  geom_line(color="darkred")+
  theme_minimal()

UD <- ggplot(Draft_vars, aes(year, undrafted, group = 1)) +
  geom_point() +
  geom_line(color="steelblue")+
  theme_minimal()

library(gridExtra)
grid.arrange(D, UD, ncol = )

# Combining two trends into single line chart

ggplot(Draft_vars, aes(x=year, group = 1)) + 
  geom_line(aes(y = drafted), size=1.5, color = "darkred") + 
  geom_line(aes(y = 300+undrafted), size=1.5, color="steelblue")+
  scale_x_continuous(breaks=seq(1996, 2022, by=5))+
  scale_y_continuous(sec.axis = sec_axis(~.-300, name = "undrafted"))+
  labs(title="Drafted and Undrafted Trends")+
  theme_minimal()

###### HEIGHT, WEIGHT AND BMI ANALYSIS ######

# Making new dataframe of Players Height, Weight, and BMI (Body Mass Index)
# BMI Formula (wight in kg/(height in m)^2)
Phisical_vars <- NBA_Players %>% group_by(player_name) %>% summarise_at(vars(player_height, player_weight), list(mean))
Phisical_vars <- Phisical_vars %>% mutate(BMI=player_weight/((player_height/100)^2)) 

head(Phisical_vars)

# Visualize distributionn of height
# Source of Average global male height: https://ourworldindata.org/human-height.
ggplot(Phisical_vars, aes(x=player_height)) + 
  geom_histogram(binwidth=2, fill="#3b4d61")+
  geom_vline(aes(xintercept = median(player_height), 
                 color = "Average NBA PLayer"), size = 1, show.legend = TRUE)+
  geom_vline(aes(xintercept = 171, 
                 color = "Average Global Male"), size = 1, show.legend = TRUE)+
  scale_color_manual(name= "Line" , values = c("Average NBA PLayer" = "darkred", 
                                               "Average Global Male" = "steelblue"))+
  scale_x_continuous(breaks=seq(160,230,by=10))+
  labs(title = "Distribution of Height",x="Height (cm)",y="Count")+
  theme_minimal()+
  theme(legend.position = "top")+
  theme(legend.background = element_rect(fill = "white"))+
  theme(plot.title = element_text(hjust = 0.5))

#Visualize height and weight correlation
sp <- ggplot(Phisical_vars, aes(x=player_weight, y=player_height)) +
  geom_point(color="#3b4d61")+
  scale_y_continuous(breaks=seq(160, 230, by=10))+
  scale_x_continuous(breaks=seq(60, 160, by=20))+
  labs(title = "Height and Weight Correlation",x="Weight (kg)",y="Height (cm)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Adding correlation
library(ggpubr)
sp + geom_smooth(method = "lm", formula= y~x, color="darkred")+
  stat_cor(method="pearson", label.x = 120, label.y = 180)

#Analysis on BMI of NBA Players
#Taking 15 players with highest BMI since 1996
top15_BMI <- Phisical_vars %>% filter(rank(desc(BMI))<=15)
top15_BMI$BMI<- round(top15_BMI$BMI, digit=2)
head(top15_BMI)

###### PLAYERS ORIGIN ANALYSIS ######

# Make the dataframe of USA and Foreign Players
CountryCount <- NBA_Players %>% group_by(country) %>% count(player_name)
CountryDist <- CountryCount %>% group_by(country) %>% count() %>% arrange(desc(n))
USA <- CountryDist %>% filter(country == 'USA')
Foreign <- CountryDist %>% filter(!country %in% c('USA'))
Foreign <- data.frame(country=c("Foreign"), n=sum(Foreign$n))
CountryPrc <- rbind(USA, Foreign) %>% ungroup() %>% mutate(perc= scales::percent(`n` / sum(`n`)))
head(CountryPrc)

# Visualizing USA and Foreign Players percentage
ggplot(CountryPrc, aes(x = "", y = perc, fill = country)) +
  geom_col() +
  geom_text(aes(label = perc), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title="Country Precentage") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = c("USA" = "steelblue","Foreign" = "darkred"))

# Make Foreign player trend over the years
ForeignGrowth <- NBA_Players %>% filter(!country %in% c('USA'))
ForeignGrowth <- ForeignGrowth %>% group_by(season) %>% count()
ForeignGrowth <- data.frame(year = c(substr(ForeignGrowth$season, 1, 4)), 
                            total = c(ForeignGrowth$n))
ForeignGrowth$year <- as.numeric(ForeignGrowth$year)
head(ForeignGrowth)

# Visualizing Foreign players trend since 1996 
ggplot(ForeignGrowth, aes(year, total)) +
  geom_line(color="steelblue", size = 1) +
  geom_point(color="darkred")+
  labs(title="Foreign Players on NBA Trend")+
  theme_minimal()

