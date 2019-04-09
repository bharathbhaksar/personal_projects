install.packages('tidyverse')
install.packages('scales')
library(tidyverse)
library(scales)

#Load data
ccrush <- read_csv("candy_crush.csv")
#checking the data structure
head(ccrush,5)
tail(ccrush,5)
is.na(ccrush)
length(is.na(ccrush))

#finding the number of players and the period of data we have
print("Total number of player:")
length(unique(ccrush$player_id))

print("Data Set available period between:")
range(ccrush$dt)

#Computing level difficulty and adding another row to the data frame called prob_win using summation of wins / summation of attemps

lev_diff <- ccrush %>%
  group_by(level)%>%
  summarize(wins = sum(num_success),attempts = sum(num_attempts)) %>%
  mutate(prob_win= wins/attempts)
lev_diff

#Plotting using ggplot to identify the difficulty level

p1 <- ggplot(data=lev_diff,aes(x=level,y=prob_win))+geom_line(color='tomato') + labs(title ='Level vs Difficulty', x="Levels",y="Win % Difficulty")
p1 + scale_x_continuous(breaks = 1:15) + scale_y_continuous(labels = scales::percent)

#Adding points to the plot and adding difficulty line at 10%
p1 <- ggplot(data=lev_diff,aes(x=level,y=prob_win))+geom_line(color='tomato') + geom_point() + labs(title ='Level vs Difficulty', x="Levels",y="Win % Difficulty")
p1 + scale_x_continuous(breaks = 1:15) + scale_y_continuous(labels = scales::percent) + geom_hline(yintercept = 0.1, linetype = 'dashed')

#Checking the uncertinity by computing the error using square root of prob_win*(1-prob_win)/attempts)

lev_diff <- lev_diff %>%
  mutate(error = sqrt(prob_win * ((1-prob_win)/attempts)))
lev_diff
#showing the uncertinity in the data using y axis - prob_wim + error and prob_win - error
p1 <- ggplot(data=lev_diff,aes(x=level,y=prob_win))+geom_line(color='tomato') + geom_point() + labs(title ='Level vs Difficulty', x="Levels",y="Win % Difficulty")
p2 <- p1 + scale_x_continuous(breaks = 1:15) + scale_y_continuous(labels = scales::percent) + geom_hline(yintercept = 0.1, linetype = 'dashed')
p2 + geom_errorbar(aes(ymax = prob_win+error,ymin=prob_win - error))

# The probability of completing the episode without losing a single time
comp <- prod(lev_diff$prob_win)
comp
