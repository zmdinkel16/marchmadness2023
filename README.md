# March Madness 2023 Predictions 🏀 
![App Screenshot](https://raw.githubusercontent.com/zmdinkel16/marchmadness2023/main/Pictures%20Used/Screenshot%202023-03-24%20at%201.58.42%20PM.png)




## Authors ✏️

- Stuti Shrestha
- Zach Dinkel 
- Pravani Pradhan


## Brief Description of the Project ⏳
We will analyze the March Madness 2023 data and create a model to predict which team will win 2023 season. 
## Dictionary 📖
1. seed: rank in March Madness tournament
2. team: school playing in game
3. cuurent.round: current round team is in
4. kenpom.adjusted.offense: Kenpom offensive rating
5. kenpom.adjusted.defense: Kenpom defensive rating
6. kenpom.adjusted.tempo: Kenpom tempo rating
7. barttorvik.adjusted.offense: Barttorvik offensive rating
8. barttorvik.adjusted.defense: Barttorvik defensive rating
9. barthag: estimates team's winning percentage against avg team in nuetral location
10. elite.sos: strength of schedule rating
11. ft_pctg: free throw percentage
12. efg_pct: effective field goal percentage
13. ft_rate: rate of possessions resulting in a free throw
14. o_reb: offensive rebound percentage
15. d_reb: defensive rebound percentage
16. turnover: turnover percentage
17. block: block percentage
18. turnover_def: percentage of defensive possesions resulting in a turnover
19. two_pctg: 2pt percentage
20. three_pctg: 3pt percentage
21. assist: amount of scores caused by an assist
22. two_pt_def: opponent 2pt percentage
23. three_pt_def: opponent 3pt percentage
24. ft_defense: opponent free throw percentage
25. efg_def: opponent effective field goal percentage
26. ft_rate_def: amount of defensive possessions resulting in opponent free throws
27. three_pt_rate_def: amount of defensive possessions resulting in opponent 3pt attempt
28. opp_assist: opponent assist rate
29. blocked: amount of own shots blocked
30. ppp_o: points per possession on offense
31. ppp_d: points per possession allowed on defense
32. score: score in the game
## Library 📚
     1. dplyr
     2. tidyverse 
     3. readxl
     4. lubridate
     5. xgboost
     6. caret
     7. shiny
     8. ggplot2
     9. rsconnect 

## Data Cleaning 🧼 
1. Created lower-case column names and selected useful data
```
names(historical_data) <- tolower(names(historical_data))
names(r64_current) <- tolower(names(r64_current))

historical_data <- historical_data %>%
  select(-year, -team.round, -kenpom.adjusted.efficiency, 
         -barttorvik.adjusted.efficiency, -wins.above.bubble, -win.., -id, -team.1)
```
2. Split the data into a team1 and team2 dataframe
```
r64_historical_team1 <- r64_historical[seq(1, nrow(r64_historical), 2), ]
r64_historical_team2 <- r64_historical[seq(2, nrow(r64_historical), 2), ]
```
3. Created a df to show the statisitcal differences in each matchup
```
r64_historical_matchup_differences <- data.frame(
  seed = r64_historical_team1$seed - r64_historical_team2$seed,
  kenpom_o = r64_historical_team1$kenpom.adjusted.offense - r64_historical_team2$kenpom.adjusted.offense,
  kenpom_d = r64_historical_team1$kenpom.adjusted.defense - r64_historical_team2$kenpom.adjusted.defense,
  kenpom_tempo = r64_historical_team1$kenpom.adjusted.tempo - r64_historical_team2$kenpom.adjusted.tempo,
  bart_o = r64_historical_team1$barttorvik.adjusted.offense - r64_historical_team2$barttorvik.adjusted.offense,
  bart_d = r64_historical_team1$barttorvik.adjusted.defense - r64_historical_team2$barttorvik.adjusted.defense,
  bart_tempo = r64_historical_team1$barttorvik.adjusted.tempo - r64_historical_team2$barttorvik.adjusted.tempo,
  barthag = r64_historical_team1$barthag - r64_historical_team2$barthag,
  elite_sos = r64_historical_team1$elite.sos - r64_historical_team2$elite.sos,
  ft_pctg = r64_historical_team1$free.throw.. - r64_historical_team2$free.throw..,
  efg_pct = r64_historical_team1$efg.. - r64_historical_team2$efg..,
  ft_rate = r64_historical_team1$free.throw.rate - r64_historical_team2$free.throw.rate,
  o_reb = r64_historical_team1$offensive.rebound.. - r64_historical_team2$offensive.rebound..,
  d_reb = r64_historical_team1$defensive.rebound.. - r64_historical_team2$defensive.rebound..,
  turnover = r64_historical_team1$turnover.. - r64_historical_team2$turnover..,
  block = r64_historical_team1$block.. - r64_historical_team2$block..,
  turnover_def = r64_historical_team1$turnover...defense - r64_historical_team2$turnover...defense,
  two_pctg = r64_historical_team1$x2pt.. - r64_historical_team2$x2pt..,
  three_pctg = r64_historical_team1$x3pt.. - r64_historical_team2$x3pt..,
  assist = r64_historical_team1$assist.. - r64_historical_team2$assist..,
  two_pt_def = r64_historical_team1$x2pt...defense - r64_historical_team2$x2pt...defense,
  three_pt_def = r64_historical_team1$x3pt...defense - r64_historical_team2$x3pt...defense,
  ft_defense = r64_historical_team1$free.throw...defense - r64_historical_team2$free.throw...defense,
  efg_def = r64_historical_team1$efg...defense - r64_historical_team2$efg...defense,
  ft_rate_def = r64_historical_team1$free.throw.rate.defense - r64_historical_team2$free.throw.rate.defense,
  three_pt_rate_def = r64_historical_team1$x3pt.rate.defense - r64_historical_team2$x3pt.rate.defense,
  opp_assist = r64_historical_team1$op.assist.. - r64_historical_team2$op.assist..,
  blocked = r64_historical_team1$blocked.. - r64_historical_team2$blocked..,
  ppp_o = r64_historical_team1$points.per.possession.offense - r64_historical_team2$points.per.possession.offense,
  ppp_d = r64_historical_team1$points.per.possession.defense - r64_historical_team2$points.per.possession.defense,
  score = r64_historical_team1$score - r64_historical_team2$score
)
```
4. Converted the score outcome column to a win probability
```
r64_historical_matchup_differences$prob_win <- ''

r64_historical_matchup_differences$prob_win <- ifelse(r64_historical_matchup_differences$score > 0, .99, .01)

r64_historical_matchup_differences <- r64_historical_matchup_differences %>%
  select(-score)
```
## Data Summary 📽️
1. Split the data into training and testing sets
```
set.seed(123)
trainIndex <- createDataPartition(r64_historical_matchup_differences$prob_win, p = 0.7, list = FALSE)
training_data <- r64_historical_matchup_differences[trainIndex, ]
testing_data <- r64_historical_matchup_differences[-trainIndex, ]
```
2. Used XG Boost model to train and test a prediction model
```
params <- list(objective = "reg:logistic", eval_metric = "logloss")
r64_xgb_model <- xgboost(data = as.matrix(training_data[, -31]), label = training_data$prob_win, 
                         nthread = 4, nrounds = 10,  params = params)

importance <- xgb.importance(model = r64_xgb_model)
print(importance)

test_pred <- predict(r64_xgb_model, as.matrix(testing_data[, -31]))
```
## Data Analysis 🧐📊
## Predictions
1. Used our XG Boost model to make predictions for the current year
```
current_pred <- predict(r64_xgb_model, as.matrix(r64_current_matchup_differences[, -31]))

results_r64 <- data.frame(predicted_win_prob = current_pred)

## Join in teams to final prediction
results_r64$team <- r64_team1$team
results_r64$opponent <- r64_team2$team
results_r64$predicted_winner <- ifelse(results_r64$predicted_win_prob >= .5, r64_team1$team, r64_team2$team)
results_r64$game <- seq_along(results_r64$team)
```

2. The rest of our predictions can be found in the Prediction Files folder:
* Our predicted Final Four is NC State, Purdue, Houston, and UCLA
* Our predicted March Madness winner is UCLA

## Shiny App 💎
Code used for interactive Shinny:
```
server<-function(input,output){
  
  output$plotMarchMadness <- renderPlot({
    
    ggplot(data_df, aes_string(x=input$X, y= input$Y, z= input$Z))+
      geom_point()+
      geom_smooth()
  })
  output$table_01<-DT::renderDataTable(data_df[,c(input$X,input$Y,input$Z, input$Splitby)],options = list(pageLength = 4))
  
}
```
![App Screenshot](https://raw.githubusercontent.com/zmdinkel16/marchmadness2023/main/Pictures%20Used/Screenshot%202023-03-24%20at%2011.04.43%20PM.png)

In graph 1, we can see the seed based on points per offense in the line graph and points per defense in the scatter point. The line graph shows that the seed 1 has the highest point of offense and seed 16 has the lowest point of offense. We can see seed 8 to seed 12 have similar points for offense. 
We can see in the scatter point that the point of defense of seed 3 is the highest and  seed 16 has the lowest point of defense.

![App Screenshot](https://raw.githubusercontent.com/zmdinkel16/marchmadness2023/main/Pictures%20Used/Screenshot%202023-03-24%20at%2011.07.27%20PM.png)

In the graph 2, we can observe that the Kenpom efficiency rating and barttorvik  ratings are not the same but are similar to each other, the scatter points and the graph drawn are in the same range, not much difference. Seed 1 as usual has the highest efficiency pont than other seeds and seed 16 has the lowest. 
 

## Links🔗
GithubLink:
https://github.com/zmdinkel16/marchmadness2023  

ShinnyLink:
https://stutishrestha.shinyapps.io/Project2/  

## Conclusion 🎀
By using the xgboost() function, we were easily able to create a machine learning model to train itself on historical data and make predictions on march madness winners. While 3 of our 4 final four teams have lost in the tournament already, we still have a great base model that can be further tuned to become more accurate. We understand that march madness is impossible to predict, but our model is a great start and we are proud to have created such a product. 
