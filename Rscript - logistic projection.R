library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(xgboost)
library(caret)
rm(list=ls())

setwd("~/Betting/CBB/March Madness")

historical_data <- read.csv("historical_game_data.csv")
r64_current <- read.csv("2023_game_data.csv")


names(historical_data) <- tolower(names(historical_data))
names(r64_current) <- tolower(names(r64_current))

historical_data <- historical_data %>%
  select(-year, -team.round, -kenpom.adjusted.efficiency, 
         -barttorvik.adjusted.efficiency, -wins.above.bubble, -win.., -id, -team.1)

r64_current <- r64_current %>%
  select(-year, -team.round, -current.round, -kenpom.adjusted.efficiency, 
         -barttorvik.adjusted.efficiency, -wins.above.bubble, -win.., -team.1)

# play in games

# r64
r64_historical <- historical_data %>%
  filter(current.round == 64)

r64_historical_team1 <- r64_historical[seq(1, nrow(r64_historical), 2), ]
r64_historical_team2 <- r64_historical[seq(2, nrow(r64_historical), 2), ]

r64_team1 <- r64_current[seq(1, nrow(r64_current), 2), ]
r64_team2 <- r64_current[seq(2, nrow(r64_current), 2), ]

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
  # efg_pct = r64_historical_team1$efg.. - r64_historical_team2$efg..,
  ft_rate = r64_historical_team1$free.throw.rate - r64_historical_team2$free.throw.rate,
  o_reb = r64_historical_team1$offensive.rebound.. - r64_historical_team2$offensive.rebound..,
  # d_reb = r64_historical_team1$defensive.rebound.. - r64_historical_team2$defensive.rebound..,
  turnover = r64_historical_team1$turnover.. - r64_historical_team2$turnover..,
  block = r64_historical_team1$block.. - r64_historical_team2$block..,
  # turnover_def = r64_historical_team1$turnover...defense - r64_historical_team2$turnover...defense,
  score = r64_historical_team1$score - r64_historical_team2$score
  # opp_o_reb = r64_historical_team1$op.o.reb.. - r64_historical_team2$op.o.reb..,
  # opp_d_reb = r64_historical_team1$op.d.reb.. - r64_historical_team2$op.d.reb..,
  # two_pctg = historical_team1$x2pt.. - historical_team2$x2pt..,
  # three_pctg = historical_team1$x3pt.. - historical_team2$x3pt..,
  # three_rate = historical_team1$x3pt.rate - historical_team2$x3pt.rate,
  # assist = historical_team1$assist.. - historical_team2$assist..,
  # two_pt_def = historical_team1$x2pt...defense - historical_team2$x2pt...defense,
  # three_pt_def = historical_team1$x3pt...defense - historical_team2$x3pt...defense,
  # ft_defense = historical_team1$free.throw...defense - historical_team2$free.throw...defense,
  # efg_def = historical_team1$efg...defense - historical_team2$efg...defense,
  # ft_rate_def = historical_team1$free.throw.rate.defense - historical_team2$free.throw.rate.defense,
  # three_pt_rate_def = historical_team1$x3pt.rate.defense - historical_team2$x3pt.rate.defense,
  # opp_assist = historical_team1$op.assist.. - historical_team2$op.assist..,
  # blocked = historical_team1$blocked.. - historical_team2$blocked..,
  # ppp_o = historical_team1$points.per.possession.offense - historical_team2$points.per.possession.offense,
  # ppp_d = historical_team1$points.per.possession.defense - historical_team2$points.per.possession.defense,
)

r64_current_matchup_differences <- data.frame(
  seed = r64_team1$seed - r64_team2$seed,
  kenpom_o = r64_team1$kenpom.adjusted.offense - r64_team2$kenpom.adjusted.offense,
  kenpom_d = r64_team1$kenpom.adjusted.defense - r64_team2$kenpom.adjusted.defense,
  kenpom_tempo = r64_team1$kenpom.adjusted.tempo - r64_team2$kenpom.adjusted.tempo,
  bart_o = r64_team1$barttorvik.adjusted.offense - r64_team2$barttorvik.adjusted.offense,
  bart_d = r64_team1$barttorvik.adjusted.defense - r64_team2$barttorvik.adjusted.defense,
  bart_tempo = r64_team1$barttorvik.adjusted.tempo - r64_team2$barttorvik.adjusted.tempo,
  barthag = r64_team1$barthag - r64_team2$barthag,
  elite_sos = r64_team1$elite.sos - r64_team2$elite.sos,
  ft_pctg = r64_team1$free.throw.. - r64_team2$free.throw..,
  # efg_pct = r64_team1$efg.. - r64_team2$efg..,
  ft_rate = r64_team1$free.throw.rate - r64_team2$free.throw.rate,
  o_reb = r64_team1$offensive.rebound.. - r64_team2$offensive.rebound..,
  # d_reb = r64_team1$defensive.rebound.. - r64_team2$defensive.rebound..,
  turnover = r64_team1$turnover.. - r64_team2$turnover..,
  block = r64_team1$block.. - r64_team2$block..
  # turnover_def = r64_team1$turnover...defense - r64_team2$turnover...defense
  # opp_o_reb = r64_team1$op.o.reb.. - r64_team2$op.o.reb..,
  # opp_d_reb = r64_team1$op.d.reb.. - r64_team2$op.d.reb..,
  # two_pctg = current_team1$x2pt.. - current_team2$x2pt..,
  # three_pctg = current_team1$x3pt.. - current_team2$x3pt..,
  # three_rate = current_team1$x3pt.rate - current_team2$x3pt.rate,
  # assist = current_team1$assist.. - current_team2$assist..,
  # two_pt_def = current_team1$x2pt...defense - current_team2$x2pt...defense,
  # three_pt_def = current_team1$x3pt...defense - current_team2$x3pt...defense,
  # ft_defense = current_team1$free.throw...defense - current_team2$free.throw...defense,
  # efg_def = current_team1$efg...defense - current_team2$efg...defense,
  # ft_rate_def = current_team1$free.throw.rate.defense - current_team2$free.throw.rate.defense,
  # three_pt_rate_def = current_team1$x3pt.rate.defense - current_team2$x3pt.rate.defense,
  # opp_assist = current_team1$op.assist.. - current_team2$op.assist..,
  # blocked = current_team1$blocked.. - current_team2$blocked..,
  # ppp_o = current_team1$points.per.possession.offense - current_team2$points.per.possession.offense,
  # ppp_d = current_team1$points.per.possession.defense - current_team2$points.per.possession.defense
)

r64_current_matchup_differences$prob_win <- ''

set.seed(123)
r64_historical_matchup_differences$prob_win <- ''
trainIndex <- createDataPartition(r64_historical_matchup_differences$score, p = 0.7, list = FALSE)
training_data <- r64_historical_matchup_differences[trainIndex, ]
testing_data <- r64_historical_matchup_differences[-trainIndex, ]

training_data$prob_win <- plogis(training_data$score)
testing_data$prob_win <- plogis(testing_data$score)

training_data <- training_data %>%
  select(-score)

testing_data <- testing_data %>%
  select(-score)

params <- list(objective = "reg:logistic", eval_metric = "logloss")
r64_xgb_model <- xgboost(data = as.matrix(training_data[, -15]), label = training_data$prob_win, 
                         nthread = 4, nrounds = 10,  params = params)

importance <- xgb.importance(feature_names = colnames(as.matrix(training_data[, -15])), model = r64_xgb_model)
head(importance)

test_pred <- predict(r64_xgb_model, as.matrix(testing_data[, -15]))

mean((testing_data$prob_win - test_pred)^2) #mse
caret::MAE(testing_data$prob_win, test_pred) #mae
caret::RMSE(testing_data$prob_win, test_pred)

current_pred <- predict(r64_xgb_model, as.matrix(r64_current_matchup_differences[, -15]))

results_r64 <- data.frame(predicted_win_prob = current_pred)

results_r64$team <- r64_team1$team
results_r64$opponent <- r64_team2$team
results_r64$predicted_winner <- ifelse(results_r64$predicted_win_prob >= .5, r64_team1$team, r64_team2$team)
results_r64$game <- seq_along(results_r64$team)

write.csv(results_r64, "predicted_scores_round_64_v3.csv", row.names = FALSE)





# r32
r32_historical <- historical_data %>%
  filter(current.round == 32)

r32_historical_team1 <- r32_historical[seq(1, nrow(r32_historical), 2), ]
r32_historical_team2 <- r32_historical[seq(2, nrow(r32_historical), 2), ]

r32_historical_matchup_differences <- data.frame(
  seed = r32_historical_team1$seed - r32_historical_team2$seed,
  kenpom_o = r32_historical_team1$kenpom.adjusted.offense - r32_historical_team2$kenpom.adjusted.offense,
  kenpom_d = r32_historical_team1$kenpom.adjusted.defense - r32_historical_team2$kenpom.adjusted.defense,
  kenpom_tempo = r32_historical_team1$kenpom.adjusted.tempo - r32_historical_team2$kenpom.adjusted.tempo,
  bart_o = r32_historical_team1$barttorvik.adjusted.offense - r32_historical_team2$barttorvik.adjusted.offense,
  bart_d = r32_historical_team1$barttorvik.adjusted.defense - r32_historical_team2$barttorvik.adjusted.defense,
  bart_tempo = r32_historical_team1$barttorvik.adjusted.tempo - r32_historical_team2$barttorvik.adjusted.tempo,
  barthag = r32_historical_team1$barthag - r32_historical_team2$barthag,
  elite_sos = r32_historical_team1$elite.sos - r32_historical_team2$elite.sos,
  ft_pctg = r32_historical_team1$free.throw.. - r32_historical_team2$free.throw..,
  # efg_pct = r32_historical_team1$efg.. - r32_historical_team2$efg..,
  ft_rate = r32_historical_team1$free.throw.rate - r32_historical_team2$free.throw.rate,
  o_reb = r32_historical_team1$offensive.rebound.. - r32_historical_team2$offensive.rebound..,
  # d_reb = r32_historical_team1$defensive.rebound.. - r32_historical_team2$defensive.rebound..,
  turnover = r32_historical_team1$turnover.. - r32_historical_team2$turnover..,
  block = r32_historical_team1$block.. - r32_historical_team2$block..,
  # turnover_def = r32_historical_team1$turnover...defense - r32_historical_team2$turnover...defense,
  score = r32_historical_team1$score - r32_historical_team2$score
  # opp_o_reb = r32_historical_team1$op.o.reb.. - r32_historical_team2$op.o.reb..,
  # opp_d_reb = r32_historical_team1$op.d.reb.. - r32_historical_team2$op.d.reb..,
  # two_pctg = historical_team1$x2pt.. - historical_team2$x2pt..,
  # three_pctg = historical_team1$x3pt.. - historical_team2$x3pt..,
  # three_rate = historical_team1$x3pt.rate - historical_team2$x3pt.rate,
  # assist = historical_team1$assist.. - historical_team2$assist..,
  # two_pt_def = historical_team1$x2pt...defense - historical_team2$x2pt...defense,
  # three_pt_def = historical_team1$x3pt...defense - historical_team2$x3pt...defense,
  # ft_defense = historical_team1$free.throw...defense - historical_team2$free.throw...defense,
  # efg_def = historical_team1$efg...defense - historical_team2$efg...defense,
  # ft_rate_def = historical_team1$free.throw.rate.defense - historical_team2$free.throw.rate.defense,
  # three_pt_rate_def = historical_team1$x3pt.rate.defense - historical_team2$x3pt.rate.defense,
  # opp_assist = historical_team1$op.assist.. - historical_team2$op.assist..,
  # blocked = historical_team1$blocked.. - historical_team2$blocked..,
  # ppp_o = historical_team1$points.per.possession.offense - historical_team2$points.per.possession.offense,
  # ppp_d = historical_team1$points.per.possession.defense - historical_team2$points.per.possession.defense,
)

set.seed(123)
r32_historical_matchup_differences$prob_win <- ''

trainIndex <- createDataPartition(r32_historical_matchup_differences$score, p = 0.7, list = FALSE)
training_data <- r32_historical_matchup_differences[trainIndex, ]
testing_data <- r32_historical_matchup_differences[-trainIndex, ]

training_data$prob_win <- plogis(training_data$score)
testing_data$prob_win <- plogis(testing_data$score)

training_data <- training_data %>%
  select(-score)

testing_data <- testing_data %>%
  select(-score)

params <- list(objective = "reg:logistic", eval_metric = "logloss")
r32_xgb_model <- xgboost(data = as.matrix(training_data[, -15]), label = training_data$prob_win, 
                         nthread = 4, nrounds = 10,  params = params)

importance <- xgb.importance(feature_names = colnames(as.matrix(training_data[, -15])), model = r32_xgb_model)
head(importance)

test_pred <- predict(r32_xgb_model, as.matrix(testing_data[, -15]))

mean((testing_data$prob_win - test_pred)^2) #mse
caret::MAE(testing_data$prob_win, test_pred) #mae
caret::RMSE(testing_data$prob_win, test_pred)

r32_games <- data.frame(game = c(1, 29, 17, 13, 21, 9, 25, 5,
                                 2, 30, 18, 14, 22, 10, 26, 6,
                                 3, 31, 19, 15, 23, 11, 27, 7,
                                 4, 32, 20, 16, 24, 12, 28, 8))

r32_games <- r32_games %>%
  left_join(results_r64, c('game')) %>%
  select(predicted_winner) %>%
  rename(team = predicted_winner) %>%
  left_join(r64_current, c('team'))

r32_team1 <- r32_games[seq(1, nrow(r32_games), 2), ]
r32_team2 <- r32_games[seq(2, nrow(r32_games), 2), ]

r32_matchup_differences <- data.frame(
  seed = r32_team1$seed - r32_team2$seed,
  kenpom_o = r32_team1$kenpom.adjusted.offense - r32_team2$kenpom.adjusted.offense,
  kenpom_d = r32_team1$kenpom.adjusted.defense - r32_team2$kenpom.adjusted.defense,
  kenpom_tempo = r32_team1$kenpom.adjusted.tempo - r32_team2$kenpom.adjusted.tempo,
  bart_o = r32_team1$barttorvik.adjusted.offense - r32_team2$barttorvik.adjusted.offense,
  bart_d = r32_team1$barttorvik.adjusted.defense - r32_team2$barttorvik.adjusted.defense,
  bart_tempo = r32_team1$barttorvik.adjusted.tempo - r32_team2$barttorvik.adjusted.tempo,
  barthag = r32_team1$barthag - r32_team2$barthag,
  elite_sos = r32_team1$elite.sos - r32_team2$elite.sos,
  ft_pctg = r32_team1$free.throw.. - r32_team2$free.throw..,
  # efg_pct = r32_team1$efg.. - r32_team2$efg..,
  ft_rate = r32_team1$free.throw.rate - r32_team2$free.throw.rate,
  o_reb = r32_team1$offensive.rebound.. - r32_team2$offensive.rebound..,
  # d_reb = r32_team1$defensive.rebound.. - r32_team2$defensive.rebound..,
  turnover = r32_team1$turnover.. - r32_team2$turnover..,
  block = r32_team1$block.. - r32_team2$block..
  # turnover_def = r32_team1$turnover...defense - r32_team2$turnover...defense
  # opp_o_reb = r32_team1$op.o.reb.. - r32_team2$op.o.reb..,
  # opp_d_reb = r32_team1$op.d.reb.. - r32_team2$op.d.reb..,
  # two_pctg = current_team1$x2pt.. - current_team2$x2pt..,
  # three_pctg = current_team1$x3pt.. - current_team2$x3pt..,
  # three_rate = current_team1$x3pt.rate - current_team2$x3pt.rate,
  # assist = current_team1$assist.. - current_team2$assist..,
  # two_pt_def = current_team1$x2pt...defense - current_team2$x2pt...defense,
  # three_pt_def = current_team1$x3pt...defense - current_team2$x3pt...defense,
  # ft_defense = current_team1$free.throw...defense - current_team2$free.throw...defense,
  # efg_def = current_team1$efg...defense - current_team2$efg...defense,
  # ft_rate_def = current_team1$free.throw.rate.defense - current_team2$free.throw.rate.defense,
  # three_pt_rate_def = current_team1$x3pt.rate.defense - current_team2$x3pt.rate.defense,
  # opp_assist = current_team1$op.assist.. - current_team2$op.assist..,
  # blocked = current_team1$blocked.. - current_team2$blocked..,
  # ppp_o = current_team1$points.per.possession.offense - current_team2$points.per.possession.offense,
  # ppp_d = current_team1$points.per.possession.defense - current_team2$points.per.possession.defense
)

r32_matchup_differences$prob_win <- ''

r32_pred <- predict(r32_xgb_model, as.matrix(r32_matchup_differences[, -15]))

results_r32 <- data.frame(predicted_win_prob = r32_pred)

results_r32$team <- r32_team1$team
results_r32$opponent <- r32_team2$team
results_r32$predicted_winner <- ifelse(results_r32$predicted_win_prob >= .5, r32_team1$team, r32_team2$team)
results_r32$game <- seq_along(results_r32$team)

write.csv(results_r32, "predicted_scores_round_32_v3.csv", row.names = FALSE)








# r16
r16_historical <- historical_data %>%
  filter(current.round == 16)

r16_historical_team1 <- r16_historical[seq(1, nrow(r16_historical), 2), ]
r16_historical_team2 <- r16_historical[seq(2, nrow(r16_historical), 2), ]

r16_historical_matchup_differences <- data.frame(
  seed = r16_historical_team1$seed - r16_historical_team2$seed,
  kenpom_o = r16_historical_team1$kenpom.adjusted.offense - r16_historical_team2$kenpom.adjusted.offense,
  kenpom_d = r16_historical_team1$kenpom.adjusted.defense - r16_historical_team2$kenpom.adjusted.defense,
  kenpom_tempo = r16_historical_team1$kenpom.adjusted.tempo - r16_historical_team2$kenpom.adjusted.tempo,
  bart_o = r16_historical_team1$barttorvik.adjusted.offense - r16_historical_team2$barttorvik.adjusted.offense,
  bart_d = r16_historical_team1$barttorvik.adjusted.defense - r16_historical_team2$barttorvik.adjusted.defense,
  bart_tempo = r16_historical_team1$barttorvik.adjusted.tempo - r16_historical_team2$barttorvik.adjusted.tempo,
  barthag = r16_historical_team1$barthag - r16_historical_team2$barthag,
  elite_sos = r16_historical_team1$elite.sos - r16_historical_team2$elite.sos,
  ft_pctg = r16_historical_team1$free.throw.. - r16_historical_team2$free.throw..,
  # efg_pct = r16_historical_team1$efg.. - r16_historical_team2$efg..,
  ft_rate = r16_historical_team1$free.throw.rate - r16_historical_team2$free.throw.rate,
  o_reb = r16_historical_team1$offensive.rebound.. - r16_historical_team2$offensive.rebound..,
  # d_reb = r16_historical_team1$defensive.rebound.. - r16_historical_team2$defensive.rebound..,
  turnover = r16_historical_team1$turnover.. - r16_historical_team2$turnover..,
  block = r16_historical_team1$block.. - r16_historical_team2$block..,
  # turnover_def = r16_historical_team1$turnover...defense - r16_historical_team2$turnover...defense,
  score = r16_historical_team1$score - r16_historical_team2$score
  # opp_o_reb = r16_historical_team1$op.o.reb.. - r16_historical_team2$op.o.reb..,
  # opp_d_reb = r16_historical_team1$op.d.reb.. - r16_historical_team2$op.d.reb..,
  # two_pctg = historical_team1$x2pt.. - historical_team2$x2pt..,
  # three_pctg = historical_team1$x3pt.. - historical_team2$x3pt..,
  # three_rate = historical_team1$x3pt.rate - historical_team2$x3pt.rate,
  # assist = historical_team1$assist.. - historical_team2$assist..,
  # two_pt_def = historical_team1$x2pt...defense - historical_team2$x2pt...defense,
  # three_pt_def = historical_team1$x3pt...defense - historical_team2$x3pt...defense,
  # ft_defense = historical_team1$free.throw...defense - historical_team2$free.throw...defense,
  # efg_def = historical_team1$efg...defense - historical_team2$efg...defense,
  # ft_rate_def = historical_team1$free.throw.rate.defense - historical_team2$free.throw.rate.defense,
  # three_pt_rate_def = historical_team1$x3pt.rate.defense - historical_team2$x3pt.rate.defense,
  # opp_assist = historical_team1$op.assist.. - historical_team2$op.assist..,
  # blocked = historical_team1$blocked.. - historical_team2$blocked..,
  # ppp_o = historical_team1$points.per.possession.offense - historical_team2$points.per.possession.offense,
  # ppp_d = historical_team1$points.per.possession.defense - historical_team2$points.per.possession.defense,
)

set.seed(123)
r16_historical_matchup_differences$prob_win <- ''

trainIndex <- createDataPartition(r16_historical_matchup_differences$score, p = 0.8, list = FALSE)
training_data <- r16_historical_matchup_differences[trainIndex, ]
testing_data <- r16_historical_matchup_differences[-trainIndex, ]

training_data$prob_win <- plogis(training_data$score)
testing_data$prob_win <- plogis(testing_data$score)

training_data <- training_data %>%
  select(-score)

testing_data <- testing_data %>%
  select(-score)

params <- list(objective = "reg:logistic", eval_metric = "logloss")
r16_xgb_model <- xgboost(data = as.matrix(training_data[, -15]), label = training_data$prob_win, 
                         nthread = 4, nrounds = 10,  params = params)

importance <- xgb.importance(feature_names = colnames(as.matrix(training_data[, -15])), model = r16_xgb_model)
head(importance)

test_pred <- predict(r16_xgb_model, as.matrix(testing_data[, -15]))

mean((testing_data$prob_win - test_pred)^2) #mse
caret::MAE(testing_data$prob_win, test_pred) #mae
caret::RMSE(testing_data$prob_win, test_pred)

r16_games <- data.frame(game = c(1, 2, 3, 4, 
                                 5, 6, 7, 8,
                                 9, 10, 11, 12,
                                 13, 14, 15, 16))

r16_games <- r16_games %>%
  left_join(results_r32, c('game')) %>%
  select(predicted_winner) %>%
  rename(team = predicted_winner) %>%
  left_join(r64_current, c('team'))

r16_team1 <- r16_games[seq(1, nrow(r16_games), 2), ]
r16_team2 <- r16_games[seq(2, nrow(r16_games), 2), ]

r16_matchup_differences <- data.frame(
  seed = r16_team1$seed - r16_team2$seed,
  kenpom_o = r16_team1$kenpom.adjusted.offense - r16_team2$kenpom.adjusted.offense,
  kenpom_d = r16_team1$kenpom.adjusted.defense - r16_team2$kenpom.adjusted.defense,
  kenpom_tempo = r16_team1$kenpom.adjusted.tempo - r16_team2$kenpom.adjusted.tempo,
  bart_o = r16_team1$barttorvik.adjusted.offense - r16_team2$barttorvik.adjusted.offense,
  bart_d = r16_team1$barttorvik.adjusted.defense - r16_team2$barttorvik.adjusted.defense,
  bart_tempo = r16_team1$barttorvik.adjusted.tempo - r16_team2$barttorvik.adjusted.tempo,
  barthag = r16_team1$barthag - r16_team2$barthag,
  elite_sos = r16_team1$elite.sos - r16_team2$elite.sos,
  ft_pctg = r16_team1$free.throw.. - r16_team2$free.throw..,
  # efg_pct = r16_team1$efg.. - r16_team2$efg..,
  ft_rate = r16_team1$free.throw.rate - r16_team2$free.throw.rate,
  o_reb = r16_team1$offensive.rebound.. - r16_team2$offensive.rebound..,
  # d_reb = r16_team1$defensive.rebound.. - r16_team2$defensive.rebound..,
  turnover = r16_team1$turnover.. - r16_team2$turnover..,
  block = r16_team1$block.. - r16_team2$block..
  # turnover_def = r16_team1$turnover...defense - r16_team2$turnover...defense
  # opp_o_reb = r16_team1$op.o.reb.. - r16_team2$op.o.reb..,
  # opp_d_reb = r16_team1$op.d.reb.. - r16_team2$op.d.reb..,
  # two_pctg = current_team1$x2pt.. - current_team2$x2pt..,
  # three_pctg = current_team1$x3pt.. - current_team2$x3pt..,
  # three_rate = current_team1$x3pt.rate - current_team2$x3pt.rate,
  # assist = current_team1$assist.. - current_team2$assist..,
  # two_pt_def = current_team1$x2pt...defense - current_team2$x2pt...defense,
  # three_pt_def = current_team1$x3pt...defense - current_team2$x3pt...defense,
  # ft_defense = current_team1$free.throw...defense - current_team2$free.throw...defense,
  # efg_def = current_team1$efg...defense - current_team2$efg...defense,
  # ft_rate_def = current_team1$free.throw.rate.defense - current_team2$free.throw.rate.defense,
  # three_pt_rate_def = current_team1$x3pt.rate.defense - current_team2$x3pt.rate.defense,
  # opp_assist = current_team1$op.assist.. - current_team2$op.assist..,
  # blocked = current_team1$blocked.. - current_team2$blocked..,
  # ppp_o = current_team1$points.per.possession.offense - current_team2$points.per.possession.offense,
  # ppp_d = current_team1$points.per.possession.defense - current_team2$points.per.possession.defense
)

r16_matchup_differences$prob_win <- ''

r16_pred <- predict(r16_xgb_model, as.matrix(r16_matchup_differences[, -15]))

results_r16 <- data.frame(predicted_win_prob = r16_pred)

results_r16$team <- r16_team1$team
results_r16$opponent <- r16_team2$team
results_r16$predicted_winner <- ifelse(results_r16$predicted_win_prob >= .5, r16_team1$team, r16_team2$team)
results_r16$game <- seq_along(results_r16$team)

write.csv(results_r16, "predicted_scores_round_16_v3.csv", row.names = FALSE)








# r8
r8_historical <- historical_data %>%
  filter(current.round == 8)

r8_historical_team1 <- r8_historical[seq(1, nrow(r8_historical), 2), ]
r8_historical_team2 <- r8_historical[seq(2, nrow(r8_historical), 2), ]

r8_historical_matchup_differences <- data.frame(
  seed = r8_historical_team1$seed - r8_historical_team2$seed,
  kenpom_o = r8_historical_team1$kenpom.adjusted.offense - r8_historical_team2$kenpom.adjusted.offense,
  kenpom_d = r8_historical_team1$kenpom.adjusted.defense - r8_historical_team2$kenpom.adjusted.defense,
  kenpom_tempo = r8_historical_team1$kenpom.adjusted.tempo - r8_historical_team2$kenpom.adjusted.tempo,
  bart_o = r8_historical_team1$barttorvik.adjusted.offense - r8_historical_team2$barttorvik.adjusted.offense,
  bart_d = r8_historical_team1$barttorvik.adjusted.defense - r8_historical_team2$barttorvik.adjusted.defense,
  bart_tempo = r8_historical_team1$barttorvik.adjusted.tempo - r8_historical_team2$barttorvik.adjusted.tempo,
  barthag = r8_historical_team1$barthag - r8_historical_team2$barthag,
  elite_sos = r8_historical_team1$elite.sos - r8_historical_team2$elite.sos,
  ft_pctg = r8_historical_team1$free.throw.. - r8_historical_team2$free.throw..,
  # efg_pct = r8_historical_team1$efg.. - r8_historical_team2$efg..,
  ft_rate = r8_historical_team1$free.throw.rate - r8_historical_team2$free.throw.rate,
  o_reb = r8_historical_team1$offensive.rebound.. - r8_historical_team2$offensive.rebound..,
  # d_reb = r8_historical_team1$defensive.rebound.. - r8_historical_team2$defensive.rebound..,
  turnover = r8_historical_team1$turnover.. - r8_historical_team2$turnover..,
  block = r8_historical_team1$block.. - r8_historical_team2$block..,
  # turnover_def = r8_historical_team1$turnover...defense - r8_historical_team2$turnover...defense,
  score = r8_historical_team1$score - r8_historical_team2$score
  # opp_o_reb = r8_historical_team1$op.o.reb.. - r8_historical_team2$op.o.reb..,
  # opp_d_reb = r8_historical_team1$op.d.reb.. - r8_historical_team2$op.d.reb..,
  # two_pctg = historical_team1$x2pt.. - historical_team2$x2pt..,
  # three_pctg = historical_team1$x3pt.. - historical_team2$x3pt..,
  # three_rate = historical_team1$x3pt.rate - historical_team2$x3pt.rate,
  # assist = historical_team1$assist.. - historical_team2$assist..,
  # two_pt_def = historical_team1$x2pt...defense - historical_team2$x2pt...defense,
  # three_pt_def = historical_team1$x3pt...defense - historical_team2$x3pt...defense,
  # ft_defense = historical_team1$free.throw...defense - historical_team2$free.throw...defense,
  # efg_def = historical_team1$efg...defense - historical_team2$efg...defense,
  # ft_rate_def = historical_team1$free.throw.rate.defense - historical_team2$free.throw.rate.defense,
  # three_pt_rate_def = historical_team1$x3pt.rate.defense - historical_team2$x3pt.rate.defense,
  # opp_assist = historical_team1$op.assist.. - historical_team2$op.assist..,
  # blocked = historical_team1$blocked.. - historical_team2$blocked..,
  # ppp_o = historical_team1$points.per.possession.offense - historical_team2$points.per.possession.offense,
  # ppp_d = historical_team1$points.per.possession.defense - historical_team2$points.per.possession.defense,
)

set.seed(123)
r8_historical_matchup_differences$prob_win <- ''

trainIndex <- createDataPartition(r8_historical_matchup_differences$score, p = 0.8, list = FALSE)
training_data <- r8_historical_matchup_differences[trainIndex, ]
testing_data <- r8_historical_matchup_differences[-trainIndex, ]

training_data$prob_win <- plogis(training_data$score)
testing_data$prob_win <- plogis(testing_data$score)

training_data <- training_data %>%
  select(-score)

testing_data <- testing_data %>%
  select(-score)

params <- list(objective = "reg:logistic", eval_metric = "logloss")
r8_xgb_model <- xgboost(data = as.matrix(training_data[, -15]), label = training_data$prob_win, 
                        nthread = 4, nrounds = 10,  params = params)

importance <- xgb.importance(feature_names = colnames(as.matrix(training_data[, -15])), model = r8_xgb_model)
head(importance)

test_pred <- predict(r8_xgb_model, as.matrix(testing_data[, -15]))

mean((testing_data$prob_win - test_pred)^2) #mse
caret::MAE(testing_data$prob_win, test_pred) #mae
caret::RMSE(testing_data$prob_win, test_pred)

r8_games <- data.frame(game = c(1, 2, 3, 4, 
                                5, 6, 7, 8))

r8_games <- r8_games %>%
  left_join(results_r64, c('game')) %>%
  select(predicted_winner) %>%
  rename(team = predicted_winner) %>%
  left_join(r64_current, c('team'))

r8_team1 <- r8_games[seq(1, nrow(r8_games), 2), ]
r8_team2 <- r8_games[seq(2, nrow(r8_games), 2), ]

r8_matchup_differences <- data.frame(
  seed = r8_team1$seed - r8_team2$seed,
  kenpom_o = r8_team1$kenpom.adjusted.offense - r8_team2$kenpom.adjusted.offense,
  kenpom_d = r8_team1$kenpom.adjusted.defense - r8_team2$kenpom.adjusted.defense,
  kenpom_tempo = r8_team1$kenpom.adjusted.tempo - r8_team2$kenpom.adjusted.tempo,
  bart_o = r8_team1$barttorvik.adjusted.offense - r8_team2$barttorvik.adjusted.offense,
  bart_d = r8_team1$barttorvik.adjusted.defense - r8_team2$barttorvik.adjusted.defense,
  bart_tempo = r8_team1$barttorvik.adjusted.tempo - r8_team2$barttorvik.adjusted.tempo,
  barthag = r8_team1$barthag - r8_team2$barthag,
  elite_sos = r8_team1$elite.sos - r8_team2$elite.sos,
  ft_pctg = r8_team1$free.throw.. - r8_team2$free.throw..,
  # efg_pct = r8_team1$efg.. - r8_team2$efg..,
  ft_rate = r8_team1$free.throw.rate - r8_team2$free.throw.rate,
  o_reb = r8_team1$offensive.rebound.. - r8_team2$offensive.rebound..,
  # d_reb = r8_team1$defensive.rebound.. - r8_team2$defensive.rebound..,
  turnover = r8_team1$turnover.. - r8_team2$turnover..,
  block = r8_team1$block.. - r8_team2$block..
  # turnover_def = r8_team1$turnover...defense - r8_team2$turnover...defense
  # opp_o_reb = r8_team1$op.o.reb.. - r8_team2$op.o.reb..,
  # opp_d_reb = r8_team1$op.d.reb.. - r8_team2$op.d.reb..,
  # two_pctg = current_team1$x2pt.. - current_team2$x2pt..,
  # three_pctg = current_team1$x3pt.. - current_team2$x3pt..,
  # three_rate = current_team1$x3pt.rate - current_team2$x3pt.rate,
  # assist = current_team1$assist.. - current_team2$assist..,
  # two_pt_def = current_team1$x2pt...defense - current_team2$x2pt...defense,
  # three_pt_def = current_team1$x3pt...defense - current_team2$x3pt...defense,
  # ft_defense = current_team1$free.throw...defense - current_team2$free.throw...defense,
  # efg_def = current_team1$efg...defense - current_team2$efg...defense,
  # ft_rate_def = current_team1$free.throw.rate.defense - current_team2$free.throw.rate.defense,
  # three_pt_rate_def = current_team1$x3pt.rate.defense - current_team2$x3pt.rate.defense,
  # opp_assist = current_team1$op.assist.. - current_team2$op.assist..,
  # blocked = current_team1$blocked.. - current_team2$blocked..,
  # ppp_o = current_team1$points.per.possession.offense - current_team2$points.per.possession.offense,
  # ppp_d = current_team1$points.per.possession.defense - current_team2$points.per.possession.defense
)

r8_matchup_differences$prob_win <- ''

r8_pred <- predict(r8_xgb_model, as.matrix(r8_matchup_differences[, -15]))

results_r8 <- data.frame(predicted_win_prob = r8_pred)

results_r8$team <- r8_team1$team
results_r8$opponent <- r8_team2$team
results_r8$predicted_winner <- ifelse(results_r8$predicted_win_prob >= .5, r8_team1$team, r8_team2$team)
results_r8$game <- seq_along(results_r8$team)

write.csv(results_r8, "predicted_scores_round_8_v3.csv", row.names = FALSE)







# r4
r4_historical <- historical_data %>%
  filter(current.round == 4)

r4_historical_team1 <- r4_historical[seq(1, nrow(r4_historical), 2), ]
r4_historical_team2 <- r4_historical[seq(2, nrow(r4_historical), 2), ]

r4_historical_matchup_differences <- data.frame(
  seed = r4_historical_team1$seed - r4_historical_team2$seed,
  kenpom_o = r4_historical_team1$kenpom.adjusted.offense - r4_historical_team2$kenpom.adjusted.offense,
  kenpom_d = r4_historical_team1$kenpom.adjusted.defense - r4_historical_team2$kenpom.adjusted.defense,
  kenpom_tempo = r4_historical_team1$kenpom.adjusted.tempo - r4_historical_team2$kenpom.adjusted.tempo,
  bart_o = r4_historical_team1$barttorvik.adjusted.offense - r4_historical_team2$barttorvik.adjusted.offense,
  bart_d = r4_historical_team1$barttorvik.adjusted.defense - r4_historical_team2$barttorvik.adjusted.defense,
  bart_tempo = r4_historical_team1$barttorvik.adjusted.tempo - r4_historical_team2$barttorvik.adjusted.tempo,
  barthag = r4_historical_team1$barthag - r4_historical_team2$barthag,
  elite_sos = r4_historical_team1$elite.sos - r4_historical_team2$elite.sos,
  ft_pctg = r4_historical_team1$free.throw.. - r4_historical_team2$free.throw..,
  # efg_pct = r4_historical_team1$efg.. - r4_historical_team2$efg..,
  ft_rate = r4_historical_team1$free.throw.rate - r4_historical_team2$free.throw.rate,
  o_reb = r4_historical_team1$offensive.rebound.. - r4_historical_team2$offensive.rebound..,
  # d_reb = r4_historical_team1$defensive.rebound.. - r4_historical_team2$defensive.rebound..,
  turnover = r4_historical_team1$turnover.. - r4_historical_team2$turnover..,
  block = r4_historical_team1$block.. - r4_historical_team2$block..,
  # turnover_def = r4_historical_team1$turnover...defense - r4_historical_team2$turnover...defense,
  score = r4_historical_team1$score - r4_historical_team2$score
  # opp_o_reb = r4_historical_team1$op.o.reb.. - r4_historical_team2$op.o.reb..,
  # opp_d_reb = r4_historical_team1$op.d.reb.. - r4_historical_team2$op.d.reb..,
  # two_pctg = historical_team1$x2pt.. - historical_team2$x2pt..,
  # three_pctg = historical_team1$x3pt.. - historical_team2$x3pt..,
  # three_rate = historical_team1$x3pt.rate - historical_team2$x3pt.rate,
  # assist = historical_team1$assist.. - historical_team2$assist..,
  # two_pt_def = historical_team1$x2pt...defense - historical_team2$x2pt...defense,
  # three_pt_def = historical_team1$x3pt...defense - historical_team2$x3pt...defense,
  # ft_defense = historical_team1$free.throw...defense - historical_team2$free.throw...defense,
  # efg_def = historical_team1$efg...defense - historical_team2$efg...defense,
  # ft_rate_def = historical_team1$free.throw.rate.defense - historical_team2$free.throw.rate.defense,
  # three_pt_rate_def = historical_team1$x3pt.rate.defense - historical_team2$x3pt.rate.defense,
  # opp_assist = historical_team1$op.assist.. - historical_team2$op.assist..,
  # blocked = historical_team1$blocked.. - historical_team2$blocked..,
  # ppp_o = historical_team1$points.per.possession.offense - historical_team2$points.per.possession.offense,
  # ppp_d = historical_team1$points.per.possession.defense - historical_team2$points.per.possession.defense,
)

set.seed(123)
r4_historical_matchup_differences$prob_win <- ''

trainIndex <- createDataPartition(r4_historical_matchup_differences$score, p = 0.8, list = FALSE)
training_data <- r4_historical_matchup_differences[trainIndex, ]
testing_data <- r4_historical_matchup_differences[-trainIndex, ]

training_data$prob_win <- plogis(training_data$score)
testing_data$prob_win <- plogis(testing_data$score)

training_data <- training_data %>%
  select(-score)

testing_data <- testing_data %>%
  select(-score)

params <- list(objective = "reg:logistic", eval_metric = "logloss")
r4_xgb_model <- xgboost(data = as.matrix(training_data[, -15]), label = training_data$prob_win, 
                        nthread = 4, nrounds = 10,  params = params)

importance <- xgb.importance(feature_names = colnames(as.matrix(training_data[, -15])), model = r4_xgb_model)
head(importance)

test_pred <- predict(r4_xgb_model, as.matrix(testing_data[, -15]))

mean((testing_data$prob_win - test_pred)^2) #mse
caret::MAE(testing_data$prob_win, test_pred) #mae
caret::RMSE(testing_data$prob_win, test_pred)

r4_games <- data.frame(game = c(1, 2, 3, 4))

r4_games <- r4_games %>%
  left_join(results_r64, c('game')) %>%
  select(predicted_winner) %>%
  rename(team = predicted_winner) %>%
  left_join(r64_current, c('team'))

r4_team1 <- r4_games[seq(1, nrow(r4_games), 2), ]
r4_team2 <- r4_games[seq(2, nrow(r4_games), 2), ]

r4_matchup_differences <- data.frame(
  seed = r4_team1$seed - r4_team2$seed,
  kenpom_o = r4_team1$kenpom.adjusted.offense - r4_team2$kenpom.adjusted.offense,
  kenpom_d = r4_team1$kenpom.adjusted.defense - r4_team2$kenpom.adjusted.defense,
  kenpom_tempo = r4_team1$kenpom.adjusted.tempo - r4_team2$kenpom.adjusted.tempo,
  bart_o = r4_team1$barttorvik.adjusted.offense - r4_team2$barttorvik.adjusted.offense,
  bart_d = r4_team1$barttorvik.adjusted.defense - r4_team2$barttorvik.adjusted.defense,
  bart_tempo = r4_team1$barttorvik.adjusted.tempo - r4_team2$barttorvik.adjusted.tempo,
  barthag = r4_team1$barthag - r4_team2$barthag,
  elite_sos = r4_team1$elite.sos - r4_team2$elite.sos,
  ft_pctg = r4_team1$free.throw.. - r4_team2$free.throw..,
  # efg_pct = r4_team1$efg.. - r4_team2$efg..,
  ft_rate = r4_team1$free.throw.rate - r4_team2$free.throw.rate,
  o_reb = r4_team1$offensive.rebound.. - r4_team2$offensive.rebound..,
  # d_reb = r4_team1$defensive.rebound.. - r4_team2$defensive.rebound..,
  turnover = r4_team1$turnover.. - r4_team2$turnover..,
  block = r4_team1$block.. - r4_team2$block..
  # turnover_def = r4_team1$turnover...defense - r4_team2$turnover...defense
  # opp_o_reb = r4_team1$op.o.reb.. - r4_team2$op.o.reb..,
  # opp_d_reb = r4_team1$op.d.reb.. - r4_team2$op.d.reb..,
  # two_pctg = current_team1$x2pt.. - current_team2$x2pt..,
  # three_pctg = current_team1$x3pt.. - current_team2$x3pt..,
  # three_rate = current_team1$x3pt.rate - current_team2$x3pt.rate,
  # assist = current_team1$assist.. - current_team2$assist..,
  # two_pt_def = current_team1$x2pt...defense - current_team2$x2pt...defense,
  # three_pt_def = current_team1$x3pt...defense - current_team2$x3pt...defense,
  # ft_defense = current_team1$free.throw...defense - current_team2$free.throw...defense,
  # efg_def = current_team1$efg...defense - current_team2$efg...defense,
  # ft_rate_def = current_team1$free.throw.rate.defense - current_team2$free.throw.rate.defense,
  # three_pt_rate_def = current_team1$x3pt.rate.defense - current_team2$x3pt.rate.defense,
  # opp_assist = current_team1$op.assist.. - current_team2$op.assist..,
  # blocked = current_team1$blocked.. - current_team2$blocked..,
  # ppp_o = current_team1$points.per.possession.offense - current_team2$points.per.possession.offense,
  # ppp_d = current_team1$points.per.possession.defense - current_team2$points.per.possession.defense
)

r4_matchup_differences$prob_win <- ''

r4_pred <- predict(r4_xgb_model, as.matrix(r4_matchup_differences[, -15]))

results_r4 <- data.frame(predicted_win_prob = r4_pred)

results_r4$team <- r4_team1$team
results_r4$opponent <- r4_team2$team
results_r4$predicted_winner <- ifelse(results_r4$predicted_win_prob >= .5, r4_team1$team, r4_team2$team)
results_r4$game <- seq_along(results_r4$team)

write.csv(results_r4, "predicted_scores_round_4_v3.csv", row.names = FALSE)







# r2
r2_historical <- historical_data %>%
  filter(current.round == 2)

r2_historical_team1 <- r2_historical[seq(1, nrow(r2_historical), 2), ]
r2_historical_team2 <- r2_historical[seq(2, nrow(r2_historical), 2), ]

r2_historical_matchup_differences <- data.frame(
  seed = r2_historical_team1$seed - r2_historical_team2$seed,
  kenpom_o = r2_historical_team1$kenpom.adjusted.offense - r2_historical_team2$kenpom.adjusted.offense,
  kenpom_d = r2_historical_team1$kenpom.adjusted.defense - r2_historical_team2$kenpom.adjusted.defense,
  kenpom_tempo = r2_historical_team1$kenpom.adjusted.tempo - r2_historical_team2$kenpom.adjusted.tempo,
  bart_o = r2_historical_team1$barttorvik.adjusted.offense - r2_historical_team2$barttorvik.adjusted.offense,
  bart_d = r2_historical_team1$barttorvik.adjusted.defense - r2_historical_team2$barttorvik.adjusted.defense,
  bart_tempo = r2_historical_team1$barttorvik.adjusted.tempo - r2_historical_team2$barttorvik.adjusted.tempo,
  barthag = r2_historical_team1$barthag - r2_historical_team2$barthag,
  elite_sos = r2_historical_team1$elite.sos - r2_historical_team2$elite.sos,
  ft_pctg = r2_historical_team1$free.throw.. - r2_historical_team2$free.throw..,
  # efg_pct = r2_historical_team1$efg.. - r2_historical_team2$efg..,
  ft_rate = r2_historical_team1$free.throw.rate - r2_historical_team2$free.throw.rate,
  o_reb = r2_historical_team1$offensive.rebound.. - r2_historical_team2$offensive.rebound..,
  # d_reb = r2_historical_team1$defensive.rebound.. - r2_historical_team2$defensive.rebound..,
  turnover = r2_historical_team1$turnover.. - r2_historical_team2$turnover..,
  block = r2_historical_team1$block.. - r2_historical_team2$block..,
  # turnover_def = r2_historical_team1$turnover...defense - r2_historical_team2$turnover...defense,
  score = r2_historical_team1$score - r2_historical_team2$score
  # opp_o_reb = r2_historical_team1$op.o.reb.. - r2_historical_team2$op.o.reb..,
  # opp_d_reb = r2_historical_team1$op.d.reb.. - r2_historical_team2$op.d.reb..,
  # two_pctg = historical_team1$x2pt.. - historical_team2$x2pt..,
  # three_pctg = historical_team1$x3pt.. - historical_team2$x3pt..,
  # three_rate = historical_team1$x3pt.rate - historical_team2$x3pt.rate,
  # assist = historical_team1$assist.. - historical_team2$assist..,
  # two_pt_def = historical_team1$x2pt...defense - historical_team2$x2pt...defense,
  # three_pt_def = historical_team1$x3pt...defense - historical_team2$x3pt...defense,
  # ft_defense = historical_team1$free.throw...defense - historical_team2$free.throw...defense,
  # efg_def = historical_team1$efg...defense - historical_team2$efg...defense,
  # ft_rate_def = historical_team1$free.throw.rate.defense - historical_team2$free.throw.rate.defense,
  # three_pt_rate_def = historical_team1$x3pt.rate.defense - historical_team2$x3pt.rate.defense,
  # opp_assist = historical_team1$op.assist.. - historical_team2$op.assist..,
  # blocked = historical_team1$blocked.. - historical_team2$blocked..,
  # ppp_o = historical_team1$points.per.possession.offense - historical_team2$points.per.possession.offense,
  # ppp_d = historical_team1$points.per.possession.defense - historical_team2$points.per.possession.defense,
)

set.seed(123)
r2_historical_matchup_differences$prob_win <- ''

trainIndex <- createDataPartition(r2_historical_matchup_differences$score, p = 0.7, list = FALSE)
training_data <- r2_historical_matchup_differences[trainIndex, ]
testing_data <- r2_historical_matchup_differences[-trainIndex, ]

training_data$prob_win <- plogis(training_data$score)
testing_data$prob_win <- plogis(testing_data$score)

training_data <- training_data %>%
  select(-score)

testing_data <- testing_data %>%
  select(-score)

params <- list(objective = "reg:logistic", eval_metric = "logloss")
r2_xgb_model <- xgboost(data = as.matrix(training_data[, -15]), label = training_data$prob_win, 
                        nthread = 4, nrounds = 10,  params = params)

importance <- xgb.importance(feature_names = colnames(as.matrix(training_data[, -15])), model = r2_xgb_model)
head(importance)

test_pred <- predict(r2_xgb_model, as.matrix(testing_data[, -15]))

mean((testing_data$prob_win - test_pred)^2) #mse
caret::MAE(testing_data$prob_win, test_pred) #mae
caret::RMSE(testing_data$prob_win, test_pred)

r2_games <- data.frame(game = c(1, 2))

r2_games <- r2_games %>%
  left_join(results_r64, c('game')) %>%
  select(predicted_winner) %>%
  rename(team = predicted_winner) %>%
  left_join(r64_current, c('team'))

r2_team1 <- r2_games[1, ]
r2_team2 <- r2_games[2, ]

r2_matchup_differences <- data.frame(
  seed = r2_team1$seed - r2_team2$seed,
  kenpom_o = r2_team1$kenpom.adjusted.offense - r2_team2$kenpom.adjusted.offense,
  kenpom_d = r2_team1$kenpom.adjusted.defense - r2_team2$kenpom.adjusted.defense,
  kenpom_tempo = r2_team1$kenpom.adjusted.tempo - r2_team2$kenpom.adjusted.tempo,
  bart_o = r2_team1$barttorvik.adjusted.offense - r2_team2$barttorvik.adjusted.offense,
  bart_d = r2_team1$barttorvik.adjusted.defense - r2_team2$barttorvik.adjusted.defense,
  bart_tempo = r2_team1$barttorvik.adjusted.tempo - r2_team2$barttorvik.adjusted.tempo,
  barthag = r2_team1$barthag - r2_team2$barthag,
  elite_sos = r2_team1$elite.sos - r2_team2$elite.sos,
  ft_pctg = r2_team1$free.throw.. - r2_team2$free.throw..,
  # efg_pct = r2_team1$efg.. - r2_team2$efg..,
  ft_rate = r2_team1$free.throw.rate - r2_team2$free.throw.rate,
  o_reb = r2_team1$offensive.rebound.. - r2_team2$offensive.rebound..,
  # d_reb = r2_team1$defensive.rebound.. - r2_team2$defensive.rebound..,
  turnover = r2_team1$turnover.. - r2_team2$turnover..,
  block = r2_team1$block.. - r2_team2$block..
  # turnover_def = r2_team1$turnover...defense - r2_team2$turnover...defense
  # opp_o_reb = r2_team1$op.o.reb.. - r2_team2$op.o.reb..,
  # opp_d_reb = r2_team1$op.d.reb.. - r2_team2$op.d.reb..,
  # two_pctg = current_team1$x2pt.. - current_team2$x2pt..,
  # three_pctg = current_team1$x3pt.. - current_team2$x3pt..,
  # three_rate = current_team1$x3pt.rate - current_team2$x3pt.rate,
  # assist = current_team1$assist.. - current_team2$assist..,
  # two_pt_def = current_team1$x2pt...defense - current_team2$x2pt...defense,
  # three_pt_def = current_team1$x3pt...defense - current_team2$x3pt...defense,
  # ft_defense = current_team1$free.throw...defense - current_team2$free.throw...defense,
  # efg_def = current_team1$efg...defense - current_team2$efg...defense,
  # ft_rate_def = current_team1$free.throw.rate.defense - current_team2$free.throw.rate.defense,
  # three_pt_rate_def = current_team1$x3pt.rate.defense - current_team2$x3pt.rate.defense,
  # opp_assist = current_team1$op.assist.. - current_team2$op.assist..,
  # blocked = current_team1$blocked.. - current_team2$blocked..,
  # ppp_o = current_team1$points.per.possession.offense - current_team2$points.per.possession.offense,
  # ppp_d = current_team1$points.per.possession.defense - current_team2$points.per.possession.defense
)

r2_matchup_differences$prob_win <- ''

r2_pred <- predict(r2_xgb_model, as.matrix(r2_matchup_differences[, -15]))

results_r2 <- data.frame(predicted_win_prob = r2_pred)

results_r2$team <- r2_team1$team
results_r2$opponent <- r2_team2$team
results_r2$predicted_winner <- ifelse(results_r2$predicted_win_prob >= .5, r2_team1$team, r2_team2$team)
results_r2$game <- seq_along(results_r2$team)

write.csv(results_r2, "predicted_scores_round_2_v3.csv", row.names = FALSE)


