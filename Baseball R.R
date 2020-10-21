setwd("~/Downloads/baseball data/core")

#read in data
baseball_data <- read.csv("Teams_2010.csv", h = TRUE)

#get sample size
sample_size <- floor(0.90 * nrow(baseball_data))

## set the seed to make your partition reproducible
set.seed(20)
train_set<- sample(seq_len(nrow(baseball_data)), size = sample_size)

baseball_cor <- subset(baseball_data, select = c(W, X1B, X2B, HR, BB, SLG, OBP, OPS, SB, BA, ERA, ERAPlus, BBA, WHIP, SOA, FP))
cor(baseball_cor)

attach(baseball_data)

#examine data
plot(ERA,W)
plot(ERAPlus,W)
plot(OPS,W)
plot(BBA,W)
plot(WHIP,W)

cor(OPS,W)
cor(ERA,W)
cor(WHIP,W)
cor(ERAPlus,W)
cor(BBA,W)


baseball_train <- baseball_data[train_set, ]
baseball_test <- baseball_data[-train_set, ]

attach(baseball_train)

######linear regression models
baseball_linear_model_1 <- lm(W ~ OPS + ERA, data = baseball_train)
summary(baseball_linear_model_1)

baseball_linear_model_2 <- lm(W ~ OPS + ERAPlus, data = baseball_train)
summary(baseball_linear_model_2)

baseball_linear_model_3 <- lm(W ~ OPS + ERAPlus + FP, data = baseball_train)
summary(baseball_linear_model_3)

baseball_linear_model_4 <- lm(W ~ OPS + ERA + FP, data = baseball_train)
summary(baseball_linear_model_4)

####projecting wins
baseball_test$Proj_Wins_1 <- predict(baseball_linear_model_1,baseball_test)
baseball_test$Proj_Wins_1 <- round(baseball_test$Proj_Wins_1, digits = 1)
baseball_test$Proj_Wins_2 <- predict(baseball_linear_model_2,baseball_test)
baseball_test$Proj_Wins_2 <- round(baseball_test$Proj_Wins_2, digits = 1)
baseball_test$Proj_Wins_3 <- predict(baseball_linear_model_3,baseball_test)
baseball_test$Proj_Wins_3 <- round(baseball_test$Proj_Wins_3, digits = 1)
baseball_test$Proj_Wins_4 <- predict(baseball_linear_model_4,baseball_test)
baseball_test$Proj_Wins_4 <- round(baseball_test$Proj_Wins_4, digits = 1)

baseball_projections <- subset(baseball_test, select = c(yearID, teamID, W, Proj_Wins_1, Proj_Wins_2, Proj_Wins_3, Proj_Wins_4))

#finding variance between actual wins and projected
baseball_projections$Proj_Wins_1_VAR <- (baseball_projections$Proj_Wins_1 - baseball_projections$W)
baseball_projections$Proj_Wins_2_VAR <- (baseball_projections$Proj_Wins_2 - baseball_projections$W)
baseball_projections$Proj_Wins_3_VAR <- (baseball_projections$Proj_Wins_3 - baseball_projections$W)
baseball_projections$Proj_Wins_4_VAR <- (baseball_projections$Proj_Wins_4 - baseball_projections$W)


#plotting actual vs. projected wins
plot(baseball_test$Proj_Wins_1, baseball_test$W, main = "Proj vs. Actual Wins", 
     xlab = "Projected Wins", ylab = "Actual Wins")
cor(baseball_test$Proj_Wins_1, baseball_test$W)

plot(baseball_test$Proj_Wins_2, baseball_test$W, main = "Proj vs. Actual Wins", 
     xlab = "Projected Wins", ylab = "Actual Wins")
cor(baseball_test$Proj_Wins_2, baseball_test$W)

plot(baseball_test$Proj_Wins_3, baseball_test$W, main = "Proj vs. Actual Wins", 
     xlab = "Projected Wins", ylab = "Actual Wins")
cor(baseball_test$Proj_Wins_3, baseball_test$W)

plot(baseball_test$Proj_Wins_4, baseball_test$W, main = "Proj vs. Actual Wins", 
     xlab = "Projected Wins", ylab = "Actual Wins")
cor(baseball_test$Proj_Wins_4, baseball_test$W)

plot(baseball_test$Proj_Wins_1, baseball_test$W, main = "Proj vs. Actual Wins", 
     xlab = "Projected Wins", ylab = "Actual Wins", col = "red", pch = 0)
points(baseball_test$Proj_Wins_2,baseball_test$W, col = "blue", pch = 1)
points(baseball_test$Proj_Wins_3,baseball_test$W, col = 611, pch = 2)
points(baseball_test$Proj_Wins_4,baseball_test$W, col = 420, pch = 3)
points(baseball_test_nn$Proj_Wins_1, baseball_test$W, col = "green", pch = 22)
abline(0,1)


anova(baseball_linear_model_1, baseball_linear_model_2)
anova(baseball_linear_model_1, baseball_linear_model_3)
anova(baseball_linear_model_1, baseball_linear_model_4)
anova(baseball_linear_model_2, baseball_linear_model_3)
anova(baseball_linear_model_2, baseball_linear_model_4)
anova(baseball_linear_model_3, baseball_linear_model_4)

View(baseball_projections)

rmse(baseball_test$Proj_Wins_1,baseball_test$W)
rmse(baseball_test$Proj_Wins_2,baseball_test$W)
rmse(baseball_test$Proj_Wins_3,baseball_test$W)
rmse(baseball_test$Proj_Wins_4,baseball_test$W)



#########
##Neural Network

#####normalize and unnormalize functions
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

unnormalize <- function(x) {
  return((max(baseball_test_nn$W)-min(baseball_test_nn$W))*x+min(baseball_test_nn$W))
}
#####

install.packages("neuralnet")
library(neuralnet)

##Data prepartion
baseball_data_nn <- subset(baseball_data, select = c(OPS, ERA, FP, W))
baseball_train_nn <- baseball_data_nn[train_set, ]
baseball_test_nn <- baseball_data_nn[-train_set, ]

baseball_data_nn_norm <- as.data.frame(lapply(baseball_data_nn[1:4], normalize))
baseball_train_nn_norm <- baseball_data_nn_norm[train_set, ]
baseball_test_nn_norm <- baseball_data_nn_norm[-train_set, ]

#Neural Network Model
baseball_nn_model_1 <- neuralnet(W ~ OPS + ERA, data = baseball_train_nn_norm, hidden = 5)
plot(baseball_nn_model_1)
##

##Getting to predicted wins
baseball_nn_model_1_results <- neuralnet::compute(baseball_nn_model_1, baseball_test_nn_norm[1:2])
predicted_strength <- baseball_nn_model_1_results$net.result
cor(predicted_strength, baseball_test_nn_norm$W)

baseball_test_nn_norm$Proj_Wins_1 <- baseball_nn_model_1_results$net.result
baseball_test_nn$Proj_Wins_1 <- baseball_nn_model_1_results$net.result

baseball_test_nn$Proj_Wins_1 <- lapply(baseball_test_nn$Proj_Wins_1,unnormalize)
baseball_test_nn$Proj_Wins_1 <- as.numeric(baseball_test_nn$Proj_Wins_1)
baseball_test_nn$Proj_Wins_1 <- round(baseball_test_nn$Proj_Wins_1,1)
##

##Adding Team and Year to data
baseball_test_nn$yearID <- baseball_test$yearID
baseball_test_nn$teamID <- baseball_test$teamID
##

baseball_test_nn$Proj_Wins_1_VAR <- round((baseball_test_nn$Proj_Wins_1 - baseball_test_nn$W),digits = 1)

####Neural Network Performance
plot(baseball_test_nn$Proj_Wins_1, baseball_test_nn$W, main = "Proj vs. Actual Wins", 
     xlab = "Projected Wins", ylab = "Actual Wins")
abline(0,1)
cor(baseball_test_nn$Proj_Wins_1,baseball_test_nn$W)

rmse(baseball_test_nn$Proj_Wins_1,baseball_test_nn$W)
###


###########
##Decision Trees

install.packages("rsample")
library(rsample)

install.packages("rpart")
library(rpart)

install.packages("dplyr")
library(dplyr)

install.packages("ipred")
library(ipred)

install.packages("caret")
library(caret)

install.packages("Metrics")
library(Metrics)

##Data Prep
baseball_data_trees <- subset(baseball_data, select = c(OPS,ERA, W))

baseball_data_trees$yearID <- baseball_data$yearID
baseball_data_trees$teamID <- baseball_data$teamID

baseball_train_trees <- baseball_data_trees[train_set, ]
baseball_test_trees <- baseball_data_trees[-train_set, ]
##

#Regression Trees Model
baseball_trees_model_1 <- rpart(
  formula = W ~ OPS + ERA,
  data    = baseball_train_trees,
  method  = "anova",
  control = list(minsplit = 10, maxdepth = 9, xval = 10)
)


########improving performance by finding optimal tuning
baseball_trees_model_1$cptable


hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(7, 15, 1)
)

baseball_trees_models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  baseball_trees_models[[i]] <- rpart(
    formula = W ~ OPS + ERA,
    data    = baseball_train_trees,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}


#finding the optimal regression tree specs
#need dplyr for %>%
hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(baseball_trees_models, get_cp),
    error = purrr::map_dbl(baseball_trees_models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)

baseball_trees_model_optimal <- rpart(
  formula = W ~ OPS + ERA,
  data    = baseball_train_trees,
  method  = "anova",
  control = list(minsplit = 17, maxdepth = 7, cp = .01)
)

Proj_Wins_Optimal <- predict(baseball_trees_model_optimal, newdata = baseball_test_trees)
rmse(Proj_Wins_Optimal, baseball_test_trees$W)


#####ipred bagging model
baseball_trees_model_bagged_ipred <- bagging(
  formula = W ~ OPS + ERA,
  data    = baseball_train_trees,
  coob = TRUE
)

baseball_trees_model_bagged_ipred

Proj_Wins_ipred <- predict(baseball_trees_model_bagged_ipred, newdata = baseball_test_trees)

rmse(Proj_Wins_ipred, baseball_test_trees$W)


#####caret bagging model
ctrl <- trainControl(method = "cv",  number = 10)

baseball_trees_model_bagged_caret <- train(
  W ~ OPS + ERA,
  data = baseball_train_trees,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

plot(varImp(baseball_trees_model_bagged_caret), 4)

Proj_Wins_caret <- predict(baseball_trees_model_bagged_caret, newdata = baseball_test_trees)
RMSE(Proj_Wins_caret, baseball_test_trees$W)

###Model Performance Analysis
baseball_test_trees$Proj_Wins_caret <- predict(baseball_trees_model_bagged_caret, newdata = baseball_test_trees)
baseball_test_trees$Proj_Wins_ipred <- predict(baseball_trees_model_bagged_ipred, newdata = baseball_test_trees)
baseball_test_trees$Proj_Wins_optimal <- predict(baseball_trees_model_optimal, newdata = baseball_test_trees)


#Plotting model predictions
plot(baseball_test_trees$Proj_Wins_caret, baseball_test_trees$W, main = "Proj vs. Actual Wins", 
     xlab = "Projected Wins", ylab = "Actual Wins", col = "red", pch = 0)
points(baseball_test_trees$Proj_Wins_ipred,baseball_test_trees$W, col = "blue", pch = 1)
points(baseball_test_trees$Proj_Wins_optimal,baseball_test_trees$W, col = 611, pch = 2)

cor(baseball_test_trees$Proj_Wins_caret, baseball_test_trees$W)
cor(baseball_test_trees$Proj_Wins_ipred,baseball_test_trees$W)
cor(baseball_test_trees$Proj_Wins_optimal,baseball_test_trees$W)


###http://uc-r.github.io/regression_trees

###########

pirates_2019 <- data.frame(OPS = .714, ERA = 4.95, FP = .983, WHIP = 1.39)
predict(baseball_linear_model_4, newdata = pirates_2019)*(162-58)/162
predict(baseball_linear_model_4, newdata = pirates_2019, interval = "confidence")*(162-58)/162

##predicting 2019 Wins for remaining games

team_data_6_4_2019 <- read.csv("Team data 6-4-2019.csv", header = TRUE)
team_data_6_4_2019$Games <- team_data_6_4_2019$W+team_data_6_4_2019$L

##Regression Predictions
team_data_6_4_2019$Proj_Win_LM <- (predict(baseball_linear_model_4, newdata = team_data_6_4_2019)/162)*(162-team_data_6_4_2019$Games)
team_data_6_4_2019$Proj_Win_LM <- round(team_data_6_4_2019$Proj_Win_LM,1)
team_data_6_4_2019$Proj_Tot_Win_LM <- team_data_6_4_2019$Proj_Win_LM + team_data_6_4_2019$W
team_data_6_4_2019$Proj_Tot_Loss_LM <- 162 - team_data_6_4_2019$Proj_Tot_Win_LM


##NN predictions
team_data_6_4_2019_norm <- as.data.frame(lapply(team_data_6_4_2019[4:7], normalize))
team_data_6_4_2019_norm <- team_data_6_4_2019_norm[c("OPS","ERA","FP")]
baseball_64_model_results <- neuralnet::compute(baseball_nn_model_1, team_data_6_4_2019_norm)
predicted_strength <- baseball_64_model_results$net.result

team_data_6_4_2019$Proj_Win_NN <- baseball_64_model_results$net.result

team_data_6_4_2019$Proj_Win_NN <- lapply(team_data_6_4_2019$Proj_Win_NN,unnormalize)
team_data_6_4_2019$Proj_Win_NN <- as.numeric(team_data_6_4_2019$Proj_Win_NN)
team_data_6_4_2019$Proj_Win_NN <- (round(team_data_6_4_2019$Proj_Win_NN,1)/162)*(162-team_data_6_4_2019$Games)
team_data_6_4_2019$Proj_Win_NN <- round(team_data_6_4_2019$Proj_Win_NN,1)
team_data_6_4_2019$Proj_Tot_Win_NN <- team_data_6_4_2019$Proj_Win_NN + team_data_6_4_2019$W
team_data_6_4_2019$Proj_Tot_Loss_NN <- 162 - team_data_6_4_2019$Proj_Tot_Win_NN

##Regression Trees Predictions
team_data_6_4_2019$Proj_Win_Trees <- round((predict(baseball_trees_model_bagged_ipred, newdata = team_data_6_4_2019)/162)*(162-team_data_6_4_2019$Games),1)
team_data_6_4_2019$Proj_Tot_Win_Trees <- team_data_6_4_2019$Proj_Win_Trees + team_data_6_4_2019$W
team_data_6_4_2019$Proj_Tot_Loss_Trees <- 162 - team_data_6_4_2019$Proj_Tot_Win_Trees


write.csv(team_data_6_4_2019, "MLB 2019 Projected Wins 6-4-2019 v2.csv")

##https://www.baseball-reference.com/leagues/MLB/2019.shtml



#######Plots

plot(baseball_test$Proj_Wins_4, baseball_test$W, main = "Proj vs. Actual Wins", 
     xlab = "Projected Wins", ylab = "Actual Wins", col = "red", pch = 0)
points(baseball_test_trees$Proj_Wins_ipred,baseball_test_trees$W, col = "blue", pch = 1)
points(baseball_test_nn$Proj_Wins_1, baseball_test$W, col = "green", pch = 2)
abline(0,1)
legend(55, 105, legend=c("Reg", "Trees", "NN"),
       col=c("red", "blue", "green"), pch = c(0,1,2), cex=0.8)


baseball_data$kbb <- baseball_data$SO/baseball_data$BB
plot(baseball_data$kbb,baseball_data$W)


##2020 team data

hitting_data_2020 <- read.csv("FanGraphs Leaderboard-20.csv", h = TRUE)
hitting_data_2020 <- hitting_data_2020 %>% filter(hitting_data_2020$Team != "")
team_hitting_data_2020 <- hitting_data_2020 %>% group_by(Team) %>% summarise(PA = sum(PA), AB = sum(AB), 
                                                                      H = sum(H),X2B = sum(X2B),X3B = sum(X3B), HR = sum(HR),BB = sum(BB),
                                                                      num_players = n())
hitting_data_2020 <- left_join(hitting_data_2020, team_hitting_data_2020, by = c("Team" = "Team"))       

hitting_data_2020$OPS_team <- hitting_data_2020$OPS*(hitting_data_2020$PA.x/hitting_data_2020$PA.y)

team_hitting_data_2020 <- hitting_data_2020 %>% group_by(Team) %>% summarise(OPS = sum(OPS_team))

pitching_data_2020 <- read.csv("FanGraphs Leaderboard-21.csv", h = TRUE)
pitching_data_2020 <- pitching_data_2020 %>% filter(pitching_data_2020$Team != "")
team_pitching_data_2020 <- pitching_data_2020 %>% group_by(Team) %>% summarise(ERA = sum(ER)/(sum(IP)/9),num_players = n())

projection_data_2020 <- left_join(team_hitting_data_2020, team_pitching_data_2020, by = c("Team" = "Team"))


#Rank OPS/Era
projection_data_2020$OPS_Rank <- round(rank(-projection_data_2020$OPS),0)
projection_data_2020$ERA_Rank <- round(rank(projection_data_2020$ERA),0)

###2020 Projections
##Regression Predictions
projection_data_2020$Proj_Win_LM <- (predict(baseball_linear_model_1, newdata = projection_data_2020))
projection_data_2020$Proj_Win_LM <- round(projection_data_2020$Proj_Win_LM,1)/162*60
projection_data_2020$Proj_Loss_LM <- 60 - projection_data_2020$Proj_Win_LM

plot(baseball_linear_model_1)

##NN predictions
projection_data_2020_norm <- as.data.frame(lapply(projection_data_2020[2:3], normalize))
projection_data_2020_norm <- projection_data_2020_norm[c("OPS","ERA")]
baseball_model_results <- neuralnet::compute(baseball_nn_model_1, projection_data_2020_norm)
predicted_strength <- baseball_model_results$net.result

projection_data_2020$Proj_Win_NN <- baseball_model_results$net.result

projection_data_2020$Proj_Win_NN <- lapply(projection_data_2020$Proj_Win_NN,unnormalize)
projection_data_2020$Proj_Win_NN <- as.numeric(projection_data_2020$Proj_Win_NN)
projection_data_2020$Proj_Win_NN <- (round(projection_data_2020$Proj_Win_NN,1))
projection_data_2020$Proj_Win_NN <- round(projection_data_2020$Proj_Win_NN,1)/162*60
projection_data_2020$Proj_Loss_NN <- 60 - projection_data_2020$Proj_Win_NN

##Regression Trees Predictions
projection_data_2020$Proj_Win_Trees <- round((predict(baseball_trees_model_bagged_caret, newdata = projection_data_2020)))/162*60
projection_data_2020$Proj_Loss_Trees <- 60 - projection_data_2020$Proj_Win_Trees

##AVG Model Predictions
projection_data_2020$Proj_Win_AVG <- round((projection_data_2020$Proj_Win_NN + projection_data_2020$Proj_Win_Trees + projection_data_2020$Proj_Win_LM)/3,2)
projection_data_2020$Proj_Loss_AVG <- 60 - projection_data_2020$Proj_Win_AVG

projection_data_2020$AVG_Win_Percent <- projection_data_2020$Proj_Win_AVG/60

write.csv(projection_data_2020, "MLB 2020 Projected Wins RoS.csv")

projection_data_2020$Actual_Wins <- c(26,29,36,32,35,29,30,34,25,43,29,35,27,31,26,26,25,37,28,19,22,40,24,31,26,26,23,36,35,33)

par(mfrow=c(2,2))
plot(projection_data_2020$Proj_Win_AVG, projection_data_2020$Actual_Wins, 
     main="Avg Proj vs. Actual Wins - 2020 MLB Season",xlab = "Projected Wins", ylab = "Actual Wins", col=0, xlim = c(15,45), ylim = c(15,45))
abline(0,1)
text(x=projection_data_2020$Proj_Win_AVG, projection_data_2020$Actual_Wins, 
     labels=c("Angels", "Astros", "A's", "Blue Jays", "Braves", "Brewers", "Cardinals", "Cubs", "Diamondbacks", "Dodgers",
              "Giants", "Indians","Mariners", "Marlins", "Mets", "Nationals", "O's", "Padres", "Phillies","Pirates","Rangers",
              "Rays", "Red Sox", "Reds", "Rockies", "Royals","Tigers","Twins","White Sox","Yankees"), col=1)
text(20,40,"Overperform", col = "green")
text(40,20,"Underperform", col = "red")
text(41,35, labels=paste("Mean ABS Error: ",as.character(round(mean(abs(projection_data_2020$Proj_Win_AVG-projection_data_2020$Actual_Wins)),2))), col = "blue")


plot(projection_data_2020$Proj_Win_LM, projection_data_2020$Actual_Wins, 
     main="Lin Regression Proj vs. Actual Wins - 2020 MLB Season",xlab = "Projected Wins", ylab = "Actual Wins", col=0, xlim = c(15,45), ylim = c(15,45))
abline(0,1)
text(x=projection_data_2020$Proj_Win_LM, projection_data_2020$Actual_Wins, 
     labels=c("Angels", "Astros", "A's", "Blue Jays", "Braves", "Brewers", "Cardinals", "Cubs", "Diamondbacks", "Dodgers",
              "Giants", "Indians","Mariners", "Marlins", "Mets", "Nationals", "O's", "Padres", "Phillies","Pirates","Rangers",
              "Rays", "Red Sox", "Reds", "Rockies", "Royals","Tigers","Twins","White Sox","Yankees"), col=1)
text(20,40,"Overperform", col = "green")
text(40,20,"Underperform", col = "red")
text(41,35, labels=paste("Mean ABS Error: ",as.character(round(mean(abs(projection_data_2020$Proj_Win_LM-projection_data_2020$Actual_Wins)),2))), col = "blue")


plot(projection_data_2020$Proj_Win_NN, projection_data_2020$Actual_Wins, 
     main="Nueral Network Proj vs. Actual Wins - 2020 MLB Season",xlab = "Projected Wins", ylab = "Actual Wins", col=0, xlim = c(15,45), ylim = c(15,45))
abline(0,1)
text(x=projection_data_2020$Proj_Win_NN, projection_data_2020$Actual_Wins, 
     labels=c("Angels", "Astros", "A's", "Blue Jays", "Braves", "Brewers", "Cardinals", "Cubs", "Diamondbacks", "Dodgers",
              "Giants", "Indians","Mariners", "Marlins", "Mets", "Nationals", "O's", "Padres", "Phillies","Pirates","Rangers",
              "Rays", "Red Sox", "Reds", "Rockies", "Royals","Tigers","Twins","White Sox","Yankees"), col=1)
text(20,40,"Overperform", col = "green")
text(40,20,"Underperform", col = "red")
text(41,35, labels=paste("Mean ABS Error: ",as.character(round(mean(abs(projection_data_2020$Proj_Win_NN-projection_data_2020$Actual_Wins)),2))), col = "blue")

plot(projection_data_2020$Proj_Win_Trees, projection_data_2020$Actual_Wins, 
     main="Regression Trees Proj vs. Actual Wins - 2020 MLB Season",xlab = "Projected Wins", ylab = "Actual Wins", col=0, xlim = c(15,45), ylim = c(15,45))
abline(0,1)
text(x=projection_data_2020$Proj_Win_Trees, projection_data_2020$Actual_Wins, 
     labels=c("Angels", "Astros", "A's", "Blue Jays", "Braves", "Brewers", "Cardinals", "Cubs", "Diamondbacks", "Dodgers",
              "Giants", "Indians","Mariners", "Marlins", "Mets", "Nationals", "O's", "Padres", "Phillies","Pirates","Rangers",
              "Rays", "Red Sox", "Reds", "Rockies", "Royals","Tigers","Twins","White Sox","Yankees"), col=1)
text(20,40,"Overperform", col = "green")
text(40,20,"Underperform", col = "red")
text(41,35, labels=paste("Mean ABS Error: ",as.character(round(mean(abs(projection_data_2020$Proj_Win_Trees-projection_data_2020$Actual_Wins)),2))), col = "blue")



mean(abs(projection_data_2020$Proj_Win_AVG-projection_data_2020$Actual_Wins))
mean(abs(projection_data_2020$Proj_Win_LM-projection_data_2020$Actual_Wins))
mean(abs(projection_data_2020$Proj_Win_NN-projection_data_2020$Actual_Wins))
mean(abs(projection_data_2020$Proj_Win_Trees-projection_data_2020$Actual_Wins))

#team investigation
pirates_hitting_2020 <- baseball_data_2020 %>% filter(Team == "Pirates")
pirates_pitching_2020 <- pitching_data_2020 %>% filter(Team == "Pirates")

mets_hitting_2020 <- baseball_data_2020 %>% filter(Team == "Mets")
mets_pitching_2020 <- pitching_data_2020 %>% filter(Team == "Mets")

orioles_hitting_2020 <- baseball_data_2020 %>% filter(Team == "Orioles")
orioles_pitching_2020 <- pitching_data_2020 %>% filter(Team == "Orioles")
