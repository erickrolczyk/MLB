setwd("~/Downloads/baseball data/core")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(randomForest)
library(pdp)
library(gbm)
library(kknn)
download.file("https://raw.githubusercontent.com/ChicagoBoothML/HelpR/master/docv.R", "docv.R")
source("docv.R")
library(neuralnet)
library(png)
library(ggimage)
library(teamcolors)
library(ggrepel)
library(rmarkdown)
library(h2o)
library(ranger)
library(xgboost)
library(bestglm)
library(glmnet)
library(modelsummary)
library(NbClust)
library(cluster)
library(factoextra)
library(ggfortify)
install.packages('bit64', repos = "https://cran.rstudio.com")




#read in data
baseball_data <- read.csv("Teams Updated.csv", h = TRUE)

#Adding Data Fields
baseball_data$BA <- baseball_data$H/baseball_data$AB
baseball_data$OBP <- (baseball_data$H + baseball_data$BB + baseball_data$HBP)/(baseball_data$AB + baseball_data$BB + baseball_data$HBP + baseball_data$SF)
baseball_data$X1B <- baseball_data$H-baseball_data$X2B-baseball_data$X3B-baseball_data$HR
baseball_data$SLG <- ((baseball_data$X1B*1) + (baseball_data$X2B*2) + (baseball_data$X3B*3) + (baseball_data$HR*4))/(baseball_data$AB)
baseball_data$OPS <- (baseball_data$OBP + baseball_data$SLG)
baseball_data$wOBA <- (((.7*baseball_data$BB*.975)+(.73*baseball_data$HBP)+(.89*baseball_data$X1B)+(1.25*baseball_data$X2B)+(1.58*baseball_data$X3B)+(2.02*baseball_data$HR))/(baseball_data$AB+baseball_data$SF+(.975*baseball_data$BB)+baseball_data$HBP))
baseball_data$FIP <- (((baseball_data$HRA*13)+((baseball_data$BBA*1.1)*3)-(baseball_data$SOA*2))/(baseball_data$IPouts/3)) + 3
baseball_data$WHIP <- (baseball_data$HA + baseball_data$BBA)/(baseball_data$IPouts/3)
baseball_data$team_yr <- paste(baseball_data$franchID, baseball_data$yearID, sep = "-")
baseball_data$division_league <- paste(baseball_data$lgID, baseball_data$divID, sep = "-")

#Getting Prior Year Wins
baseball_data$W_LY <- c(0)
numcol <- ncol(baseball_data)
for(i in 1:nrow(baseball_data)){
    temp_year_id <- baseball_data[i,1]
    temp_franch_id <- baseball_data[i,4]
       for(x in 1:i){
                   if(baseball_data[x,4] == temp_franch_id & baseball_data[x,1] == (temp_year_id - 1)){
                     baseball_data[i,numcol] <- baseball_data[x,9]/baseball_data[x,7]
                     break
                   }
    }
  }

#Changing to Factors
baseball_data$division_league <- as.factor(baseball_data$division_league)
baseball_data$divID <- as.factor(baseball_data$divID)
baseball_data$lgID <- as.factor(baseball_data$lgID)
baseball_data$franchID <- as.factor(baseball_data$franchID)
baseball_data$teamID <- as.factor(baseball_data$teamID)

#Strength of Schedule
lg_strength <- baseball_data  %>% group_by(lgID, yearID) %>% summarise(lg_wins = sum(W), lg_num_teams = n(), lg_avg_W = lg_wins/lg_num_teams, lg_num_games = sum(W) + sum(L), lg_win_pct = lg_wins/lg_num_games)
div_strength <- baseball_data %>% group_by(division_league,lgID, yearID) %>% summarise(div_wins = sum(W), div_num_teams = n(), div_avg_W = div_wins/div_num_teams, div_num_games = sum(W) + sum(L), div_win_pct = div_wins/div_num_games)
div_strength_2 <- div_strength  %>% group_by(division_league) %>% summarise(div_avg_wins = mean(div_avg_W))
baseball_data$yearID_temp = baseball_data$yearID - 1
baseball_data <- baseball_data %>% left_join(lg_strength, by = c("lgID" = "lgID", "yearID_temp" = "yearID"))
baseball_data <- baseball_data %>% left_join(div_strength, by = c("division_league" = "division_league", "yearID_temp" = "yearID"))
baseball_data <- subset(baseball_data, select = -c(yearID_temp, lgID.y))
names(baseball_data)[2] <- "lgID"
baseball_data$lg_avg_wins_LY <- (baseball_data$lg_wins - baseball_data$W)/(baseball_data$lg_num_teams-1)
baseball_data$lg_win_pct_LY <- (baseball_data$lg_wins - baseball_data$W)/(baseball_data$lg_num_games - baseball_data$G)
baseball_data$div_avg_wins_LY <- (baseball_data$div_wins - baseball_data$W)/(baseball_data$div_num_teams-1)
baseball_data$div_win_pct_LY <- (baseball_data$div_wins - baseball_data$W)/(baseball_data$div_num_games - baseball_data$G)

#Create Subsets
baseball_data_1900 <- baseball_data %>% filter(yearID >= 1900) 
year_x = 2000
baseball_data_2000 <- baseball_data %>% filter(yearID >= year_x & yearID != 2020) 
baseball_data_2020 <- baseball_data %>% filter(yearID == 2020) 

#Exploration
ggplot(baseball_data_2000, aes(x=yearID, y = wOBA, color = W)) + geom_point()+
  labs(x = "Year",
       y = "wOBA",
       title = "wOBA Change by Year",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom")


ggplot(baseball_data_1900, aes(x=yearID, y = SLG)) + geom_point()+
  labs(x = "Year",
       y = "SLG",
       title = "Slugging % Change by Year",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom")+
  stat_summary(fun=mean, aes(group=1), geom="line", colour="red")

ggplot(baseball_data_1900, aes(x=yearID, y = SB/G)) + geom_point()+
  labs(x = "Year",
       y = "SB per Game",
       title = "Stolen Bases Change by Year",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom")+
  stat_summary(fun=mean, aes(group=1), geom="line", colour="red")

ggplot(baseball_data_1900, aes(x=yearID, y = ERA)) + geom_point()+
  labs(x = "Year",
       y = "ERA",
       title = "ERA Change by Year",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom")+
  stat_summary(fun=mean, aes(group=1), geom="line", colour="red")

ggplot(baseball_data_1900, aes(x=yearID, y = WHIP)) + geom_point()+
  labs(x = "Year",
       y = "WHIP",
       title = "WHIP Change by Year",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom")+
  stat_summary(fun=mean, aes(group=1), geom="line", colour="red")

ggplot(baseball_data_1900, aes(x=yearID, y = HR/G)) + geom_point() + 
  labs(x = "Year",
       y = "Home Runs per Game",
       title = "Home Runs Change by Year",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom")+
  stat_summary(fun=mean, aes(group=1), geom="line", colour="red")

ggplot(baseball_data_1900, aes(x=yearID, y = CG/G)) + geom_point()+
  labs(x = "Year",
       y = "Complete Game %",
       title = "Complete Games Change by Year",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom")+
  stat_summary(fun=mean, aes(group=1), geom="line", colour="red")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

ggplot(baseball_data_1900 %>% filter(W_LY > 0 & G >= 160), aes(x=W_LY, y = W/G)) + geom_point()+
  labs(x = "Win % Prior Yr",
       y = "Win % Following Yr",
       title = "Does Last Year Matter?",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom") + 
  geom_smooth(method='lm', colour = "red") +
  geom_abline(slope = 1,intercept = 0, colour = "green", linetype = "dashed")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))

ggplot(div_strength %>% filter(yearID >= 2000 & yearID < 2020), aes(fill=division_league, y=div_avg_W, x=yearID)) + 
  geom_bar(position="stack", stat="identity") + facet_grid(~ lgID) +
  labs(x = "Year",
       y = "Average Wins / Yr",
       title = "MLB Division/League Performance",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom")+
  scale_fill_discrete(name = "League-Division")+
  geom_text(aes(label=round(div_avg_W, digits = 1)), size = 3, position=position_stack(),vjust = -0.2)

ggplot(baseball_data %>% filter(yearID >= 2000 & yearID < 2020), aes(x=SO, y = W)) + geom_point()+
  labs(x = "Strike Outs (Batters)",
       y = "Wins",
       title = "Strike Outs Impact on Wins",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom") + 
  geom_smooth(method='lm', colour = "red")

ggplot(baseball_data %>% filter(yearID >= 2000 & yearID < 2020), aes(x=SOA, y = W)) + geom_point()+
  labs(x = "Strike Outs (Pitchers)",
       y = "Wins",
       title = "Strike Outs Impact on Wins",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom") + 
  geom_smooth(method='lm', colour = "red")

ggplot(baseball_data %>% filter(yearID >= 2000 & yearID < 2020), aes(x=BPF, y = OPS)) + geom_point()+
  labs(x = "Ball Park Factors (3 yrs)",
       y = "OPS",
       title = "BPF on OPS",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom") + 
  geom_smooth(method='lm', colour = "red")

ggplot(baseball_data %>% filter(yearID >= 2000 & yearID < 2020), aes(x=PPF, y = ERA)) + geom_point()+
  labs(x = "Ball Park Factors (3 yrs)",
       y = "ERA",
       title = "BPF on ERA",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom") + 
  geom_smooth(method='lm', colour = "red")


ggplot(baseball_data_2000 %>% filter(yearID >= 2000 & yearID < 2020), aes(x=lg_avg_wins_LY, y = W)) + geom_point()+
  labs(x = "Avg League Wins in Prior Year",
       y = "W",
       title = "Strength of Schedule",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom") + 
  geom_smooth(method='lm', colour = "red")


ggplot(baseball_data_2000 %>% filter(yearID >= 2000 & yearID < 2020), aes(x=lg_win_pct_LY, y = W/G)) + geom_point()+
  labs(x = "League Winning % in Prior Year",
       y = "Win % Following Year",
       title = "Strength of Schedule",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom") + 
  geom_smooth(method='lm', colour = "red")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

ggplot(baseball_data_2000 %>% filter(yearID >= 2000 & yearID < 2020), aes(x=div_avg_wins_LY, y = W)) + geom_point()+
  labs(x = "Avg Division Wins in Prior Year",
       y = "Win % Following Year",
       title = "Strength of Schedule",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom") + 
  geom_smooth(method='lm', colour = "red")

ggplot(baseball_data_2000 %>% filter(yearID >= 2000 & yearID < 2020), aes(x=div_win_pct_LY, y = W/G)) + geom_point()+
  labs(x = "Division Winning % in Prior Year",
       y = "Team Wins",
       title = "Strength of Schedule",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom") + 
  geom_smooth(method='lm', colour = "red")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

ggplot(baseball_data_2000 %>% filter(yearID >= 2000 & yearID < 2020), aes(x=wOBA, y = W)) + geom_point()+
  labs(x = "Weighted On-Base Average",
       y = "Team Wins",
       title = "wOBA on Wins",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom") + 
  geom_smooth(method='lm', colour = "red")

ggplot(baseball_data_2000 %>% filter(yearID >= 2000 & yearID < 2020), aes(x=FIP, y = W)) + geom_point()+
  labs(x = "FIP",
       y = "Team Wins",
       title = "FIP on Wins",
       subtitle = "MLB Data from Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom") + 
  geom_smooth(method='lm', colour = "red")

cor(baseball_data_2000$HR, baseball_data_2000$W)
cor(baseball_data_2000$FIP, baseball_data_2000$W)
cor(baseball_data_2000$ERA, baseball_data_2000$W)
cor(baseball_data_2000$OPS, baseball_data_2000$W)
cor(baseball_data_2000$wOBA, baseball_data_2000$W)
cor(baseball_data_2000$W_LY, baseball_data_2000$W)
cor(baseball_data_2000$div_win_pct_LY, (baseball_data_2000$W/baseball_data_2000$G))
cor(baseball_data_2000$lg_win_pct_LY, (baseball_data_2000$W/baseball_data_2000$G))

#Playoffs
division_winners <- baseball_data_2000  %>% group_by(DivWin, yearID) %>% summarise(avg_wins_DIV = mean(W)) %>% filter(DivWin == "Y")
wildcar_winners <- baseball_data_2000  %>% group_by(WCWin, yearID) %>% summarise(avg_wins_WC = mean(W)) %>% filter(WCWin == "Y")
playoffs <- baseball_data_2000  %>% group_by(DivWin,WCWin,yearID) %>% summarise(avg_wins = mean(W) )%>% filter(DivWin == "Y" | WCWin == "Y")

ggplot(data = playoffs, aes(x=yearID, y=avg_wins, color = DivWin)) +  
  geom_line() + geom_point(baseball_data_2000, mapping =  aes(x=yearID, y=W, color = DivWin), alpha = .2) +
  geom_hline(yintercept = mean(division_winners$avg_wins_DIV), linetype = "dashed", color = "red") + 
  labs(x = "Year",
       y = "Avg Wins",
       title = "Avg Wins for Division and Wild Card Winners: 2000-2019",
       subtitle = "MLB Data from Sean Lahman") + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom")+
  scale_color_manual(values=c("blue","red"), name="Legend",
                    labels=c("Non-Division Winner", "Division Winner")) +
  annotate("text", x = 2010, y = 115, label = paste("Avg Wins for Div", round(mean(baseball_data_2000$W[baseball_data_2000$DivWin =="Y"]),1),sep = " "), size = 3)+
  annotate("text", x = 2010, y = 110, label = paste("Avg Wins for WC", round(mean(baseball_data_2000$W[baseball_data_2000$WCWin =="Y"]),1),sep = " "), size = 3)+
  annotate("text", x = 2005, y = 115, label = paste("Min Wins for Div", min(baseball_data_2000$W[baseball_data_2000$DivWin =="Y"]),sep = " "), size = 3)+
  annotate("text", x = 2005, y = 110, label = paste("Min Wins for WC", min(baseball_data_2000$W[baseball_data_2000$WCWin =="Y"]),sep = " "), size = 3)+
  annotate("text", x = 2015, y = 115, label = paste("Max Wins for Div", max(baseball_data_2000$W[baseball_data_2000$DivWin =="Y"]),sep = " "), size = 3)+
  annotate("text", x = 2015, y = 110, label = paste("Max Wins for WC", max(baseball_data_2000$W[baseball_data_2000$WCWin =="Y"]),sep = " "), size = 3)


#get sample size
sample_size <- floor(.75 * nrow(baseball_data_2000))

## set the seed to make your partition reproducible
set.seed(24)
train_set<- sample(seq_len(nrow(baseball_data_2000)), size = sample_size)
baseball_train <- baseball_data_2000[train_set, ]
baseball_test <- baseball_data_2000[-train_set, ]


team_colors <- data.frame(league_pal("mlb",1))
names(team_colors)[1] <- "Primary"
team_colors$Secondary <- league_pal("mlb",2)
team_colors$Team <- c("ARI","ATL","BAL","BOS","CHC","CHW","CIN","CLE","COL","DET","HOU","KCR","ANA","LAD","FLA","MIL",
                      "MIN","NYM","NYY","OAK","PHI","PIT","SDP","SFG","SEA","STL","TBD","TEX","TOR","WSN")
baseball_test <- baseball_test %>% left_join(team_colors, by = c("franchID" = "Team"))


#Linear Regression Models
baseball_linear_model_1 <- lm(W ~ OPS + ERA + W_LY + lg_win_pct_LY, data = baseball_train)
summary(baseball_linear_model_1)

baseball_linear_model_2 <- lm(W ~ wOBA + ERA + FP + W_LY + lg_win_pct_LY, data = baseball_train)
summary(baseball_linear_model_2)

baseball_linear_model_3 <- lm(W ~ ERA + lg_win_pct_LY + WHIP + wOBA + OPS + W_LY + OBP + div_win_pct_LY, data = baseball_train)
summary(baseball_linear_model_3)

baseball_linear_model_4 <- lm(W ~ OPS + ERA + FP, data = baseball_train)
summary(baseball_linear_model_4)

lm_model_1_pred <- predict(baseball_linear_model_1, baseball_test)
lm_model_1_RMSE <- sqrt(mean((baseball_test$W - lm_model_1_pred)^2));lm_model_1_RMSE

lm_model_2_pred <- predict(baseball_linear_model_2, baseball_test)
lm_model_2_RMSE <- sqrt(mean((baseball_test$W - lm_model_2_pred)^2));lm_model_2_RMSE

lm_model_3_pred <- predict(baseball_linear_model_3, baseball_test)
lm_model_3_RMSE <- sqrt(mean((baseball_test$W - lm_model_3_pred)^2));lm_model_3_RMSE

lm_model_4_pred <- predict(baseball_linear_model_4, baseball_test)
lm_model_4_RMSE <- sqrt(mean((baseball_test$W - lm_model_4_pred)^2));lm_model_4_RMSE


baseball_test$lm_model_1_pred <- lm_model_1_pred
baseball_test$lm_model_2_pred <- lm_model_2_pred
baseball_test$lm_model_3_pred <- lm_model_3_pred
baseball_test$lm_model_4_pred <- lm_model_4_pred
ggplot(baseball_test, aes(x=lm_model_1_pred, y=W, label = team_yr)) + 
  geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Linear Model 1 (ERA+OPS+LY W+LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-lm_model_1_pred) >= 5),
                  aes(x=lm_model_1_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=lm_model_2_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Linear Model 2 (ERA+wOBA+FP+LY W+LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-lm_model_2_pred) >= 5),
                  aes(x=lm_model_2_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=lm_model_3_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Linear Model 3 (ERA+lg_win_pct_LY+WHIP+wOBA+OPS+W_LY+OBP+div_win_pct_LY)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-lm_model_3_pred) >= 5),
                  aes(x=lm_model_3_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=lm_model_4_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Linear Model 4 (ERA+OPS+FP)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-lm_model_4_pred) >= 5),
                  aes(x=lm_model_4_pred, y = W,label=team_yr), size = 2.5,vjust = -1)



###Random Forest
rf_model_1 <- ranger(
  formula = W ~ OPS + ERA + W_LY + lg_win_pct_LY, 
  data = baseball_train, num.trees = 250, mtry = 4, min.node.size = 5, seed = 99
)
rf_model_1_pred <- predict(rf_model_1, data=baseball_test)$predictions
rf_model_1_RMSE <- sqrt(mean((baseball_test$W - rf_model_1_pred)^2));rf_model_1_RMSE


rf_model_2 <- ranger(
  formula = W ~ wOBA + ERA + FP + W_LY + lg_win_pct_LY, 
  data = baseball_train, num.trees = 250, mtry = 5, min.node.size = 5, seed = 99
)
rf_model_2_pred <- predict(rf_model_2, data=baseball_test)$predictions
rf_model_2_RMSE <- sqrt(mean((baseball_test$W - rf_model_2_pred)^2));rf_model_2_RMSE


rf_model_3 <- ranger(
  formula = W ~ ERA + lg_win_pct_LY + WHIP + wOBA + OPS + W_LY + OBP + div_win_pct_LY, 
  data = baseball_train, num.trees = 250, mtry = 5, min.node.size = 5, seed = 99
)
rf_model_3_pred <- predict(rf_model_3, data=baseball_test)$predictions
rf_model_3_RMSE <- sqrt(mean((baseball_test$W - rf_model_3_pred)^2));rf_model_3_RMSE


rf_model_4 <- ranger(
  formula = W ~ OPS + ERA + FP, 
  data = baseball_train, num.trees = 250, mtry = 3, min.node.size = 5, seed = 99
)
rf_model_4_pred <- predict(rf_model_4, data=baseball_test)$predictions
rf_model_4_RMSE <- sqrt(mean((baseball_test$W - rf_model_4_pred)^2));rf_model_4_RMSE


baseball_test$rf_model_1_pred <- rf_model_1_pred
baseball_test$rf_model_2_pred <- rf_model_2_pred
baseball_test$rf_model_3_pred <- rf_model_3_pred
baseball_test$rf_model_4_pred <- rf_model_4_pred
ggplot(baseball_test, aes(x=rf_model_1_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Random Forest Model 1 (ERA+OPS+LY W+LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-rf_model_1_pred) >= 5),
                  aes(x=rf_model_1_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=rf_model_2_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Random Forest Model 2 (ERA+wOBA+FP+LY W+LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-rf_model_2_pred) >= 5),
                  aes(x=rf_model_2_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=rf_model_3_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Random Forest Model 3 (ERA+lg_win_pct_LY+WHIP+wOBA+OPS+W_LY+OBP+div_win_pct_LY)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-rf_model_3_pred) >= 5),
                  aes(x=rf_model_3_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=rf_model_4_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Random Forest Model 4 (ERA+OPS+FP)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-rf_model_4_pred) >= 5),
                  aes(x=rf_model_4_pred, y = W,label=team_yr), size = 2.5,vjust = -1)


#### kNN
mmsc=function(x) {return((x-min(x))/(max(x)-min(x)))}
x = cbind("ERA" = baseball_data_2000$ERA, "WHIP" = baseball_data_2000$WHIP, "OPS" = baseball_data_2000$OPS, 
          "FP" = baseball_data_2000$FP, "W_LY" = baseball_data_2000$W_LY, "lg_win_pct_LY" = baseball_data_2000$lg_win_pct_LY, 
          "wOBA" = baseball_data_2000$wOBA, "OBP" = baseball_data_2000$OBP, "div_win_pct_LY" = baseball_data_2000$div_win_pct_LY)
y = cbind("W" = baseball_data_2000$W)
y_vector = baseball_data_2000$W
xs = apply(x,2,mmsc)
ys = apply(y,2,mmsc)

x_train <- xs[train_set, ]
x_test <- xs[-train_set, ]
y_train <- y[train_set, ]
y_test <- y[-train_set, ]

##Find which k is best for kNN
par(mfrow=c(1,1))
kv = 2:20 
n = length(y_vector)
cvmean = rep(0,length(kv)) #will keep average rmse here
ndocv = 10 #number of CV splits to try
cvmat = matrix(0,length(kv),ndocv) #keep results for each split
for(i in 1:ndocv) {
  cvtemp = docvknn(xs,y_vector,kv,nfold=10)
  cvmean = cvmean + cvtemp
  cvmat[,i] = sqrt(cvtemp/n)
}
cvmean = cvmean/ndocv
cvmean = sqrt(cvmean/n)
plot(kv,cvmean,type="n",ylim=range(cvmat),xlab="k",cex.lab=1.5)
for(i in 1:ndocv) lines(kv,cvmat[,i],col=i,lty=3) #plot each result
lines(kv,cvmean,type="b",col="black",lwd=5)
abline(h=min(cvmean))


k_opt = 14
knn_train = data.frame("W" = y_train,x_train)
knn_test  = data.frame("W" = y_test,x_test)

knn_model_1 = kknn(W ~ OPS + ERA + W_LY + lg_win_pct_LY,knn_train,knn_test,k=k_opt,kernel = "triangular")
knn_model_1_RMSE <- sqrt(mean((baseball_test$W - knn_model_1$fitted)^2));knn_model_1_RMSE

knn_model_2 = kknn(W ~ wOBA + ERA + FP + W_LY + lg_win_pct_LY, knn_train, knn_test,k=k_opt,kernel = "triangular")
knn_model_2_RMSE <- sqrt(mean((baseball_test$W - knn_model_2$fitted)^2));knn_model_2_RMSE

knn_model_3 = kknn(W ~ ERA + lg_win_pct_LY + WHIP + wOBA + OPS + W_LY + OBP + div_win_pct_LY, 
                   knn_train, knn_test,k=k_opt,kernel = "triangular")
knn_model_3_RMSE <- sqrt(mean((baseball_test$W - knn_model_3$fitted)^2));knn_model_3_RMSE

knn_model_4 = kknn(W ~ OPS + ERA + FP ,knn_train,knn_test,k=k_opt,kernel = "triangular")
knn_model_4_RMSE <- sqrt(mean((baseball_test$W - knn_model_4$fitted)^2));knn_model_4_RMSE


baseball_test$knn_model_1_pred <- knn_model_1$fitted
baseball_test$knn_model_2_pred <- knn_model_2$fitted
baseball_test$knn_model_3_pred <- knn_model_3$fitted
baseball_test$knn_model_4_pred <- knn_model_4$fitted
ggplot(baseball_test, aes(x=knn_model_1_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "kNN Model 1 (ERA+OPS+LY W+LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-knn_model_1_pred) >= 5),
                  aes(x=knn_model_1_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=knn_model_2_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "kNN Model 2 (ERA+wOBA+FP+LY W+LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-knn_model_2_pred) >= 5),
                  aes(x=knn_model_2_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=knn_model_3_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "kNN Model 3 (ERA+lg_win_pct_LY+WHIP+wOBA+OPS+W_LY+OBP+div_win_pct_LY)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-knn_model_3_pred) >= 5),
                  aes(x=knn_model_3_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=knn_model_4_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "kNN Model 4 (ERA+OPS+FP)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-knn_model_4_pred) >= 5),
                  aes(x=knn_model_4_pred, y = W,label=team_yr), size = 2.5,vjust = -1)


#Boosted Model
MAX_TREES=7500
boost_model_1 = gbm(W ~ ERA + OPS + W_LY + lg_win_pct_LY, distribution = "gaussian", data=baseball_train,
                    n.trees=MAX_TREES, interaction.depth = 2, shrinkage = 0.0025)
boost_model_1_pred <- predict(boost_model_1, newdata = baseball_test, n.trees = MAX_TREES)
boost_model_1_RMSE <- sqrt(mean((baseball_test$W - boost_model_1_pred)^2));boost_model_1_RMSE

boost_model_2 = gbm(W ~ ERA + wOBA + FP + W_LY + lg_win_pct_LY, distribution = "gaussian", data=baseball_train,
                    n.trees=MAX_TREES, interaction.depth = 2, shrinkage = 0.0025)
boost_model_2_pred <- predict(boost_model_2, newdata = baseball_test, n.trees = MAX_TREES)
boost_model_2_RMSE <- sqrt(mean((baseball_test$W - boost_model_2_pred)^2));boost_model_2_RMSE

boost_model_3 = gbm(W ~ ERA + lg_win_pct_LY + WHIP + wOBA + OPS + W_LY + OBP + div_win_pct_LY, 
                    distribution = "gaussian", data=baseball_train, n.trees=MAX_TREES, interaction.depth = 2, shrinkage = 0.0025)
boost_model_3_pred <- predict(boost_model_3, newdata = baseball_test, n.trees = MAX_TREES)
boost_model_3_RMSE <- sqrt(mean((baseball_test$W - boost_model_3_pred)^2));boost_model_3_RMSE

boost_model_4 = gbm(W ~ ERA + OPS + FP, distribution = "gaussian", data=baseball_train,
                    n.trees=MAX_TREES, interaction.depth = 2, shrinkage = 0.0025)
boost_model_4_pred <- predict(boost_model_4, newdata = baseball_test, n.trees = MAX_TREES)
boost_model_4_RMSE <- sqrt(mean((baseball_test$W - boost_model_4_pred)^2));boost_model_4_RMSE

baseball_test$boost_model_1_pred <- boost_model_1_pred
baseball_test$boost_model_2_pred <- boost_model_2_pred
baseball_test$boost_model_3_pred <- boost_model_3_pred
baseball_test$boost_model_4_pred <- boost_model_4_pred
ggplot(baseball_test, aes(x=boost_model_1_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Boosted Model 1 (ERA+OPS+LY W+LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-boost_model_1_pred) >= 5),
                  aes(x=boost_model_1_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=boost_model_2_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Boosted Model 2 (ERA+wOBA+FP+LY W+LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-boost_model_2_pred) >= 5),
                  aes(x=boost_model_2_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=boost_model_3_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Boosted Model 3 (ERA+lg_win_pct_LY+WHIP+wOBA+OPS+W_LY+OBP+div_win_pct_LY)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-boost_model_3_pred) >= 5),
                  aes(x=boost_model_3_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=boost_model_4_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Boosted Model 4 (ERA+OPS+FP)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-boost_model_4_pred) >= 5),
                  aes(x=boost_model_4_pred, y = W,label=team_yr), size = 2.5,vjust = -1)



#Neural Nets
##Data Transformation
max_W <- max(baseball_data_2000$W)
min_W <- min(baseball_data_2000$W)
un_mmsc=function(x) {return((max_W-min_W)*x+min_W)}
ys_train <- cbind("W" = ys[train_set, ])
ys_test <- cbind("W"= ys[-train_set, ])
nn_train = data.frame(ys_train,x_train)
nn_test  = data.frame(ys_test,x_test)

nn_model_1 <- neuralnet(W ~ OPS + ERA + W_LY + lg_win_pct_LY, data = nn_train, hidden = c(10,10))
nn_model_1_pred <- predict(nn_model_1, newdata = nn_test)
nn_model_1_pred <- apply(nn_model_1_pred,2,un_mmsc)
nn_model_1_RMSE <- sqrt(mean((baseball_test$W - nn_model_1_pred)^2));nn_model_1_RMSE

nn_model_2 <- neuralnet(W ~ wOBA + ERA + FP + W_LY + lg_win_pct_LY, data = nn_train, hidden = c(10,10))
nn_model_2_pred <- predict(nn_model_2, newdata = nn_test)
nn_model_2_pred <- apply(nn_model_2_pred,2,un_mmsc)
nn_model_2_RMSE <- sqrt(mean((baseball_test$W - nn_model_2_pred)^2));nn_model_2_RMSE

nn_model_3 <- neuralnet(W ~ ERA + lg_win_pct_LY + WHIP + wOBA + OPS + W_LY + OBP + div_win_pct_LY, data = nn_train, hidden = c(10,10))
nn_model_3_pred <- predict(nn_model_3, newdata = nn_test)
nn_model_3_pred <- apply(nn_model_3_pred,2,un_mmsc)
nn_model_3_RMSE <- sqrt(mean((baseball_test$W - nn_model_3_pred)^2));nn_model_3_RMSE

nn_model_4 <- neuralnet(W ~ OPS + ERA + FP, data = nn_train, hidden = c(10,10))
nn_model_4_pred <- predict(nn_model_4, newdata = nn_test)
nn_model_4_pred <- apply(nn_model_4_pred,2,un_mmsc)
nn_model_4_RMSE <- sqrt(mean((baseball_test$W - nn_model_4_pred)^2));nn_model_4_RMSE


baseball_test$nn_model_1_pred <- nn_model_1_pred
baseball_test$nn_model_2_pred <- nn_model_2_pred
baseball_test$nn_model_3_pred <- nn_model_3_pred
baseball_test$nn_model_4_pred <- nn_model_4_pred
ggplot(baseball_test, aes(x=nn_model_1_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "NN Model 1 (ERA+OPS+LY W+LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-nn_model_1_pred) >= 5),
                  aes(x=nn_model_1_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=nn_model_2_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "NN Model 2 (ERA+wOBA+FP+LY W+LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-nn_model_2_pred) >= 5),
                  aes(x=nn_model_2_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=nn_model_3_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "NN Model 3 (ERA+lg_win_pct_LY+WHIP+wOBA+OPS+W_LY+OBP+div_win_pct_LY)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-nn_model_3_pred) >= 5),
                  aes(x=nn_model_3_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=nn_model_4_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "NN Model 4 (ERA+OPS+FP)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-nn_model_4_pred) >= 5),
                  aes(x=nn_model_4_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

#######h2o Deep Learning NN
h2o.shutdown()
baseball_train_h2o_temp <- baseball_data_2000[train_set, ]
baseball_test_h2o_temp <- baseball_data_2000[-train_set, ]
h2o.init(nthreads=-1, max_mem_size="2G")
h2o.no_progress()


baseball_train_h2o <- as.h2o(baseball_train_h2o_temp)
baseball_test_h2o <- as.h2o(baseball_test_h2o_temp)
response <- "W"

hyper_params <- list(
  hidden=list(c(32,32,32),c(64,64)),
  input_dropout_ratio=c(0,0.05),
  l1=c(0,1e-5,1e-3),
  l2=c(0,1e-5,1e-3),
  max_w2=c(5,10)
)


predictors_1 <- c("OPS", "ERA",  "W_LY", "lg_win_pct_LY")
grid_1 <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid_1",
  training_frame=baseball_train_h2o,
  validation_frame=baseball_test_h2o,
  x=predictors_1,
  y=response,
  epochs=10,
  stopping_metric="RMSE",
  stopping_tolerance=1e-2,
  stopping_rounds=2,
  score_duty_cycle=0.025,
  hyper_params=hyper_params
)

grid_1 <- h2o.getGrid("dl_grid_1", sort_by="RMSE", decreasing=FALSE)
grid_1@summary_table[1,]
best_model_1 <- h2o.getModel(grid_1@model_ids[[1]])
pred_best_1 <- as.data.frame(h2o.predict(best_model_1, baseball_test_h2o))
baseball_test <- cbind(baseball_test, pred_best_1)
names(baseball_test)[ncol(baseball_test)] <- "h2o_model_1_pred"
h2o_model_1_RMSE <- sqrt(mean((baseball_test$W - baseball_test$h2o_model_1_pred)^2));h2o_model_1_RMSE


predictors_2 <- c("wOBA", "ERA", "FP", "W_LY", "lg_win_pct_LY")
grid_2 <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid_2",
  training_frame=baseball_train_h2o,
  validation_frame=baseball_test_h2o,
  x=predictors_2,
  y=response,
  epochs=10,
  stopping_metric="RMSE",
  stopping_tolerance=1e-2,
  stopping_rounds=2,
  score_duty_cycle=0.025,
  hyper_params=hyper_params
)

grid_2 <- h2o.getGrid("dl_grid_2", sort_by="RMSE", decreasing=FALSE)
grid_2@summary_table[1,]
best_model_2 <- h2o.getModel(grid_2@model_ids[[1]])
pred_best_2 <- as.data.frame(h2o.predict(best_model_2, baseball_test_h2o))
baseball_test <- cbind(baseball_test, pred_best_2)
names(baseball_test)[ncol(baseball_test)] <- "h2o_model_2_pred"
h2o_model_2_RMSE <- sqrt(mean((baseball_test$W - baseball_test$h2o_model_2_pred)^2));h2o_model_2_RMSE


predictors_3 <- c("ERA", "lg_win_pct_LY", "WHIP", "wOBA", "OPS", "W_LY", "OBP", "div_win_pct_LY")
grid_3 <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid_3",
  training_frame=baseball_train_h2o,
  validation_frame=baseball_test_h2o,
  x=predictors_3,
  y=response,
  epochs=10,
  stopping_metric="RMSE",
  stopping_tolerance=1e-2,
  stopping_rounds=2,
  score_duty_cycle=0.025,
  hyper_params=hyper_params
)

grid_3 <- h2o.getGrid("dl_grid_3", sort_by="RMSE", decreasing=FALSE)
grid_3@summary_table[1,]
best_model_3 <- h2o.getModel(grid_3@model_ids[[1]])
pred_best_3 <- as.data.frame(h2o.predict(best_model_3, baseball_test_h2o))
baseball_test <- cbind(baseball_test, pred_best_3)
names(baseball_test)[ncol(baseball_test)] <- "h2o_model_3_pred"
h2o_model_3_RMSE <- sqrt(mean((baseball_test$W - baseball_test$h2o_model_3_pred)^2));h2o_model_3_RMSE


predictors_4 <- c("OPS", "ERA", "FP")
grid_4 <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid_4",
  training_frame=baseball_train_h2o,
  validation_frame=baseball_test_h2o,
  x=predictors_4,
  y=response,
  epochs=10,
  stopping_metric="RMSE",
  stopping_tolerance=1e-2,
  stopping_rounds=2,
  score_duty_cycle=0.025,
  hyper_params=hyper_params
)

grid_4 <- h2o.getGrid("dl_grid_4", sort_by="RMSE", decreasing=FALSE)
grid_4@summary_table[1,]
best_model_4 <- h2o.getModel(grid_4@model_ids[[1]])
pred_best_4 <- as.data.frame(h2o.predict(best_model_4, baseball_test_h2o))
baseball_test <- cbind(baseball_test, pred_best_4)
names(baseball_test)[ncol(baseball_test)] <- "h2o_model_4_pred"
h2o_model_4_RMSE <- sqrt(mean((baseball_test$W - baseball_test$h2o_model_4_pred)^2));h2o_model_4_RMSE


ggplot(baseball_test, aes(x=h2o_model_1_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "h2o NN Model 1 (OPS + ERA + LY W +LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-h2o_model_1_pred) >= 5),
                  aes(x=h2o_model_1_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=h2o_model_2_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "h2o NN Model 2 (ERA + wOBA + FP + LY W + LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-h2o_model_2_pred) >= 5),
                  aes(x=h2o_model_2_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=h2o_model_3_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "h2o NN Model 3 (ERA+lg_win_pct_LY+WHIP+wOBA+OPS+W_LY+OBP+div_win_pct_LY)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-h2o_model_3_pred) >= 5),
                  aes(x=h2o_model_3_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=h2o_model_4_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "h2o NN Model 4 (ERA + OPS + FP)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-h2o_model_4_pred) >= 5),
                  aes(x=h2o_model_4_pred, y = W,label=team_yr), size = 2.5,vjust = -1)
#########


##AVG Model
baseball_test$avg_model_1_pred <- (baseball_test$lm_model_1_pred + 
                                     baseball_test$rf_model_1_pred + 
                                     baseball_test$knn_model_1_pred + 
                                     baseball_test$boost_model_1_pred + 
                                     baseball_test$nn_model_1_pred+
                                     baseball_test$h2o_model_1_pred)/6
avg_model_1_RMSE <- sqrt(mean((baseball_test$W - baseball_test$avg_model_1_pred )^2));avg_model_1_RMSE

baseball_test$avg_model_2_pred <- (baseball_test$lm_model_2_pred + 
                                     baseball_test$rf_model_2_pred + 
                                     baseball_test$knn_model_2_pred + 
                                     baseball_test$boost_model_2_pred + 
                                     baseball_test$nn_model_2_pred+
                                     baseball_test$h2o_model_2_pred)/6
avg_model_2_RMSE <- sqrt(mean((baseball_test$W - baseball_test$avg_model_2_pred )^2));avg_model_2_RMSE

baseball_test$avg_model_3_pred <- (baseball_test$lm_model_3_pred + 
                                     baseball_test$rf_model_3_pred + 
                                     baseball_test$knn_model_3_pred + 
                                     baseball_test$boost_model_3_pred + 
                                     baseball_test$nn_model_3_pred+
                                     baseball_test$h2o_model_3_pred)/6
avg_model_3_RMSE <- sqrt(mean((baseball_test$W - baseball_test$avg_model_3_pred )^2));avg_model_3_RMSE

baseball_test$avg_model_4_pred <- (baseball_test$lm_model_4_pred + 
                                     baseball_test$rf_model_4_pred + 
                                     baseball_test$knn_model_4_pred + 
                                     baseball_test$boost_model_4_pred + 
                                     baseball_test$nn_model_4_pred+
                                     baseball_test$h2o_model_4_pred)/6
avg_model_4_RMSE <- sqrt(mean((baseball_test$W - baseball_test$avg_model_4_pred )^2));avg_model_4_RMSE

baseball_test$avg_avg_model <- (baseball_test$avg_model_1_pred + 
                                  baseball_test$avg_model_2_pred +
                                  baseball_test$avg_model_4_pred +
                                  baseball_test$avg_model_4_pred)/4
avg_avg_model_RMSE <- sqrt(mean((baseball_test$W - baseball_test$avg_avg_model )^2));avg_avg_model_RMSE



ggplot(baseball_test, aes(x=avg_model_1_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Average Model 1 (ERA+OPS+LY W+LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7)) + 
        geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-avg_model_1_pred) >= 5),
                  aes(x=avg_model_1_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=avg_model_2_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Average Model 2 (ERA+wOBA+FP+LY W+LY Lg W%)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7)) + 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-avg_model_2_pred) >= 5),
                  aes(x=avg_model_2_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=avg_model_3_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Average Model 3 (ERA+lg_win_pct_LY+WHIP+wOBA+OPS+W_LY+OBP+div_win_pct_LY)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7)) + 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-avg_model_3_pred) >= 5),
                  aes(x=avg_model_3_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=avg_model_4_pred, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Average Model 4  (ERA+OPS+FP)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom")+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-avg_model_4_pred) >= 5),
                  aes(x=avg_model_4_pred, y = W,label=team_yr), size = 2.5,vjust = -1)

ggplot(baseball_test, aes(x=avg_avg_model, y=W, label = team_yr)) + geom_point(shape=21, fill = baseball_test$Primary, color = baseball_test$Secondary)+
  labs(x = "Model Wins Prediction",
       y = "Actual Wins",
       title = "Model Performance",
       subtitle = "Average (of every model) Model")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7),
        legend.position = "bottom")+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_test, abs(W-avg_avg_model) >= 5),
                  aes(x=avg_avg_model, y = W,label=team_yr), size = 2.5,vjust = -1)


#####Next Year Projections

batter_projections <- read.csv("FanGraphs 2021 - Batters - 3-10.csv", h = TRUE)
pitcher_projections <- read.csv("FanGraphs 2021 - Pitchers - 3-10.csv", h = TRUE)

batter_team_projections <- batter_projections  %>% group_by(Team) %>% summarise(PA = sum(PA), AB = sum(AB), H = sum(H), X1B = (H - sum(X2B)- sum(X3B)- sum(HR)), X2B = sum(X2B), X3B = sum(X3B), HR = sum(HR), BB = sum(BB), HBP = sum(HBP), num_player = n())
batter_team_projections$wOBA <- ((.7*batter_team_projections$BB*.975)+(.73*batter_team_projections$HBP)+(.89*batter_team_projections$X1B)+(1.25*batter_team_projections$X2B)+(1.58*batter_team_projections$X3B)+(2.02*batter_team_projections$HR))/(batter_team_projections$PA - (batter_team_projections$BB*.025))
batter_team_projections$SF <- (batter_team_projections$PA - batter_team_projections$AB - batter_team_projections$BB - batter_team_projections$HBP)
batter_team_projections$OBP <- (batter_team_projections$H + batter_team_projections$BB + batter_team_projections$HBP)/(batter_team_projections$AB + batter_team_projections$BB + batter_team_projections$HBP + batter_team_projections$SF)
batter_team_projections$SLG <- ((batter_team_projections$X1B*1) + (batter_team_projections$X2B*2) + (batter_team_projections$X3B*3) + (batter_team_projections$HR*4))/(batter_team_projections$AB)
batter_team_projections$OPS <- (batter_team_projections$OBP + batter_team_projections$SLG)
pitcher_team_projections <- pitcher_projections  %>% group_by(Team) %>% summarise(IP = sum(IP), ER = sum(ER), ERA = 9*ER/IP, HRA = sum(HR), BBA = sum(BB), SOA = sum(SO), HA = sum(H), WHIP = (HA+BBA)/IP, num_player = n())
pitcher_team_projections$FIP <- (((pitcher_team_projections$HRA*13)+((pitcher_team_projections$BBA*1.1)*3)-(pitcher_team_projections$SOA*2))/(pitcher_team_projections$IP)) + 3


team_projections <- batter_team_projections %>% left_join(pitcher_team_projections, by = c("Team" = "Team"))
year_projection = 2021
team_projections$year <- year_projection
team_projections$yearID_temp <- team_projections$year - 1
team_projections$Team <- c("FA" = "", "Diamondbacks" ="ARI","Braves" ="ATL", "Orioles" ="BAL","Red Sox" ="BOS","Cubs" ="CHC",
                           "White Sox" ="CHW","Reds" ="CIN","Indians" ="CLE","Rockies" ="COL","Tigers" ="DET","Astros" ="HOU",
                           "Royals" ="KCR", "Angels" = "ANA","Dodgers" ="LAD", "Marlins" ="FLA","Brewers" ="MIL","Twins" ="MIN",
                           "Mets" ="NYM", "Yankees" ="NYY", "Athletics" ="OAK", "Phillies" ="PHI", "Pirates" ="PIT","Padres" ="SDP",
                           "Mariners" ="SEA", "Giants" ="SFG","Cardinals" ="STL","Rays" ="TBD", "Rangers" ="TEX","Blue Jays" ="TOR",
                           "Nationals" ="WSN")
temp_data <- baseball_data %>% select(franchID, yearID, FP, G, W, L, lgID, divID, division_league) %>% filter(yearID == year_projection-1)
temp_data$W_LY <-temp_data$W/temp_data$G
lg_strength_proj <- temp_data  %>% group_by(lgID, yearID) %>% summarise(lg_wins = sum(W), lg_num_teams = n(), lg_avg_W = lg_wins/lg_num_teams, lg_num_games = sum(W) + sum(L), lg_win_pct = lg_wins/lg_num_games)
div_strength_proj <- temp_data %>% group_by(division_league, yearID) %>% summarise(div_wins = sum(W), div_num_teams = n(), div_avg_W = div_wins/div_num_teams, div_num_games = sum(W) + sum(L), div_win_pct = div_wins/div_num_games)
temp_data <- temp_data %>% left_join(lg_strength_proj, by = c("lgID" = "lgID", "yearID" = "yearID"))
temp_data <- temp_data %>% left_join(div_strength_proj, by = c("division_league" = "division_league", "yearID" = "yearID"))
temp_data$lg_avg_wins_LY <- (temp_data$lg_wins - temp_data$W)/(temp_data$lg_num_teams-1)
temp_data$lg_win_pct_LY <- (temp_data$lg_wins - temp_data$W)/(temp_data$lg_num_games - temp_data$G)
temp_data$div_avg_wins_LY <- (temp_data$div_wins - temp_data$W)/(temp_data$div_num_teams-1)
temp_data$div_win_pct_LY <- (temp_data$div_wins - temp_data$W)/(temp_data$div_num_games - temp_data$G)
temp_data_2 <- temp_data %>% select(franchID, yearID, FP, W_LY, lg_win_pct_LY, div_win_pct_LY,lgID, divID, division_league)
team_projections <- team_projections %>% left_join(temp_data_2, by = c("Team" = "franchID", "yearID_temp" = "yearID"))
team_projections <- subset(team_projections, select = -c(yearID_temp, num_player.x, num_player.y))
team_projections <- team_projections %>% filter(Team != "")
team_projections$W <- c(81)
team_proj_temp <- team_projections

##Linear Model
baseball_linear_model_1_proj <- lm(W ~ OPS + ERA + W_LY + lg_win_pct_LY, data = baseball_data_2000)
team_projections$lm_model_1_proj <- predict(baseball_linear_model_1_proj, team_projections)

baseball_linear_model_2_proj <- lm(W ~ wOBA + ERA + FP + W_LY + lg_win_pct_LY, data = baseball_data_2000)
team_projections$lm_model_2_proj <- predict(baseball_linear_model_2_proj, team_projections)

baseball_linear_model_3_proj <- lm(W ~ ERA + lg_win_pct_LY + WHIP + wOBA + OPS + W_LY + OBP + div_win_pct_LY, data = baseball_data_2000)
team_projections$lm_model_3_proj <- predict(baseball_linear_model_3_proj, team_projections)

baseball_linear_model_4_proj <- lm(W ~ OPS + ERA + FP, data = baseball_data_2000)
team_projections$lm_model_4_proj <- predict(baseball_linear_model_4_proj, team_projections)
##

##Random Forest Model
rf_model_1_proj <- rf_model_1 <- ranger(
                      formula = W ~ OPS + ERA + W_LY + lg_win_pct_LY, 
                      data = baseball_data_2000, num.trees = 250, mtry = 4, min.node.size = 5, seed = 99
                    )
team_projections$rf_model_1_proj <- predict(rf_model_1_proj, data = team_projections)$predictions

rf_model_2_proj <- ranger(
                      formula = W ~ wOBA + ERA + FP + W_LY + lg_win_pct_LY, 
                      data = baseball_data_2000, num.trees = 250, mtry = 5, min.node.size = 5, seed = 99
                    )
team_projections$rf_model_2_proj <- predict(rf_model_2_proj, data = team_projections)$predictions

rf_model_3_proj <- ranger(
  formula = W ~ ERA + lg_win_pct_LY + WHIP + wOBA + OPS + W_LY + OBP + div_win_pct_LY, 
  data = baseball_data_2000, num.trees = 250, mtry = 5, min.node.size = 5, seed = 99
)
team_projections$rf_model_3_proj <- predict(rf_model_3_proj, data = team_projections)$predictions

rf_model_4_proj <- ranger(
                      formula = W ~ OPS + ERA + FP, 
                      data = baseball_data_2000, num.trees = 250, mtry = 3, min.node.size = 5, seed = 99
                    )
team_projections$rf_model_4_proj <- predict(rf_model_4_proj, data = team_projections)$predictions
##

##Boost Model
boost_model_1_proj = gbm(W ~ OPS + ERA + W_LY + lg_win_pct_LY, distribution = "gaussian", data=baseball_data_2000,
                         n.trees=MAX_TREES, interaction.depth = 1, shrinkage = 0.0025)
team_projections$boost_model_1_proj <- predict(boost_model_1_proj, newdata = team_projections, n.trees = MAX_TREES)

boost_model_2_proj = gbm(W ~ wOBA + ERA + FP + W_LY + lg_win_pct_LY, distribution = "gaussian", data=baseball_data_2000,
                    n.trees=MAX_TREES, interaction.depth = 1, shrinkage = 0.0025)
team_projections$boost_model_2_proj <- predict(boost_model_2_proj, newdata = team_projections, n.trees = MAX_TREES)

boost_model_3_proj = gbm(W ~ ERA + lg_win_pct_LY + WHIP + wOBA + OPS + W_LY + OBP + div_win_pct_LY, 
                         distribution = "gaussian", data=baseball_data_2000, n.trees=MAX_TREES, 
                         interaction.depth = 1, shrinkage = 0.0025)
team_projections$boost_model_3_proj <- predict(boost_model_3_proj, newdata = team_projections, n.trees = MAX_TREES)

boost_model_4_proj = gbm(W ~ OPS + ERA + FP, distribution = "gaussian", data=baseball_data_2000,
                         n.trees=MAX_TREES, interaction.depth = 1, shrinkage = 0.0025)
team_projections$boost_model_4_proj <- predict(boost_model_4_proj, newdata = team_projections, n.trees = MAX_TREES)
##

##kNN Model
temp_baseball_data <- subset(baseball_data_2000, select = c(franchID, W, ERA, OPS, FP, W_LY,lg_win_pct_LY, wOBA, OBP, div_win_pct_LY, WHIP))
temp_team_projections <- subset(team_projections, select = c(Team, W, ERA, OPS, FP, W_LY,lg_win_pct_LY, wOBA, OBP, div_win_pct_LY, WHIP))
names(temp_team_projections)[1]<-"franchID"
temp_team_projections <- rbind(temp_baseball_data, temp_team_projections)


x_proj = cbind("ERA" = temp_team_projections$ERA, "OPS" = temp_team_projections$OPS, "FP" = temp_team_projections$FP, 
               "W_LY" = temp_team_projections$W_LY, "lg_win_pct_LY" = temp_team_projections$lg_win_pct_LY, 
               "wOBA" = temp_team_projections$wOBA, "OBP" = temp_team_projections$OBP, 
               "div_win_pct_LY" = temp_team_projections$div_win_pct_LY, "WHIP" = temp_team_projections$WHIP)
y_proj = cbind("W" = temp_team_projections$W)
y_vector_proj = temp_team_projections$W
x_train_proj_scale = apply(x_proj,2,mmsc)
y_train_proj_scale = apply(y_proj,2,mmsc)

num_rows <- nrow(baseball_data_2000)
num_rows_2 <- nrow(temp_team_projections)
x_train_proj <- x_train_proj_scale[1:num_rows, ]
x_test_proj <- x_train_proj_scale[(num_rows+1):num_rows_2, ]
y_train_proj <- y_proj[1:num_rows, ]
y_test_proj <- y_proj[(num_rows+1):num_rows_2, ]


knn_train_proj = data.frame("W" = y_train_proj,x_train_proj)
knn_test_proj  = data.frame(x_test_proj)

knn_model_1_proj = kknn(W ~ OPS + ERA + W_LY + lg_win_pct_LY, knn_train_proj, knn_test_proj,k=k_opt,kernel = "triangular")
team_projections$knn_model_1_proj <- knn_model_1_proj$fitted

knn_model_2_proj = kknn(W ~ wOBA + ERA + FP + W_LY + lg_win_pct_LY, knn_train_proj, knn_test_proj,k=k_opt,kernel = "triangular")
team_projections$knn_model_2_proj <- knn_model_2_proj$fitted

knn_model_3_proj = kknn(W ~ ERA + lg_win_pct_LY + WHIP + wOBA + OPS + W_LY + OBP + div_win_pct_LY, 
                        knn_train_proj, knn_test_proj,k=k_opt,kernel = "triangular")
team_projections$knn_model_3_proj <- knn_model_3_proj$fitted

knn_model_4_proj = kknn(W ~ OPS + ERA + FP, knn_train_proj, knn_test_proj,k=k_opt,kernel = "triangular")
team_projections$knn_model_4_proj <- knn_model_4_proj$fitted
####

#NN Model
ys_train_proj <- cbind("W" = y_train_proj_scale[1:num_rows, ])
ys_test_proj <- cbind("W"= y_train_proj_scale[(num_rows+1):num_rows_2, ])
nn_train_proj = data.frame(ys_train_proj,x_train_proj)
nn_test_proj  = data.frame(ys_test_proj,x_test_proj)

nn_model_1_proj <- neuralnet(W ~ OPS + ERA + W_LY + lg_win_pct_LY, data = nn_train_proj, hidden = c(10,10))
nn_model_1_pred_proj <- predict(nn_model_1_proj, newdata = nn_test_proj)
nn_model_1_pred_proj <- apply(nn_model_1_pred_proj,2,un_mmsc)
team_projections$nn_model_1_proj <- nn_model_1_pred_proj

nn_model_2_proj <- neuralnet(W ~ wOBA + ERA + FP + W_LY + lg_win_pct_LY, data = nn_train_proj, hidden = c(10,10))
nn_model_2_pred_proj <- predict(nn_model_2_proj, newdata = nn_test_proj)
nn_model_2_pred_proj <- apply(nn_model_2_pred_proj,2,un_mmsc)
team_projections$nn_model_2_proj <- nn_model_2_pred_proj

nn_model_3_proj <- neuralnet(W ~ ERA + lg_win_pct_LY + WHIP + wOBA + OPS + W_LY + OBP + div_win_pct_LY, 
                             data = nn_train_proj, hidden = c(10,10))
nn_model_3_pred_proj <- predict(nn_model_3_proj, newdata = nn_test_proj)
nn_model_3_pred_proj <- apply(nn_model_3_pred_proj,2,un_mmsc)
team_projections$nn_model_3_proj <- nn_model_3_pred_proj

nn_model_4_proj <- neuralnet(W ~ OPS + ERA + FP, data = nn_train_proj, hidden = c(10,10))
nn_model_4_pred_proj <- predict(nn_model_4_proj, newdata = nn_test_proj)
nn_model_4_pred_proj <- apply(nn_model_4_pred_proj,2,un_mmsc)
team_projections$nn_model_4_proj <- nn_model_4_pred_proj
##

#####h2o Deep Learning NN
h2o.shutdown()
baseball_train_h2o_temp <- baseball_data_2000
h2o.init(nthreads=-1, max_mem_size="2G")
h2o.no_progress()

baseball_train_h2o <- as.h2o(baseball_train_h2o_temp)
team_projection_h2o <- as.h2o(team_proj_temp[,-1])
response <- "W"

#Tuning Params
hyper_params <- list(
  hidden=list(c(32,32,32),c(64,64)),
  input_dropout_ratio=c(0,0.05),
  l1=c(0,1e-5,1e-3),
  l2=c(0,1e-5,1e-3),
  max_w2=c(5,10)
)
#

grid_1 <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid_1",
  training_frame=baseball_train_h2o,
  x=predictors_1,
  y=response,
  epochs=10,
  stopping_metric="RMSE",
  stopping_tolerance=1e-2,
  stopping_rounds=2,
  score_duty_cycle=0.025,
  hyper_params=hyper_params
)

grid_1 <- h2o.getGrid("dl_grid_1", sort_by="RMSE", decreasing=FALSE)
grid_1@summary_table[1,]
best_model_1 <- h2o.getModel(grid_1@model_ids[[1]])
pred_best_1 <- as.data.frame(h2o.predict(best_model_1, team_projection_h2o))
team_projections <- cbind(team_projections, pred_best_1)
names(team_projections)[ncol(team_projections)] <- "h2o_model_1_proj"


grid_2 <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid_2",
  training_frame=baseball_train_h2o,
  x=predictors_2,
  y=response,
  epochs=10,
  stopping_metric="RMSE",
  stopping_tolerance=1e-2,
  stopping_rounds=2,
  score_duty_cycle=0.025,
  hyper_params=hyper_params
)

grid_2 <- h2o.getGrid("dl_grid_2", sort_by="RMSE", decreasing=FALSE)
grid_2@summary_table[1,]
best_model_2 <- h2o.getModel(grid_2@model_ids[[1]])
pred_best_2 <- as.data.frame(h2o.predict(best_model_2, team_projection_h2o))
team_projections <- cbind(team_projections, pred_best_2)
names(team_projections)[ncol(team_projections)] <- "h2o_model_2_proj"


grid_3 <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid_3",
  training_frame=baseball_train_h2o,
  x=predictors_3,
  y=response,
  epochs=10,
  stopping_metric="RMSE",
  stopping_tolerance=1e-2,
  stopping_rounds=2,
  score_duty_cycle=0.025,
  hyper_params=hyper_params
)

grid_3 <- h2o.getGrid("dl_grid_3", sort_by="RMSE", decreasing=FALSE)
grid_3@summary_table[1,]
best_model_3 <- h2o.getModel(grid_3@model_ids[[1]])
pred_best_3 <- as.data.frame(h2o.predict(best_model_3, team_projection_h2o))
team_projections <- cbind(team_projections, pred_best_3)
names(team_projections)[ncol(team_projections)] <- "h2o_model_3_proj"


grid_4 <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid_4",
  training_frame=baseball_train_h2o,
  x=predictors_4,
  y=response,
  epochs=10,
  stopping_metric="RMSE",
  stopping_tolerance=1e-2,
  stopping_rounds=2,
  score_duty_cycle=0.025,
  hyper_params=hyper_params
)

grid_4 <- h2o.getGrid("dl_grid_4", sort_by="RMSE", decreasing=FALSE)
grid_4@summary_table[1,]
best_model_4 <- h2o.getModel(grid_4@model_ids[[1]])
pred_best_4 <- as.data.frame(h2o.predict(best_model_4, team_projection_h2o))
team_projections <- cbind(team_projections, pred_best_4)
names(team_projections)[ncol(team_projections)] <- "h2o_model_4_proj"
###########


##Avg Model
team_projections$avg_model_1_proj <- (team_projections$lm_model_1_proj+
                                      team_projections$boost_model_1_proj+
                                      team_projections$rf_model_1_proj+
                                      team_projections$knn_model_1_proj+
                                      team_projections$nn_model_1_proj+
                                      team_projections$h2o_model_1_proj)/6

team_projections$avg_model_2_proj <- (team_projections$lm_model_2_proj+
                                      team_projections$boost_model_2_proj+
                                      team_projections$rf_model_2_proj+
                                      team_projections$knn_model_2_proj+
                                      team_projections$nn_model_2_proj+ 
                                      team_projections$h2o_model_2_proj)/6

team_projections$avg_model_3_proj <- (team_projections$lm_model_3_proj+
                                      team_projections$boost_model_3_proj+
                                      team_projections$rf_model_3_proj+
                                      team_projections$knn_model_3_proj+
                                      team_projections$nn_model_3_proj+
                                      team_projections$h2o_model_3_proj)/6

team_projections$avg_model_4_proj <- (team_projections$lm_model_4_proj+
                                      team_projections$boost_model_4_proj+
                                      team_projections$rf_model_4_proj+
                                      team_projections$knn_model_4_proj+
                                      team_projections$nn_model_4_proj+ 
                                      team_projections$h2o_model_4_proj)/6

team_projections$avg_avg_proj <- (team_projections$avg_model_1_proj+
                                  team_projections$avg_model_2_proj+
                                  team_projections$avg_model_3_proj+
                                  team_projections$avg_model_4_proj)/4
##

##Visualizing Projected Wins
team_colors <- data.frame(league_pal("mlb",1))
names(team_colors)[1] <- "Primary"
team_colors$Secondary <- league_pal("mlb",2)
team_colors$Team <- c("ARI","ATL","BAL","BOS","CHC","CHW","CIN","CLE","COL","DET","HOU","KCR","ANA","LAD","FLA","MIL",
                      "MIN","NYM","NYY","OAK","PHI","PIT","SDP","SFG","SEA","STL","TBD","TEX","TOR","WSN")
team_projections <- team_projections %>% left_join(team_colors, by = c("Team" = "Team"))
team_logos <- data.frame(teamcolors %>%
  filter(league == "mlb") %>%
  pull(logo))
names(team_logos)[1] <- "Logo"
team_logos$Team <- c("ARI","ATL","BAL","BOS","CHC","CHW","CIN","CLE","COL","DET","HOU","KCR","ANA","LAD","FLA","MIL",
                      "MIN","NYM","NYY","OAK","PHI","PIT","SDP","SFG","SEA","STL","TBD","TEX","TOR","WSN")
team_projections <- team_projections %>% left_join(team_logos, by = c("Team" = "Team"))


g1 <- ggplot(team_projections, aes(x = Team, y = avg_model_2_proj)) + 
  geom_bar(stat="identity", fill = team_projections$Primary, color = team_projections$Secondary) + #facet_grid(vars(lgID), scales = "free")+
  geom_image(image = team_projections$Logo, y = team_projections$avg_model_2_proj+6, asp = 2.5, size=.03) +
  theme_light() +
  labs(x = "Team",
       y = "2021 Projected Wins\n 6 Model Avg",
       title = "2021 MLB Team Projections",
       subtitle = "Underlying data from Fangraphs + Sean Lahman")+
  geom_text(aes(label=round(avg_model_2_proj, digits = 0)), position=position_stack(),vjust = -0.25,fontface = "bold") + ylim(0,105)

g2 <- ggplot(team_projections, aes(x = Team, y = avg_model_2_proj)) + 
  geom_bar(stat="identity", fill = team_projections$Primary, color = team_projections$Secondary) + #facet_grid(vars(lgID), scales = "free")+
  geom_image(image = team_projections$Logo, y = team_projections$avg_model_2_proj+6, asp = 2.5, size=.03) +
  theme_light() +
  labs(x = "Team",
    y = "2021 Projected Wins\n 6 Model Avg",
    title = "2021 MLB Team Projections",
    subtitle = "Underlying data from Fangraphs + Sean Lahman")+
  geom_text(aes(label=round(avg_model_2_proj, digits = 0)), position=position_stack(),vjust = -0.25,fontface = "bold") + ylim(0,105)

g3 <- ggplot(team_projections, aes(x = Team, y = avg_model_3_proj)) + 
  geom_bar(stat="identity", fill = team_projections$Primary, color = team_projections$Secondary) + #facet_grid(vars(lgID), scales = "free")+
  geom_image(image = team_projections$Logo, y = team_projections$avg_model_3_proj+6, asp = 2.5, size=.03) +
  theme_light() +
  labs(x = "Team",
       y = "2021 Projected Wins\n 6 Model Avg",
       title = "2021 MLB Team Projections",
       subtitle = "Underlying data from Fangraphs + Sean Lahman")+
  geom_text(aes(label=round(avg_model_3_proj, digits = 0)), position=position_stack(),vjust = -0.25,fontface = "bold") + ylim(0,105)

g4 <- ggplot(team_projections, aes(x = Team, y = avg_model_4_proj)) + 
  geom_bar(stat="identity", fill = team_projections$Primary, color = team_projections$Secondary) + #facet_grid(vars(lgID), scales = "free")+
  geom_image(image = team_projections$Logo, y = team_projections$avg_model_4_proj+6, asp = 2.5, size=.03) +
  theme_light() +
  labs(x = "Team",
       y = "2021 Projected Wins\n 6 Model Avg",
       title = "2021 MLB Team Projections",
       subtitle = "Underlying data from Fangraphs + Sean Lahman")+
  geom_text(aes(label=round(avg_model_4_proj, digits = 0)), position=position_stack(),vjust = -0.25,fontface = "bold") + ylim(0,105)

g_avg <- ggplot(team_projections, aes(x = Team, y = avg_avg_proj)) + 
  geom_bar(stat="identity", fill = team_projections$Primary, color = team_projections$Secondary) + #facet_grid(vars(lgID), scales = "free")+
  geom_image(image = team_projections$Logo, y = team_projections$avg_avg_proj+6, asp = 2.5, size=.03) +
  theme_light() +
  labs(x = "Team",
       y = "2021 Projected Wins",
       title = "2021 MLB Team Projections",
       subtitle = "24 Model Average",
       caption = "Underlying data from Fangraphs + Sean Lahman")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8))+
  geom_text(aes(label=round(avg_avg_proj, digits = 1)), position=position_stack(),vjust = -0.25,fontface = "bold") + ylim(0,107)


gridExtra::grid.arrange(g1, g2, g4, ncol = 3)



  #https://github.com/beanumber/teamcolors
#https://stackoverflow.com/questions/60764518/inserting-an-image-to-a-bar-chart-in-ggplot





#####Variable Importance
temp_test <- subset(baseball_train, select = -c(yearID, lgID, teamID,franchID,divID,Rank,G,Ghome,L,DivWin,WCWin, LgWin, WSWin, name,
                                                attendance, teamIDBR,teamIDlahman45, teamIDretro,team_yr, division_league,lg_wins, 
                                                lg_num_teams, lg_avg_W, lg_num_games,lg_win_pct,div_wins,div_num_teams,div_avg_W, 
                                                div_num_games, div_win_pct, park, R, RA, AB, H, SB, CS, HBP, SF, ER, CG, SHO, SV, 
                                                IPouts, HA, HRA, BBA, E, DP, BA,lg_avg_wins_LY, div_avg_wins_LY, X1B, X2B, X3B))
x <- as.matrix(temp_test) 
y = baseball_train$W
lasso.fit = glmnet(x, y)
plot(lasso.fit, xvar = "lambda")
plot(lasso.fit, xvar = "norm")


lasso.cv = cv.glmnet(x, y)
plot(lasso.cv, sign.lambda = -1)

glmnet.fit <- lasso.cv$glmnet.fit
plot(glmnet.fit, xvar = "lambda")
abline(v = log(lasso.cv$lambda.min), lty = 2, col = "red")
abline(v = log(lasso.cv$lambda.1se), lty = 2, col = "green")
legend("topright", legend = c("min", "1se"), lty = 2, col = c("red","green"))
coef(lasso.cv, s = c(lasso.cv$lambda.min, lasso.cv$lambda.1se))
log(lasso.cv$lambda.min);log(lasso.cv$lambda.1se)


gbm.stump = gbm(W ~ ., data = temp_test, distribution = "gaussian", 
                interaction.depth = 1, n.trees = 5000, shrinkage = 0.01, 
                cv.folds = 5, n.cores = 1, verbose = FALSE)
par(mar = c(4, 10, 1, 1))
summary(gbm.stump, las = 2)
###########


####CLUSTERING
baseball_data_2000 <- baseball_data_2000 %>% left_join(team_colors, by = c("franchID" = "Team"))

cluster_reduced_1 <- subset(baseball_data_2000, select = c(team_yr, W, Primary, Secondary, ERA, WHIP, OBP, wOBA, FP, div_win_pct_LY, lg_win_pct_LY))

team_projections$team_yr <- paste(team_projections$Team, team_projections$year, sep = "-")
cluster_reduced_2 <- subset(team_projections, select = c(team_yr, avg_avg_proj, Primary, Secondary, ERA, WHIP, OBP, wOBA, FP, div_win_pct_LY, lg_win_pct_LY))
names(cluster_reduced_2)[2] = "W"

cluster_reduced_3 <- rbind(cluster_reduced_1,cluster_reduced_2)
cluster_reduced_reduced <- subset(cluster_reduced_3, select = c(ERA, WHIP, OBP, wOBA, FP, div_win_pct_LY, lg_win_pct_LY))

pca <- prcomp(cluster_reduced_reduced, scale=TRUE);pca
screeplot(pca, type="lines")
abline(h=1, lty=2)

cluster_reduced_scaled <- cluster_reduced_reduced %>% scale()

f1 <- fviz_nbclust(cluster_reduced_scaled, kmeans, method = "wss") #+ geom_vline(xintercept = 6, linetype = 2)
f2 <- fviz_nbclust(cluster_reduced_scaled, kmeans, method = "silhouette")
gridExtra::grid.arrange(f1, f2, ncol = 2)

cl <- kmeans(cluster_reduced_scaled, centers = 100, nstart = 1000, iter.max = 500, set.seed(24))
cl$centers
cl$size
clusters <- aggregate(cluster_reduced_reduced, by=list(cl$cluster), FUN=mean)
clusters
baseball_cluster <- data.frame(cluster_reduced_3, cluster = factor(cl$cluster))

autoplot(pca, data = baseball_cluster, 
         colour = 'cluster', 
         loadings = TRUE, 
         loadings.colour = 'blue',
         loadings.label = TRUE, 
         loadings.label.size = 3)

clust_num_PIT <- baseball_cluster$cluster[baseball_cluster$team_yr == "PIT-2021"]
ggplot(baseball_cluster, aes(x=ERA, y=wOBA)) + geom_point(shape=21, fill = baseball_cluster$Primary, color = baseball_cluster$Secondary, aes(size = W))+
  labs(x = "Team ERA",
       y = "Team wOBA",
       title = "Cluster Analysis",
       subtitle = "Clustering Based on 7 Most Important Predictors | Wins for 2021 Teams are Projected | Teams Labeled are in 2021 Pirates Cluster")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_cluster, cluster == clust_num_PIT),
                  aes(x=ERA, y = wOBA, label=paste(team_yr, "W:", round(W,0), sep = " ")), size = 2.5,vjust = -1, color = "black", fontface = "bold")

clust_num_LAD <- baseball_cluster$cluster[baseball_cluster$team_yr == "LAD-2021"]
ggplot(baseball_cluster, aes(x=ERA, y=wOBA)) + geom_point(shape=21, fill = baseball_cluster$Primary, color = baseball_cluster$Secondary, aes(size = W))+
  labs(x = "Team ERA",
       y = "Team wOBA",
       title = "Cluster Analysis",
       subtitle = "Clustering Based on 7 Most Important Predictors | Wins for 2021 Teams are Projected | Teams Labeled are in 2021 Dodgers Cluster")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 7))+ 
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data=subset(baseball_cluster, cluster == clust_num_LAD),
                  aes(x=ERA, y = wOBA, label=paste(team_yr, "W:", round(W,0), sep = " ")), size = 2.5,vjust = -1, color = "black", fontface = "bold")
##############


##http://rstudio-pubs-static.s3.amazonaws.com/53162_cd16ee63c24747459ccd180f69f07810.html
