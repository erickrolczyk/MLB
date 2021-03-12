# MLB
MLB Team Win Total Projections


Baseball is probably the easiest sport to start doing analytics with - the data is everywhere and highly predictive. Throughout this stream, I tried used 2010-2019 team data to create predictive (some ML some not) models to estimate MLB Team Season Wins based on inputs. I examined using OPS/OPS+, ERA/ERA+, and Fielding Percentage as inputs to my models. Ultimately I decided on using solely ERA and OPS given the simplicity and ease of prior data. 

I've added new work that uses data from 2000-2019 to predict Wins in the 2021 season. I used linear regression, kNN, Neural Networks (2 different types), boosting and random forest models. I look at varibale selection methods to identify the best covariates to use and even use clustering to identify similar teams over the past 2 decades.
