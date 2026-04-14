# Logistic Regression Code

# Libraries
library(tidyverse)
library(dplyr)

# read in csv (make sure working directory is the repo)
best_pic_noms <- read_csv('all_tmbd_movie_info.csv')
colnames(best_pic_noms) <- c('year', 'ceremony', 'award', 'winner', 'film_name', 'film_producers', 'revenue', 'budget', 'runtime', 'vote_average_list', 'vote_count', 'popularity', 'genre', 'imdb_rating', 'rotten_tomato_score', 'mppa_rating', 'directors')

# cleaning data (removing any movies that didn't have a budget or revenue)
# ended up removing 20 observations
cleaned_best_noms <- best_pic_noms %>% filter(revenue > 0 & budget > 0)
# do log of revenue and budget (highly skewed to the right), not linear
cleaned_best_noms$log_revenue <- log(cleaned_best_noms$revenue)
cleaned_best_noms$log_budget <- log(cleaned_best_noms$budget)
cleaned_best_noms$ROI <- cleaned_best_noms$revenue / cleaned_best_noms$budget

summary(cleaned_best_noms)

# confirm winner is a numeric
cleaned_best_noms$winner <- as.numeric(cleaned_best_noms$winner)

# Do Individual Models (only one predictor)
AIC1.1 <- matrix(rep(0,9), ncol=1)
rownames(AIC1.1) <- c("log_revenue", "log_budget", "runtime", "vote_average_list", "vote_count", "popularity", "imdb_rating", "rotten_tomato_score", "ROI")

mod1.1a <- glm(winner ~ log_revenue, family=binomial, data=cleaned_best_noms)
summary(mod1.1a)
AIC1.1[1] <- AIC(mod1.1a)
predict1.1a <- predict(mod1.1a, type="response")
oscars1.1a <- ifelse(predict1.1a >= 0.5,1,0)
table(cleaned_best_noms$winner, oscars1.1a)

mod1.1b <- glm(winner ~ log_budget, family=binomial, data=cleaned_best_noms)
summary(mod1.1b)
AIC1.1[2] <- AIC(mod1.1b)
predict1.1b <- predict(mod1.1b, type="response")
oscars1.1b <- ifelse(predict1.1b >= 0.5,1,0)
table(cleaned_best_noms$winner, oscars1.1b)

mod1.1c <- glm(winner ~ runtime, family=binomial, data=cleaned_best_noms)
summary(mod1.1c)
AIC1.1[3] <- AIC(mod1.1c)
predict1.1c <- predict(mod1.1c, type="response")
oscars1.1c <- ifelse(predict1.1c >= 0.5,1,0)
table(cleaned_best_noms$winner, oscars1.1c)

mod1.1d <- glm(winner ~ vote_average_list, family=binomial, data=cleaned_best_noms)
summary(mod1.1d)
AIC1.1[4] <- AIC(mod1.1d)
predict1.1d <- predict(mod1.1d, type="response")
oscars1.1d <- ifelse(predict1.1d >= 0.5,1,0)
table(cleaned_best_noms$winner, oscars1.1d)

mod1.1e <- glm(winner ~ vote_count, family=binomial, data=cleaned_best_noms)
summary(mod1.1e)
AIC1.1[5] <- AIC(mod1.1e)
predict1.1e <- predict(mod1.1e, type="response")
oscars1.1e <- ifelse(predict1.1e >= 0.5,1,0)
table(cleaned_best_noms$winner, oscars1.1e)

mod1.1f <- glm(winner ~ popularity, family=binomial, data=cleaned_best_noms)
summary(mod1.1f)
AIC1.1[6] <- AIC(mod1.1f)
predict1.1f <- predict(mod1.1f, type="response")
oscars1.1f <- ifelse(predict1.1f >= 0.5,1,0)
table(cleaned_best_noms$winner, oscars1.1f)

mod1.1g <- glm(winner ~ imdb_rating, family=binomial, data=cleaned_best_noms)
summary(mod1.1g)
AIC1.1[7] <- AIC(mod1.1g)
predict1.1g <- predict(mod1.1g, type="response")
oscars1.1g <- ifelse(predict1.1g >= 0.5,1,0)
table(cleaned_best_noms$winner, oscars1.1g)

mod1.1h <- glm(winner ~ rotten_tomato_score, family=binomial, data=cleaned_best_noms)
summary(mod1.1h)
AIC1.1[8] <- AIC(mod1.1h)
predict1.1h <- predict(mod1.1h, type="response")
oscars1.1h <- ifelse(predict1.1h >= 0.5,1,0)
table(cleaned_best_noms$winner, oscars1.1h)


mod1.1i <- glm(winner ~ ROI, family=binomial, data=cleaned_best_noms)
summary(mod1.1i)
AIC1.1[9] <- AIC(mod1.1i)
predict1.1i <- predict(mod1.1i, type="response")
oscars1.1i <- ifelse(predict1.1i >= 0.5,1,0)
table(cleaned_best_noms$winner, oscars1.1i)

AIC1.1

# Comparing Models
mod.full <- glm(winner ~ log_revenue + log_budget + runtime + vote_average_list + vote_count + popularity + imdb_rating + rotten_tomato_score + ROI, family=binomial, data=cleaned_best_noms)
mod.null <- glm(winner ~ 1, family=binomial, data=cleaned_best_noms) # intercept only model

library(MASS)
stepAIC(mod.full, direction="backward")
stepAIC(mod.null, direction="forward", scope=list(upper=mod.full, lower=mod.null))

# Try out the best fit on predicting (use backwards)
bestFit <- glm(winner ~ , family=binomial, data=cleaned_best_noms)
predict1.1Best <- predict(bestFit, type="response")
oscars.1Best <- ifelse(predict1.1Best >= 0.5,1,0)
table(cleaned_best_noms$winner, oscars.1Best)


# Manually
# ALL logistic regression
model_all <- glm(winner ~ log_revenue + log_budget + runtime + vote_average_list + vote_count + popularity + imdb_rating + rotten_tomato_score + ROI, data = cleaned_best_noms, family = binomial)
summary(model_all)


# Predicted Probabilities
cleaned_best_noms$pred_prob <- predict(model_all, type='response')
cleaned_best_noms$pred_class <- ifelse(cleaned_best_noms$pred_prob > 0.5, 1, 0)
# 0 = nominated, 1 = winner

# Confusion Table
table(Actual = cleaned_best_noms$winner, Predicted = cleaned_best_noms$pred_class)
# Left to Right
# True Negative: Movie did not win, model correctly predicted no win.
# False Positive: Movie lost, but model predicted winner.
# False Negative: Movie actually won, but model predicted loser.
# True Positive: Movie won, model correctly predicted winner.

# Odds Ratio
exp(coef(model_all))

# Potential Log Regression Models
# M1: Winner ~ IMDb
model1 <- glm(winner ~ imdb_rating, data = cleaned_best_noms, family = binomial)
summary(model1)
# M2: Winner ~ IMDb + RottenTomatoes
model2 <- glm(winner ~ imdb_rating + rotten_tomato_score, data = cleaned_best_noms, family = binomial)
summary(model2)
# M3: Winner ~ IMDb + RottenTomatoes + Runtime
model3 <- glm(winner ~ runtime + imdb_rating + rotten_tomato_score, data = cleaned_best_noms, family = binomial)
summary(model3)
# M4: Winner ~ IMDb + RottenTomatoes + ROI
model4 <- glm(winner ~ imdb_rating + rotten_tomato_score + ROI, data = cleaned_best_noms, family = binomial)
summary(model4)
# M5: Winner ~ log_revenue + log_budget + ROI
model5 <- glm(winner ~ log_revenue + log_budget + ROI, data = cleaned_best_noms, family = binomial)
summary(model5)
# M6: winner ~ log_revenue + log_budget + imdb + rotten_tomato_score + runtime
model6 <- glm(winner ~ log_revenue + log_budget + runtime + imdb_rating + rotten_tomato_score, data = cleaned_best_noms, family = binomial)
summary(model6)
# M7: winner ~ log_revenue + log_budget + imdb + rotten_tomato_score
model7 <- glm(winner ~ log_revenue + log_budget + imdb_rating + rotten_tomato_score, data = cleaned_best_noms, family = binomial)
summary(model7)
# M8: winner ~ ROI + imdb + rotten_tomato_score + runtime
model8 <- glm(winner ~ ROI + runtime + imdb_rating + rotten_tomato_score, data = cleaned_best_noms, family = binomial)
summary(model8)

# Comparing Models (AIC)
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model_all)