use_varb <- (acs2017_ny$AGE >= 25) & (acs2017_ny$AGE <= 75) & (acs2017_ny$LABFORCE == 2) & (acs2017_ny$WKSWORK2 > 4) & (acs2017_ny$UHRSWORK >= 35) & (acs2017_ny$DEGFIELD == "Fine Arts")
dat_use <- subset(acs2017_ny,use_varb)

summary(dat_use)

model_temp1 <- lm(INCWAGE ~ AGE + female + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg, data = dat_use)
summary(model_temp1)

NNobs <- length(dat_use$INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
to_be_predicted2 <- data.frame(AGE = 25:75, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)


lines(yhat ~ AGE, data = to_be_predicted2)
