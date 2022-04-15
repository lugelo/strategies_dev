rm(list=ls())
library(lme4)
library(lmerTest)

#' Load and tidy up data
dat <- read.csv("Time_Analysis_coded.csv")
head(dat)
tail(dat)

#' Any NAs?
sum(is.na(dat))

#' Create factors
dat$household_ID <- factor(dat$household_ID)
dat$district <- factor(dat$district)
dat$ward <- factor(dat$ward)
dat$village <- factor(dat$village)
dat$subvillage <- factor(dat$subvillage)
dat$Intervention <- factor(dat$Intervention)


#' Check design
table(dat$district)
table(dat$district, dat$ward)
table(dat$district, dat$Intervention)
table(dat$district, dat$campaign_type)

#' Check each row is a unique HH
length(dat$household_ID)
length(unique(dat$household_ID))

#' Check villages are uniquely labelled
table(rowSums(table(dat$village, dat$ward) > 0))
#' ...no re-use of village labels across wards.  

#' Check sub-villages are uniquely labelled
table(rowSums(table(dat$subvillage, dat$village) > 0))
#' ...one subvillages label occurs in two villages. Which one?
rev(sort(rowSums(table(dat$subvillage, dat$village) > 0)))
#' ... it's number 43:
dat[dat$subvillage == "43", ]

#' I'll assume this is an error and create a unique ID for subvillages
dat$subvillage <- paste(dat$village, dat$subvillage, sep = "-")

#' How many observations at the lowest level (subvillage)
summary(c(table(dat$subvillage)))
#' mean is 5, range is 1-10.  

#' Response: time taken by owner to reach the vaccination point
summary(dat$cp_travel_time_there) # no zeroes, handy for log transformation
summary(dat$cp_duration) # no zeroes, handy for log transformation
tapply(dat$cp_travel_time_there, dat$Intervention, summary)
tapply(dat$cp_duration, dat$Intervention, summary)

old.par <- par(mfrow = c(2, 4), mar = c(4.1, 4.1, 3.1, 2.1))
hist(dat$cp_travel_time_there)
hist(log(dat$cp_travel_time_there))
plot(cp_travel_time_there ~ Intervention, data = dat)
plot(cp_travel_time_there ~ Intervention, data = dat, log = "y")
hist(dat$cp_duration)
hist(log(dat$cp_duration))
boxplot(cp_duration ~ Intervention, data = dat)
boxplot(cp_duration ~ Intervention, data = dat, log = "y")
par(old.par)

#' LMM with log transformed response should work - logged 
#' times look pretty normal within intervention groups.  

#' Create a logged response 
dat$cp_duration.log <- log(dat$cp_duration)

#' Fit model  
fit.dur.full <-
  lmer(cp_duration.log ~ Intervention + district + (1 | ward) + (1 | village) + (1 | subvillage), 
       data = dat)
#' Ideally district would be a random effect but there aren't enough levels, so we'll
#' fit it as a fixed effect, and drop it if it isn't significant.  

#' Methods: Variation in duration of ... between interventions was modelled using a 
#' linear mixed model (LMM) fitted using REML, where log_e(duration) was the 
#' response, intervention and district were categorical fixed effects, 
#' and nested random intercepts were fitted at the ward, village and subvillage levels. 
#' The response was log-transformed to meet the assumptions of 
#' homoscedasticity and normality of residuals, which were checked visually by plotting 
#' the residuals against the fitted values.

plot(fit.dur.full) 
#' ...looks fine. 
#' Compare with the untransformed outcome:
plot(refit(fit.dur.full, newresp = dat$cp_duration))
#' ...severe heteroscedasticity.  

#' Model stats:
summary(fit.dur.full)
#' ...fairly substantial random effect variances at every level.  

#' Test for differences between districts and interventions
drop1(fit.dur.full)
#' District is n.s. (P = 0.099) so drop it:
fit.dur <- update(fit.dur.full, ~ . - district)
#' Test intervention
drop1(fit.dur)
#' ...no difference (P = 0.38). However I guess we should keep intervention
#' in the model because we want to estimate mean duration in each arm?  


#' Methods: Differences between districts and intervention arms in mean of 
#' log_e duration of ... were tested using an F-test, where Satterthwaite's 
#' approximation to the denominator degrees of freedom was used.  

#' Predict from model. Can do this by fitting a "no intercept" version of the model:
fit.dur.ni <- update(fit.dur, ~ . -1)
summary(fit.dur.ni)

#' Make table of predictions
pred.data <- data.frame(Intervention = paste0("Intervention", (levels(dat$Intervention))))
pred.data

#' Add logged predictions
pred.data$log.dur <- fixef(fit.dur.ni)[pred.data$Intervention]
pred.data$se <- coef(summary(fit.dur.ni))[pred.data$Intervention, "Std. Error"]
pred.data$df <- coef(summary(fit.dur.ni))[pred.data$Intervention, "df"] 
pred.data$log.ci.lower <- pred.data$log.dur - pred.data$se * qt(0.975, df = pred.data$df)
pred.data$log.ci.upper <- pred.data$log.dur + pred.data$se * qt(0.975, df = pred.data$df)
pred.data

#' Back-transform predictions.  
#' This requires correction for bias due to averaging over log-normal residual 
#' errors and random effects (Jensen's inequality).   
#' The back-transformation is exp(log.scale.mean + 0.5 * sum.of.variances)
sum.var <- sum(unlist(VarCorr(fit.dur))) + sigma(fit.dur)^2

#' So the simple method is biased:
exp(pred.data$log.dur)

#' This is unbiased:
pred.data$dur <- exp(pred.data$log.dur + 0.5 * sum.var)
pred.data$ci.lower <- exp(pred.data$log.ci.lower + 0.5 * sum.var)
pred.data$ci.upper <- exp(pred.data$log.ci.upper + 0.5 * sum.var)
pred.data

#' Methods: Wald 95% confidence intervals for mean duration within each intervention arm
#' were calculated in the lmerTest package (ref: citation("lmerTest")) 
#' using Satterthwaite's approximation to the t-distribution degrees of freedom. 
#' Back-transformation of mean duration of ... and its 95% confidence limits from the log
#' scale was adjusted for bias due to Jensen's inequality 
#' (ref: https://doi.org/10.1098/rsif.2017.0213).  


#' Out of interest, compare predicted means with raw means:
cbind(Pred = pred.data$dur, Raw = tapply(dat$cp_duration, dat$Intervention, mean))
#' ...some are quite different.  

#' Model-predicted means and 95% CIs:
pred.data[, c("Intervention", "dur", "ci.lower", "ci.upper")]
#' The 95% CIs are wide because there is substantial variation at the 
#' ward level so the effective sample size is closer to the number of wards per
#' intervention group than to the number of villages, SV, or HH

#' Create a logged response time to the central point
dat$cp_travel_time_there.log <- log(dat$cp_travel_time_there)

#' Fit model  
fit.dur.full <-
  lmer(cp_travel_time_there.log ~ Intervention + district + (1 | ward) + (1 | village) + (1 | subvillage), 
       data = dat)

plot(fit.dur.full) 
#' ...looks fine. 
#' Compare with the untransformed outcome:
plot(refit(fit.dur.full, newresp = dat$cp_travel_time_there))
#' ...severe heteroscedasticity.  

#' Model stats:
summary(fit.dur.full)
#' ...fairly substantial random effect variances at every level.  

#' Test for differences between districts and interventions
drop1(fit.dur.full)
#' District is n.s. (P = 0.099) so drop it:
fit.dur <- update(fit.dur.full, ~ . - district)
#' Test intervention
drop1(fit.dur)
#' ...no difference (P = 0.38). However I guess we should keep intervention
#' in the model because we want to estimate mean duration in each arm?  


#' Methods: Differences between districts and intervention arms in mean of 
#' log_e duration of ... were tested using an F-test, where Satterthwaite's 
#' approximation to the denominator degrees of freedom was used.  

#' Predict from model. Can do this by fitting a "no intercept" version of the model:
fit.dur.ni <- update(fit.dur, ~ . -1)
summary(fit.dur.ni)

#' Make table of predictions
pred.data <- data.frame(Intervention = paste0("Intervention", (levels(dat$Intervention))))
pred.data

#' Add logged predictions
pred.data$log.dur <- fixef(fit.dur.ni)[pred.data$Intervention]
pred.data$se <- coef(summary(fit.dur.ni))[pred.data$Intervention, "Std. Error"]
pred.data$df <- coef(summary(fit.dur.ni))[pred.data$Intervention, "df"] 
pred.data$log.ci.lower <- pred.data$log.dur - pred.data$se * qt(0.975, df = pred.data$df)
pred.data$log.ci.upper <- pred.data$log.dur + pred.data$se * qt(0.975, df = pred.data$df)
pred.data

#' Back-transform predictions.  
#' This requires correction for bias due to averaging over log-normal residual 
#' errors and random effects (Jensen's inequality).   
#' The back-transformation is exp(log.scale.mean + 0.5 * sum.of.variances)
sum.var <- sum(unlist(VarCorr(fit.dur))) + sigma(fit.dur)^2

#' So the simple method is biased:
exp(pred.data$log.dur)

#' This is unbiased:
pred.data$dur <- exp(pred.data$log.dur + 0.5 * sum.var)
pred.data$ci.lower <- exp(pred.data$log.ci.lower + 0.5 * sum.var)
pred.data$ci.upper <- exp(pred.data$log.ci.upper + 0.5 * sum.var)
pred.data
