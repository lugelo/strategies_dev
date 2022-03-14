rm(list=ls())
library(lme4)
library(DHARMa)

#' Load and tidy up data
dat <- read.csv("binomial_TP2.csv")
dat<-dat[,1:18]

head(dat)
tail(dat)
str(dat)

#' Check denominator variables
table(dat$dog_total,useNA = "ifany") # no NAs
table(dat$number_dogs_seen,useNA = "ifany") # no NAs, some 0s

#' Check vaccination cert data
unique(dat$vacc_card_status)
table(dat$vacc_card_status,dat$dogs_vacc_cert,useNA = "ifany") 
table(dat$dog_total - dat$dogs_vacc_cert)
#' several cases where vacc_card status is 0 or unknown, but non-zero numbers of
#' vaccination certificates have been recorded - recording errors? 359 NAs in
#' number with certificates, which correspond with NAs in vacc_card_status
#' Also 7 records where the number of certificates was greater than the total 
#' number of dogs - these cannot be included in the analysis
NAs <- dat[which(is.na(dat$dogs_vacc_cert)),]
head(NAs)
unique(NAs$owner_recall_vacc_no) # NAs for dogs_vacc_cert also NAs for owner_recall_vacc_no
table(NAs$Strategy,NAs$district) # NAs cover a mix of all strategies and districts so hopefully not a problem

#' Check owner recall data
unique(dat$owner_recall_vacc)
table(dat$owner_recall_vacc,dat$owner_recall_vacc_no,useNA = "ifany") 
table(dat$owner_recall_vacc,dat$owner_recall_not_vacc_no,useNA = "ifany")
table(dat$dog_total-(dat$owner_recall_not_vacc_no+ifelse(!is.na(dat$owner_recall_vacc_no),dat$owner_recall_vacc_no,0)))
#' some NAs in numbers recalled to be vaccinated where owner_recall_vacc is
#' missing or unknown - makes sense. However, these NAs are not matced by NAs in
#' owner_recall_not_vacc_no.  Also the sum of owner_recall_not_vacc_no and
#' owner_recall_vacc_no does not always equal dogs total... sometimes greater
#' other times less than
table(dat$dog_total-dat$owner_recall_vacc_no)
#' the number of dogs recalled as being vaccinated is >dog_total in 5 cases -
#' will need to drop these data from the models

#' Check microchip data
table(dat$microchip_scanned_dogs_yes,useNA = "ifany")
table(dat$microchip_scanned_dogs_no,useNA = "ifany")
table(dat$dog_total-dat$microchip_scanned_dogs_yes,useNA = "ifany")
#' most records are NA for microchip_scanned_dogs_no
#' 2 cases where recorded more microchipped dogs than dog_total - willl need to 
#' remove from analysis 


#' Check EVs
length(which(is.na(dat$household_ID)|dat$household_ID==""))
length(which(is.na(dat$district)|dat$district==""))
length(which(is.na(dat$ward)|dat$ward==""))
length(which(is.na(dat$village)|dat$village==""))
length(which(is.na(dat$sub_village)|dat$sub_village==""))
length(which(is.na(dat$Strategy)|dat$Strategy==""))
length(which(is.na(dat$campaign_type)|dat$campaign_type==""))
#' no NAs or blanks

#' Create factors
dat$household_ID <- factor(dat$household_ID)
dat$district <- factor(dat$district)
dat$ward <- factor(dat$ward)
dat$village <- factor(dat$village)
dat$sub_village <- factor(dat$sub_village)
dat$Strategy <- factor(dat$Strategy)
dat$campaign_type <- factor(dat$campaign_type)

str(dat)

#' Check design
table(dat$district)
table(dat$district, dat$ward) # data on 4 wards for each district
table(dat$district, dat$Strategy) # data on all 4 strategies for all districts
table(dat$district, dat$campaign_type) 

#' Check villages are uniquely labelled
table(rowSums(table(dat$village, dat$ward) > 0))
#' ...no re-use of village labels across wards.  

#' Check sub-villages are uniquely labelled
table(rowSums(table(dat$sub_village, dat$village) > 0)) # some subvillage numbers are repeated in different villages
rev(sort(rowSums(table(dat$sub_village, dat$village) > 0))) # subvillage numbers that are repeated in different villages include: 126, 68, 67, 44 and 28
dat$sub_village <- paste(dat$village, dat$sub_village, sep = "-")

#' Check each row is a unique HH
length(dat$household_ID)
length(unique(dat$household_ID)) # some household IDs are not unique
dat[which(dat$household_ID%in%dat$household_ID[which(duplicated(dat$household_ID))]),] 
# duplicated household IDs were generally both in the same sub_village - should check these aren't really duplicated data!
# for now will assume not duplicates and assign new individual household IDs
dat$household_ID <- 1:nrow(dat)

#' How many observations at the lowest level (subvillage)
summary(c(table(dat$sub_village)))
#' mean is 10, range is 2-20



#' Microchip model
#' ------------------------

microchip_dat <- dat[which(dat$number_dogs_seen>=dat$microchip_scanned_dogs_yes),]
model_microchip <- glmer(cbind(microchip_scanned_dogs_yes, number_dogs_seen - microchip_scanned_dogs_yes) ~ 
                           Strategy + district + 
                           (1 | ward) + (1 | village) + (1 | sub_village) + (1 | household_ID) , 
                         data = microchip_dat, family = binomial,control = glmerControl(optimizer = "bobyqa"))
# singular fit - maybe one of the RE variances being estimated as 0?
summary(model_microchip) # ward seems to be the problem...
# Refit without ward
model_microchip <- glmer(cbind(microchip_scanned_dogs_yes, number_dogs_seen - microchip_scanned_dogs_yes) ~ 
                           Strategy + district + 
                           (1 | village) + (1 | sub_village) + (1 | household_ID), 
                         data = microchip_dat, family = binomial,control = glmerControl(optimizer = "bobyqa"))
summary(model_microchip) # better

#' Check whether any variables should be dropped
drop1(model_microchip,test = "Chisq")
# keep both EVs

#' Model checking
sim_resid <- simulateResiduals(model_microchip,10000)
plot(sim_resid) # mostly fine but looks like a dispersion issue
testDispersion(sim_resid) # underdispersed
testZeroInflation(sim_resid) # no evidence of zero inflation
#' not sure whether the underdispersion is a problem - it tends to lead to false
#' negatives rather than positives, and we found an effect anyway.  It's also
#' not strong enough to be obvious from the qqplot



#' Recall model
#' ------------------------

owner_recall_dat <- dat[which(dat$dog_total>=dat$owner_recall_vacc_no),] # not sure whether cases where owner_recall is unknown/0 should be included?
model_recall <- glmer(cbind(owner_recall_vacc_no, dog_total - owner_recall_vacc_no) ~ Strategy + district + 
                        (1 | ward) + (1 | village) + (1 | sub_village) + (1 | household_ID), 
                      data = owner_recall_dat, family = binomial,control = glmerControl(optimizer = "bobyqa"))
# singularity
summary(model_recall)
# small RE variances for all except household ID. Try losing ward first
# (smallest variance)
model_recall <- glmer(cbind(owner_recall_vacc_no, dog_total - owner_recall_vacc_no) ~ 
                        Strategy + district + 
                        (1 | village) + (1 | sub_village) + (1 | household_ID), 
                      data = owner_recall_dat, family = binomial,control = glmerControl(optimizer = "bobyqa"))
summary(model_recall)
# drop sub_village
model_recall <- glmer(cbind(owner_recall_vacc_no, dog_total - owner_recall_vacc_no) ~ 
                        Strategy + district + 
                        (1 | village) + (1 | household_ID), 
                      data = owner_recall_dat, family = binomial,control = glmerControl(optimizer = "bobyqa"))
summary(model_recall)
# drop village
model_recall <- glmer(cbind(owner_recall_vacc_no, dog_total - owner_recall_vacc_no) ~ 
                        Strategy + district + 
                        (1 | household_ID), 
                      data = owner_recall_dat, family = binomial,control = glmerControl(optimizer = "bobyqa"))
summary(model_recall)

#' Check whether any variables should be dropped
drop1(model_recall,test = "Chisq")
# strategy not significant but leave in since it's what we're interested in 

#' Model checking
sim_resid <- simulateResiduals(model_recall,10000)
plot(sim_resid) # mostly fine but looks like a dispersion issue
testDispersion(sim_resid) # underdispersed
testZeroInflation(sim_resid) 
# too few zeros compared to what we would expect... should the NA records
# actually be zeros? NAs all correspond to cases where owner is recorded as
# being unable to recall whether vaccinated or not, so perhaps can assume not.
# Trying that out now - to check with Ahmed!

owner_recall_dat <- dat
owner_recall_dat$owner_recall_vacc_no[which(is.na(owner_recall_dat$owner_recall_vacc_no))]<-0 # replacing NAs with 0
owner_recall_dat <- owner_recall_dat[which(owner_recall_dat$dog_total>=owner_recall_dat$owner_recall_vacc_no),] 
model_recall <- glmer(cbind(owner_recall_vacc_no, dog_total - owner_recall_vacc_no) ~ Strategy + district + 
                        (1 | ward) + (1 | village) + (1 | sub_village) + (1 | household_ID), 
                      data = owner_recall_dat, family = binomial,control = glmerControl(optimizer = "bobyqa"))
# no singularity issues this time (ward variance is quite small though)
summary(model_recall)
# and stronger effects

#' Check whether any variables should be dropped
drop1(model_recall,test = "Chisq")
# keep both EVs

#' Model checking
sim_resid <- simulateResiduals(model_recall,10000)
plot(sim_resid) # a little patterning in the residuals, and a dispersion issue
testDispersion(sim_resid) # underdispersed - even worse than in the microchip model... and yet the qqlot doesn't look that bad... 
testZeroInflation(sim_resid) # but zeros look ok now





#' Certificates model
#' ------------------------

cert_dat <- dat[which(dat$dog_total>=dat$dogs_vacc_cert),] 
model_cert <- glmer(cbind(dogs_vacc_cert, dog_total - dogs_vacc_cert) ~ 
                      Strategy + district + 
                      (1 | ward) + (1 | village) + (1 | sub_village) + (1 | household_ID), 
                    data = cert_dat, family = binomial,control = glmerControl(optimizer = "bobyqa"))
# singularity
summary(model_cert)
# remove ward 
model_cert <- glmer(cbind(dogs_vacc_cert, dog_total - dogs_vacc_cert) ~ 
                      Strategy + district + 
                      (1 | village) + (1 | sub_village) + (1 | household_ID), 
                    data = cert_dat, family = binomial,control = glmerControl(optimizer = "bobyqa"))
summary(model_cert)
# remove sub_village
model_cert <- glmer(cbind(dogs_vacc_cert, dog_total - dogs_vacc_cert) ~ 
                      Strategy + district + 
                      (1 | village) + (1 | household_ID), 
                    data = cert_dat, family = binomial,control = glmerControl(optimizer = "bobyqa"))
summary(model_cert)
# remove village
model_cert <- glmer(cbind(dogs_vacc_cert, dog_total - dogs_vacc_cert) ~ 
                      Strategy + district + 
                      (1 | household_ID), 
                    data = cert_dat, family = binomial,control = glmerControl(optimizer = "bobyqa"))
summary(model_cert)

#' Check whether any variables should be dropped
drop1(model_cert,test = "Chisq")
# retain both EVs

#' Model checking
sim_resid <- simulateResiduals(model_cert,10000)
plot(sim_resid) # mostly fine but looks like a dispersion issue
testDispersion(sim_resid) # underdispersed
testZeroInflation(sim_resid) 
# again, too few zeros compared to what we would expect... try switching NAs for
# zeros again.

cert_dat <- dat
cert_dat$dogs_vacc_cert[which(is.na(cert_dat$dogs_vacc_cert))]<-0 # replacing NAs with 0
cert_dat <- cert_dat[which(cert_dat$dog_total>=cert_dat$dogs_vacc_cert),] 
model_cert <- glmer(cbind(dogs_vacc_cert, dog_total - dogs_vacc_cert) ~ 
                        Strategy + district + 
                        (1 | ward) + (1 | village) + (1 | sub_village) + (1 | household_ID), 
                      data = cert_dat, family = binomial,control = glmerControl(optimizer = "bobyqa"))
summary(model_cert)
# 0 variance for ward
model_cert <- glmer(cbind(dogs_vacc_cert, dog_total - dogs_vacc_cert) ~ 
                      Strategy + district + 
                      (1 | village) + (1 | sub_village) + (1 | household_ID), 
                    data = cert_dat, family = binomial,control = glmerControl(optimizer = "bobyqa"))
summary(model_cert)

#' Check whether any variables should be dropped
drop1(model_cert,test = "Chisq")
# retain both EVs

#' Model checking
sim_resid <- simulateResiduals(model_cert,10000)
plot(sim_resid) # mostly fine but looks like a dispersion issue
testDispersion(sim_resid) # underdispersed
testZeroInflation(sim_resid) # but zeros look better



#' Get predictions for each ward
#' ------------------------

#' Create data from which to make predictions
pred_data <- data.frame(district=as.factor(rep(c(0:2),each=4)),
                        Strategy=as.factor(rep(0:3,3)),
                        ward=as.factor(c(0,6,5,2,
                                         3,4,1,9,
                                         10,11,8,9)),
                        village=NA,
                        sub_village=NA,
                        household_ID=NA)


#' Microchip model
system.time({
  boot_microchip <- bootMer(model_microchip, 
                            function(x) predict(x, newdata = pred_data, re.form = NA, type = 'response'), 
                            nsim = 100,re.form=NA)
})
# slow (3.5min for me) - need to try and get parallel option working...
pred_data$microchip_mean <- predict(model_microchip,newdata=pred_data,re.form=NA,type="response")
pred_data$microchip_upper = apply(boot_microchip$t, 2, function(x) as.numeric(quantile(x, probs=.025)))
pred_data$microchip_lower = apply(boot_microchip$t, 2, function(x) as.numeric(quantile(x, probs=.975)))
pred_data$microchip_mean-apply(boot_microchip$t, 2, mean)


#' Recall model
system.time({
  boot_recall <- bootMer(model_recall, 
                         function(x) predict(x, newdata = pred_data, re.form = ~(1|ward), type = 'response'), 
                         nsim = 100,re.form=~(1|ward))
})
pred_data$recall_mean <- predict(model_recall,newdata=pred_data,re.form=NA,type="response")
pred_data$recall_upper = apply(boot_recall$t, 2, function(x) as.numeric(quantile(x, probs=.025)))
pred_data$recall_lower = apply(boot_recall$t, 2, function(x) as.numeric(quantile(x, probs=.975)))
pred_data$recall_mean-apply(boot_recall$t, 2, mean)


#' Certificate model
system.time({
  boot_cert <- bootMer(model_cert, 
                       function(x) predict(x, newdata = pred_data, re.form = NA, type = 'response'), 
                       nsim = 100,re.form=NA)
})
pred_data$cert_mean <- predict(model_cert,newdata=pred_data,re.form=NA,type="response")
pred_data$cert_upper = apply(boot_cert$t, 2, function(x) as.numeric(quantile(x, probs=.025)))
pred_data$cert_lower = apply(boot_cert$t, 2, function(x) as.numeric(quantile(x, probs=.975)))
pred_data$cert_mean-apply(boot_cert$t, 2, mean)




#' Plot
#' ------------------------

pdf("VaccinationCoverageMethods.pdf",width=6,height=5)

cols <- 1:4
par(mar=c(5,5.5,1,1))
plot(c(0,1),c(0,1),type="l",
     ylim=c(0,1),xlim=c(0,1),bty="l",cex.lab=1.1,cex.axis=0.9,
     xlab="Vaccination coverage (microchip)",
     ylab="Vaccination coverage\n(certificate/owner recall)")
points(pred_data$microchip_mean,pred_data$cert_mean,
       pch=15,col=cols)
points(pred_data$microchip_mean,pred_data$recall_mean,
       pch=17,col=cols)
arrows(pred_data$microchip_lower,pred_data$cert_mean,
       pred_data$microchip_upper,pred_data$cert_mean,
       col=cols,angle=90,code=3,length = 0.05)
arrows(pred_data$microchip_mean,pred_data$cert_lower,
       pred_data$microchip_mean,pred_data$cert_upper,
       col=cols,angle=90,code=3,length = 0.05)
arrows(pred_data$microchip_lower,pred_data$recall_mean,
       pred_data$microchip_upper,pred_data$recall_mean,
       col=cols,angle=90,code=3,length = 0.05)
arrows(pred_data$microchip_mean,pred_data$recall_lower,
       pred_data$microchip_mean,pred_data$recall_upper,
       col=cols,angle=90,code=3,length = 0.05)

legend(0.55,0.3,as.character(0:3),title="Strategy:",col=cols,pch=15,bty="n",title.adj = 0)
legend(0.75,0.3,c("certificate","owner recall"),title="Method:",pch=c(15,17),bty="n",title.adj = 0)

dev.off()


