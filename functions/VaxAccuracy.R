rm(list = ls())

#' Load and tidy up data
dat <- read.csv("Vax Accuracy.csv")
dat$Recall <- dat$dog_vacc_status..owner.
dat$Certificate <- dat$dogs_vacc_cert..Cert.
dat$Microchip <- dat$microchip_scanned_dogs_yes.microchip.

#' Any NAs?
sum(is.na(dat))

#' Calculate sensitivity and specificity of owner recall  

#' Crosstabs:
table(Microchip = dat$Microchip, Recall = dat$Recall)
table(Microchip = dat$Microchip, Certificate = dat$Certificate)
#' ...interesting that so many have certificates but no microchip (low 
#' specificity). Are microchips not as reliable as we think or are these 
#' certificates for other dogs?  

#' Function to calculate sensitivity and specificity with 95% CIs
#' from two binary tests, one gold standard and one error-prone.
test.stats <- 
  function(gold.standard, error.prone) {
    
    # Check data are all 0 or 1 and no NAs
    stopifnot(all(c(gold.standard, error.prone) %in% 0:1))
    
    # Number of true positives
    tp <- sum(error.prone[gold.standard == 1])
    # Number of false negatives
    fn <- sum(error.prone[gold.standard == 1] == 0)
    # Number of false positives
    fp <- sum(error.prone[gold.standard == 0])
    # Number of true negatives
    tn <- sum(error.prone[gold.standard == 0] == 0)
    
    # Sensitivity and specificity with 95% CIs
    sens.stats <- binom.test(tp, tp + fn)
    spec.stats <- binom.test(tn, tn + fp)

    # Make table of results for output
    out <-
      rbind(
        Sensitivity = c(sens.stats$estimate, sens.stats$conf.int),
        Specificity = c(spec.stats$estimate, spec.stats$conf.int))
    colnames(out) <- c("Estimate", "CIlower", "CIupper")
    out
  }

test.stats(gold.standard = dat$Microchip, error.prone = dat$Recall)
test.stats(gold.standard = dat$Microchip, error.prone = dat$Certificate)
