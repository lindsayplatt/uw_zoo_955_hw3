library(readxl)
library(tidyverse)
library(lubridate)



# reading in the data
data = read_xlsx("BSB_tagging_data.xlsx")

# removing NA values from the data
data <- data %>% filter(!is.na(Date_at_recapture)) %>% 
  filter(!is.na(Date_at_capture)) %>% 
  filter(!is.na(Length_at_capture))

data <- data %>% mutate(month = month(Date_at_recapture))

# filter for data taken after July
lateSeason <- data %>% 
  filter(month > 7) %>% 
  # make a column of true/false which says whether a female change sex
  filter(Sex_at_capture == "F") %>%
  mutate(changed = Sex_at_recapture != "F")

#shape 1 = k+1, number of successes (9)
k = sum(lateSeason$changed)
n = nrow(lateSeason)

shape1 = k +1

#shape 2 = n-k+1, where n is total number of observations
shape2 = n-k+1

# make a framework to plot the density against
quantFish = seq(0, 1, 0.01)

# calculating the density, mainly based on n and k
densityFish = dbeta(quantFish, shape1 , shape2)

##### Question 1.1 #####

# plot the density function
plot(quantFish, densityFish)
     
##### Question 1.2 #####

# give the 95% confidence interval
ci_probs <- c(0.025,0.975)
qbeta(ci_probs, shape1, shape2)
# 95% confident that the proportion of female sea bass
# that changed sex at the end of the spawning season
# is between 0.1728 and 0.4940

##### Question 1.3a #####

# Q1.3a: Does the length of a female influence its probability of sex 
# change given that it was recaptured after the end of the spawning season?  
# Give a p value to support your answer.  
glm_length_mod <- glm(changed ~ Length_at_capture, binomial(link = "logit"), data = lateSeason)
glm_length_mod_sum <- summary(glm_length_mod)
coefficients(glm_length_mod_sum)
# No, p value of 0.11

##### Question 1.3b #####

# Q1.3b: By how much is the log odds of sex change predicted to change 
# for every millimeter increase in length?

# Assuming the the `Length_at_capture` values were given in `mm` in the
# data, the coefficient associated with length would give the log odds
# of sex change per mm increase in length.
coefficients(glm_length_mod_sum)['Length_at_capture', 'Estimate']
# 0.045 increase in log odds per mm

##### Question 1.4 #####

# Q1.4: Plot the relationship between the probability of sex change for 
# these individuals and length.  Overlay the model estimated relationship 
# on the data. Label axes appropriately and provide a figure caption

# Plot the regression line by constructing fitted values for the range of lengths
length_range <- range(lateSeason$Length_at_capture)
length_seq <- seq(length_range[1], length_range[2], by=1)
prob_changed_seq <- predict.glm(glm_length_mod, list(Length_at_capture = length_seq), type="response")

par(mar = c(6.5,4,4,1))
plot(as.numeric(changed) ~ Length_at_capture, lateSeason,
     xlim = length_range, ylim = c(0,1),
     xlab = "Length at capture (mm)",
     ylab = "Probability of female sex change",
     main = "Probability of female black sea bass\nsex change vs length at capture")
mtext(side=1, line=5.5, cex=0.75, adj=0.5,
      'Figure 1. Relationship between female black sea bass sex change\nat the end of the spawning season and their length at capture')
lines(length_seq, prob_changed_seq)

##### Question 1.5 #####

# Q1.5: Provide a 95% prediction interval for the probability of sex change 
# for a female of length 300 mm.
prob_changed_300 <- predict.glm(glm_length_mod, list(Length_at_capture = 300), 
                                type="link", se.fit = TRUE)

ilink <- glm_length_mod$family$linkinv
p300 <- ilink(prob_changed_300$fit)
p300_lower <- ilink(prob_changed_300$fit - (2 * prob_changed_300$se.fit))
p300_upper <- ilink(prob_changed_300$fit + (2 * prob_changed_300$se.fit))
sprintf('We are 95%% confident that a female of length 300m will have between a %s and %s probability of a sex change.',
        round(p300_lower, digits=3), round(p300_upper, digits=3))

##### Question 1.6 #####

#Q1.6: Are there any problems with applying logistic regression 
# to the data from this study?

# Checking some assumptions:
# No multicollinearity because we only have one independent variable
# This is definitely binary data for the dependent variable
# Independent variables are linearly related to the log odds (see below)
logodds_changed_seq <- predict.glm(glm_length_mod, list(Length_at_capture = length_seq),type="link")
plot(length_seq, logodds_changed_seq,
     xlab = "Length at capture (mm)",
     ylab = "Log odds of female sex change")

# Logistic regression seems like an appropriate model to apply to this dataset, 
# even though the effect of length at capture on female black seas bass sex
# change was not statistically significant (p = 0.11, so p > 0.05).
