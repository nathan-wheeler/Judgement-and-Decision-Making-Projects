library(rstan)
rstan_options(auto_write = TRUE)
library(tidyverse)
library(loo)
library(brms)
library(car)
library(CCA)
library(gtools)

#Load data
d <- read.csv("Variation_in_Bias_Cleaned.csv")

#Clean data
d <- d %>% mutate(
  Subject = subject %>% as.factor,
  QExtOpt_Confirmation_Bias = QExtOpt_Confirmation_Bias - QReal_Confirmation_Bias,
  QExtPess_Confirmation_Bias = QExtPess_Confirmation_Bias - QReal_Confirmation_Bias,
  QModOpt_Confirmation_Bias = QModOpt_Confirmation_Bias - QReal_Confirmation_Bias,
  QModPess_Confirmation_Bias = QModPess_Confirmation_Bias - QReal_Confirmation_Bias,
  QExtOpt_Rational = QExtOpt_Rational - QReal_Rational,
  QExtPess_Rational = QExtPess_Rational - QReal_Rational,
  QModOpt_Rational = QModOpt_Rational - QReal_Rational,
  QModPess_Rational = QModPess_Rational - QReal_Rational,
  QExtOpt_Heuristic = QExtOpt_Heuristic - QReal_Heuristic,
  QExtPess_Heuristic = QExtPess_Heuristic - QReal_Heuristic,
  QModOpt_Heuristic = QModOpt_Heuristic - QReal_Heuristic,
  QModPess_Heuristic = QModPess_Heuristic - QReal_Heuristic,
  QExtOpt_Accuracy = QExtOpt_Accuracy - QReal_Accuracy,
  QExtPess_Accuracy = QExtPess_Accuracy - QReal_Accuracy,
  QModOpt_Accuracy = QModOpt_Accuracy - QReal_Accuracy,
  QModPess_Accuracy = QModPess_Accuracy - QReal_Accuracy,
  
  QExtOpt_Is_Absent = ifelse(is.na(QExtOpt_Confirmation_Bias),1,0),
  QModOpt_Is_Absent = ifelse(is.na(QModOpt_Confirmation_Bias),1,0),
  QModPess_Is_Absent = ifelse(is.na(QModPess_Confirmation_Bias),1,0),
  QExtPess_Is_Absent = ifelse(is.na(QExtPess_Confirmation_Bias),1,0),
  
  Reward_Condition = Reward_Condition  %>% factor(levels=c("Extremely Negative; ExtOpt, Real, ExtPess",
                                                           "Neutral; ExtOpt, Real, ExtPess",
                                                           "Extremely Positive; ExtOpt, Real, ExtPess" ,
                                                           "Moderately Negative; Real, ModPess, ExtPess",
                                                           "Neutral; Real, ModPess, ExtPess",
                                                           "Neutral; ExtOpt, ModOpt, Real",
                                                           "Moderately Positive; ExtOpt, ModOpt, Real")),
  CueChoice = ifelse(cuechoice ==1,"ExtOpt",
                     ifelse(cuechoice ==2, "ModOpt",
                            ifelse(cuechoice ==3,"Real",
                                   ifelse(cuechoice ==4, "ModPess",
                                          "ExtPess")))) %>% factor(levels=c("ExtOpt","ModOpt","Real","ModPess","ExtPess")),
  Follow_Advice = ifelse(observe == exploitchoice,1,0),
  Proportion_Real = ifelse(cuechoice == 3,1,0)
)

d[is.na(d)] <- 0


#Fit Simultaneous Regression
Simultaneous_Regression_Bayesian <- brm(data = d, 
                   family = categorical(link = logit, refcat =  3),
                   bf(cuechoice ~ 1,
                      nlf(mu1 ~ AExtOpt*(1-QExtOpt_Is_Absent) + bEVRational * QExtOpt_Rational + bEVHeuristic * QExtOpt_Heuristic + bEVConfirmationBias * QExtOpt_Confirmation_Bias  + bEVAccuracy * QExtOpt_Accuracy + -10000*QExtOpt_Is_Absent),
                      nlf(mu2 ~ AModOpt*(1-QModOpt_Is_Absent) + bEVRational * QModOpt_Rational + bEVHeuristic * QModOpt_Heuristic + bEVConfirmationBias * QModOpt_Confirmation_Bias  + bEVAccuracy * QModOpt_Accuracy + -10000*QModOpt_Is_Absent),
                      nlf(mu4 ~ AModPess*(1-QModPess_Is_Absent) + bEVRational * QModPess_Rational + bEVHeuristic * QModPess_Heuristic + bEVConfirmationBias * QModPess_Confirmation_Bias  + bEVAccuracy * QModPess_Accuracy + -10000*QModPess_Is_Absent),
                      nlf(mu5 ~ AExtPess*(1-QExtPess_Is_Absent) + bEVRational * QExtPess_Rational + bEVHeuristic * QExtPess_Heuristic + bEVConfirmationBias * QExtPess_Confirmation_Bias  + bEVAccuracy * QExtPess_Accuracy + -10000*QExtPess_Is_Absent),
                      bEVRational + bEVHeuristic + bEVConfirmationBias + bEVAccuracy ~ 1 + (1|Subject),
                      AExtOpt + AModOpt + AModPess + AExtPess ~ 1 + (1|Subject),
                      nl = TRUE),
                   prior = c(prior(normal(0, 1), class = b, nlpar = AExtOpt),
                             prior(cauchy(0, 1), class = sd,nlpar = AExtOpt),
                             prior(normal(0, 1), class = b, nlpar = AModOpt),
                             prior(cauchy(0, 1), class = sd,nlpar = AModOpt),
                             prior(normal(0, 1), class = b, nlpar = AModPess),
                             prior(cauchy(0, 1), class = sd,nlpar = AModPess),
                             prior(normal(0, 1), class = b, nlpar = AExtPess),
                             prior(cauchy(0, 1), class = sd,nlpar = AExtPess),
                             prior(normal(0, 5), class = b, nlpar = bEVRational),
                             prior(cauchy(0, 1), class = sd,nlpar = bEVRational),
                             prior(normal(0, 5), class = b, nlpar = bEVHeuristic),
                             prior(cauchy(0, 1), class = sd,nlpar = bEVHeuristic),
                             prior(normal(0, 5), class = b, nlpar = bEVConfirmationBias),
                             prior(cauchy(0, 1), class = sd,nlpar = bEVConfirmationBias),
                             prior(normal(0, 5), class = b, nlpar = bEVAccuracy),
                             prior(cauchy(0, 1), class = sd,nlpar = bEVAccuracy)),
                   iter = 6000, warmup = 1500, cores = 4, chains = 4, control=list(adapt_delta=0.99, max_treedepth=15), seed = 123,
                   file = "Simultaneous_Regression_Bayesian")

summary(Simultaneous_Regression_Bayesian)
