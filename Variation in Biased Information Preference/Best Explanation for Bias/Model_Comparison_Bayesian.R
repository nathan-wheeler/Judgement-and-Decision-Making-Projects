library(rstan)
rstan_options(auto_write = TRUE)
library(tidyverse)
library(loo)
library(brms)
#load data
d <- read.csv("Variation_in_Bias_Cleaned.csv")

#clean data
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
  
  Condition1 = ifelse(condition == 1,1,0),
  Condition2 = ifelse(condition == 2,1,0),
  Condition3 = ifelse(condition == 3,1,0),
  Condition4 = ifelse(condition == 4,1,0),
  Condition5 = ifelse(condition == 5,1,0),
  Condition6 = ifelse(condition == 6,1,0),
  Condition7 = ifelse(condition == 7,1,0),
  
  Reward_Condition = Reward_Condition  %>% factor(levels=c("Extremely Negative; ExtOpt, Real, ExtPess",
                                                           "Neutral; ExtOpt, Real, ExtPess",
                                                           "Extremely Positive; ExtOpt, Real, ExtPess" ,
                                                           "Moderately Negative; Real, ModPess, ExtPess",
                                                           "Neutral; Real, ModPess, ExtPess",
                                                           "Neutral; ExtOpt, ModOpt, Real",
                                                           "Moderately Positive; ExtOpt, ModOpt, Real")),
  CueChoice = case_when(
    cuechoice == 1 ~ "ExtOpt",
    cuechoice == 2 ~ "ModOpt",
    cuechoice == 3 ~ "Real",
    cuechoice == 4 ~ "ModPess",
    cuechoice == 5 ~ "ExtPess"
  ) %>% factor(levels = c("ExtOpt", "ModOpt", "Real", "ModPess", "ExtPess"))
  
)

d[is.na(d)] <- 0


#Fit Models of Information Preference
Rational_BRM <- brm(data = d, 
                    family = categorical(link = logit, refcat =  3),
                    bf(cuechoice ~ 1,
                       nlf(mu1 ~ AExtOpt*(1-QExtOpt_Is_Absent) + bEV * QExtOpt_Rational + -10000*QExtOpt_Is_Absent),
                       nlf(mu2 ~ AModOpt*(1-QModOpt_Is_Absent) + bEV * QModOpt_Rational + -10000*QModOpt_Is_Absent),
                       nlf(mu4 ~ AModPess*(1-QModPess_Is_Absent) + bEV * QModPess_Rational + -10000*QModPess_Is_Absent),
                       nlf(mu5 ~ AExtPess*(1-QExtPess_Is_Absent) + bEV * QExtPess_Rational + -10000*QExtPess_Is_Absent),
                       bEV ~ 1 + (1|Subject),
                       AExtOpt + AModOpt + AModPess + AExtPess ~ 1 + (1|Subject)),
                    prior = c(prior(normal(0, 1), class = b, nlpar = AExtOpt),
                              prior(cauchy(0, 1), class = sd,nlpar = AExtOpt),
                              prior(normal(0, 1), class = b, nlpar = AModOpt),
                              prior(cauchy(0, 1), class = sd,nlpar = AModOpt),
                              prior(normal(0, 1), class = b, nlpar = AModPess),
                              prior(cauchy(0, 1), class = sd,nlpar = AModPess),
                              prior(normal(0, 1), class = b, nlpar = AExtPess),
                              prior(cauchy(0, 1), class = sd,nlpar = AExtPess),
                              prior(normal(0, 5), class = b, nlpar = bEV),
                              prior(cauchy(0, 1), class = sd,nlpar = bEV)),
                    iter = 4000, warmup = 1000, cores = 4, chains = 4, seed = 123,
                    file = "Rational_BRM")
summary(Rational_BRM)

Heuristic_BRM <- brm(data = d, 
                     family = categorical(link = logit, refcat =  3),
                     bf(cuechoice ~ 1,
                        nlf(mu1 ~ AExtOpt*(1-QExtOpt_Is_Absent) + bEV * QExtOpt_Heuristic + -10000*QExtOpt_Is_Absent),
                        nlf(mu2 ~ AModOpt*(1-QModOpt_Is_Absent) + bEV * QModOpt_Heuristic + -10000*QModOpt_Is_Absent),
                        nlf(mu4 ~ AModPess*(1-QModPess_Is_Absent) + bEV * QModPess_Heuristic + -10000*QModPess_Is_Absent),
                        nlf(mu5 ~ AExtPess*(1-QExtPess_Is_Absent) + bEV * QExtPess_Heuristic + -10000*QExtPess_Is_Absent),
                        bEV ~ 1 + (1|Subject),
                        AExtOpt + AModOpt + AModPess + AExtPess ~ 1 + (1|Subject)),
                     prior = c(prior(normal(0, 1), class = b, nlpar = AExtOpt),
                               prior(cauchy(0, 1), class = sd,nlpar = AExtOpt),
                               prior(normal(0, 1), class = b, nlpar = AModOpt),
                               prior(cauchy(0, 1), class = sd,nlpar = AModOpt),
                               prior(normal(0, 1), class = b, nlpar = AModPess),
                               prior(cauchy(0, 1), class = sd,nlpar = AModPess),
                               prior(normal(0, 1), class = b, nlpar = AExtPess),
                               prior(cauchy(0, 1), class = sd,nlpar = AExtPess),
                               prior(normal(0, 5), class = b, nlpar = bEV),
                               prior(cauchy(0, 1), class = sd,nlpar = bEV)),
                     iter = 4000, warmup = 1000, cores = 4, chains = 4, seed = 123,
                     file = "Heuristic_BRM")
summary(Heuristic_BRM)

Confirmation_Bias_BRM <- brm(data = d, 
                                family = categorical(link = logit, refcat =  3),
                                bf(cuechoice ~ 1,
                                   nlf(mu1 ~ AExtOpt*(1-QExtOpt_Is_Absent) + bEV * QExtOpt_Confirmation_Bias + -10000*QExtOpt_Is_Absent),
                                   nlf(mu2 ~ AModOpt*(1-QModOpt_Is_Absent) + bEV * QModOpt_Confirmation_Bias + -10000*QModOpt_Is_Absent),
                                   nlf(mu4 ~ AModPess*(1-QModPess_Is_Absent) + bEV * QModPess_Confirmation_Bias + -10000*QModPess_Is_Absent),
                                   nlf(mu5 ~ AExtPess*(1-QExtPess_Is_Absent) + bEV * QExtPess_Confirmation_Bias + -10000*QExtPess_Is_Absent),
                                   bEV ~ 1 + (1|Subject),
                                   AExtOpt + AModOpt + AModPess + AExtPess ~ 1 + (1|Subject)),
                                prior = c(prior(normal(0, 1), class = b, nlpar = AExtOpt),
                                          prior(cauchy(0, 1), class = sd,nlpar = AExtOpt),
                                          prior(normal(0, 1), class = b, nlpar = AModOpt),
                                          prior(cauchy(0, 1), class = sd,nlpar = AModOpt),
                                          prior(normal(0, 1), class = b, nlpar = AModPess),
                                          prior(cauchy(0, 1), class = sd,nlpar = AModPess),
                                          prior(normal(0, 1), class = b, nlpar = AExtPess),
                                          prior(cauchy(0, 1), class = sd,nlpar = AExtPess),
                                          prior(normal(0, 5), class = b, nlpar = bEV),
                                          prior(cauchy(0, 1), class = sd,nlpar = bEV)),
                                iter = 4000, warmup = 1000, cores = 4, chains = 4, seed = 123,
                             file = "Confirmation_Bias_BRM")
summary(Confirmation_Bias_BRM)

Accuracy_BRM <- brm(data = d, 
                       family = categorical(link = logit, refcat =  3),
                       bf(cuechoice ~ 1,
                          nlf(mu1 ~ AExtOpt*(1-QExtOpt_Is_Absent) + bEV * QExtOpt_Accuracy + -10000*QExtOpt_Is_Absent),
                          nlf(mu2 ~ AModOpt*(1-QModOpt_Is_Absent) + bEV * QModOpt_Accuracy + -10000*QModOpt_Is_Absent),
                          nlf(mu4 ~ AModPess*(1-QModPess_Is_Absent) + bEV * QModPess_Accuracy + -10000*QModPess_Is_Absent),
                          nlf(mu5 ~ AExtPess*(1-QExtPess_Is_Absent) + bEV * QExtPess_Accuracy + -10000*QExtPess_Is_Absent),
                          bEV ~ 1 + (1|Subject),
                          AExtOpt + AModOpt + AModPess + AExtPess ~ 1 +(1|Subject)),
                       prior = c(prior(normal(0, 1), class = b, nlpar = AExtOpt),
                                 prior(cauchy(0, 1), class = sd,nlpar = AExtOpt),
                                 prior(normal(0, 1), class = b, nlpar = AModOpt),
                                 prior(cauchy(0, 1), class = sd,nlpar = AModOpt),
                                 prior(normal(0, 1), class = b, nlpar = AModPess),
                                 prior(cauchy(0, 1), class = sd,nlpar = AModPess),
                                 prior(normal(0, 1), class = b, nlpar = AExtPess),
                                 prior(cauchy(0, 1), class = sd,nlpar = AExtPess),
                                 prior(normal(0, 5), class = b, nlpar = bEV),
                                 prior(cauchy(0, 1), class = sd,nlpar = bEV)),
                       iter = 4000, warmup = 1000, cores = 4, chains = 4, seed = 123,
                    file = "Accuracy_BRM")
summary(Accuracy_BRM)


#Derive WAIC comparison criteria
Rational_BRM <- add_criterion(Rational_BRM,"waic", pointwise = TRUE)
saveRDS(Rational_BRM,"Rational_BRM.rds")

Heuristic_BRM <- add_criterion(Heuristic_BRM,"waic", pointwise = TRUE)
saveRDS(Heuristic_BRM,"Heuristic_BRM.rds")

Confirmation_Bias_BRM <- add_criterion(Confirmation_Bias_BRM,"waic", pointwise = TRUE)
saveRDS(Confirmation_Bias_BRM,"Confirmation_Bias_BRM.rds")

Accuracy_BRM <- add_criterion(Accuracy_BRM,"waic", pointwise = TRUE)
saveRDS(Accuracy_BRM,"Accruacy_BRM.rds")

#Compare Models
loo_compare(Rational_BRM,Confirmation_Bias_BRM,Heuristic_BRM,Accuracy_BRM, criterion = "waic")
