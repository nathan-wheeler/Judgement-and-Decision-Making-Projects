library(rstan)
rstan_options(auto_write = TRUE)
library(tidyverse)
library(loo)
library(brms)

d <- read.csv("Variation_in_Bias_Cleaned.csv")

d <- d %>% mutate(
  Subject = subject %>% as.factor,
  
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


Descriptive_Bayesian_Regression_Model <- brm(data = d, 
                     family = categorical(link = logit, refcat =  3),
                     bf(cuechoice ~ 1,
                        nlf(mu1 ~ AExtOptCondition1*Condition1+ AExtOptCondition2*Condition2 + AExtOptCondition3*Condition3 + AExtOptCondition6*Condition6 +AExtOptCondition7*Condition7 + -10000*QExtOpt_Is_Absent),
                        nlf(mu2 ~ AModOptCondition6*Condition6 +AModOptCondition7*Condition7 + -10000*QModOpt_Is_Absent),
                        nlf(mu4 ~ AModPessCondition4*Condition4 +AModPessCondition5*Condition5 + -10000*QModPess_Is_Absent),
                        nlf(mu5 ~ AExtPessCondition1*Condition1 + AExtPessCondition2*Condition2 + AExtPessCondition3*Condition3 +AExtPessCondition4*Condition4 +AExtPessCondition5*Condition5 + -10000*QExtPess_Is_Absent),
                        AExtOptCondition1 + AExtOptCondition2 + AExtOptCondition3 + AExtOptCondition6 + AExtOptCondition7 + AModOptCondition6  + AModOptCondition7  + AModPessCondition4 + AModPessCondition5 + AExtPessCondition1 + AExtPessCondition2 + AExtPessCondition3 + AExtPessCondition4 + AExtPessCondition5 ~ 1 +(1||Subject)),
                     prior = c(prior(normal(0, 1), class = b, nlpar = AExtOptCondition1),
                               prior(cauchy(0, 1), class = sd,nlpar = AExtOptCondition1),
                               prior(normal(0, 1), class = b, nlpar = AExtOptCondition2),
                               prior(cauchy(0, 1), class = sd,nlpar = AExtOptCondition2),
                               prior(normal(0, 1), class = b, nlpar = AExtOptCondition3),
                               prior(cauchy(0, 1), class = sd,nlpar = AExtOptCondition3),
                               prior(normal(0, 1), class = b, nlpar = AExtOptCondition6),
                               prior(cauchy(0, 1), class = sd,nlpar = AExtOptCondition6),
                               prior(normal(0, 1), class = b, nlpar = AExtOptCondition7),
                               prior(cauchy(0, 1), class = sd,nlpar = AExtOptCondition7),
                               prior(normal(0, 1), class = b, nlpar = AModOptCondition6),
                               prior(cauchy(0, 1), class = sd,nlpar = AModOptCondition6),
                               prior(normal(0, 1), class = b, nlpar = AModOptCondition7),
                               prior(cauchy(0, 1), class = sd,nlpar = AModOptCondition7),
                               prior(normal(0, 1), class = b, nlpar = AModPessCondition4),
                               prior(cauchy(0, 1), class = sd,nlpar = AModPessCondition4),
                               prior(normal(0, 1), class = b, nlpar = AModPessCondition5),
                               prior(cauchy(0, 1), class = sd,nlpar = AModPessCondition5),
                               prior(normal(0, 1), class = b, nlpar = AExtPessCondition1),
                               prior(cauchy(0, 1), class = sd,nlpar = AExtPessCondition1),
                               prior(normal(0, 1), class = b, nlpar = AExtPessCondition2),
                               prior(cauchy(0, 1), class = sd,nlpar = AExtPessCondition2),
                               prior(normal(0, 1), class = b, nlpar = AExtPessCondition3),
                               prior(cauchy(0, 1), class = sd,nlpar = AExtPessCondition3),
                               prior(normal(0, 1), class = b, nlpar = AExtPessCondition4),
                               prior(cauchy(0, 1), class = sd,nlpar = AExtPessCondition4),
                               prior(normal(0, 1), class = b, nlpar = AExtPessCondition5),
                               prior(cauchy(0, 1), class = sd,nlpar = AExtPessCondition5)),
                     iter = 4000, warmup = 1000, cores = 4, chains = 4, control=list(adapt_delta=0.99, max_treedepth=15), seed = 123,
                     file = "Descriptive_Bayesian_Regression_Model")
summary(Descriptive_Bayesian_Regression_Model)