library(rstan)
rstan_options(auto_write = TRUE)
library(tidyverse)
library(loo)
library(brms)

d <- read.csv("Extreme_Bias_1_Cleaned.csv")

d <- d %>% mutate(
  PerLeft = 1- (TotTrialNumber - 1)/max(d$TotTrialNumber),
  
  Reward_Condition = case_when(
    rewardcontext == -1 ~ "Neg",
    rewardcontext == 0  ~ "Neut",
    rewardcontext == 1  ~ "Pos"
  ) %>% factor(levels = c("Neg", "Neut", "Pos")),
  
  Prior_Condition = case_when(
    prior == 0.25 ~ "Low",
    prior == 0.5  ~ "Equal",
    prior == 0.75 ~ "High"
  ) %>% factor(levels = c("Low", "Equal", "High")),
  
  Condition = paste0(Reward_Condition,Prior_Condition) %>% factor(levels=c("NegLow","NegEqual","NegHigh","NeutLow","NeutEqual","NeutHigh","PosLow","PosEqual","PosHigh")),
  
  subject = subject %>% as.factor,
  
  CueChoice = case_when(
    cuechoice == 1 ~ "Opt",
    cuechoice == 2 ~ "Real",
    cuechoice == 3 ~ "Pess"
  ) %>% factor(levels = c("Pess", "Real", "Opt")),
  
  Bias_Condition = ifelse(CueCondition.x == 1,"Moderate","Extreme") %>% factor(levels=c("Moderate","Extreme")),
)


Extreme_Bias_1_BRM <- brm(cuechoice ~ 0 + Condition:Bias_Condition + Bias_Condition:PerLeft + (0 + Condition:Bias_Condition + Bias_Condition:PerLeft||subject),
                     family= categorical(link = logit,refcat=2),
                     prior = c(prior(normal(0, 1), class = b,dpar = mu1),
                               prior(cauchy(0, 1), class = sd,dpar = mu1),
                               prior(normal(0, 1), class = b,dpar = mu3),
                               prior(cauchy(0, 1), class = sd,dpar = mu3)),
                     data = d,
                     iter = 6000, warmup = 1000, control=list(max_treedepth=15), cores = 4, chains = 4, seed = 123,
                     file = "Extreme_Bias_1_BRM")
summary(Extreme_Bias_1_BRM)
