library(rstan)
rstan_options(auto_write = TRUE)
library(tidyverse)
library(loo)
library(brms)

d <- read.csv("Moderate_Bias_Cleaned.csv")

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
  ) %>% factor(levels = c("Pess", "Real", "Opt"))
)

Moderate_Bias_BRM <- brm(cuechoice ~ 0 + Condition + PerLeft + (0 + Condition + PerLeft||subject),
                     family= categorical(link = logit,refcat=2),
                     prior = c(prior(normal(0, 1), class = b,dpar = mu1),
                               prior(cauchy(0, 1), class = sd,dpar = mu1),
                               prior(normal(0, 1), class = b,dpar = mu3),
                               prior(cauchy(0, 1), class = sd,dpar = mu3)),
                     data = d,
                     iter = 5000, warmup = 1000, control=list(max_treedepth=15), cores = 4, chains = 4, seed = 123,
                     file = "Moderate_Bias_BRM",  save_pars = save_pars(all = TRUE))
summary(Moderate_Bias_BRM)