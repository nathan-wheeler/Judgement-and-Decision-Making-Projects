library(rstan)
rstan_options(auto_write = TRUE)
library(tidyverse)
library(loo)
library(brms)

d <- read.csv("Extreme_Bias_2_Cleaned.csv")

d <- d %>% mutate(
  subject = subject %>% as.factor,
  
  Reward_Condition = Reward_Condition %>% factor(levels=c("Extremely Negative","Moderately Negative", "Neutral","Moderately Positive", "Extremely Positive")),
  
  CueChoice = case_when(
    cuechoice == 1 ~ "Opt",
    cuechoice == 2 ~ "Real",
    cuechoice == 3 ~ "Pess"
  ) %>% factor(levels = c("Pess", "Real", "Opt"))
  
)

Extreme_Bias_2_BRM <- brm(cuechoice ~ 0 + Reward_Condition + (0 + Reward_Condition|subject),
                     family= categorical(link = logit,refcat=2),
                     prior = c(prior(normal(0, 1), class = b,dpar = mu1),
                               prior(cauchy(0, 1), class = sd,dpar = mu1),
                               prior(normal(0, 1), class = b,dpar = mu3),
                               prior(cauchy(0, 1), class = sd,dpar = mu3),
                               prior(lkj(2), class = cor)),
                     data = d,
                     iter = 4000, warmup = 1000, cores = 4, chains = 4, seed = 123, control=list(max_treedepth=15),
                     file = "Extreme_Bias_2_BRM")
summary(Extreme_Bias_2_BRM)