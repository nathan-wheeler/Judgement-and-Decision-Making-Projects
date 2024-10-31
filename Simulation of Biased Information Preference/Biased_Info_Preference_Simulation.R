library(tidyverse)
library(ggpubr)
library(stringr)

#Calculate and Normalize Expected Values for each Reward Condition and Information Valuation Strategy
Q <- function(Jprob = list(O1S1 = .375,O2S1 = .125,O1S2 = .125, O2S2 = .375), Reward = list(S1L = 5, S1R = 0, S2L = -5, S2R= 0), Model = "Confirmation_Bias",p1 = .25,p2=1,p3 = 0.25, p4 = .5){
  if(Model == "Rational"){
    EQ <- ifelse(Jprob$O1S1*Reward$S1L + Jprob$O1S2*Reward$S2L >
                   Jprob$O1S1*Reward$S1R + Jprob$O1S2*Reward$S2R,
                 Jprob$O1S1*Reward$S1L + Jprob$O1S2*Reward$S2L,
                 Jprob$O1S1*Reward$S1R + Jprob$O1S2*Reward$S2R) +
      ifelse(Jprob$O2S1*Reward$S1L + Jprob$O2S2*Reward$S2L > 
               Jprob$O2S1*Reward$S1R + Jprob$O2S2*Reward$S2R,
             Jprob$O2S1*Reward$S1L + Jprob$O2S2*Reward$S2L,
             Jprob$O2S1*Reward$S1R + Jprob$O2S2*Reward$S2R)
  } else if(Model == "Heuristic"){
    EQ <-  Jprob$O1S1*Reward$S1L + Jprob$O1S2*Reward$S2L + Jprob$O2S1*Reward$S1R + Jprob$O2S2*Reward$S2R
  } else if(Model == "Confirmation_Bias"){
    EQ <- ifelse((Jprob$O1S1 +Jprob$O2S1)*Reward$S1L + (Jprob$O1S2 +Jprob$O2S2)*Reward$S2L  >
                   (Jprob$O1S1 +Jprob$O2S1)*Reward$S1R + (Jprob$O1S2 +Jprob$O2S2)*Reward$S2R,
                 (Jprob$O1S1 + Jprob$O1S2)*((Jprob$O1S1 +Jprob$O2S1)*Reward$S1L + (Jprob$O1S2 +Jprob$O2S2)*Reward$S2L -
                                              ((Jprob$O1S1 +Jprob$O2S1)*Reward$S1R + (Jprob$O1S2 +Jprob$O2S2)*Reward$S2R)),
                 (Jprob$O2S1 + Jprob$O2S2)*((Jprob$O1S1 +Jprob$O2S1)*Reward$S1R + (Jprob$O1S2 +Jprob$O2S2)*Reward$S2R - 
                                              ((Jprob$O1S1 +Jprob$O2S1)*Reward$S1L + (Jprob$O1S2 +Jprob$O2S2)*Reward$S2L))) 
  } else if(Model == "Accuracy"){
    EQ <-  Jprob$O1S1 + Jprob$O2S2
  }
  CQ <- ifelse((Jprob$O1S1 +Jprob$O2S1)*Reward$S1L + (Jprob$O1S2 +Jprob$O2S2)*Reward$S2L  >
                 (Jprob$O1S1 +Jprob$O2S1)*Reward$S1R + (Jprob$O1S2 +Jprob$O2S2)*Reward$S2R,
               (Jprob$O1S1 +Jprob$O2S1)*Reward$S1L + (Jprob$O1S2 +Jprob$O2S2)*Reward$S2L,
               (Jprob$O1S1 +Jprob$O2S1)*Reward$S1R + (Jprob$O1S2 +Jprob$O2S2)*Reward$S2R) 
  return(list(EQ,CQ))
}

d <- data.frame(Condition = rep(1:5,each =6),
                Reward_Condition = rep(c("Extreme Negative", "Moderate Negative","Neutral","Moderate Positive","Extreme Positive"),each =6) %>%
                  factor(levels =c("Extreme Negative", "Moderate Negative","Neutral","Moderate Positive","Extreme Positive")) ,
                Cue = rep(c("Pessimistic","Realistic","Optimistic"),10) %>% factor(levels=c("Pessimistic","Realistic","Optimistic")),
                Bias_Condition = rep(c("Moderate Bias","Extreme Bias"),each =3) %>% factor(levels = c("Moderate Bias","Extreme Bias")))
for(i in c("Rational","Confirmation_Bias", "Heuristic","Accuracy")){
  QExtOptC1 <- Q(list(O1S1 = .5,O2S1 = 0, O1S2 = .5, O2S2 = .0), Reward = list(S1L = 1, S1R = 0, S2L = -10, S2R= 0), Model = i)[[1]]
  QModOptC1 <- Q(list(O1S1 = .5,O2S1 = 0, O1S2 = .275, O2S2 = .225), Reward = list(S1L = 1, S1R = 0, S2L = -10, S2R= 0), Model = i)[[1]]
  QRealC1 <- Q(list(O1S1 = .4,O2S1 = .1, O1S2 = .1, O2S2 = .4), Reward = list(S1L = 1, S1R = 0, S2L = -10, S2R= 0), Model = i)[[1]]
  QModPessC1 <- Q(list(O1S1 = .225,O2S1 = .275, O1S2 = 0, O2S2 = .5), Reward = list(S1L = 1, S1R = 0, S2L = -10, S2R= 0), Model = i)[[1]]
  QExtPessC1 <- Q(list(O1S1 = 0,O2S1 = .5, O1S2 = 0, O2S2 = .5), Reward = list(S1L = 1, S1R = 0, S2L = -10, S2R= 0), Model = i)[[1]]
  
  QExtOptC2 <- Q(list(O1S1 = .5,O2S1 = 0, O1S2 = .5, O2S2 = .0), Reward = list(S1L = 1, S1R = 0, S2L = -5, S2R= 0), Model = i)[[1]]
  QModOptC2 <- Q(list(O1S1 = .5,O2S1 = 0, O1S2 = .275, O2S2 = .225), Reward = list(S1L = 1, S1R = 0, S2L = -5, S2R= 0), Model = i)[[1]]
  QRealC2 <- Q(list(O1S1 = .4,O2S1 = .1, O1S2 = .1, O2S2 = .4), Reward = list(S1L = 1, S1R = 0, S2L = -5, S2R= 0), Model = i)[[1]]
  QModPessC2 <- Q(list(O1S1 = .225,O2S1 = .275, O1S2 = 0, O2S2 = .5), Reward = list(S1L = 1, S1R = 0, S2L = -5, S2R= 0), Model = i)[[1]]
  QExtPessC2 <- Q(list(O1S1 = 0,O2S1 = .5, O1S2 = 0, O2S2 = .5), Reward = list(S1L = 1, S1R = 0, S2L = -5, S2R= 0), Model = i)[[1]]
  
  QExtOptC3 <- Q(list(O1S1 = .5,O2S1 = 0, O1S2 = .5, O2S2 = .0), Reward = list(S1L = 1, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  QModOptC3 <- Q(list(O1S1 = .5,O2S1 = 0, O1S2 = .275, O2S2 = .225), Reward = list(S1L = 1, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  QRealC3 <- Q(list(O1S1 = .4,O2S1 = .1, O1S2 = .1, O2S2 = .4), Reward = list(S1L = 1, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  QModPessC3 <- Q(list(O1S1 = .225,O2S1 = .275, O1S2 = 0, O2S2 = .5), Reward = list(S1L = 1, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  QExtPessC3 <- Q(list(O1S1 = 0,O2S1 = .5, O1S2 = 0, O2S2 = .5), Reward = list(S1L = 1, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  
  QExtOptC4 <- Q(list(O1S1 = .5,O2S1 = 0, O1S2 = .5, O2S2 = .0), Reward = list(S1L = 5, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  QModOptC4 <- Q(list(O1S1 = .5,O2S1 = 0, O1S2 = .275, O2S2 = .225), Reward = list(S1L = 5, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  QRealC4 <- Q(list(O1S1 = .4,O2S1 = .1, O1S2 = .1, O2S2 = .4), Reward = list(S1L = 5, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  QModPessC4 <- Q(list(O1S1 = .225,O2S1 = .275, O1S2 = 0, O2S2 = .5), Reward = list(S1L = 5, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  QExtPessC4 <- Q(list(O1S1 = 0,O2S1 = .5, O1S2 = 0, O2S2 = .5), Reward = list(S1L = 5, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  
  QExtOptC5 <- Q(list(O1S1 = .5,O2S1 = 0, O1S2 = .5, O2S2 = .0), Reward = list(S1L = 10, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  QModOptC5 <- Q(list(O1S1 = .5,O2S1 = 0, O1S2 = .275, O2S2 = .225), Reward = list(S1L = 10, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  QRealC5 <- Q(list(O1S1 = .4,O2S1 = .1, O1S2 = .1, O2S2 = .4), Reward = list(S1L = 10, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  QModPessC5 <- Q(list(O1S1 = .225,O2S1 = .275, O1S2 = 0, O2S2 = .5), Reward = list(S1L = 10, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  QExtPessC5 <- Q(list(O1S1 = 0,O2S1 = .5, O1S2 = 0, O2S2 = .5), Reward = list(S1L = 10, S1R = 0, S2L = -1, S2R= 0), Model = i)[[1]]
  
  QExtOpt <- c(QExtOptC1,QExtOptC2,QExtOptC3,QExtOptC4,QExtOptC5,QRealC1,QRealC2,QRealC3,QRealC4,QRealC5,QExtPessC1,QExtPessC2,QExtPessC3,QExtPessC4,QExtPessC5) %>% scale
  QModOpt <- c(QModOptC1,QModOptC2,QModOptC3,QModOptC4,QModOptC5,QRealC1,QRealC2,QRealC3,QRealC4,QRealC5,QModPessC1,QModPessC2,QModPessC3,QModPessC4,QModPessC5) %>% scale
  QExtReal <- c(QRealC1,QRealC2,QRealC3,QRealC4,QRealC5,QExtOptC1,QExtOptC2,QExtOptC3,QExtOptC4,QExtOptC5,QExtPessC1,QExtPessC2,QExtPessC3,QExtPessC4,QExtPessC5) %>% scale
  QModReal <- c(QRealC1,QRealC2,QRealC3,QRealC4,QRealC5,QModOptC1,QModOptC2,QModOptC3,QModOptC4,QModOptC5,QModPessC1,QModPessC2,QModPessC3,QModPessC4,QModPessC5) %>% scale
  QModPess <- c(QModPessC1,QModPessC2,QModPessC3,QModPessC4,QModPessC5,QRealC1,QRealC2,QRealC3,QRealC4,QRealC5,QModOptC1,QModOptC2,QModOptC3,QModOptC4,QModOptC5) %>% scale
  QExtPess <- c(QExtPessC1,QExtPessC2,QExtPessC3,QExtPessC4,QExtPessC5,QRealC1,QRealC2,QRealC3,QRealC4,QRealC5,QExtOptC1,QExtOptC2,QExtOptC3,QExtOptC4,QExtOptC5) %>% scale
  
  d <- d %>% mutate(
    "EV_{i}" := case_when(
      Bias_Condition == "Extreme Bias" & Cue == "Optimistic" & Condition == 5 ~ QExtOpt[5],
      Bias_Condition == "Extreme Bias" & Cue == "Optimistic" & Condition == 4 ~ QExtOpt[4],
      Bias_Condition == "Extreme Bias" & Cue == "Optimistic" & Condition == 3 ~ QExtOpt[3],
      Bias_Condition == "Extreme Bias" & Cue == "Optimistic" & Condition == 2 ~ QExtOpt[2],
      Bias_Condition == "Extreme Bias" & Cue == "Optimistic" & Condition == 1 ~ QExtOpt[1],
      
      Bias_Condition == "Extreme Bias" & Cue == "Realistic" & Condition == 5 ~ QExtReal[5],
      Bias_Condition == "Extreme Bias" & Cue == "Realistic" & Condition == 4 ~ QExtReal[4],
      Bias_Condition == "Extreme Bias" & Cue == "Realistic" & Condition == 3 ~ QExtReal[3],
      Bias_Condition == "Extreme Bias" & Cue == "Realistic" & Condition == 2 ~ QExtReal[2],
      Bias_Condition == "Extreme Bias" & Cue == "Realistic" & Condition == 1 ~ QExtReal[1],
      
      Bias_Condition == "Extreme Bias" & Cue == "Pessimistic" & Condition == 5 ~ QExtPess[5],
      Bias_Condition == "Extreme Bias" & Cue == "Pessimistic" & Condition == 4 ~ QExtPess[4],
      Bias_Condition == "Extreme Bias" & Cue == "Pessimistic" & Condition == 3 ~ QExtPess[3],
      Bias_Condition == "Extreme Bias" & Cue == "Pessimistic" & Condition == 2 ~ QExtPess[2],
      Bias_Condition == "Extreme Bias" & Cue == "Pessimistic" & Condition == 1 ~ QExtPess[1],
      
      Bias_Condition == "Moderate Bias" & Cue == "Optimistic" & Condition == 5 ~ QModOpt[5],
      Bias_Condition == "Moderate Bias" & Cue == "Optimistic" & Condition == 4 ~ QModOpt[4],
      Bias_Condition == "Moderate Bias" & Cue == "Optimistic" & Condition == 3 ~ QModOpt[3],
      Bias_Condition == "Moderate Bias" & Cue == "Optimistic" & Condition == 2 ~ QModOpt[2],
      Bias_Condition == "Moderate Bias" & Cue == "Optimistic" & Condition == 1 ~ QModOpt[1],
      
      Bias_Condition == "Moderate Bias" & Cue == "Realistic" & Condition == 5 ~ QModReal[5],
      Bias_Condition == "Moderate Bias" & Cue == "Realistic" & Condition == 4 ~ QModReal[4],
      Bias_Condition == "Moderate Bias" & Cue == "Realistic" & Condition == 3 ~ QModReal[3],
      Bias_Condition == "Moderate Bias" & Cue == "Realistic" & Condition == 2 ~ QModReal[2],
      Bias_Condition == "Moderate Bias" & Cue == "Realistic" & Condition == 1 ~ QModReal[1],
      
      Bias_Condition == "Moderate Bias" & Cue == "Pessimistic" & Condition == 5 ~ QModPess[5],
      Bias_Condition == "Moderate Bias" & Cue == "Pessimistic" & Condition == 4 ~ QModPess[4],
      Bias_Condition == "Moderate Bias" & Cue == "Pessimistic" & Condition == 3 ~ QModPess[3],
      Bias_Condition == "Moderate Bias" & Cue == "Pessimistic" & Condition == 2 ~ QModPess[2],
      Bias_Condition == "Moderate Bias" & Cue == "Pessimistic" & Condition == 1 ~ QModPess[1]
    )
  )
}

##Simulate Preference for Biased information for each Information Valuation Strategy using a Softmax
dP <- d %>% 
  group_by(Reward_Condition,Bias_Condition) %>% 
  mutate(
    EVRationalExp = exp(10*EV_Rational),
    EVHeuristicExp = exp(7*EV_Heuristic),
    EVCBExp = exp(1.5*EV_Confirmation_Bias),
    EVAccExp = exp(EV_Accuracy),
    EV_Rational_Probs = EVRationalExp/sum(EVRationalExp),
    EV_Heuristic_Probs = EVHeuristicExp/sum(EVHeuristicExp),
    EV_CB_Probs = EVCBExp/sum(EVCBExp),
    EV_Acc_Probs = EVAccExp/sum(EVAccExp))


##Plot Simulated Preference for biased information under each information valuation strategy 
P_Rational <- dP %>% ggplot(aes(x=Reward_Condition, y=EV_Rational_Probs, color= Cue, group = Cue)) + 
  geom_point(position=position_dodge(width = 0.2), size = 4) +
  geom_line(position=position_dodge(width = 0.2)) +
  ylim(c(0,1)) +
  scale_fill_brewer(palette="Paired") +
  facet_wrap(~Bias_Condition) +
  labs(x = "Reward Condition", y = "Predicted Probability of Choice",title = "Predictions of Rational Model") +
  theme(plot.subtitle = element_text(hjust = 0.5),plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


P_Heuristic <- dP %>% ggplot(aes(x=Reward_Condition, y=EV_Heuristic_Probs, color= Cue, group = Cue)) + 
  geom_point(position=position_dodge(width = 0.2), size = 4) +
  geom_line(position=position_dodge(width = 0.2)) +
  ylim(c(0,1)) +
  scale_fill_brewer(palette="Paired") +
  facet_wrap(~Bias_Condition) +
  labs(x = "Reward Condition", y = "Predicted Probability of Choice",title = "Predictions of Trust-Based Model") +
  theme(plot.subtitle = element_text(hjust = 0.5),plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


P_CB <- dP %>% ggplot(aes(x=Reward_Condition, y=EV_CB_Probs, color= Cue, group = Cue)) + 
  geom_point(position=position_dodge(width = 0.2), size = 4) +
  geom_line(position=position_dodge(width = 0.2)) +
  ylim(c(0,1)) +
  scale_fill_brewer(palette="Paired") +
  facet_wrap(~Bias_Condition) +
  labs(x = "Reward Condition", y = "Predicted Probability of Choice",title = "Predictions of Congeniality Model") +
  theme(plot.subtitle = element_text(hjust = 0.5),plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


P_Acc <- dP %>% ggplot(aes(x=Reward_Condition, y=EV_Acc_Probs, color= Cue, group = Cue)) + 
  geom_point(position=position_dodge(width = 0.2), size = 4) +
  geom_line(position=position_dodge(width = 0.2)) +
  ylim(c(0,1)) +
  scale_fill_brewer(palette="Paired") +
  facet_wrap(~Bias_Condition) +
  labs(x = "Reward Condition", y = "Predicted Probability of Choice",title = "Predictions of Accuracy Model") +
  theme(plot.subtitle = element_text(hjust = 0.5),plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


ggarrange(P_Rational, P_Heuristic, P_CB,P_Acc,
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2)


