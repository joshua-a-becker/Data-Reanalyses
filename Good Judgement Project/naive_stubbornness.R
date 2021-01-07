rm(list=ls());gc()
library(tidyverse)

d = read.csv("data/survey_fcasts.yr1.csv")

q_all = read.csv("data/ifps.csv")
q = q_all %>%
  subset(
    q_status=="closed" 
    & q_type == 0
    & n_opts==2
    ) %>%
  mutate(
    outcome = outcome=='a'
  ) %>%
  select(c("ifp_id","q_status","outcome"))

myd = d %>%
  subset(
    ctt%in%c("2a","2b","2c") 
    & ifp_id %in% q$ifp_id
    & answer_option == 'a'
    ) %>%
  merge(q, by="ifp_id")

ind_d=myd %>% 
  group_by(ifp_id, user_id) %>%
  summarize(
      final_answer = value[fcast_date==max(fcast_date)]
    , initial_answer = value[fcast_date==min(fcast_date)][1]
    , did_change = length(fcast_date)>1
    , count = length(fcast_date)
    , outcome=unique(outcome)
    , indvote = initial_answer>0.5
    , votecorrect=indvote==outcome
    , brier_final = (final_answer - outcome)^2
    , brier_initial = (initial_answer - outcome) ^2
    , conf = abs(initial_answer-0.5)*2
  )

sum_d = ind_d %>%
  group_by(ifp_id) %>%
  summarize(
      crowd = mean(final_answer)
    , vote = mean(final_answer>=0.5)
    , crowd_bin = crowd>=0.5
    , vote_bin = vote>=0.5
    , outcome=unique(outcome)
    , vote_correct = vote_bin==outcome
    , pct_correct = mean((final_answer>=0.5)==outcome)
    , crowd_brier = (crowd-outcome)^2
    , vote_brier = (vote-outcome)^2
    , cor=cor.test(did_change*1, brier_initial)$est
    , cor2=cor.test(did_change*1, votecorrect*1)$est
    , glm=coef(glm(did_change ~ brier_initial, family="binomial"))[2]
    , glm2=coef(glm(did_change ~ votecorrect, family="binomial"))[2]
  )

glm((ind_d$outcome==(ind_d$initial_answer>0.5)) ~ ind_d$did_change) %>%
  summary

glm((ind_d$outcome==(ind_d$initial_answer>0.5)) ~ ind_d$conf) %>%
  summary

hist(sum_d$pct_correct)

ggplot(sum_d, aes(x=pct_correct, y=cor2)) +
  geom_point() +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0.5)

cor.test(sum_d$pct_correct, sum_d$cor2)
cor.test(sum_d$pct_correct, sum_d$glm2)

cor.test(ind_d$conf, ind_d$brier_initial)
cor.test(ind_d$did_change*1, ind_d$brier_initial)
