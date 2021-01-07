library(stats)
library(tidyverse)

last=function(x){x[length(x)]}

d.1 = read.csv("data/survey_fcasts.yr1.csv") %>%
  mutate(year=1)
d.2 = read.csv("data/survey_fcasts.yr2.csv") %>%
  mutate(year=2) %>%
  subset(user_id %in% d.1$user_id)

d.3 = read.csv("data/survey_fcasts.yr2.csv") %>%
  mutate(year=2)


d = rbind(d.1, d.2)

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
    ctt%in%c("1a") 
    & ifp_id %in% q$ifp_id
    & answer_option == 'a'
    ) %>%
  merge(q, by="ifp_id")

ind_d=myd %>% 
  group_by(ifp_id, user_id, year) %>%
  summarize(
      final_answer = value[fcast_date==max(fcast_date)] %>% last
    , initial_answer = value[fcast_date==min(fcast_date)][1]
    , did_change = length(fcast_date)>1
    , count = length(fcast_date)
    , outcome=unique(outcome)
    , brier_final = (final_answer - outcome)^2
    , brier_initial = (initial_answer - outcome) ^2
    , conf = abs(initial_answer-0.5)*2
  )

by_user = ind_d %>%
  subset(year==1) %>%
  pivot_wider(  id_cols=c(user_id)
              , values_from=final_answer
              , names_from=ifp_id)

u_mat = by_user[,-1] %>%
  as.matrix

u_mat[is.na(u_mat)]<-0.5

my_clust = kmeans(u_mat, centers=10, nstart=25)

by_user$cluster = my_clust$cluster


with_cluster = ind_d %>% 
  merge(by_user[,c("user_id","cluster")], by="user_id") %>%
  group_by(ifp_id) %>%
  mutate(
    normalized = final_answer - mean(final_answer)
  )

aov(normalized ~ as.factor(cluster), 
    with_cluster %>% subset(year==2)
    ) %>%
  summary


re_group = with_cluster %>%
  subset(year==2)


test =  d.3 %>% subset(
  ctt%in%c("1a") 
  & ifp_id %in% q$ifp_id
  & answer_option == 'a'
  & user_id %in% with_cluster$user_id
) %>%
  merge(q, by="ifp_id") %>%
  group_by(ifp_id, user_id, year) %>%
  summarize(
    final_answer = value[fcast_date==max(fcast_date)] %>% last
    , initial_answer = value[fcast_date==min(fcast_date)][1]
    , did_change = length(fcast_date)>1
    , count = length(fcast_date)
    , outcome=unique(outcome)
    , brier_final = (final_answer - outcome)^2
    , brier_initial = (initial_answer - outcome) ^2
    , conf = abs(initial_answer-0.5)*2
  ) %>%
  group_by(ifp_id) %>%
  mutate(
    normalized = final_answer - mean(final_answer)
  ) %>%
  merge(by_user[,c("user_id","cluster")], by="user_id")
  

aov(normalized ~ as.factor(cluster), 
    test
) %>%
  summary


### try some regrouping...

assemble_group = function(n, k, clusters) {
  use_clusters = sample(unique(clusters), k)
  sample(which(clusters %in% use_clusters), n)
}


synthetic = data.frame(
  num_groups=numeric()
  ,est=numeric()
  ,ifp_id=character()
  , clust_id = numeric()
  , m=numeric()
)
n=10
for(i in 1:1000) {
  print(i)
  for(m in 1:10) {
    for(q_id in unique(test$ifp_id)) {
      
      tryCatch(
        {    
          dat = subset(test, ifp_id == q_id)
          group = dat[assemble_group(n, m, dat$cluster),]
          synthetic[nrow(synthetic)+1,] = data.frame(
            num_groups = length(unique(group$cluster)) 
            , est = mean(group$final_answer)
            , ifp_id = q_id
            , clust_id = mean(group$cluster)
            ,m=m
          )
        }, error=function(e){}
      )
    }
  }
}


final_d = synthetic %>%
  merge(q[,c("ifp_id","outcome")]) %>%
  mutate(
    brier = (est-outcome*1)^2
  )


ggplot(final_d, aes(x=m, y=brier)) +
  stat_summary(fun="mean", geom="point")

ggplot(final_d, aes(x=num_groups, y=brier)) +
  stat_summary(fun="mean", geom="point")


my_stats = final_d %>%
  group_by(ifp_id) %>%
  summarize(
    cor = cor.test(brier, num_groups)$est
  )

t.test(my_stats$cor)
