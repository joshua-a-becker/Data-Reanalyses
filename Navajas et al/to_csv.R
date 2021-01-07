d = rmatio::read.mat("data_temp.mat")
str(d)
r1 = d$r_p1
r2 = d$r_p2

truth = data.frame(
  task=1:8
  , truth=d$corre
)


truth_norm = data.frame(
    task=1:8
  , truth_norm=d$ncorre
)



Qs = c(1, 3, 5, 6) #interacted questions

dim(d$group_p1[,,1])


out_df = lapply(1:280, FUN=function(i){
  r1 = data.frame(d$group_p1[,,i] )
  r2 = data.frame(d$group_p3[,,i] )
  colnames(r1) = colnames(r2) = Qs
  r1$trial = r2$trial = i
  r1$subject= r2$subject = 1:5
  r1_long = reshape2::melt(r1, id.vars=c("trial", "subject"))
  colnames(r1_long)[4]="pre_influence"
  r2_long = reshape2::melt(r2, id.vars=c("trial", "subject"))
  colnames(r2_long)[4]="post_influence"
  colnames(r1_long)[3]=colnames(r2_long)[3]="task"
  
  outdf = merge(r1_long, r2_long, by=c("trial","subject","task"))
}) %>% do.call(rbind, .) %>%
  merge(truth, by="task")


out_df_normalized = lapply(1:280, FUN=function(i){
  r1 = data.frame(d$ngroup_p1[,,i] )
  r2 = data.frame(d$ngroup_p3[,,i] )
  colnames(r1) = colnames(r2) = Qs
  r1$trial = r2$trial = i
  r1$subject= r2$subject = 1:5
  r1_long = reshape2::melt(r1, id.vars=c("trial", "subject"))
  colnames(r1_long)[4]="pre_influence_norm"
  r2_long = reshape2::melt(r2, id.vars=c("trial", "subject"))
  colnames(r2_long)[4]="post_influence_norm"
  colnames(r1_long)[3]=colnames(r2_long)[3]="task"
  
  outdf = merge(r1_long, r2_long, by=c("trial","subject","task"))
}) %>% do.call(rbind, .) %>%
  merge(truth_norm, by="task") %>%
  merge(., out_df, by=c("trial","subject","task"))

write.csv(out_df_normalized,"navajas_et_al.csv")
