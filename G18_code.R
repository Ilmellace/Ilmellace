
library(soccermatics)
library(worldfootballR)
library(dplyr)
library(data.table)
library(fastDummies)
library(proxy)
library(rCAT)
library(tidymodels)
library(vip)
library(parsnip)
library(patchwork)
library(stringr)
library(glue)



data_shots<-fread("Progetto finale/dati.csv", sep = ";")



data_shots<-data_shots%>%
  mutate(Player=player_name,
         league_name=case_when(league_name=='1. Bundesliga'~'Bundesliga',
                               .default=league_name))%>%
  select(-c(min_added,player_name,min))%>%
  glimpse()



dati_mod<-data_shots%>%
  filter(!(situation=='FromCorner' & (y>67 |y<1)))%>%
  select(c(x,y,shot_type,situation,is_own_goal,event_type,expected_goals,
           league_name,parent_league_season,Player))%>%
  filter(!(situation%in%c('Penalty','FreeKick')) & is_own_goal=='FALSE' &
           parent_league_season%in%c('2020/2021','2021/2022'))%>%
  mutate(result=case_when(event_type=='Goal' ~ "1", event_type!='Goal' ~ "0"),
         situation=case_when(grepl('Piece',situation) ~ "SetPiece",
                             grepl('Corner',situation)~ "SetPiece",
                             grepl('Play',situation) ~ "RegularPlay",
                             grepl('Fast',situation) ~'RegularPlay'),
         shot_type=case_when(shot_type%in%c('Header','OtherBodyParts')~'NoFoot',
                             shot_type=='LeftFoot'~'Foot',
                             shot_type=='RightFoot'~'Foot'))%>%
  select(-event_type)

dati_mod<-dati_mod%>%
  select(-is_own_goal)%>%
  mutate(result=as.factor(result),
         shot_type=as.factor(shot_type),
         situation=as.factor(situation))

rm(data_shots)


ggplot(data = dati_mod)+geom_bar(aes(x=league_name,y=after_stat(count/sum(count)),
                                     fill=league_name),col='black')+
  labs(y='Frequenze Relative',x='League',title='Campionati Rappresentati')+guides(fill='none')+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_brewer(palette = "Set1")+labs(x='')

ggplot(data = dati_mod)+geom_bar(aes(x=parent_league_season,y=after_stat(count/sum(count)),
                                     fill=parent_league_season),col='black',width = 0.5)+
  labs(y='Frequenze Relative',x='Season',title='Stagioni Rappresentate')+guides(fill='none')+
  scale_y_continuous(labels=scales::percent)+scale_fill_brewer(palette = 'Dark2')+
  labs(x='')


ggplot(data=dati_mod)+geom_bar(aes(x=result,y=after_stat(count/sum(count)),fill=result),
                               col='black',width=0.5)+
  scale_fill_manual(values=c('yellow4','black'))+
  guides(fill='none')+
  labs(x='',y='Frequenze Relative',title='Result')+
  scale_x_discrete(labels=c('No Goal','Goal'))+scale_y_continuous(labels=scales::percent)

soccerPitch(arrow = "none", 
            title = "Tiri in base al risultato",theme='grass',
            lengthPitch = 105,widthPitch = 68) +
  geom_point(data = dati_mod, aes(x = x, y = y,col=result,alpha=result) ,size = 1.5)+
  theme(legend.position=c(0.15,0.5),legend.background = element_rect(fill='white'))+
  scale_color_discrete(labels=c('No Goal','Goal'),name='Risultato')+guides(alpha='none')

soccerPitch(arrow = "none", 
            title = "Tiri in base alla parte del corpo utilizzata",theme='grass',
            lengthPitch = 105,widthPitch = 68) +
  geom_point(data = dati_mod, aes(x = x, y = y, col =shot_type),
             size=1.5,alpha=0.5)+
  scale_color_manual(values=c('yellow3','purple'),name='Parte del corpo')+
  theme(legend.position=c(0.15,0.5),legend.background = element_rect(fill='white'))

soccerPitch(arrow = "none", 
            title = "Tiri in base alla situazione di gioco",theme='grass',lengthPitch = 105,widthPitch = 68) +
  geom_point(data = dati_mod, aes(x = x, y = y, col =situation),
             size=1.5,alpha=0.5)+
  scale_color_manual(values=c('orange2','grey'),name='Situazione')+
  theme(legend.position=c(0.15,0.5),legend.background = element_rect(fill='white'))




cords_goal<-rbind(c(105,30.34),c(105,37.66))

cords_shot<-dati_mod%>%
  select(x,y)

# formula di carnot
carnot<-function(x){
  goal_line=(dist(cords_goal))^2
  dist_post1=(dist(rbind(cords_goal[1,],x)))^2
  dist_post2=(dist(rbind(cords_goal[2,],x)))^2
  
  resu=(-goal_line+dist_post1+dist_post2)/(2*sqrt(dist_post1)*sqrt(dist_post2))
  angl<-acos(resu)
  return(angl)
}

# calcolo angolo (in gradi)

angle<-apply(cords_shot, 1, carnot)
dati_mod<-tibble(dati_mod,angle=rad2deg(angle))

soccerPitchHalf(lengthPitch = 105,widthPitch = 68,
                title = "Angolo di Tiro",theme='grass') +
  geom_segment(data = dati_mod[1:1,], aes(y=x,x=y,yend=105,xend=30.34),linewidth =1)+
  geom_point(aes(y=105,x=30.34))+
  geom_segment(data = dati_mod[1:1,], aes(y=x,x=y,yend=105,xend=37.66),linewidth=1)+
  geom_point(aes(y=105,x=37.66))+coord_cartesian(ylim=c(70,120))+
  geom_arc_bar(data=dati_mod[1:1,],
               aes(x0=y,y0=x,r=2,r0=0,
                   end=-cart2pol(x,y)$theta,
                   start=-(cart2pol(x,y)$theta+deg2rad(angle))),fill='red')+
  geom_label_repel(data = dati_mod[1:1,],aes(x=y+3,y=x,label=glue("{round(angle,2)}°")))
seqq<-seq(30.34,37.66,0.01)
seq1<-rep(105,length(seqq))

midpoint_goal<-cbind(105,34)

distances<-proxy::dist(cords_shot,midpoint_goal,method = 'euclidean')

dati_mod<-tibble(dati_mod,dist=as.vector(distances))

soccerPitchHalf(lengthPitch = 105,widthPitch = 68,
                title = " Distanza dalla porta",theme='grass') +
  geom_segment(data = dati_mod[1:2,], aes(y=x,x=y,yend=105,xend=34),linewidth=1,col='black')+coord_cartesian(ylim=c(70,120))+
  geom_point(data = dati_mod[1:2,],aes(x=y,y=x))+
  geom_label_repel(data=dati_mod[1:2,],aes(x=y+2,y=x,label=glue('{round(dist,2)}m')))


# split dei dati
set.seed(123)

split<-initial_split(dati_mod,strata=result,prop=0.6)
test<-testing(split)
train<-training(split)

tr_xg<-train$expected_goals
tst_xg<-test$expected_goals

train<-train%>%
  select(-expected_goals)

cores <- parallel::detectCores()

rm(dati_mod)
rm(angle)
rm(cords_goal)
rm(distances)

# randomforest
show_engines('rand_forest')

args(rand_forest)


# Per far sì che il computer lavori in parallelo utilizziamo:
cores <- parallel::detectCores()

rf <-
  rand_forest(trees= tune(),mtry=tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger",num.threads = cores,importance = "impurity")

recp_rf <-
  recipe(data=train,result ~.)%>%
  update_role(parent_league_season,league_name,Player,x,y,new_role = 'ID')%>%
  step_interact(terms= ~ dist:angle)%>%
  step_interact(terms=~ dist:shot_type)%>%
  step_dummy(shot_type)%>%
  step_dummy(situation)%>%
  step_bin2factor(starts_with('situation'),levels = c('0','1'))%>%
  step_bin2factor(starts_with('shot_type'),levels = c('0','1'))

summary(recp_rf)

wf <- workflow() %>%
  add_model(rf) %>%
  add_recipe(recp_rf)

rs<- vfold_cv(train, v = 5,strata = result)

mtry_grid=1:length(recp_rf[recp_rf$var_info$role=='predictor'])

met_vals <- metric_set(roc_auc,sens,accuracy,spec)        

ctrl     <- control_grid(verbose   = T, 
                         save_pred = TRUE,
                         allow_par = TRUE) 

rf_grid<-
  wf%>% 
  tune_grid(
    resamples = rs,
    metrics   = met_vals,
    control   = ctrl,
    grid=expand.grid(trees=seq(10,200,5),mtry=mtry_grid))

show_best(rf_grid,'roc_auc')
best_rf <- select_best(rf_grid, "roc_auc")

wf_rf <- 
  wf %>% 
  finalize_workflow(best_rf)

fit_rf <- 
  wf_rf%>% fit(data = train) 

fit_rf%>% 
  extract_fit_parsnip()%>% 
  vip(all_permutations=T)+labs(title='Importanza delle variabili')

train_pred_rf<-augment(fit_rf,train)

train_pred_rf%>%
  conf_mat(.pred_class,truth=result)

rbind(train_pred_rf%>%
                     accuracy(.pred_class,truth=result),
                   train_pred_rf%>%
                     sens(.pred_class,truth=result,event_level='second'))

test_pred_rf<-augment(fit_rf,test)%>%
  mutate(result=as.factor(result))

test_pred_rf%>%
  conf_mat(.pred_class,truth=result)


rbind(test_pred_rf%>%
                     accuracy(.pred_class,truth=result),
                   test_pred_rf%>%
                     sens(.pred_class,truth=result,event_level='second'),
                   test_pred_rf%>%
                     spec(.pred_class,truth=result,event_level='second'))

auc_train_rf<-train_pred_rf%>%
  roc_auc(truth = result,.pred_1,event_level = 'second')

roc_train_rf<-train_pred_rf%>% 
  roc_curve(truth = result, .pred_1,event_level = 'second') %>% 
  autoplot()+ggtitle('Train ROC Random Forest')+
  annotate('label',label=glue('Area={round(auc_train_rf$.estimate,2)}'),x=0.6,y=.25,
           col='black',fill='yellow')

auc_test_rf<-test_pred_rf%>%
  roc_auc(truth = result,.pred_1,event_level = 'second')

roc_test_rf<-test_pred_rf%>% 
  roc_curve(truth = result, .pred_1,event_level = 'second') %>% 
  autoplot()+ggtitle('Test ROC Random Forest')+
  annotate('label',label=glue('Area={round(auc_test_rf$.estimate,2)}'),x=0.6,y=.25,
           col='black',fill='yellow')

roc_train_rf|roc_test_rf


ggplot()+geom_histogram(data=test_pred_rf%>%mutate(Color=ifelse(.pred_1>0.5,'1','0'))
                        ,aes(x=.pred_1,fill=Color))+labs(title='Expected Goals',
                                                         x='xG')+
  scale_fill_discrete(labels=c('<=0.5','>0.5'))

# cutoff

test_pred_rf%>%
  mutate(newCut=as.factor(ifelse(.pred_1>0.15,'1','0')))%>%
  conf_mat(truth=result,newCut)

rbind(test_pred_rf%>%
                     mutate(newCut=as.factor(ifelse(.pred_1>0.15,'1','0')))%>%
                     sens(truth=result,newCut,event_level='second'),
                   test_pred_rf%>%
                     mutate(newCut=as.factor(ifelse(.pred_1>0.15,'1','0')))%>%
                     spec(truth=result,newCut,event_level='second'),
                   test_pred_rf%>%
                     mutate(newCut=as.factor(ifelse(.pred_1>0.15,'1','0')))%>%
                     accuracy(truth=result,newCut))

rm(rf_grid)

# Gradient Boosting

args(boost_tree)

grad<-boost_tree(trees= tune(), tree_depth = tune(), learn_rate = tune()) %>%
  set_mode("classification") %>%
  set_engine("xgboost", num.threads = cores,eval_metric='auc')

recp_gb <-
  recipe(data=train,result ~.)%>%
  update_role(parent_league_season,league_name,Player,x,y,new_role = 'ID')%>%
  step_interact(terms= ~ dist:angle)%>%
  step_interact(terms=~ dist:shot_type)%>%
  step_dummy(shot_type)%>%
  step_dummy(situation)

summary(recp_gb)

wf_gb <- workflow() %>%
  add_model(grad) %>%
  add_recipe(recp_gb)

rs<- vfold_cv(train, v = 5,strata = result)

met_vals <- metric_set(roc_auc, sens, accuracy,spec)        

ctrl     <- control_grid(verbose   = F, 
                         save_pred = TRUE,
                         allow_par = TRUE) 

gb_grid <-
  wf_gb%>% 
  tune_grid(
    resamples = rs,
    metrics   = met_vals,
    control   = ctrl,
    grid=expand.grid(trees=seq(10,100,10),
                     tree_depth=seq(5,10,1),learn_rate=seq(0.005,0.01,0.001)))

show_best(gb_grid, "roc_auc")

best_gb <- select_best(gb_grid, "roc_auc")


wf_gb_best <- 
  wf_gb%>% 
  finalize_workflow(best_gb)


fit_gb <-wf_gb_best %>% fit(data = train) 


# importanza delle variabili
fit_gb%>% 
  extract_fit_parsnip()%>% 
  vip(all_permutations=T)+labs(title='Importanza: Gain Score')


## sarebbe equivalente a considerare il gain score a partire da questa funzione
library(xgboost)
xgb.importance(model=fit_gb$fit$fit$fit) %>% head()


train_pred_gb<-augment(fit_gb,train)

train_pred_gb%>%
  conf_mat(.pred_class,truth=result)

rbind(train_pred_gb%>%
                     accuracy(.pred_class,truth=result),
                   train_pred_gb%>%
                     sens(.pred_class,truth=result,event_level = 'second'))


### Matrice di Confusione e Accuratezza Test Set


test_pred_gb<-augment(fit_gb,test)

test_pred_gb%>%
  conf_mat(.pred_class,truth=result)


rbind(test_pred_gb%>%
  accuracy(.pred_class,truth=result),
  test_pred_gb%>%
  sens(.pred_class,truth=result,event_level='second'))


#cutoff
ggplot()+geom_histogram(data=test_pred_gb%>%mutate(Color=ifelse(.pred_1>0.5,'1','0'))
                        ,aes(x=.pred_1,fill=Color))+
  labs(title='Expected Goals Gradient Boosting',x='xG')+
  scale_fill_discrete(labels=c('<=0.5','>0.5'))


ggplot()+geom_histogram(data=test_pred_rf,aes(x=.pred_1,fill='Random Forest'))+
  labs(title=' Confronto Expected Goals',x='xG')+
  geom_histogram(data=test_pred_gb,aes(x=.pred_1,fill='Gradient Boosting'))+
  scale_fill_manual(name='Modello',values=c('yellow2','green4'))

test_pred_gb%>%
  mutate(newCut=as.factor(ifelse(.pred_1>0.3,'1','0')))%>%
  conf_mat(truth=result,newCut)

rbind(test_pred_gb%>%
                     mutate(newCut=as.factor(ifelse(.pred_1>0.3,'1','0')))%>%
                     sens(truth=result,newCut,event_level='second'),
                   test_pred_gb%>%
                     mutate(newCut=as.factor(ifelse(.pred_1>0.3,'1','0')))%>%
                     spec(truth=result,newCut,event_level='second'),
                   test_pred_gb%>%
                     mutate(newCut=as.factor(ifelse(.pred_1>0.3,'1','0')))%>%
                     accuracy(truth=result,newCut))

auc_train_gb<-train_pred_gb%>%
  roc_auc(truth = result,.pred_1,event_level = 'second')

roc_train_gb<-train_pred_gb %>% 
  roc_curve(truth = result, .pred_1,event_level = 'second') %>% 
  autoplot()+ggtitle('Train ROC Gradient Boosting')+
  annotate('label',label=glue('Area={round(auc_train_gb$.estimate,2)}'),x=0.6,y=.25,
           col='black',fill='yellow')

auc_test_gb=test_pred_gb%>%
  roc_auc(truth = result,.pred_1,event_level = 'second')

roc_test_gb<-test_pred_gb%>% 
  roc_curve(truth = result, .pred_1,event_level = 'second') %>% 
  autoplot()+ggtitle('Test ROC Gradient Boosting')+
  annotate('label',label=glue('Area={round(auc_test_gb$.estimate,2)}'),x=0.6,y=.25,
           col='black',fill='yellow')

roc_train_gb|roc_test_gb

p1=(roc_train_rf+labs(title='')|roc_test_rf+labs(title=''))
p1=p1+plot_annotation(title = 'Random Forest',tag_levels = list(c('Train','Test')),
                      theme=theme(plot.title.position = 'plot'))

p2=(roc_train_gb+labs(title='')|roc_test_gb+labs(title=''))
p2=p2+plot_annotation(title = 'Gradient Boosting',tag_levels = list(c('Train','Test')))

wrap_elements(p1)/wrap_elements(p2)

summary(test_pred_gb$.pred_1)

confronto<-tibble(fotmob_xg=test$expected_goals,rf_xg=test_pred_rf$.pred_1,
                  goal=as.numeric(test$result),gb_xg=test_pred_gb$.pred_1)%>%
  mutate(goal=goal-1)%>%
  filter(fotmob_xg<=1)

apply(confronto,2,sum)

confront1<-confronto%>%
  slice_sample(n=1000)

N<-nrow(confront1)
N<-as.numeric(N)
n_goal=sum(confront1$goal)

sim_rf<-matrix(0,10000,N)
sim_fot<-matrix(0,10000,N)
sim_gb<-matrix(0,10000,N)


for(i in 1:N) {
  sim_rf[,i]<-rbinom(10000,1,confront1$rf_xg[i])
}

for(i in 1:N) {
  sim_fot[,i]<-rbinom(10000,1,confront1$fotmob_xg[i])
}

for(i in 1:N) {
  sim_gb[,i]<-rbinom(10000,1,confront1$gb_xg[i])
}

gol_rf<-apply(sim_rf,1,sum)
gol_gb<-apply(sim_gb,1,sum)
gol_fot<-apply(sim_fot,1,sum)

prob_rf<-table(gol_rf)
prob_rf<-prop.table(prob_rf)
prob_rf<-data.frame(prob_rf)%>%
  rename(gol=gol_rf)


prob_gb<-table(gol_gb)
prob_gb<-prop.table(prob_gb)
prob_gb<-data.frame(prob_gb)%>%
  rename(gol=gol_gb)

prob_fot<-table(gol_fot)
prob_fot<-prop.table(prob_fot)
prob_fot<-data.frame(prob_fot)%>%
  rename(gol=gol_fot)


prob_fot<-prob_fot%>%
  left_join(prob_rf,by='gol')%>%
  left_join(prob_gb,by='gol')%>%
  rename(fotmob=Freq.x,rf=Freq.y,gb=Freq)%>%
  replace_na(list(rf=0,fotmob=0,gb=0))



prob_fot<-pivot_longer(prob_fot, cols=c('fotmob','rf','gb'), names_to='variable', 
                       values_to="value")


sim_plot<-ggplot(data = prob_fot,aes(x=gol,y=value,fill=variable,alpha=gol==n_goal))+
  geom_bar(stat='identity',position='dodge')+
  labs(x = 'Gol',y = 'Probabilità',
       fill='Modello:')+scale_alpha_manual(values=c(0.5,1))+
  theme_bw()+scale_x_discrete(breaks=seq(75,150,5))+guides(alpha='none')+
  theme(legend.position = c(0.8,0.8))+scale_y_continuous(labels=scales::percent)+
  annotate('label',label=glue('Gol segnati:{n_goal}'),x=55,y=0.035,fill='yellow')+
  scale_fill_discrete(labels=c('Fotmob','Gradient Boosting','Random Forest'))
sim_plot

prob_fot%>%filter(gol==n_goal)%>%rename(Modello=variable,Probabilità=value)%>%
               mutate(Modello=case_when(Modello=='rf'~'Random Forest',
                                        Modello=='gb'~'Gradient Boosting',
                                        .default=Modello))
