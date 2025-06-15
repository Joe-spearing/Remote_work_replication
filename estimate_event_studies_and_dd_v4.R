rm(list=ls())
library(fixest)
library(lmtest)
library(synthdid)
library(did)
library(stringr)
library(stargazer)
library(car)
library(DescTools)

###########
#Load Data#
###########

#setwd('\\\\storage.its.york.ac.uk\\che\\Projects\\remote_work\\UKDA-6931-stata\\stata\\stata13_se\\ukhls')
setwd('Z:\\Projects\\remote_work\\UKDA-6931-stata\\stata\\stata13_se\\ukhls')
full_df<-read.csv('US_data_remote_work_v2.CSV')

###############
#data cleaning#
###############

#drop negatives
vars.with.negatives<-
  c(  "pid",
      "employment_stat",          "age",                   
      "sex",                      "GHQ1",                   
      "GHQ2",                     "GHQ3",                   
      "GHQ4",                     "GHQ5",                   
      "GHQ6",                     "GHQ7",                   
      "GHQ8",                     "GHQ9",                  
      "GHQ10",                    "GHQ11",           
      "GHQ12",                    "marital_stat",  
      "education",                "monthly_labor_income",    
      "full_time",                "working_hours",           
      "paid_work",                "region",                  
      "wave_number",              "year",                    
      "month",                    "day",        
      "occupation",               "last_occupation",     
      "SF12_mh",                  "SF12_ph",                
      "health_in_general",               
      "use_remote_work",          "no_kids",                 
      "weights",                  "job_level",
      "home_owner",              "ethnicity",
      'job_satisfaction',
      'wkaut1', 'wkaut2',
      'wkaut3','wkaut4',
      'wkaut5','lonely')
for (var in vars.with.negatives){
  full_df[,var]<-ifelse(full_df[,var]<0,NA,
                        full_df[,var])
}

full_df[,"offer_remote_work"]<-
  ifelse(is.na(full_df[,"offer_remote_work"]),
         NA,full_df[,"offer_remote_work"])
full_df[,"use_remote_work"]<-
  ifelse(is.na(full_df[,"use_remote_work"]),
         NA,full_df[,"use_remote_work"])
full_df[,"use_remote_work"]<-
  ifelse(full_df[,"offer_remote_work"]==0,0,
         full_df[,"use_remote_work"])

#Construct use and offer remote work which is a purely binary treatment
offer.remote.work.incl.nw<-
  as.data.frame(ifelse(full_df[,"offer_remote_work"]<0,0,
                       full_df[,"offer_remote_work"]))
use.remote.work.incl.nw<-
  as.data.frame(ifelse(full_df[,"use_remote_work"]<0|full_df[,"offer_remote_work"]<=0,0,
                       full_df[,"use_remote_work"]))

colnames(offer.remote.work.incl.nw)<-'offer_remote_work_incl_nw'
colnames(use.remote.work.incl.nw)<-'use_remote_work_incl_nw'

full_df<-cbind(full_df,offer.remote.work.incl.nw,use.remote.work.incl.nw)

#df<-subset(full_df,use_remote_work<0&offer_remote_work>=0)

full_df[,"offer_remote_work"]<-
  ifelse(full_df[,"offer_remote_work"]<0,
         NA,full_df[,"offer_remote_work"])
full_df[,"use_remote_work"]<-
  ifelse(full_df[,"use_remote_work"]<0,
         NA,full_df[,"use_remote_work"])

anxiety.and.depression<-as.data.frame(full_df[,'GHQ2']+
                                        full_df[,'GHQ5']+
                                        full_df[,'GHQ6']+
                                        full_df[,'GHQ9'])
colnames(anxiety.and.depression)<-'anxiety_and_depression'  

loss.of.confidence<-as.data.frame(full_df[,'GHQ10']+
                                    full_df[,'GHQ11'])
colnames(loss.of.confidence)<-'loss_of_confidence'

social.dysfunction<-as.data.frame(full_df[,'GHQ1']+
                                    full_df[,'GHQ3']+
                                    full_df[,'GHQ4']+
                                    full_df[,'GHQ7']+
                                    full_df[,'GHQ8']+
                                    full_df[,'GHQ12'])
colnames(social.dysfunction)<-'social_dysfunction'

for (val in seq(from=1,to=12,by=1)){
  full_df[,paste('GHQ',val,sep='')]<-ifelse(full_df[,paste('GHQ',val,sep='')]<3,
                                            0,1)
}

ghq12.caseness<-as.data.frame(full_df[,'GHQ1']+
                                full_df[,'GHQ2']+
                                full_df[,'GHQ3']+
                                full_df[,'GHQ4']+
                                full_df[,'GHQ5']+
                                full_df[,'GHQ6']+
                                full_df[,'GHQ7']+
                                full_df[,'GHQ8']+
                                full_df[,'GHQ9']+
                                full_df[,'GHQ10']+
                                full_df[,'GHQ11']+
                                full_df[,'GHQ12'])

colnames(ghq12.caseness)<-'GHQ12_caseness'
full_df<-cbind(full_df,ghq12.caseness,anxiety.and.depression,
               loss.of.confidence,social.dysfunction)

high.caseness<-as.data.frame(ifelse(full_df[,'GHQ12_caseness']>8,1,0))
colnames(high.caseness)<-'high_caseness'
full_df<-cbind(full_df,high.caseness)

full_df[,'sex']<-full_df[,'sex']-1
full_df[,'paid_work']<-ifelse(full_df[,'paid_work']==1,1,0)
#turn remote workability into a treatment
remote.work.treat<-as.data.frame(ifelse(full_df[,'teleworkability_2019_occ']>0.5,
                                        1,
                                        0))
colnames(remote.work.treat)<-'remote_work_treat'
has.kids<-as.data.frame(ifelse(full_df[,'no_kids']>0,
                               1,
                               0))
colnames(has.kids)<-'has_kids'

#create a marriage variable which works for me
my.mar.var<-as.data.frame(ifelse(full_df[,'marital_stat']==2|
                                   full_df[,'marital_stat']==3,1,0))
colnames(my.mar.var)<-'my_mar_stat'

white<-as.data.frame(ifelse(full_df[,'ethnicity']<5,1,0))
colnames(white)<-'white'

owns.home<-as.data.frame(ifelse(full_df[,'home_owner']<3,1,0))
colnames(owns.home)<-'owns_home'

new.educ<-as.data.frame(ifelse(full_df[,'education']<3,1,
                               ifelse(full_df[,'education']>2&full_df[,'education']<5,2,
                                      ifelse(full_df[,'education']>4,3,0))))
colnames(new.educ)<-'new_educ'

lockdown<-as.data.frame(
  ifelse((full_df[,'month']==3&full_df[,'day']>22&full_df[,'year']==2020)|
           (full_df[,'month']>3&full_df[,'month']<7&full_df[,'year']==2020)|
           (full_df[,'month']==7&full_df[,'day']<4&full_df[,'year']==2020)|
           (full_df[,'month']==11&full_df[,'day']>4&full_df[,'year']==2020)|
           (full_df[,'month']==12&full_df[,'day']<2&full_df[,'year']==2020)|
           (full_df[,'month']==1&full_df[,'day']>5&full_df[,'year']==2021)|
           (full_df[,'month']>2&full_df[,'month']<4&full_df[,'year']==2021)|
           (full_df[,'month']==4&full_df[,'day']<12&full_df[,'year']==2021),
         1,0))
colnames(lockdown)<-'lockdown'

one.dig.occ<-as.data.frame(substr(full_df[,'occupation'],1,1))
colnames(one.dig.occ)<-'one_dig_occ'

full_df<-cbind(full_df,remote.work.treat,has.kids,my.mar.var,
               white,owns.home,new.educ,lockdown,one.dig.occ)

#Add in year doubles
year.double<-as.data.frame(
  ifelse(full_df[,'year']==2014|full_df[,'year']==2015,20142015,
         ifelse(full_df[,'year']==2016|full_df[,'year']==2017,20162017,
                ifelse(full_df[,'year']==2018|full_df[,'year']==2019,20182019,
                       ifelse(full_df[,'year']==2020|full_df[,'year']==2021,20202021,
                              ifelse(full_df[,'year']==2022|full_df[,'year']==2023,20222023,0))))))

colnames(year.double)<-'year_doubles'
full_df<-cbind(full_df,year.double)

#allocate personality variables to each periods
for (per.var in
     c("agreeableness",
       "conscientiousness",
       "extraversion",
       "neuroticism",
       "openness")){
  
  df<-subset(full_df,agreeableness>0)
  colnames(df)[match(per.var,colnames(df))]<-'this_per_var'
  
  agg.by.pid<-aggregate(this_per_var~pid,data=df,FUN=mean)
  
  per.var.df<-as.data.frame(agg.by.pid[match(full_df[,'pid'],agg.by.pid[,1]),2])
  colnames(per.var.df)<-paste(per.var,'_all_waves',sep='')
  full_df<-cbind(full_df,per.var.df)
  
}

#allocate some variables in 2019 to all periods
for (var in
     c("has_kids",
       "SF12_ph",
       "health_in_general",
       "commute_time","job_level",'owns_home','paid_work',
       'education','one_dig_occ','marital_stat',
       'my_mar_stat')){
  
  df<-subset(full_df,year==2019)
  colnames(df)[match(var,colnames(df))]<-'this_var'
  df<-subset(df,this_var>=0)
  
  #only latest wave if applicable
  agg.by.pid<-aggregate(wave_number~pid,data=df,FUN=max)
  last_wave<-agg.by.pid[match(full_df[,'pid'],agg.by.pid[,1]),2]
  
  var.df<-
    as.data.frame(df[match(interaction(full_df[,'pid'],last_wave),
                           interaction(df[,'pid'],df[,'wave_number'])),
                     'this_var'])
  
  colnames(var.df)<-paste(var,'_2019_year',sep='')
  full_df<-cbind(full_df,var.df)
  
}

#Need a first treat variable for C&S estimator
first.treat.yr<-as.data.frame(ifelse(full_df[,'remote_work_treat']==1,
                                  2020,2025))
colnames(first.treat.yr)<-'first_treat_year'
first.treat.yr.double<-as.data.frame(ifelse(full_df[,'remote_work_treat']==1,
                                     20202021,20242025))
colnames(first.treat.yr.double)<-'first_treat_year_doubles'
full_df<-cbind(full_df,first.treat.yr,first.treat.yr.double)

for (var in c('has_kids_2019_year',
              'education_2019_year',
              'marital_stat_2019_year',
              'owns_home_2019_year',
              'job_level_2019_year',
              'one_dig_occ_2019_year',
              'my_mar_stat_2019_year')){
  for (x in unique(full_df[,var])){
    
    new.var<-as.data.frame(
      ifelse(full_df[,var]==x,
             1,
             0))
    colnames(new.var)<-paste(
      var,'is',x,sep=''
    )
    
    full_df<-cbind(full_df,new.var)
    
  }
}

for (var in c('wkaut1','wkaut2','wkaut3',
              'wkaut4','wkaut5')){
  
  full_df[,var]<-ifelse(full_df[,var]<3,0,1)
  
}

overall.low.autonomy<-as.data.frame(full_df[,'wkaut1']+
                                      full_df[,'wkaut2']+
                                      full_df[,'wkaut3']+
                                      full_df[,'wkaut4']+
                                      full_df[,'wkaut5'])
colnames(overall.low.autonomy)<-'low_autonomy_summary'
full_df<-cbind(full_df,overall.low.autonomy)

med.job.sat<-median(subset(full_df,job_satisfaction>=0)[,'job_satisfaction'])

high.job.satisfaction<-as.data.frame(
                      ifelse(full_df[,'job_satisfaction']>
                                med.job.sat,1,0))
colnames(high.job.satisfaction)<-'high_job_satisfaction'

full_df<-cbind(full_df,high.job.satisfaction)

is.v.lonely<-as.data.frame(ifelse(full_df[,'lonely']==3,1,0))
colnames(is.v.lonely)<-'is_often_lonely'
is.lonely<-as.data.frame(ifelse(full_df[,'lonely']==1,0,1))
colnames(is.lonely)<-'is_lonely'
full_df<-cbind(full_df,is.lonely,is.v.lonely)

#loneliness by year
lonely.year<-lm(is_lonely~factor(year),data=full_df)
v.lonely.year<-lm(is_often_lonely~factor(year),data=full_df)
par(mar=c(4,2,2,2),mfrow=c(1,2))
plot(seq(from=2017,to=2023,by=1),
     c(coef(lonely.year)[1],
       coef(lonely.year)[1]+
       coef(lonely.year)[paste('factor(year)',
                        seq(from=2018,to=2023,by=1),
                           sep='')]),type='l',
     main='Lonely',xlab=' ')
plot(seq(from=2017,to=2023,by=1),
      c(coef(v.lonely.year)[1],
        coef(v.lonely.year)[1]+
          coef(v.lonely.year)[paste('factor(year)',
                                  seq(from=2018,to=2023,by=1),
                                  sep='')]),
     main='Often lonely',type='l',xlab=' ')

#carer variables
table(full_df[,'care_hours'])
cares<-as.data.frame(ifelse(full_df[,'care_hours']==(-8),0,
              ifelse(full_df[,'care_hours']>0,1,
                     NA)))
cares.intensively<-as.data.frame(ifelse(full_df[,'care_hours']==(-8)|
                                  full_df[,'care_hours']==1|
                                  full_df[,'care_hours']==2,0,
                            ifelse(full_df[,'care_hours']>2,1,
                                   NA)))
colnames(cares)<-'cares'
colnames(cares.intensively)<-'cares_intensively'

full_df<-cbind(full_df,cares,cares.intensively)

#some employment variables
employed<-as.data.frame(as.numeric(full_df[,'employment_stat']<3))
unemployed<-as.data.frame(as.numeric(full_df[,'employment_stat']==3))
retired<-as.data.frame(as.numeric(full_df[,'employment_stat']==4))
colnames(employed)<-'employed'
colnames(unemployed)<-'unemployed'
colnames(retired)<-'retired'
full_df<-cbind(full_df,employed,retired,unemployed)

#create a list of dependent variables
dependent.variables<-c("GHQ12_caseness",                
                       "anxiety_and_depression", "loss_of_confidence",
                       "social_dysfunction", 'SF12_mh',
                       'wkaut1','wkaut2','wkaut3','wkaut4','wkaut5',
                       'low_autonomy_summary',
                        'job_satisfaction','high_job_satisfaction',
                       'high_caseness','use_remote_work_incl_nw',
                       'is_lonely','is_often_lonely','employed',
                       'unemployed','retired',
                       'cares','cares_intensively')

full_df[,'year']<-as.numeric(as.character(full_df[,'year']))

############################
#Treatment propensity scores
############################
#p(attrit) in each year:

pids.in.sample<-unique(subset(df,year==2019)[,'pid'])

observations.df<-as.data.frame(
  cbind(c(pids.in.sample,
          pids.in.sample,
          pids.in.sample,
          pids.in.sample,
          pids.in.sample),
        c(seq(from=2020,to=2020,length.out=length(pids.in.sample)),
          seq(from=2021,to=2021,length.out=length(pids.in.sample)),
          seq(from=2022,to=2022,length.out=length(pids.in.sample)),
          seq(from=2023,to=2023,length.out=length(pids.in.sample)),
          seq(from=2024,to=2024,length.out=length(pids.in.sample))))
)
colnames(observations.df)<-c('pid','year')
observed<-as.numeric(
  is.na(match(interaction(observations.df[,'pid'],
                          observations.df[,'year']),
              interaction(full_df[,'pid'],
                          full_df[,'year'])))==FALSE)
sex<-subset(full_df,year==2019)[match(observations.df[,'pid'],
                                      subset(full_df,year==2019)[,'pid']),'sex']
age<-subset(full_df,year==2019)[match(observations.df[,'pid'],
                                      subset(full_df,year==2019)[,'pid']),'age']
marital_stat_2019_year<-subset(full_df,year==2019)[match(observations.df[,'pid'],
                                                         subset(full_df,year==2019)[,'pid']),'marital_stat_2019_year']
education_2019_year<-subset(full_df,year==2019)[match(observations.df[,'pid'],
                                                      subset(full_df,year==2019)[,'pid']),'education_2019_year']
job_level_2019_year<-subset(full_df,year==2019)[match(observations.df[,'pid'],
                                                      subset(full_df,year==2019)[,'pid']),'job_level_2019_year']
one_dig_occ_2019_year<-subset(full_df,year==2019)[match(observations.df[,'pid'],
                                                        subset(full_df,year==2019)[,'pid']),'one_dig_occ_2019_year']
has_kids_2019_year<-subset(full_df,year==2019)[match(observations.df[,'pid'],
                                                     subset(full_df,year==2019)[,'pid']),'has_kids_2019_year']
remote_work_treat<-subset(full_df,year==2019)[match(observations.df[,'pid'],
                                                    subset(full_df,year==2019)[,'pid']),'remote_work_treat']
owns_home_2019_year<-subset(full_df,year==2019)[match(observations.df[,'pid'],
                                                      subset(full_df,year==2019)[,'pid']),'owns_home_2019_year']

observations.df<-cbind(observations.df,
                       observed,
                       sex,
                       age,
                       marital_stat_2019_year,
                       education_2019_year,
                       has_kids_2019_year,
                       job_level_2019_year,
                       one_dig_occ_2019_year,
                       remote_work_treat,
                       owns_home_2019_year)

observed.model<-glm(observed ~ factor(year)*age+
                      factor(year)*sex+
                      factor(year)*marital_stat_2019_year+
                      factor(year)*education_2019_year+
                      factor(year)*has_kids_2019_year+
                      factor(year)*job_level_2019_year+
                      factor(year)*one_dig_occ_2019_year+
                      factor(year)*remote_work_treat+
                      factor(year)*owns_home_2019_year,
                    family = binomial(link = "logit"), 
                    data = subset(observations.df,
                                  age>0&
                                  sex>=0&
                                  marital_stat_2019_year>0&
                                  education_2019_year>0&
                                  has_kids_2019_year>=0&
                                  job_level_2019_year>0&
                                  one_dig_occ_2019_year>0&
                                  remote_work_treat>=0&
                                  owns_home_2019_year>=0))

var(observed.model$fitted.values)/
  var(observations.df[,'observed'])

stargazer(PseudoR2(observed.model))

p.observed.key<-as.data.frame(
  cbind(observed.model$fitted.values,
        observed.model$data[,'pid'],
        observed.model$data[,'year'])
)
colnames(p.observed.key)<-c('fitted_value','pid','year')

fitted.p.observed<-as.data.frame(
  p.observed.key[match(interaction(full_df[,'pid'],
                                 full_df[,'year']),
                     interaction(p.observed.key[,'pid'],
                                 p.observed.key[,'year'])),
               'fitted_value'])
colnames(fitted.p.observed)<-'p_observed'
full_df<-cbind(full_df,fitted.p.observed)

full_df[,'p_observed']<-ifelse(full_df[,'year']<2020,1,
                               full_df[,'p_observed'])

#For synth dd need a treated variable
treated<-as.data.frame(
        full_df[,'remote_work_treat']*(full_df[,'year']>2019))
colnames(treated)<-'treated'
full_df<-cbind(full_df,treated)

####################
#create year dummies
####################

year.dummy<-as.data.frame(as.factor(full_df[,'year']))
colnames(year.dummy)<-'year_dummy'
year.doubles.dummy<-as.data.frame(as.factor(full_df[,'year_doubles']))
colnames(year.doubles.dummy)<-'year_doubles_dummy'
full_df<-cbind(full_df,year.dummy,year.doubles.dummy)
full_df[,'year_dummy']<-relevel(full_df[,'year_dummy'],ref='2019')
full_df[,'year_doubles_dummy']<-relevel(full_df[,'year_doubles_dummy'],ref='20182019')

####################
#Drop pre-2014 years and post 2022 years
####################

full_df<-subset(full_df,year>=2014&year<2023)

###############
#Event studies#
###############

#Store the event study coefficients in these matrices
results.data.frame<-as.data.frame(matrix(ncol=23,nrow=0))

colnames(results.data.frame)<-
  c('sex','estimate','sample',
    'period_definition',
    'estimator','2014','2015','2016','2017',
    '2018','2020','2021','2022',
    '20142015','20162017',
    '20202021','20222023','dd_est',
    'N','pre_treat_mean','dependent_variable',
    'pre_trend_p_value','subset')

#the set of estimators is:
#TWFE (done)
#TWFE with controls (done)
#C&S (done)
#C&S with PS weighting (done)
#IPW (done)
#IPW with controls (done)
#balanced sample
#balanced sample with controls
#synth dd
#synth dd with controls
residualize_y<-function(dep_var,year_type,df){
  
  formula<-as.formula(
    paste(dep_var,
          '~factor(',year_type,')*factor(education_2019_year)',
          '+factor(',year_type,')*factor(my_mar_stat_2019_year)',
          '+factor(',year_type,')*factor(owns_home_2019_year)',
          '+factor(',year_type,')*factor(has_kids_2019_year)',
          '|pid+',year_type,sep=''))
  
  my.reg<-feols(formula,cluster=~pid,
                se='cluster',
                data=subset(df,(remote_work_treat==0|year<2020)))

  
  y_res<-df[,dep_var]
  
  #residualize education
  for (y in unique(df[,year_type])[2:length(unique(df[,year_type]))]){
    for (e in c(2,3,4,5,9)){
      y_res<-y_res-
    my.reg$coefficients[paste('factor(',year_type,')',y,
    ':factor(education_2019_year)',e,sep='')]*
        (df[,'year']==y)*
        (df[,'education_2019_year']==e)
    }
  }
  #residualize mar stat
  for (y in unique(df[,year_type])[2:length(unique(df[,year_type]))]){
      y_res<-y_res-
        my.reg$coefficients[paste('factor(',year_type,')',y,
                        ':factor(my_mar_stat_2019_year)1',sep='')]*
      (df[,'year']==y)*
        (df[,'my_mar_stat_2019_year']==1)
  }
  #residualize home owner
  for (y in unique(df[,year_type])[2:length(unique(df[,year_type]))]){
      y_res<-y_res-
        my.reg$coefficients[paste('factor(',year_type,')',y,
                  ':factor(owns_home_2019_year)1',sep='')]*
      (df[,'year']==y)*
        (df[,'owns_home_2019_year']==1)
  }
  #residualize having kids
  for (y in unique(df[,year_type])[2:length(unique(df[,year_type]))]){
    y_res<-y_res-
      my.reg$coefficients[paste('factor(',year_type,')',y,
                                ':factor(has_kids_2019_year)1',sep='')]*
    (df[,'year']==y)*
      (df[,'has_kids_2019_year']==1)
  }
  
  #df1<-cbind(df,y_res)

  return(y_res)
  
}

estimate_twfe<-function(dep_var,year_type,df){
  
  formula<-as.formula(
    paste(dep_var,'~(year>2019)*remote_work_treat+factor(',year_type,')',
          '|pid',sep=''))
  
  my.reg<-feols(formula,cluster=~pid,
                se='cluster',
                data=df)
  
  df<-df[is.na(df[,dep_var])==FALSE,]
  
  pre.trend.periods<-sort(unique(subset(df,year<2020)[,year_type]))
  
  reference.period<-unique(df[,year_type])[substr(unique(df[,year_type]),
                          str_length(unique(df[,year_type])),              
                          str_length(unique(df[,year_type])))==9]
  df[,year_type]<-as.factor(df[,year_type])
  df[,year_type]<-relevel(df[,year_type],
                ref=as.character(reference.period))
  
  formula<-as.formula(
    paste(dep_var,'~factor(',year_type,')*remote_work_treat+factor(',year_type,')',
          '|pid',sep=''))
  
  es.reg<-feols(formula,cluster=~pid,
                se='cluster',
                data=df)
  
  time.list<-sort(unique(df[,year_type]))
  time.list<-time.list[substr(time.list,str_length(time.list)-3,str_length(time.list))!='2019']
  
  F.test<-linearHypothesis(es.reg, 
                           paste('factor(',year_type,')',
                                 pre.trend.periods[1:(length(pre.trend.periods)-1)],
                                 ':remote_work_treat=0',sep=''),
                   white.adjust = "hc1")
  
  ouputs<-c(my.reg$coeftable['year > 2019TRUE:remote_work_treat',1],
            my.reg$coeftable['year > 2019TRUE:remote_work_treat',2],
            es.reg$coeftable[paste('factor(',year_type,')',time.list,':remote_work_treat',sep=''),1],
            es.reg$coeftable[paste('factor(',year_type,')',time.list,':remote_work_treat',sep=''),2],
            1-my.reg$ssr/my.reg$ssr_null,
            mean(df[,dep_var][is.na(df[,dep_var])==FALSE]),
            length(df[,dep_var][is.na(df[,dep_var])==FALSE&
                                  is.na(df[,'remote_work_treat'])==FALSE&
                                  is.na(df[,'year'])==FALSE]),
            F.test$`Pr(>Chisq)`[2])
  
  names(ouputs)<-c('dd_est','dd_se',
                   paste('est_factor(',year_type,')',time.list,':remote_work_treat',sep=''),
                   paste('se_factor(',year_type,')',time.list,':remote_work_treat',sep=''),
                   'R_sq',
                   'pre_treat_mean',
                   'N','pretrend_p-value')
  
  return(ouputs)
  
}

estimate_twfe_ipw<-function(dep_var,year_type,df){
  
  df<-df[is.na(df[,dep_var])==FALSE,]
  
  formula<-as.formula(
    paste(dep_var,'~(year>2019)*remote_work_treat+factor(',year_type,')',
          '|pid',sep=''))
  
  my.reg<-feols(formula,cluster=~pid,
                se='cluster',
                data=df,
                weights=1/df[,'p_observed'])
  
  pre.trend.periods<-sort(unique(subset(df,year<2020)[,year_type]))
  
  reference.period<-unique(df[,year_type])[substr(unique(df[,year_type]),
                                                  str_length(unique(df[,year_type])),              
                                                  str_length(unique(df[,year_type])))==9]
  df[,year_type]<-as.factor(df[,year_type])
  df[,year_type]<-relevel(df[,year_type],
                          ref=as.character(reference.period))
  
  formula<-as.formula(
    paste(dep_var,'~factor(',year_type,')*remote_work_treat+factor(',year_type,')',
          '|pid',sep=''))
  
  es.reg<-feols(formula,cluster=~pid,
                se='cluster',
                data=df,
                weights=1/df[,'p_observed'])
  
  time.list<-sort(unique(df[,year_type]))
  time.list<-time.list[substr(time.list,str_length(time.list)-3,str_length(time.list))!='2019']
  
  F.test<-linearHypothesis(es.reg, 
                           paste('factor(',year_type,')',
                                 pre.trend.periods[1:(length(pre.trend.periods)-1)],
                                 ':remote_work_treat=0',sep=''),
                           white.adjust = "hc1")
  
  ouputs<-c(my.reg$coeftable['year > 2019TRUE:remote_work_treat',1],
            my.reg$coeftable['year > 2019TRUE:remote_work_treat',2],
            es.reg$coeftable[paste('factor(',year_type,')',time.list,':remote_work_treat',sep=''),1],
            es.reg$coeftable[paste('factor(',year_type,')',time.list,':remote_work_treat',sep=''),2],
            1-my.reg$ssr/my.reg$ssr_null,
            mean(df[,dep_var][is.na(df[,dep_var])==FALSE]),
            length(df[,dep_var][is.na(df[,dep_var])==FALSE&
                                  is.na(df[,'remote_work_treat'])==FALSE&
                                  is.na(df[,'year'])==FALSE]),
            F.test$`Pr(>Chisq)`[2])
  
  names(ouputs)<-c('dd_est','dd_se',
                   paste('est_factor(',year_type,')',time.list,':remote_work_treat',sep=''),
                   paste('se_factor(',year_type,')',time.list,':remote_work_treat',sep=''),
                   'R_sq',
                   'pre_treat_mean',
                   'N','pretrend_p-value')
  
  return(ouputs)
  
}

get_people_in_1_year_balanced_panel<-function(df){
  
  df<-subset(df,year<2024&year>=2014&year!=0)
  year.by.pid<-as.data.frame(as.matrix(table(df[,'year'],df[,'pid'])))
  pids.to.drop<-unique(subset(year.by.pid,Freq==0)[,'Var2'])
  pid.list<-unique(subset(df,pid%in%pids.to.drop==FALSE)[,'pid'])
  
  return(pid.list)
  
}
get_people_in_2_year_balanced_panel<-function(df){
  
  pids.to.drop<-unique(subset(as.data.frame(as.matrix(table(df[,'year_doubles'],df[,'pid']))),Freq==0)[,'Var2'])
  pid.list<-unique(subset(df,pid%in%pids.to.drop==FALSE)[,'pid'])
  
  return(pid.list)
  
}

create_synth_dd_data_frame<-function(pid.list,df){
  
  df<-subset(df,pid%in%pid.list&year>=2014)
  return(df)
  
}


drop_multiples<-function(df,year_var){
  
  pid.int.year<-interaction(df[,year_var],df[,'pid'])
  df<-cbind(df,pid.int.year)
  max.waves<-aggregate(wave_number~pid.int.year,FUN=max,data=df)
  df<-df[match(interaction(max.waves[,1],max.waves[,2]),
               interaction(df[,year_var],df[,'pid'],df[,'wave_number'])),]
  return(df)
  
}

#create a list of datasets based on subsetting
ever.work.remote.pre.2020<-unique(subset(full_df,year<2020&use_remote_work_incl_nw==1)[,'pid'])
last.pre.treat.wave<-aggregate(year~pid,FUN=max,
  data=unique(subset(full_df,year<2020)))
last.pre.treat.wave<-cbind(last.pre.treat.wave,
                           full_df[match(interaction(last.pre.treat.wave[,1],last.pre.treat.wave[,2]),
                                         interaction(full_df[,'pid'],full_df[,'year'])),'n_adults_in_hh'])

adults.pre.treatment<-as.data.frame(last.pre.treat.wave[match(full_df[,'pid'],last.pre.treat.wave[,1]),3])
colnames(adults.pre.treatment)<-'adults_pre_treatment'
full_df<-cbind(full_df,adults.pre.treatment)

df.list<-list(full_df,
              subset(full_df,pid%in%ever.work.remote.pre.2020==FALSE),
              subset(full_df,marital_stat_2019_year==2|marital_stat_2019_year==3),
              subset(full_df,marital_stat_2019_year!=2&marital_stat_2019_year!=3))

subset.list<-c('all',
               'in-person work before 2020',
               'married',
               'not_married')

for (dep_var in dependent.variables){
  for (s in c(0,1,2)){
#      for (pop.subset in seq(from=1,to=4,by=1)){
        for (pop.subset in seq(from=1,to=1,by=1)){
        for (yr in c('year','year_doubles')){
  
  df<-as.data.frame(df.list[pop.subset])
      
  time.list<-sort(unique(df[is.na(df[,dep_var])==FALSE,yr]))
  time.list<-time.list[substr(time.list,str_length(time.list)-3,
                              str_length(time.list))!='2019']
  
  ##############
  #C&S with PSW#
  ##############
  
  #I don't like this solution but the matrix is non-singular for some
  #variables if the last year is included:
  if(pop.subset>2){
    df_cs<-subset(df,year<2022)
  }else{
    df_cs<-df
  }

  new.data<-as.data.frame(matrix(nrow=2,ncol=23))
  colnames(new.data)<-
    c('sex','estimate','sample',
      'period_definition',
      'estimator','2014','2015','2016','2017',
      '2018','2020','2021','2022',
      '20142015','20162017',
      '20202021','20222023','dd_est',
      'N','pre_treat_mean','dependent_variable',
      'pre_trend_p_value','subset')
  
    c.and.s<-att_gt(yname = dep_var,
                  tname = yr,
                  idname = "pid",
                  gname = paste("first_treat_",yr,sep=''),
                  xformla = ~
                    my_mar_stat_2019_yearis1+
                    has_kids_2019_year+
                    owns_home_2019_year+
                    education_2019_yearis2+
                    education_2019_yearis3+
                    education_2019_yearis4+
                    education_2019_yearis5+
                    education_2019_yearis9,
                  data = subset(df_cs,sex<=s&
                                  sex>=max(ceiling(s/2)-max(s-1,0),0)),
                  allow_unbalanced_panel=TRUE
  )
  
  new.data[,'sex']<-c('men','women','both')[s+1]
  new.data[,'subset']<-subset.list[pop.subset]
  new.data[,'estimate']<-c('point','se')
  new.data[,'sample']<-'unbalanced'
  new.data[,'period_definition']<-yr
  new.data[,'estimator']<-'C&S_with_PS'
  new.data[,'dependent_variable']<-dep_var
  
  if(dep_var!='use_remote_work_incl_nw'|pop.subset!=2){
    new.data[,'pre_trend_p_value']<-c(c.and.s$Wpval,c.and.s$Wpval)
  }
  
  
  es <- aggte(c.and.s, type = "dynamic")
  
  new.data[1,match(time.list[1:length(es$att.egt)],colnames(new.data))]<-es$att.egt
  new.data[2,match(time.list[1:length(es$att.egt)],colnames(new.data))]<-es$se.egt
  
  new.data[1,'dd_est']<-es$overall.att
  new.data[2,'dd_est']<-es$overall.se
  
  new.data[,'N']<-sum(is.na(subset(df_cs,my_mar_stat_2019_year>=0&
                                has_kids_2019_year>=0&
                                owns_home_2019_year>=0&
                                education_2019_year>0&sex<=s&
                                  sex>=max(ceiling(s/2)-max(s-1,0),0))[,dep_var])==FALSE)
  
  pre.treat.vals<-subset(df_cs,sex<=s&
                           sex>=max(ceiling(s/2)-max(s-1,0),0)&
                           year<2020&
                           owns_home_2019_year>=0&
                           education_2019_year>=0&
                           my_mar_stat_2019_year>=0&
                           has_kids_2019_year>=0)[,dep_var]
  
  new.data[,'pre_treat_mean']<-
    sum(pre.treat.vals[is.na(pre.treat.vals)==FALSE])/
    length(pre.treat.vals[is.na(pre.treat.vals)==FALSE])
  
  
  results.data.frame<-rbind(results.data.frame,
                            new.data)

}
    
    #synthetic dd results
    
    if(dep_var %in%
       c("wkaut1","wkaut2",
         "wkaut3","wkaut4",                
         "wkaut5", "use_remote_work_incl_nw",
         'low_autonomy_summary')==FALSE){
    
    ######################  
    #1 year balanced panel
    ######################
    
    time.list<-sort(unique(df[is.na(df[,dep_var])==FALSE,'year']))
    time.list<-time.list[substr(time.list,str_length(time.list)-3,
                                str_length(time.list))!='2019']
    
    my.df<-subset(df,treated>=0&sex<=s&
                    sex>=max(ceiling(s/2)-max(s-1,0),0))
    colnames(my.df)[match(dep_var,colnames(my.df))]<-'dep_var'
    my.df<-subset(my.df,dep_var>=0&treated>=0)
    
    pid.list<-get_people_in_1_year_balanced_panel(my.df)
    synth.dd.df<-create_synth_dd_data_frame(pid.list,my.df)
    synth.dd.df<-drop_multiples(synth.dd.df,'year')
    
    setup<-panel.matrices(synth.dd.df, unit='pid',time='year',
                          outcome='dep_var',treatment='treated')
    tau.hat<-synthdid_estimate(setup$Y, setup$N0, setup$T0)
    se<-sqrt(vcov(tau.hat,'jackknife'))
    
    time.weights<-cbind(seq(from=2014,to=2019,by=1),  
                        attributes(tau.hat)$weights$lambda)
  
    control.unit.weights<-
      cbind(rownames(setup$W)[1:setup$N0],attributes(tau.hat)$weights$omega)
    
    unit.weight<-as.data.frame(
      ifelse(is.na(match(synth.dd.df[,'pid'],control.unit.weights[,1])),
             1/length(unique(subset(synth.dd.df,remote_work_treat==1)[,'pid'])),
             control.unit.weights[match(synth.dd.df[,'pid'],control.unit.weights[,1]),2])
    )
    colnames(unit.weight)<-'unit_weights'
    times.weights<-as.data.frame(
      ifelse(is.na(match(synth.dd.df[,'year'],time.weights[,1])),
             1/length(unique(subset(synth.dd.df,year>2019)[,'year'])),
             time.weights[match(synth.dd.df[,'year'],time.weights[,1]),2])
    )
    colnames(times.weights)<-'time_weights'
    synth.dd.df<-cbind(synth.dd.df,unit.weight,times.weights)
    
    weighted.event.study<-feols(dep_var~remote_work_treat*factor(year_dummy)|pid,
                          data=synth.dd.df,weights=as.numeric(synth.dd.df[,'unit_weights']),
                            cluster=~pid)
    
    F.test.weighted<-linearHypothesis(weighted.event.study, 
                  paste('remote_work_treat:factor(year_dummy)',
                  time.list[time.list<2019],
                  '=0',sep=''),white.adjust = "hc1")
    
    unweighted.event.study<-feols(dep_var~remote_work_treat*factor(year_dummy)|pid,
                                  data=synth.dd.df,cluster=~pid)
    
    F.test.unweighted<-linearHypothesis(unweighted.event.study, 
                                        paste('remote_work_treat:factor(year_dummy)',
                                              time.list[time.list<2019],
                                              '=0',sep=''),
                                        white.adjust = "hc1")
    
    unweighted.did<-feols(dep_var~treated|pid+year,
                          data=synth.dd.df,cluster=~pid)
    
    weighted.did<-feols(dep_var~treated|pid+year,
                          data=synth.dd.df,cluster=~pid)
    
    new.data<-as.data.frame(matrix(nrow=2,ncol=23))
    colnames(new.data)<-
      c('sex','estimate','sample',
        'period_definition',
        'estimator','2014','2015','2016','2017',
        '2018','2020','2021','2022',
        '20142015','20162017',
        '20202021','20222023','dd_est',
        'N','pre_treat_mean','dependent_variable',
        'pre_trend_p_value','subset') 
    
    new.data[,'sex']<-c('men','women','both')[s+1]
    new.data[,'subset']<-subset.list[pop.subset]
    new.data[,'estimate']<-c('point','se')
    new.data[,'sample']<-'balanced'
    new.data[,'period_definition']<-'year'
    new.data[,'estimator']<-'twfe'
    new.data[,'dependent_variable']<-dep_var
    new.data[,'pre_trend_p_value']<-F.test.unweighted$`Pr(>Chisq)`[2]
    
    new.data[1,match(time.list,colnames(new.data))]<-
      unweighted.event.study$coeftable[paste('remote_work_treat:factor(year_dummy)',
      c(time.list[time.list<2019],
        seq(from=2020,to=2022,by=1)),sep=''),1]
    new.data[2,match(time.list,colnames(new.data))]<-
      unweighted.event.study$coeftable[paste('remote_work_treat:factor(year_dummy)',
                                             c(time.list[time.list<2019],
                                               seq(from=2020,to=2022,by=1)),sep=''),2]
    
    new.data[1,'dd_est']<-unweighted.did$coeftable['treated',1]
    new.data[2,'dd_est']<-unweighted.did$coeftable['treated',2]
    
    new.data[,'N']<-nrow(synth.dd.df)
    new.data[,'pre_treat_mean']<-mean(subset(synth.dd.df,year<2020)[,'dep_var'])
    
    results.data.frame<-rbind(results.data.frame,
                              new.data)
    
    new.data<-as.data.frame(matrix(nrow=2,ncol=23))
    colnames(new.data)<-
      c('sex','estimate','sample',
        'period_definition',
        'estimator','2014','2015','2016','2017',
        '2018','2020','2021','2022',
        '20142015','20162017',
        '20202021','20222023','dd_est',
        'N','pre_treat_mean','dependent_variable',
        'pre_trend_p_value','subset') 
    
    new.data[,'sex']<-c('men','women','both')[s+1]
    new.data[,'subset']<-subset.list[pop.subset]
    new.data[,'estimate']<-c('point','se')
    new.data[,'sample']<-'balanced'
    new.data[,'period_definition']<-'year'
    new.data[,'estimator']<-'synth_dd'
    new.data[,'dependent_variable']<-dep_var
    new.data[,'pre_trend_p_value']<-F.test.weighted$`Pr(>Chisq)`[2]
    
    new.data[1,match(time.list,colnames(new.data))]<-
      unweighted.event.study$coeftable[paste('remote_work_treat:factor(year_dummy)',
                                             c(time.list[time.list<2019],
                                               seq(from=2020,to=2022,by=1)),sep=''),1]
    new.data[2,match(time.list,colnames(new.data))]<-
      unweighted.event.study$coeftable[paste('remote_work_treat:factor(year_dummy)',
                                             c(time.list[time.list<2019],
                                               seq(from=2020,to=2022,by=1)),sep=''),2]
    
    new.data[1,'dd_est']<-tau.hat
    new.data[2,'dd_est']<-se
    
    new.data[,'N']<-nrow(synth.dd.df)
    new.data[,'pre_treat_mean']<-
      sum(subset(synth.dd.df,year<2020)[,'dep_var']*
            as.numeric(subset(synth.dd.df,year<2020)[,'unit_weights'])*
            subset(synth.dd.df,year<2020)[,'time_weights'])/
      sum(as.numeric(subset(synth.dd.df,year<2020)[,'unit_weights'])*
            subset(synth.dd.df,year<2020)[,'time_weights'])
    
    results.data.frame<-rbind(results.data.frame,
                              new.data)
    
    my.df<-cbind(my.df,
                 residualize_y('dep_var','year',my.df))
    colnames(my.df)[ncol(my.df)]<-'resid_dep_var'
    my.df<-my.df[is.na(my.df[,'resid_dep_var'])==FALSE,]
    
    synth.dd.df<-create_synth_dd_data_frame(pid.list,my.df)
    synth.dd.df<-drop_multiples(synth.dd.df,'year')
    
    setup<-panel.matrices(synth.dd.df, unit='pid',time='year',
                          outcome='resid_dep_var',
                          treatment='treated')
    tau.hat<-synthdid_estimate(setup$Y, setup$N0, setup$T0)
    se<-sqrt(vcov(tau.hat,'jackknife'))
    
    time.weights<-cbind(seq(from=2014,to=2019,by=1),  
                        attributes(tau.hat)$weights$lambda)
    
    control.unit.weights<-
      cbind(rownames(setup$W)[1:setup$N0],attributes(tau.hat)$weights$omega)
    
    unit.weight<-as.data.frame(
      ifelse(is.na(match(synth.dd.df[,'pid'],control.unit.weights[,1])),
             1/length(unique(subset(synth.dd.df,remote_work_treat==1)[,'pid'])),
             control.unit.weights[match(synth.dd.df[,'pid'],control.unit.weights[,1]),2])
    )
    colnames(unit.weight)<-'unit_weights'
    times.weights<-as.data.frame(
      ifelse(is.na(match(synth.dd.df[,'year'],time.weights[,1])),
             1/length(unique(subset(synth.dd.df,year>2019)[,'year'])),
             time.weights[match(synth.dd.df[,'year'],time.weights[,1]),2])
    )
    colnames(times.weights)<-'time_weights'
    synth.dd.df<-cbind(synth.dd.df,unit.weight,times.weights)
    
    weighted.event.study<-feols(dep_var~remote_work_treat*factor(year_dummy)|pid,
                data=synth.dd.df,weights=as.numeric(synth.dd.df[,'unit_weights']),
                              cluster=~pid)
    
    F.test.weighted<-linearHypothesis(weighted.event.study, 
                                      paste('remote_work_treat:factor(year_dummy)',
                                            time.list[time.list<2019],
                                            '=0',sep=''),white.adjust = "hc1")
    
    unweighted.event.study<-feols(dep_var~remote_work_treat*factor(year_dummy)|pid,
                                  data=synth.dd.df,cluster=~pid)
    
    F.test.unweighted<-linearHypothesis(unweighted.event.study, 
                                        paste('remote_work_treat:factor(year_dummy)',
                                              time.list[time.list<2019],
                                              '=0',sep=''),
                                        white.adjust = "hc1")
    
    unweighted.did<-feols(dep_var~treated|pid+year,
                          data=synth.dd.df,cluster=~pid)
    
    weighted.did<-feols(dep_var~treated|pid+year,
                        data=synth.dd.df,cluster=~pid)
    
    new.data<-as.data.frame(matrix(nrow=2,ncol=23))
    colnames(new.data)<-
      c('sex','estimate','sample',
        'period_definition',
        'estimator','2014','2015','2016','2017',
        '2018','2020','2021','2022',
        '20142015','20162017',
        '20202021','20222023','dd_est',
        'N','pre_treat_mean','dependent_variable',
        'pre_trend_p_value','subset') 
    
    new.data[,'sex']<-c('men','women','both')[s+1]
    new.data[,'subset']<-subset.list[pop.subset]
    new.data[,'estimate']<-c('point','se')
    new.data[,'sample']<-'balanced'
    new.data[,'period_definition']<-'year'
    new.data[,'estimator']<-'synth_dd_contr'
    new.data[,'dependent_variable']<-dep_var
    new.data[,'pre_trend_p_value']<-F.test.unweighted$`Pr(>Chisq)`[2]
    
    new.data[1,match(time.list,colnames(new.data))]<-
      unweighted.event.study$coeftable[paste('remote_work_treat:factor(year_dummy)',
                                             c(time.list[time.list<2019],
                                               seq(from=2020,to=2022,by=1)),sep=''),1]
    new.data[2,match(time.list,colnames(new.data))]<-
      unweighted.event.study$coeftable[paste('remote_work_treat:factor(year_dummy)',
                                             c(time.list[time.list<2019],
                                               seq(from=2020,to=2022,by=1)),sep=''),2]
    
    new.data[1,'dd_est']<-unweighted.did$coeftable['treated',1]
    new.data[2,'dd_est']<-unweighted.did$coeftable['treated',2]
    
    new.data[,'N']<-nrow(synth.dd.df)
    new.data[,'pre_treat_mean']<-mean(subset(synth.dd.df,year<2020)[,'dep_var'])
    
    results.data.frame<-rbind(results.data.frame,
                              new.data)
    
  
    
    }
    ######################  
    #2 year balanced panel
    ######################
    
    if(dep_var!='use_remote_work_incl_nw'|pop.subset!=2){    
        
    time.list<-sort(unique(subset(df,year<2022)[
    is.na(df[,dep_var])==FALSE,'year_doubles']))
    pre.treat.times<-sort(unique(subset(df,year<2020)[
      is.na(df[,dep_var])==FALSE,'year_doubles']))
    time.list<-time.list[substr(time.list,str_length(time.list)-3,
                                str_length(time.list))!='2019']
    pre.treat.times<-pre.treat.times[substr(pre.treat.times,str_length(time.list)-3,
                                      str_length(pre.treat.times))!='2019']
    
    my.df<-subset(df,treated>=0&year<2022&sex<=s&
                    sex>=max(ceiling(s/2)-max(s-1,0),0))
    colnames(my.df)[match(dep_var,colnames(my.df))]<-'dep_var'
    my.df<-subset(my.df,dep_var>=0&treated>=0)
    
    pid.list<-get_people_in_2_year_balanced_panel(subset(my.df,
                        year<2022&year>2013&year_doubles!=0))
    synth.dd.df<-create_synth_dd_data_frame(pid.list,
                subset(my.df,year<2022&year>2013&year_doubles!=0))
    synth.dd.df<-drop_multiples(synth.dd.df,'year_doubles')
    
    setup<-panel.matrices(synth.dd.df, unit='pid',time='year_doubles',
                          outcome='dep_var',treatment='treated')
    tau.hat<-synthdid_estimate(setup$Y, setup$N0, setup$T0)
    se<-sqrt(vcov(tau.hat,'jackknife'))
    
    control.unit.weights<-
      cbind(rownames(setup$W)[1:setup$N0],attributes(tau.hat)$weights$omega)
    
    time.weights<-cbind(c('20142015','20162017','20182019'),  
                        attributes(tau.hat)$weights$lambda)
    
    unit.weight<-as.data.frame(
      ifelse(is.na(match(synth.dd.df[,'pid'],control.unit.weights[,1])),
             1/length(unique(subset(synth.dd.df,remote_work_treat==1)[,'pid'])),
             control.unit.weights[match(synth.dd.df[,'pid'],control.unit.weights[,1]),2])
    )
    colnames(unit.weight)<-'unit_weights'
    times.weights<-as.data.frame(
      ifelse(is.na(match(synth.dd.df[,'year_doubles'],time.weights[,1])),
             1/length(unique(subset(synth.dd.df,year>2019)[,'year_doubles'])),
             time.weights[match(synth.dd.df[,'year_doubles'],time.weights[,1]),2])
    )
    colnames(times.weights)<-'time_weights'
    synth.dd.df<-cbind(synth.dd.df,unit.weight,times.weights)
    
    weighted.event.study<-feols(dep_var~remote_work_treat*factor(year_doubles_dummy)|pid,
                                data=synth.dd.df,
                                weights=as.numeric(synth.dd.df[,'unit_weights']),
                                cluster=~pid)
    
    unweighted.event.study<-feols(dep_var~remote_work_treat*factor(year_doubles_dummy)|pid,
                                  data=synth.dd.df,cluster=~pid)
    
    unweighted.did<-feols(dep_var~treated|pid+year_doubles,
                          data=synth.dd.df,cluster=~pid)
    
    F.test.weighted<-linearHypothesis(weighted.event.study, 
                    paste('remote_work_treat:factor(year_doubles_dummy)',
                          pre.treat.times,
                                            '=0',sep=''),
                                      white.adjust = "hc1")
    
    F.test.unweighted<-linearHypothesis(unweighted.event.study, 
                                      paste('remote_work_treat:factor(year_doubles_dummy)',
                                            pre.treat.times,
                                            '=0',sep=''),
                                      white.adjust = "hc1")
    
    new.data<-as.data.frame(matrix(nrow=2,ncol=23))
    colnames(new.data)<-
      c('sex','estimate','sample',
        'period_definition',
        'estimator','2014','2015','2016','2017',
        '2018','2020','2021','2022',
        '20142015','20162017',
        '20202021','20222023','dd_est',
        'N','pre_treat_mean','dependent_variable',
        'pre_trend_p_value','subset') 
    
    new.data[,'sex']<-c('men','women','both')[s+1]
    new.data[,'subset']<-subset.list[pop.subset]
    new.data[,'estimate']<-c('point','se')
    new.data[,'sample']<-'balanced'
    new.data[,'period_definition']<-'year_doubles'
    new.data[,'estimator']<-'twfe'
    new.data[,'dependent_variable']<-dep_var
    new.data[,'pre_trend_p_value']<-F.test.unweighted$`Pr(>Chisq)`[2]
    
    new.data[1,match(time.list,colnames(new.data))]<-
      unweighted.event.study$coeftable[
        paste('remote_work_treat:factor(year_doubles_dummy)',
              time.list,sep=''),1]
    new.data[2,match(time.list,colnames(new.data))]<-
      unweighted.event.study$coeftable[
        paste('remote_work_treat:factor(year_doubles_dummy)',
              time.list,sep=''),2]
    
    new.data[1,'dd_est']<-unweighted.did$coeftable['treated',1]
    new.data[2,'dd_est']<-unweighted.did$coeftable['treated',2]
    
    new.data[,'N']<-nrow(synth.dd.df)
    new.data[,'pre_treat_mean']<-mean(subset(synth.dd.df,year<2020)[,'dep_var'])
    
    results.data.frame<-rbind(results.data.frame,
                              new.data)
    
    new.data<-as.data.frame(matrix(nrow=2,ncol=23))
    colnames(new.data)<-
      c('sex','estimate','sample',
        'period_definition',
        'estimator','2014','2015','2016','2017',
        '2018','2020','2021','2022',
        '20142015','20162017',
        '20202021','20222023','dd_est',
        'N','pre_treat_mean','dependent_variable',
        'pre_trend_p_value','subset')  
    
    new.data[,'sex']<-c('men','women','both')[s+1]
    new.data[,'subset']<-subset.list[pop.subset]
    new.data[,'estimate']<-c('point','se')
    new.data[,'sample']<-'balanced'
    new.data[,'period_definition']<-'year_doubles'
    new.data[,'estimator']<-'synth_dd'
    new.data[,'dependent_variable']<-dep_var
    new.data[,'pre_trend_p_value']<-F.test.weighted$`Pr(>Chisq)`[2]
    
    new.data[1,match(time.list,colnames(new.data))]<-
      weighted.event.study$coeftable[
        paste('remote_work_treat:factor(year_doubles_dummy)',
        time.list,sep=''),1]
    new.data[2,match(time.list,colnames(new.data))]<-
      weighted.event.study$coeftable[  
        paste('remote_work_treat:factor(year_doubles_dummy)',
               time.list,sep=''),2]
    
    new.data[1,'dd_est']<-tau.hat
    new.data[2,'dd_est']<-se
    
    new.data[,'N']<-nrow(synth.dd.df)
    new.data[,'pre_treat_mean']<-
      sum(subset(synth.dd.df,year<2020)[,'dep_var']*
            as.numeric(subset(synth.dd.df,year<2020)[,'unit_weights'])*
            as.numeric(subset(synth.dd.df,year<2020)[,'time_weights']))/
      sum(as.numeric(subset(synth.dd.df,year<2020)[,'unit_weights'])*
            as.numeric(subset(synth.dd.df,year<2020)[,'time_weights']))
    
    results.data.frame<-rbind(results.data.frame,
                              new.data)
    
    my.df<-cbind(my.df,
                 residualize_y('dep_var','year_doubles',my.df))
    colnames(my.df)[ncol(my.df)]<-'resid_dep_var'
    my.df<-my.df[is.na(my.df[,'resid_dep_var'])==FALSE,]
    
    synth.dd.df<-create_synth_dd_data_frame(pid.list,my.df)
    synth.dd.df<-drop_multiples(synth.dd.df,'year_doubles')
    
    setup<-panel.matrices(synth.dd.df, unit='pid',time='year_doubles',
                          outcome='resid_dep_var',
                          treatment='treated')
    tau.hat<-synthdid_estimate(setup$Y, setup$N0, setup$T0)
    se<-sqrt(vcov(tau.hat,'jackknife'))
    
    control.unit.weights<-
      cbind(rownames(setup$W)[1:setup$N0],attributes(tau.hat)$weights$omega)
    
    time.weights<-cbind(c('20142015','20162017','20182019'),  
                        attributes(tau.hat)$weights$lambda)
    
    unit.weight<-as.data.frame(
      ifelse(is.na(match(synth.dd.df[,'pid'],control.unit.weights[,1])),
             1/length(unique(subset(synth.dd.df,remote_work_treat==1)[,'pid'])),
             control.unit.weights[match(synth.dd.df[,'pid'],control.unit.weights[,1]),2])
    )
    colnames(unit.weight)<-'unit_weights'
    times.weights<-as.data.frame(
      ifelse(is.na(match(synth.dd.df[,'year_doubles'],time.weights[,1])),
             1/length(unique(subset(synth.dd.df,year>2019)[,'year_doubles'])),
             time.weights[match(synth.dd.df[,'year_doubles'],time.weights[,1]),2])
    )
    colnames(times.weights)<-'time_weights'
    synth.dd.df<-cbind(synth.dd.df,unit.weight,times.weights)
    
    weighted.event.study<-feols(dep_var~remote_work_treat*factor(year_doubles_dummy)|pid,
                                data=synth.dd.df,
                                weights=as.numeric(synth.dd.df[,'unit_weights']),
                                cluster=~pid)
    
    unweighted.event.study<-feols(dep_var~remote_work_treat*factor(year_doubles_dummy)|pid,
                                  data=synth.dd.df,cluster=~pid)
    
    unweighted.did<-feols(dep_var~treated|pid+year_doubles,
                          data=synth.dd.df,cluster=~pid)
    
    F.test.weighted<-linearHypothesis(weighted.event.study, 
                                      paste('remote_work_treat:factor(year_doubles_dummy)',
                                            pre.treat.times,
                                            '=0',sep=''),
                                      white.adjust = "hc1")
    
    F.test.unweighted<-linearHypothesis(unweighted.event.study, 
                                        paste('remote_work_treat:factor(year_doubles_dummy)',
                                              pre.treat.times,
                                              '=0',sep=''),
                                        white.adjust = "hc1")
    
    new.data<-as.data.frame(matrix(nrow=2,ncol=23))
    colnames(new.data)<-
      c('sex','estimate','sample',
        'period_definition',
        'estimator','2014','2015','2016','2017',
        '2018','2020','2021','2022',
        '20142015','20162017',
        '20202021','20222023','dd_est',
        'N','pre_treat_mean','dependent_variable',
        'pre_trend_p_value','subset') 
    
    new.data[,'sex']<-c('men','women','both')[s+1]
    new.data[,'subset']<-subset.list[pop.subset]
    new.data[,'estimate']<-c('point','se')
    new.data[,'sample']<-'balanced'
    new.data[,'period_definition']<-'year_doubles'
    new.data[,'estimator']<-'synth_dd_contr'
    new.data[,'dependent_variable']<-dep_var
    new.data[,'pre_trend_p_value']<-F.test.weighted$`Pr(>Chisq)`[2]
    
    new.data[1,match(time.list,colnames(new.data))]<-
      weighted.event.study$coeftable[
        paste('remote_work_treat:factor(year_doubles_dummy)',
              time.list,sep=''),1]
    new.data[2,match(time.list,colnames(new.data))]<-
      weighted.event.study$coeftable[
        paste('remote_work_treat:factor(year_doubles_dummy)',
              time.list,sep=''),2]
    
    new.data[1,'dd_est']<-tau.hat
    new.data[2,'dd_est']<-se
    
    new.data[,'N']<-nrow(synth.dd.df)
    new.data[,'pre_treat_mean']<-mean(subset(synth.dd.df,year<2020)[,'dep_var'])
    
    results.data.frame<-rbind(results.data.frame,
                              new.data)

}
}
}
}

#############################
#loneliness results for unmarried people

yr<-'year'
pop.subset<-4

for (dep_var in c("is_lonely","is_often_lonely")){
  for (s in c(0,1,2)){
    
    df<-full_df
    df_cs<-subset(df,my_mar_stat_2019_year==0)
    
    time.list<-sort(unique(df[is.na(df[,dep_var])==FALSE,yr]))
    time.list<-time.list[substr(time.list,str_length(time.list)-3,
                                str_length(time.list))!='2019']
    
    my.df<-subset(df,treated>=0&sex<=s&
                    sex>=max(ceiling(s/2)-max(s-1,0),0))
    colnames(my.df)[match(dep_var,colnames(my.df))]<-'dep_var'
    my.df<-subset(my.df,dep_var>=0&treated>=0)
    
    my.df<-cbind(my.df,
                 residualize_y('dep_var','year',my.df))
    colnames(my.df)[ncol(my.df)]<-'resid_dep_var'
    my.df<-my.df[is.na(my.df[,'resid_dep_var'])==FALSE,]
    my.df<-subset(my.df,my_mar_stat_2019_year==0)
    
    pid.list<-get_people_in_1_year_balanced_panel(my.df)
    synth.dd.df<-create_synth_dd_data_frame(pid.list,my.df)
    synth.dd.df<-drop_multiples(synth.dd.df,'year')
    
new.data<-as.data.frame(matrix(nrow=2,ncol=23))
colnames(new.data)<-
  c('sex','estimate','sample',
    'period_definition',
    'estimator','2014','2015','2016','2017',
    '2018','2020','2021','2022',
    '20142015','20162017',
    '20202021','20222023','dd_est',
    'N','pre_treat_mean','dependent_variable',
    'pre_trend_p_value','subset')

c.and.s<-att_gt(yname = dep_var,
                tname = yr,
                idname = "pid",
                gname = paste("first_treat_",yr,sep=''),
                xformla = ~
                  has_kids_2019_year+
                  owns_home_2019_year+
                  education_2019_yearis2+
                  education_2019_yearis3+
                  education_2019_yearis4+
                  education_2019_yearis5+
                  education_2019_yearis9,
                data = subset(df_cs,sex<=s&
                                sex>=max(ceiling(s/2)-max(s-1,0),0)&
                                my_mar_stat_2019_year==0),
                allow_unbalanced_panel=TRUE
)

new.data[,'sex']<-c('men','women','both')[s+1]
new.data[,'subset']<-subset.list[pop.subset]
new.data[,'estimate']<-c('point','se')
new.data[,'sample']<-'unbalanced'
new.data[,'period_definition']<-yr
new.data[,'estimator']<-'C&S_with_PS'
new.data[,'dependent_variable']<-dep_var
new.data[,'pre_trend_p_value']<-c(c.and.s$Wpval,c.and.s$Wpval)

es <- aggte(c.and.s, type = "dynamic")

new.data[1,match(time.list[1:length(es$att.egt)],colnames(new.data))]<-es$att.egt
new.data[2,match(time.list[1:length(es$att.egt)],colnames(new.data))]<-es$se.egt

new.data[1,'dd_est']<-es$overall.att
new.data[2,'dd_est']<-es$overall.se

new.data[,'N']<-sum(is.na(subset(df_cs,
                                   has_kids_2019_year>=0&
                                   owns_home_2019_year>=0&
                                   education_2019_year>0&sex<=s&
                                   sex>=max(ceiling(s/2)-max(s-1,0),0))[,dep_var])==FALSE)

pre.treat.vals<-subset(df_cs,sex<=s&
                         sex>=max(ceiling(s/2)-max(s-1,0),0)&
                         year<2020&
                         owns_home_2019_year>=0&
                         education_2019_year>=0&
                         my_mar_stat_2019_year>=0&
                         has_kids_2019_year>=0)[,dep_var]

new.data[,'pre_treat_mean']<-
  sum(pre.treat.vals[is.na(pre.treat.vals)==FALSE])/
  length(pre.treat.vals[is.na(pre.treat.vals)==FALSE])


results.data.frame<-rbind(results.data.frame,
                          new.data)

#synth dd estimator
setup<-panel.matrices(synth.dd.df, unit='pid',time='year',
                      outcome='resid_dep_var',
                      treatment='treated')
tau.hat<-synthdid_estimate(setup$Y, setup$N0, setup$T0)
se<-sqrt(vcov(tau.hat,'jackknife'))

time.weights<-cbind(seq(from=2014,to=2019,by=1),  
                    attributes(tau.hat)$weights$lambda)

control.unit.weights<-
  cbind(rownames(setup$W)[1:setup$N0],attributes(tau.hat)$weights$omega)

unit.weight<-as.data.frame(
  ifelse(is.na(match(synth.dd.df[,'pid'],control.unit.weights[,1])),
         1/length(unique(subset(synth.dd.df,remote_work_treat==1)[,'pid'])),
         control.unit.weights[match(synth.dd.df[,'pid'],control.unit.weights[,1]),2])
)
colnames(unit.weight)<-'unit_weights'
times.weights<-as.data.frame(
  ifelse(is.na(match(synth.dd.df[,'year'],time.weights[,1])),
         1/length(unique(subset(synth.dd.df,year>2019)[,'year'])),
         time.weights[match(synth.dd.df[,'year'],time.weights[,1]),2])
)
colnames(times.weights)<-'time_weights'
synth.dd.df<-cbind(synth.dd.df,unit.weight,times.weights)

weighted.event.study<-feols(dep_var~remote_work_treat*factor(year_dummy)|pid,
                            data=synth.dd.df,weights=as.numeric(synth.dd.df[,'unit_weights']),
                            cluster=~pid)

F.test.weighted<-linearHypothesis(weighted.event.study, 
                                  paste('remote_work_treat:factor(year_dummy)',
                                        time.list[time.list<2019],
                                        '=0',sep=''),white.adjust = "hc1")

unweighted.event.study<-feols(dep_var~remote_work_treat*factor(year_dummy)|pid,
                              data=synth.dd.df,cluster=~pid)

F.test.unweighted<-linearHypothesis(unweighted.event.study, 
                                    paste('remote_work_treat:factor(year_dummy)',
                                          time.list[time.list<2019],
                                          '=0',sep=''),
                                    white.adjust = "hc1")

unweighted.did<-feols(dep_var~treated|pid+year,
                      data=synth.dd.df,cluster=~pid)

weighted.did<-feols(dep_var~treated|pid+year,
                    data=synth.dd.df,cluster=~pid)

new.data<-as.data.frame(matrix(nrow=2,ncol=23))
colnames(new.data)<-
  c('sex','estimate','sample',
    'period_definition',
    'estimator','2014','2015','2016','2017',
    '2018','2020','2021','2022',
    '20142015','20162017',
    '20202021','20222023','dd_est',
    'N','pre_treat_mean','dependent_variable',
    'pre_trend_p_value','subset') 

new.data[,'sex']<-c('men','women','both')[s+1]
new.data[,'subset']<-subset.list[pop.subset]
new.data[,'estimate']<-c('point','se')
new.data[,'sample']<-'balanced'
new.data[,'period_definition']<-'year'
new.data[,'estimator']<-'synth_dd_contr'
new.data[,'dependent_variable']<-dep_var
new.data[,'pre_trend_p_value']<-F.test.unweighted$`Pr(>Chisq)`[2]

new.data[1,match(time.list,colnames(new.data))]<-
  unweighted.event.study$coeftable[paste('remote_work_treat:factor(year_dummy)',
                                         c(time.list[time.list<2019],
                                           seq(from=2020,to=2022,by=1)),sep=''),1]
new.data[2,match(time.list,colnames(new.data))]<-
  unweighted.event.study$coeftable[paste('remote_work_treat:factor(year_dummy)',
                                         c(time.list[time.list<2019],
                                           seq(from=2020,to=2022,by=1)),sep=''),2]

new.data[1,'dd_est']<-unweighted.did$coeftable['treated',1]
new.data[2,'dd_est']<-unweighted.did$coeftable['treated',2]

new.data[,'N']<-nrow(synth.dd.df)
new.data[,'pre_treat_mean']<-mean(subset(synth.dd.df,year<2020)[,'dep_var'])

results.data.frame<-rbind(results.data.frame,
                          new.data)


}
}

setwd('C:\\Users\\zvh514\\OneDrive - University of York\\Documents\\remote_work')
write.csv(results.data.frame,'all_results_v2.CSV')

subset(results.data.frame,estimator%in%c('synth_dd_contr')&subset=='all'&dependent_variable=='use_remote_work_incl_nw')



#############################
#propensity score weightings#
#############################

setwd('C:\\Users\\zvh514\\OneDrive - University of York\\Documents\\remote_work\\output_charts')

dependent.variables.names<-
  c("GHQ12_caseness","anxiety_and_depression","loss_of_confidence",     
    "social_dysfunction","SF12","low autonomy, tasks",                 
    "low autonomy, pace","low autonomy, manner", "low autonomy, task order",                 
    "low autonomy, hours","low_autonomy_summary","job_satisfaction",       
    "high_job_satisfaction","high_caseness","use_remote_work",
    "is_lonely",'is_often_lonely',"employed","unemployed",             
    "retired", "cares","cares_intensively")

colnames(full_df)

setEPS(width=10,height=10)
postscript('ps_weighting_1.eps')
par(mar=c(4,2,2,2),mfrow=c(3,3))
for (dep_var in c("use_remote_work_incl_nw","GHQ12_caseness","anxiety_and_depression","loss_of_confidence","social_dysfunction","SF12_mh",
                  "job_satisfaction","high_job_satisfaction" ,"high_caseness")){

my.formula<-paste(dep_var,'~factor(year)*remote_work_treat',sep='')  
  
plot.data<-matrix(nrow=3,ncol=4)

event.study.ghq12<-lm(as.formula(my.formula),
   data=full_df)

plot.data[,1]<-coef(event.study.ghq12)[1]+coef(event.study.ghq12)[paste('factor(year)',seq(from=2020,to=2022),sep='')]
plot.data[,2]<-coef(event.study.ghq12)[1]+coef(event.study.ghq12)['remote_work_treat']+
  coef(event.study.ghq12)[paste('factor(year)',seq(from=2020,to=2022),sep='')]+
  coef(event.study.ghq12)[paste('factor(year)',seq(from=2020,to=2022),':remote_work_treat',sep='')]

event.study.ghq12<-lm(as.formula(my.formula),
                      data=full_df,weights=1/full_df[,'p_observed'])

plot.data[,3]<-coef(event.study.ghq12)[1]+coef(event.study.ghq12)[paste('factor(year)',seq(from=2020,to=2022),sep='')]
plot.data[,4]<-coef(event.study.ghq12)[1]+coef(event.study.ghq12)['remote_work_treat']+
  coef(event.study.ghq12)[paste('factor(year)',seq(from=2020,to=2022),sep='')]+
  coef(event.study.ghq12)[paste('factor(year)',seq(from=2020,to=2022),':remote_work_treat',sep='')]

plot(seq(from=2020,to=2022),plot.data[,1],
     type='o',ylab=' ',xlab='Year',main=str_replace_all(dependent.variables.names[match(dep_var,dependent.variables)],'_',' '),
     ylim=c(min(plot.data),max(plot.data)))
lines(seq(from=2020,to=2022,by=1),
      plot.data[,2],
     type='o',col=2,pch=2)


lines(seq(from=2020,to=2022,by=1),plot.data[,3],
     type='o',ylim=c(0,3),lty=2)
lines(seq(from=2020,to=2022,by=1),
      plot.data[,4],
      type='o',col=2,lty=2,pch=2)

}
legend('bottomright',legend=c('untreated, unweighted','treated, unweighted',
                              'untreated, weighted','treated, ,weighted'),
       col=c(1,2,1,2),pch=c(1,2,1,2),lty=c(1,1,2,2))
dev.off()

setEPS(width=10,height=10)
postscript('ps_weighting_2.eps')
par(mar=c(4,2,2,2),mfrow=c(4,3))
for (dep_var in c("wkaut1","wkaut2","wkaut3",                 
                  "wkaut4","wkaut5","low_autonomy_summary","is_lonely",
                  "is_often_lonely",
                  "cares","cares_intensively",
                  'unemployed','retired')){
  
  my.formula<-paste(dep_var,'~factor(year)*remote_work_treat',sep='')  
  
  plot.data<-matrix(nrow=3,ncol=4)
  
  event.study.ghq12<-lm(as.formula(my.formula),
                        data=full_df)
  
  plot.data[,1]<-coef(event.study.ghq12)[1]+coef(event.study.ghq12)[paste('factor(year)',seq(from=2020,to=2022),sep='')]
  plot.data[,2]<-coef(event.study.ghq12)[1]+coef(event.study.ghq12)['remote_work_treat']+
    coef(event.study.ghq12)[paste('factor(year)',seq(from=2020,to=2022),sep='')]+
    coef(event.study.ghq12)[paste('factor(year)',seq(from=2020,to=2022),':remote_work_treat',sep='')]
  
  event.study.ghq12<-lm(as.formula(my.formula),
                        data=full_df,weights=1/full_df[,'p_observed'])
  
  plot.data[,3]<-coef(event.study.ghq12)[1]+coef(event.study.ghq12)[paste('factor(year)',seq(from=2020,to=2022),sep='')]
  plot.data[,4]<-coef(event.study.ghq12)[1]+coef(event.study.ghq12)['remote_work_treat']+
    coef(event.study.ghq12)[paste('factor(year)',seq(from=2020,to=2022),sep='')]+
    coef(event.study.ghq12)[paste('factor(year)',seq(from=2020,to=2022),':remote_work_treat',sep='')]
  
  plot(seq(from=2020,to=2022),plot.data[,1],
       type='o',ylab=' ',xlab='Year',
       main=str_replace_all(dependent.variables.names[match(dep_var,dependent.variables)],'_',' '),
       ylim=c(min(plot.data),max(plot.data)))
  lines(seq(from=2020,to=2022,by=1),
        plot.data[,2],
        type='o',col=2,pch=2)
  
  
  lines(seq(from=2020,to=2022,by=1),plot.data[,3],
        type='o',ylim=c(0,3),lty=2)
  lines(seq(from=2020,to=2022,by=1),
        plot.data[,4],
        type='o',col=2,lty=2,pch=2)
  
}
legend('bottomright',legend=c('untreated, unweighted','treated, unweighted',
                           'untreated, weighted','treated, ,weighted'),
       col=c(1,2,1,2),pch=c(1,2,1,2),lty=c(1,1,2,2))
dev.off()

es.unbalanced<-c(unlist(subset(results.data.frame,estimator=='C&S_with_PS'&
         dependent_variable=='is_lonely'&
         subset=='all'&
         sex=='both'&
         period_definition=='year')[1,seq(from=6,to=10,by=1)]),0,
         unlist(subset(results.data.frame,estimator=='C&S_with_PS'&
                  dependent_variable=='is_lonely'&
                  subset=='all'&
                  sex=='both'&
                  period_definition=='year')[1,seq(from=11,to=13,by=1)]))
es.balanced<-c(unlist(subset(results.data.frame,estimator=='C&S_with_PS'&
                        dependent_variable=='is_lonely'&
                        subset=='all'&
                        sex=='both'&
                        period_definition=='year')[3,seq(from=6,to=10,by=1)]),0,
               unlist(subset(results.data.frame,estimator=='C&S_with_PS'&
                               dependent_variable=='is_lonely'&
                               subset=='all'&
                               sex=='both'&
                               period_definition=='year')[3,seq(from=11,to=13,by=1)]))

plot(unlist(es.unbalanced),type='l',ylim=c(-0.2,0.2))
lines(c(1,9),c(0,0))
lines(unlist(es.balanced),col=2)

es.unbalanced<-c(unlist(subset(results.data.frame,estimator=='C&S_with_PS'&
                                 dependent_variable=='use_remote_work_incl_nw'&
                                 subset=='all'&
                                 sex=='both'&
                                 period_definition=='year_doubles')[1,seq(from=6,to=10,by=1)]),0,
                 unlist(subset(results.data.frame,estimator=='C&S_with_PS'&
                                 dependent_variable=='GHQ12_caseness'&
                                 subset=='all'&
                                 sex=='both'&
                                 period_definition=='year')[1,seq(from=11,to=13,by=1)]))
es.balanced<-c(unlist(subset(results.data.frame,estimator=='C&S_with_PS'&
                               dependent_variable=='GHQ12_caseness'&
                               subset=='all'&
                               sex=='both'&
                               period_definition=='year')[3,seq(from=6,to=10,by=1)]),0,
               unlist(subset(results.data.frame,estimator=='C&S_with_PS'&
                               dependent_variable=='GHQ12_caseness'&
                               subset=='all'&
                               sex=='both'&
                               period_definition=='year')[3,seq(from=11,to=13,by=1)]))

plot(unlist(es.unbalanced),type='l',ylim=c(-0.2,0.2))
lines(c(1,9),c(0,0))
lines(unlist(es.balanced),col=2)


#heterogeneity by personality test
for (per.var in
     c("agreeableness",
       "conscientiousness",
       "extraversion",
       "neuroticism",
       "openness")){
  
  med.per.var<-median(full_df[,paste(per.var,'_all_waves',sep='')][
    is.na(full_df[,paste(per.var,'_all_waves',sep='')])==FALSE])
  
  high.per.var<-as.data.frame(
    full_df[,paste(per.var,'_all_waves',sep='')]>=
    med.per.var)
  
  colnames(high.per.var)<-paste('high_',per.var,sep='')
    
  full_df<-cbind(full_df,high.per.var)  
  
}

dep.var.yr.key<-cbind(dependent.variables,
                      c('year','year',
                        'year','year',
                        'year','year_doubles',
                        'year_doubles','year_doubles',
                        'year_doubles','year_doubles',
                        'year_doubles','year',
                        'year','year',
                        'year_doubles','year',
                        'year','year',
                        'year','year',
                        'year'))

per.list<-c("agreeableness",
  "conscientiousness",
  "extraversion",
  "neuroticism",
  "openness")

setwd('C:\\Users\\zvh514\\OneDrive - University of York\\Documents\\remote_work\\output_charts')
#heterogeneity tests

for (dep_var in dependent.variables){

  yr<-dep.var.yr.key[match(dep_var,dependent.variables),2]
 
  eff.by.pers<-matrix(ncol=2,nrow=10)  
   

 for (per_var in per.list){
   for (high in c(0,1)){
   
   
c.and.s<-att_gt(yname = dep_var,
                tname = yr,
                idname = "pid",
                gname = paste("first_treat_",yr,sep=''),
                xformla = ~ my_mar_stat_2019_yearis1+
                  has_kids_2019_year+
                  owns_home_2019_year+
                  education_2019_yearis2+
                  education_2019_yearis3+
                  education_2019_yearis4+
                  education_2019_yearis5+
                  education_2019_yearis9,
                data = full_df[full_df[,paste('high_',per_var,sep='')]==high,],
                allow_unbalanced_panel=TRUE
)

es <- aggte(c.and.s, type = "dynamic")
eff.by.pers[2*(match(per_var,per.list)-1)+high+1,1]<-es$overall.att
eff.by.pers[2*(match(per_var,per.list)-1)+high+1,2]<-es$overall.se

}
}
  
setEPS(width=10,height=4)
postscript(paste(dep_var,'_heterogeneity.eps',sep=''))
plot(seq(from=1,to=21,by=5),eff.by.pers[seq(from=1,to=9,by=2),1],
     ylim=c(min(eff.by.pers[,1]-1.96*eff.by.pers[,2]),
            max(eff.by.pers[,1]+1.96*eff.by.pers[,2])),
     xlim=c(0,23),ylab=' ',xlab=' ',xaxt='n',
     main=str_replace_all(dependent.variables.names[match(dep_var,dependent.variables)],
                          '_',' '))
axis(1,at=seq(from=2,to=22,by=5),
     labels=per.list)
points(seq(from=3,to=23,by=5),eff.by.pers[seq(from=2,to=10,by=2),1],
       col=2,pch=2)
segments(x0=seq(from=1,to=21,by=5),
         x1=seq(from=1,to=21,by=5),
         y0=eff.by.pers[seq(from=1,to=9,by=2),1]-
           1.96*eff.by.pers[seq(from=1,to=9,by=2),2],
         y1=eff.by.pers[seq(from=1,to=9,by=2),1]+
           1.96*eff.by.pers[seq(from=1,to=9,by=2),2])  
segments(x0=seq(from=3,to=23,by=5),
         x1=seq(from=3,to=23,by=5),
         y0=eff.by.pers[seq(from=2,to=10,by=2),1]-
           1.96*eff.by.pers[seq(from=2,to=10,by=2),2],
         y1=eff.by.pers[seq(from=2,to=10,by=2),1]+
           1.96*eff.by.pers[seq(from=2,to=10,by=2),2],col=2)  
segments(x0=seq(from=4.5,to=25.5,by=5),
         x1=seq(from=4.5,to=25.5,by=5),
         y0=max(eff.by.pers[,1]+
           3*eff.by.pers[,2]),
         y1=min(eff.by.pers[,1]-
              3*eff.by.pers[,2]))
lines(c(-1,25),c(0,0))
legend('topright',legend=c('low','high'),
       col=c(1,2),pch=c(1,2))
dev.off()  
}

######################################################
#worked examples of synthetic dd for job satisfaction#
######################################################
df<-full_df

time.list<-sort(unique(df[is.na(df[,dep_var])==FALSE,yr]))
time.list<-time.list[substr(time.list,str_length(time.list)-3,
                            str_length(time.list))!='2019']

my.df<-subset(df,treated>=0&sex<=s&
                sex>=max(ceiling(s/2)-max(s-1,0),0))
my.df<-subset(my.df,job_satisfaction>=0&treated>=0)

my.df<-cbind(my.df,
             residualize_y('job_satisfaction','year',my.df))
colnames(my.df)[ncol(my.df)]<-'resid_dep_var'
my.df<-my.df[is.na(my.df[,'resid_dep_var'])==FALSE,]

pid.list<-get_people_in_1_year_balanced_panel(my.df)
synth.dd.df<-create_synth_dd_data_frame(pid.list,my.df)
synth.dd.df<-drop_multiples(synth.dd.df,'year')

setup<-panel.matrices(synth.dd.df, unit='pid',time='year',
                      outcome='resid_dep_var',
                      treatment='treated')
tau.hat<-synthdid_estimate(setup$Y, setup$N0, setup$T0)
se<-sqrt(vcov(tau.hat,'jackknife'))

control.unit.weights<-
  cbind(rownames(setup$W)[1:setup$N0],attributes(tau.hat)$weights$omega)

unit.weight<-as.data.frame(
  ifelse(is.na(match(synth.dd.df[,'pid'],control.unit.weights[,1])),
         1/length(unique(subset(synth.dd.df,remote_work_treat==1)[,'pid'])),
         control.unit.weights[match(synth.dd.df[,'pid'],control.unit.weights[,1]),2])
)
colnames(unit.weight)<-'unit_weights'
synth.dd.df<-cbind(synth.dd.df,unit.weight)

weighted.event.study<-feols(resid_dep_var~remote_work_treat*factor(year_dummy)|pid,
                            data=synth.dd.df,weights=as.numeric(synth.dd.df[,'unit_weights']),
                            cluster=~pid)

plot(seq(from=2014,to=2022,by=1),
  c(weighted.event.study$coefficients[paste('remote_work_treat:factor(year_dummy)',
                                        seq(from=2014,to=2018,by=1),sep='')],
  0,
  weighted.event.study$coefficients[paste('remote_work_treat:factor(year_dummy)',
                                          seq(from=2020,to=2022,by=1),sep='')]),
  type='o',ylim=c(-0.1,0.1),main='job satisfaction event studies',
  xlab='year',ylab=' ')

#Again with lower penalty
synth.dd.df<-create_synth_dd_data_frame(pid.list,my.df)
synth.dd.df<-drop_multiples(synth.dd.df,'year')

setup<-panel.matrices(synth.dd.df, unit='pid',time='year',
                      outcome='resid_dep_var',
                      treatment='treated')
tau.hat<-synthdid_estimate(setup$Y, setup$N0, setup$T0,eta.omega=0.5)
se<-sqrt(vcov(tau.hat,'jackknife'))

control.unit.weights<-
  cbind(rownames(setup$W)[1:setup$N0],attributes(tau.hat)$weights$omega)

unit.weight<-as.data.frame(
  ifelse(is.na(match(synth.dd.df[,'pid'],control.unit.weights[,1])),
         1/length(unique(subset(synth.dd.df,remote_work_treat==1)[,'pid'])),
         control.unit.weights[match(synth.dd.df[,'pid'],control.unit.weights[,1]),2])
)
colnames(unit.weight)<-'unit_weights'
synth.dd.df<-cbind(synth.dd.df,unit.weight)

weighted.event.study<-feols(resid_dep_var~remote_work_treat*factor(year_dummy)|pid,
                            data=synth.dd.df,weights=as.numeric(synth.dd.df[,'unit_weights']),
                            cluster=~pid)

points(seq(from=2014,to=2022,by=1),
       c(weighted.event.study$coefficients[paste('remote_work_treat:factor(year_dummy)',
                                                 seq(from=2014,to=2018,by=1),sep='')],
         0,
         weighted.event.study$coefficients[paste('remote_work_treat:factor(year_dummy)',
                                                 seq(from=2020,to=2022,by=1),sep='')]),
       type='o',col=3)

legend('bottomleft',fill=c(1,3),
       legend=c('high penalty','low penalty'))