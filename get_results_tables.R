rm(list=ls())
set.seed(811072144)

data.directory<-'Z:\\Projects\\remote_work\\UKDA-6931-stata\\stata\\stata13_se\\ukhls'
output.directory<-'C:\\Users\\zvh514\\OneDrive - University of York\\Documents\\remote_work\\output_charts'

library(fixest)
library(lmtest)
library(synthdid)
library(did)
library(stringr)
library(stargazer)
###########
#Load Data#
###########

setwd(data.directory)
full_df<-read.csv('US_data_remote_work_v4.CSV')

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
      'wkaut5','lonely','main_work_location')

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
wmfh<-
  as.data.frame(ifelse(full_df[,"main_work_location"]==1,1,0))

colnames(offer.remote.work.incl.nw)<-'offer_remote_work_incl_nw'
colnames(use.remote.work.incl.nw)<-'use_remote_work_incl_nw'
colnames(wmfh)<-'work_mostly_from_home'

full_df<-cbind(full_df,offer.remote.work.incl.nw,use.remote.work.incl.nw,
               wmfh)

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

one.dig.occ<-as.data.frame(substr(full_df[,'occupation'],1,1))
colnames(one.dig.occ)<-'one_dig_occ'

full_df<-cbind(full_df,remote.work.treat,has.kids,my.mar.var,
               white,owns.home,new.educ,one.dig.occ)

#Add in year doubles
year.double<-as.data.frame(
  ifelse(full_df[,'year']==2014|full_df[,'year']==2015,20142015,
         ifelse(full_df[,'year']==2016|full_df[,'year']==2017,20162017,
                ifelse(full_df[,'year']==2018|full_df[,'year']==2019,20182019,
                       ifelse(full_df[,'year']==2020|full_df[,'year']==2021,20202021,
                              ifelse(full_df[,'year']==2022|full_df[,'year']==2023,20222023,0))))))

colnames(year.double)<-'year_doubles'
full_df<-cbind(full_df,year.double)

#Create an all-purpose control variable
my.control.reg<-lm(pid~factor(owns_home)+factor(my_mar_stat)+factor(new_educ)+
                     factor(has_kids),
                   data=full_df)
control.variable<-as.data.frame(
  coef(my.control.reg)[1]+
    coef(my.control.reg)[2]*as.numeric(full_df[,'owns_home']==1)+
    coef(my.control.reg)[3]*as.numeric(full_df[,'my_mar_stat']==1)+
    coef(my.control.reg)[4]*as.numeric(full_df[,'new_educ']==2)+
    coef(my.control.reg)[5]*as.numeric(full_df[,'new_educ']==3)+
    coef(my.control.reg)[6]*as.numeric(full_df[,'has_kids']==1))
colnames(control.variable)<-'control_variable'
full_df<-cbind(full_df,control.variable)

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
       'education','new_educ','one_dig_occ','marital_stat',
       'my_mar_stat','control_variable')){
  
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
              'new_educ_2019_year',
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
                       'cares','cares_intensively',
                       'work_mostly_from_home')

full_df[,'year']<-as.numeric(as.character(full_df[,'year']))

#######################################################
#Include only those people who are working age in 2019#
#######################################################

working.age.people<-unique(subset(full_df,year==2019&age>21&age<65)[,'pid'])
full_df<-subset(full_df,pid%in%working.age.people)

setwd(output.directory)

################
#Summarize data#
################
var.list<-c('age','sex','paid_work',
            'work_mostly_from_home','remote_work_treat',
            'GHQ12_caseness',
            'anxiety_and_depression',
            'loss_of_confidence',
            'social_dysfunction',
            'high_caseness',
            'SF12_mh',
            'job_satisfaction','high_job_satisfaction',
            'is_lonely','is_often_lonely',
            'wkaut1','wkaut2','wkaut3','wkaut4','wkaut5',
            'low_autonomy_summary'
            )
df<-subset(full_df,remote_work_treat>=0&paid_work_2019_year==1)
pids.in.sample<-unique(subset(df,year==2019)[,'pid'])
stargazer(df[,var.list],
          out='Table1')

################################################
#Does teleworkability predict working remotely?#
################################################
p.off.remote<-lm(offer_remote_work~remote_work_treat,
                 data=subset(full_df,offer_remote_work>=0&year==2019&
                               teleworkability_2019_occ>=0),
                 weights=weights)

p.use.remote<-lm(use_remote_work~remote_work_treat,
                 data=subset(full_df,use_remote_work>=0&year==2019&
                               teleworkability_2019_occ>=0),
                 weights=weights)

wfh.remote<-lm(work_mostly_from_home~remote_work_treat,
                 data=subset(full_df,use_remote_work>=0&year==2019&
                               teleworkability_2019_occ>=0),
                 weights=weights)


########################################
#Time trends
remote.work.by.year.reg<-
  lm(work_mostly_from_home~factor(year)*factor(month),
     data=subset(full_df,paid_work==1&year<2025))

month.year<-paste(c(seq(from=1,to=12,by=1),
                    seq(from=1,to=12,by=1),
                    seq(from=1,to=12,by=1),
                    seq(from=1,to=12,by=1),
                    seq(from=1,to=12,by=1),
                    seq(from=1,to=12,by=1),
                    seq(from=1,to=12,by=1),
                    seq(from=1,to=12,by=1),
                    seq(from=1,to=12,by=1),
                    seq(from=1,to=12,by=1),
                    seq(from=1,to=12,by=1),
                    seq(from=1,to=12,by=1),
                    seq(from=1,to=12,by=1),
                    seq(from=1,to=12,by=1)),
                  c(seq(from=2011,to=2011,length.out=13),
                    seq(from=2012,to=2012,length.out=13),
                    seq(from=2013,to=2013,length.out=13),
                    seq(from=2014,to=2014,length.out=13),
                    seq(from=2015,to=2015,length.out=13),
                    seq(from=2016,to=2016,length.out=13),
                    seq(from=2017,to=2017,length.out=13),
                    seq(from=2018,to=2018,length.out=13),
                    seq(from=2019,to=2019,length.out=13),
                    seq(from=2020,to=2020,length.out=13),
                    seq(from=2021,to=2021,length.out=13),
                    seq(from=2022,to=2022,length.out=13),
                    seq(from=2023,to=2023,length.out=13),
                    seq(from=2024,to=2024,length.out=13))
)

month.list<-c(seq(from=1,to=12,by=1),
              seq(from=1,to=12,by=1),
              seq(from=1,to=12,by=1),
              seq(from=1,to=12,by=1),
              seq(from=1,to=12,by=1),
              seq(from=1,to=12,by=1),
              seq(from=1,to=12,by=1),
              seq(from=1,to=12,by=1),
              seq(from=1,to=12,by=1),
              seq(from=1,to=12,by=1),
              seq(from=1,to=12,by=1),
              seq(from=1,to=12,by=1),
              seq(from=1,to=12,by=1),
              seq(from=1,to=12,by=1))

year.list<-c(seq(from=2011,to=2011,length.out=12),
             seq(from=2012,to=2012,length.out=12),
             seq(from=2013,to=2013,length.out=12),
             seq(from=2014,to=2014,length.out=12),
             seq(from=2015,to=2015,length.out=12),
             seq(from=2016,to=2016,length.out=12),
             seq(from=2017,to=2017,length.out=12),
             seq(from=2018,to=2018,length.out=12),
             seq(from=2019,to=2019,length.out=12),
             seq(from=2020,to=2020,length.out=12),
             seq(from=2021,to=2021,length.out=12),
             seq(from=2022,to=2022,length.out=12),
             seq(from=2023,to=2023,length.out=12),
             seq(from=2024,to=2024,length.out=12))

setEPS(width=10,height=4)
postscript('Figure2.eps')
par(mar=c(4,4,2,2),mfrow=c(1,1))
plot(coef(remote.work.by.year.reg)[1]+
       coef(remote.work.by.year.reg)[paste('factor(year)',year.list,sep='')]+
       ifelse(month.list==1,0,
              coef(remote.work.by.year.reg)[paste('factor(month)',
                        month.list,sep='')])+
       ifelse(month.list==1,0,
          coef(remote.work.by.year.reg)[paste('factor(year)',
          year.list,':factor(month)',month.list,sep='')]),
     xaxt='n',
     type='o',
     main='Share working mostly from home',xlab='Year',ylab=' ')
axis(1, at=seq(from=1,to=12*14,by=12), labels=seq(from=2011,to=2024,by=1)) 
dev.off()

########################################################
#Characteristics of workers in teleworkable occupations#
########################################################
df.2019<-subset(full_df,year==2019&paid_work==1)
sum.vars<-c('age','sex','GHQ12_caseness',
            'anxiety_and_depression',
            'loss_of_confidence',
            'social_dysfunction',
            'high_caseness',
            'SF12_mh','commute_time','has_kids',
            'working_hours','monthly_labor_income',
            'my_mar_stat','white','owns_home',
            'job_satisfaction','high_job_satisfaction',
            'is_lonely','is_often_lonely',
            'wkaut1','wkaut2','wkaut3','wkaut4','wkaut5',
            'low_autonomy_summary'
)
summary.by.occ.type<-matrix(nrow=length(sum.vars),ncol=2)

average_continuous_var_by_treatment_status<-function(var,treat){
  
  df<-subset(df.2019,remote_work_treat==(treat-1))
  colnames(df)[match(var,colnames(df))]<-'var'
  df<-subset(df,is.na(var)==FALSE)
  
  return(mean(df[,'var']))
  
}

discrete_var_by_treatment_status<-function(var,treat){
  
  df<-subset(df.2019,remote_work_treat==(treat-1))
  return(table(df[,var])/nrow(df))
  
}

for (treat in c(1,2)){
  for (var in sum.vars){
    
    summary.by.occ.type[match(var,sum.vars),treat]<-
      average_continuous_var_by_treatment_status(var,treat)
    
  }
}
for (var in c('new_educ','job_level','region')){
  
  summary.by.occ.type<-rbind(summary.by.occ.type,
                             cbind(discrete_var_by_treatment_status(var,1),
                                   discrete_var_by_treatment_status(var,2)))
  
}

rownames(summary.by.occ.type)<-
  c(sum.vars,
    c('Degree or equivalent',
      'A-level or GCSE',
      'Other/No qualification',
      'Management and professional',
      'Intermediate',
      'Routine',
      'London',
      'Southeast',
      'Southwest',
      'East',
      'East Midlands',
      'West Midlands',
      'North West',
      'Yorkshire and the Humber',
      'North East',
      'Wales',
      'Scotland',
      "Northern Ireland"))
colnames(summary.by.occ.type)<-c('Non-teleworkable','Teleworkable')
stargazer(summary.by.occ.type,out='Table2')

########################
#Time series structure
########################

year.table<-table(full_df[,'year'])
year.doubles.table<-table(subset(full_df,year_doubles>0)[,'year_doubles'])
year.doubles.rw.table<-table(subset(full_df,year_doubles>0&use_remote_work_incl_nw>=0&year<2025)[,'year_doubles'])

setEPS(width=10,height=4)
postscript('Figure1.eps')
par(mar=c(4,4,2,2),mfrow=c(1,1))
plot(c(min(subset(full_df,wave_number==19&year>0)[,'year']),
       max(subset(full_df,wave_number==19&year>0)[,'year'])),
     c(1,1),xlim=c(2009,2025),type='l',ylim=c(1,15),
     xlab='Years',ylab='UKHLS wave number',
     main='Years covered by UKHLS waves')
for (i in seq(from=2,to=15,by=1)){
  lines(c(min(subset(full_df,wave_number==18+i&year>0)[,'year']),
          max(subset(full_df,wave_number==18+i&year>0)[,'year'])),
        c(i,i),
        type='l')
}
dev.off()

setEPS(width=10,height=4)
postscript('FigureA1.eps')
par(mar=c(4,2,2,2),mfrow=c(1,2))
barplot(year.table,main='Observations by year')
year.pair.plot<-barplot(year.doubles.table,main='Observations by year pair',xaxt='n')
axis(1,at=year.pair.plot,
     labels=c('2014-2015',
              '2016-2017',
              '2018-2019',
              '2020-2021',
              '2022-2023'),tick='FALSE')
dev.off()

year.by.wave<-as.matrix(table(full_df[,'wave_number'],full_df[,'year']))

############################
#Raw data plot, remote work#
############################

nrow(subset(full_df,work_mostly_from_home==1&use_remote_work_incl_nw==0))/
  nrow(subset(full_df,use_remote_work_incl_nw==0))

remote.work.treated.group<-
  aggregate(use_remote_work_incl_nw~year,
            data=subset(full_df,remote_work_treat==1),
            FUN=mean)
remote.work.untreated.group<-
  aggregate(use_remote_work_incl_nw~year,
            data=subset(full_df,remote_work_treat==0),
            FUN=mean)

mostly.remote.work.treated.group<-
  aggregate(work_mostly_from_home~year,
            data=subset(full_df,remote_work_treat==1&use_remote_work_incl_nw>=0),
            FUN=mean)
mostly.remote.work.untreated.group<-
  aggregate(work_mostly_from_home~year,
            data=subset(full_df,remote_work_treat==0&use_remote_work_incl_nw>=0),
            FUN=mean)

table(subset(full_df,remote_work_treat==1&year>2019)[,'use_remote_work_incl_nw'],
      subset(full_df,remote_work_treat==1&year>2019)[,'work_mostly_from_home'])

setEPS(width=8,height=4)
postscript('use_work_from_home_raw_data_plot.eps')
par(mar=c(4,4,2,2),mfrow=c(1,1))
plot(remote.work.treated.group[,1],remote.work.treated.group[,2],
     ylab=' ',ylim=c(0,0.4),type='o',
     xlab='Year',main='Share working remotely')
points(remote.work.untreated.group[,1],remote.work.untreated.group[,2],
       lty=2,pch=2,col=2,type='o')

legend('topleft',legend=c('teleworkable 2019 occupation',
                          'non-teleworkable 2019 occupation'),
       col=c(1,2),pch=c(1,2),lty=c(1,2))
dev.off()

setEPS(width=8,height=4)
postscript('Figure3.eps')
par(mar=c(4,4,2,2),mfrow=c(1,1))
plot(mostly.remote.work.treated.group[,1],mostly.remote.work.treated.group[,2],
     ylab=' ',ylim=c(0,0.5),type='o',
     xlab='Year',main='Share working mostly from home')
points(mostly.remote.work.untreated.group[,1],
       mostly.remote.work.untreated.group[,2],
       lty=2,pch=2,col=2,type='o')

legend('topleft',legend=c('teleworkable 2019 occupation',
                          'non-teleworkable 2019 occupation'),
       col=c(1,2),pch=c(1,2),lty=c(1,2))
dev.off()

###############################################
#Load the results, and plot main event studies#
###############################################
results<-read.csv('all_results_working_age_sub_test.CSV')
colnames(results)

return_results_table<-function(results.subset){
  
  results.table<-matrix(nrow=5,ncol=1)
  results.table[1,1]<-results.subset[1,'dd_est']
  results.table[2,1]<-results.subset[2,'dd_est']
  results.table[3,1]<-results.subset[1,'N']
  results.table[4,1]<-results.subset[1,'pre_trend_p_value']
  results.table[5,1]<-results.subset[1,'pre_treat_mean']
  
  rownames(results.table)<-c('Estimate',
                             'Standard Error',
                             'Obs',
                             'p-value, pre-trend',
                             'Pre-treatment mean')
  
  return(results.table)
  
}

plot_year_event_study<-function(results.subset,my.title){
  
  df<-subset(results.subset,period_definition=='year')  
  
  points<-unlist(
    c(subset(df,estimate=='point')[,paste('X',seq(from=2014,to=2018,by=1),sep='')],
      0,
      subset(df,estimate=='point')[,paste('X',seq(from=2020,to=2023,by=1),sep='')]))
  se<-unlist(
    c(subset(df,estimate=='se')[,paste('X',seq(from=2014,to=2018,by=1),sep='')],
      0,
      subset(df,estimate=='se')[,paste('X',seq(from=2020,to=2023,by=1),sep='')]))
  
  max.est<-max((points+1.96*se)[is.na(points)==FALSE])
  min.est<-min((points-1.96*se)[is.na(points)==FALSE])
  
  min.year<-min(seq(from=2014,to=2023,by=1)[is.na(points)==FALSE])
  points<-points[is.na(points)==FALSE]
  se<-se[is.na(se)==FALSE]
  
  plot(seq(from=min.year,to=2023,by=1),points,type='o',
       ylim=c(min.est,max.est),xlab='Year',ylab='',
       main=my.title)
  segments(x0=seq(from=min.year,to=2023,by=1),
           y0=points-1.96*se,
           x1=seq(from=min.year,to=2023,by=1),
           y1=points+1.96*se)
  lines(c(min.year,2023),c(0,0))
  
}

results.subset<-subset(results,sex=='both'&
                         sample=='balanced'&
                         estimator=='synth_dd_contr'&
                         dependent_variable=='wkaut1'&
                         subset=='all')

plot_year_double_event_study<-function(results.subset,my.title){
  
  df<-subset(results.subset,period_definition=='year_doubles')  
  
  points<-unlist(
    c(subset(df,estimate=='point')[,
        c('X20142015','X20162017')],
      0,
      subset(df,estimate=='point')[,c('X20202021','X20222023')]))
  se<-unlist(
    c(subset(df,estimate=='se')[,c('X20142015','X20162017')],
      0,
      subset(df,estimate=='se')[,c('X20202021','X20222023')]))
  
  max.est<-max((points[is.na(points)==FALSE]+
                  1.96*se[is.na(se)==FALSE]))
  min.est<-min((points[is.na(points)==FALSE]-
                  1.96*se[is.na(se)==FALSE]))
  
  min.year<-min(seq(from=2014,to=2022,by=1)[is.na(points)==FALSE])
  max.year<-max(seq(from=2014,to=2022,by=1)[is.na(points)==FALSE])
  points<-points[is.na(points)==FALSE]
  se<-se[is.na(points)==FALSE]
  se<-ifelse(is.na(se),0,se)
  
  plot(seq(from=1,to=length(points),by=1),points,type='o',
       ylim=c(min.est,max.est),xlab='Year',ylab='',
       main=my.title,xaxt='n')
  segments(x0=seq(from=1,to=5,by=1),
           y0=points-1.96*se,
           x1=seq(from=1,to=5,by=1),
           y1=points+1.96*se)
  axis(1,at=seq(from=1,to=5,by=1),
       labels=c('2014-2015','2016-2017',
    '2018-2019','2020-2021',
    '2022-2023'))
  lines(c(1,5),c(0,0))
  
}

setEPS(width=10,height=4)
postscript('Figure4.eps')
par(mar=c(4,4,2,2),mfrow=c(1,1))
plot_year_event_study(subset(results,sex=='both'&
                      sample=='unbalanced'&
                      estimator=='C&S_with_PS'&
                      dependent_variable=='work_mostly_from_home'&
                      subset=='all'),
                      'Effect on mostly working from home'
)
dev.off()

setEPS(width=10,height=4)
postscript('FigureB1.eps')
par(mar=c(4,4,2,2),mfrow=c(1,1))
plot_year_event_study(subset(results,sex=='both'&
                               sample=='balanced'&
                               estimator=='synth_dd_contr'&
                               dependent_variable=='work_mostly_from_home'
                             &subset=='all'),
                      'Effect on mostly working from home'
)
dev.off()

setEPS(width=10,height=4)
postscript('example_for_slides.eps')
par(mar=c(4,4,2,2),mfrow=c(1,2))
plot_year_event_study(subset(results,sex=='both'&
                               sample=='unbalanced'&
                               estimator=='C&S_with_PS'&
                               dependent_variable=='GHQ12_caseness'&
                               subset=='all'),
                      'Double-robust DiD'
)

plot_year_event_study(subset(results,sex=='both'&
                               sample=='balanced'&
                               estimator=='synth_dd'&
                               dependent_variable=='GHQ12_caseness'&
                               subset=='all'),
                      'Synthetic DiD'
)
dev.off()

mh.list<-c('GHQ12_caseness',
           'anxiety_and_depression',
           'loss_of_confidence',
           'social_dysfunction',
           'SF12_mh','high_caseness')

setEPS(width=10,height=8)
postscript('Figure5.eps')
par(mar=c(4,4,2,2),mfrow=c(1,2))
#lonely
plot_year_event_study(subset(results,sex=='both'&
                               sample=='unbalanced'&
                               estimator=='C&S_with_PS'&
                               dependent_variable=='is_lonely'&
                               subset=='all'),
                      'Lonely, doubly robust')
#often lonely
plot_year_event_study(subset(results,sex=='both'&
                               sample=='unbalanced'&
                               estimator=='C&S_with_PS'&
                               dependent_variable=='is_often_lonely'&
                               subset=='all'),
                      'Often lonely, doubly robust')
dev.off()

setEPS(width=10,height=8)
postscript('FigureB2.eps')
par(mar=c(4,4,2,2),mfrow=c(1,2))
plot_year_event_study(subset(results,sex=='both'&
                               sample=='balanced'&
                               estimator=='synth_dd_contr'&
                               dependent_variable=='is_lonely'&
                               subset=='all'),
                      'Lonely, synthetic dd')
#often lonely
plot_year_event_study(subset(results,sex=='both'&
                               sample=='balanced'&
                               estimator=='synth_dd_contr'&
                               dependent_variable=='is_often_lonely'&
                               subset=='all'),
                      'Often lonely, synthetic dd')
dev.off()

setEPS(width=10,height=12)
postscript('Figure6.eps')
par(mar=c(4,4,2,2),mfrow=c(length(mh.list)/2,2))
for (mh in mh.list){
plot_year_event_study(subset(results,sex=='both'&
                               sample=='unbalanced'&
                               estimator=='C&S_with_PS'&
                               dependent_variable==mh&
                               subset=='all'),
                      str_replace_all(mh,'_',' '))
}
dev.off()

setEPS(width=10,height=12)
postscript('FigureB3.eps')
par(mar=c(4,4,2,2),mfrow=c(length(mh.list)/2,2))
for (mh in mh.list){
  plot_year_event_study(subset(results,sex=='both'&
                                 sample=='balanced'&
                                 estimator=='synth_dd_contr'&
                                 dependent_variable==mh&
                                 subset=='all'),
                        str_replace_all(mh,'_',' '))
}
dev.off()

setEPS(width=10,height=12)
postscript('Figure7.eps')
par(mar=c(4,4,2,2),mfrow=c(4,2))
#job satisfaction
  plot_year_event_study(subset(results,sex=='both'&
                                 sample=='unbalanced'&
                                 estimator=='C&S_with_PS'&
                                 dependent_variable=='job_satisfaction'&
                                 subset=='all'),
                        'job satisfaction')
  #high job satisfaction
  plot_year_event_study(subset(results,sex=='both'&
                                 sample=='unbalanced'&
                                 estimator=='C&S_with_PS'&
                                 dependent_variable=='high_job_satisfaction'&
                                 subset=='all'),
                        'high job satisfaction')
  #wkaut1
  plot_year_double_event_study(subset(results,sex=='both'&
                                 sample=='unbalanced'&
                                 estimator=='C&S_with_PS'&
                                 dependent_variable=='wkaut1'&
                                   subset=='all'),
                        'Low autonomy over work tasks')
  #wkaut2
  plot_year_double_event_study(subset(results,sex=='both'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut2'&
                                        subset=='all'),
                               'Low autonomy over work pace')
  #wkaut3
  plot_year_double_event_study(subset(results,sex=='both'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut3'&
                                        subset=='all'),
                               'Low autonomy over work manner')
  #wkaut4
  plot_year_double_event_study(subset(results,sex=='both'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut4'&
                                        subset=='all'),
                               'Low autonomy over task order')
  #wkaut5
  plot_year_double_event_study(subset(results,sex=='both'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut5'&
                                        subset=='all'),
                               'Low autonomy over work hours')
  #overall autonomy
  plot_year_double_event_study(subset(results,sex=='both'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='low_autonomy_summary'&
                                        subset=='all'),
                               'Low overall autonomy')
dev.off()

setEPS(width=10,height=12)
postscript('FigureB4.eps')
par(mar=c(4,4,2,2),mfrow=c(4,2))
#job satisfaction
plot_year_event_study(subset(results,sex=='both'&
                               sample=='balanced'&
                               estimator=='synth_dd_contr'&
                               dependent_variable=='job_satisfaction'&
                               subset=='all'),
                      'job satisfaction')
#high job satisfaction
plot_year_event_study(subset(results,sex=='both'&
                               sample=='balanced'&
                               estimator=='synth_dd_contr'&
                               dependent_variable=='high_job_satisfaction'&
                               subset=='all'),
                      'high job satisfaction')
#wkaut1
plot_year_double_event_study(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='wkaut1'&
                                      subset=='all'),
                             'Low autonomy over work tasks')
#wkaut2
plot_year_double_event_study(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='wkaut2'&
                                      subset=='all'),
                             'Low autonomy over work pace')
#wkaut3
plot_year_double_event_study(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='wkaut3'&
                                      subset=='all'),
                             'Low autonomy over work manner')
#wkaut4
plot_year_double_event_study(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='wkaut4'&
                                      subset=='all'),
                             'Low autonomy over task order')
#wkaut5
plot_year_double_event_study(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='wkaut5'&
                                      subset=='all'),
                             'Low autonomy over work hours')
#overall autonomy
plot_year_double_event_study(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='low_autonomy_summary'&
                                      subset=='all'),
                             'Low overall autonomy')
dev.off()


plot_year_event_study(subset(results,sex=='both'&
                               sample=='balanced'&
                               estimator=='synth_dd'&
                               dependent_variable=='GHQ12_caseness'&
                               subset=='all'),
                      'Synthetic DiD'
)

plot_year_event_study(subset(results,sex=='both'&
                               sample=='balanced'&
                               estimator=='synth_dd'&
                               dependent_variable=='is_lonely'&
                               subset=='all'),
                      'Double-robust DiD'
)

plot_year_double_event_study(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd'&
                                      subset=='all'&
                              dependent_variable=='use_remote_work_incl_nw'),
                             'Use remote work, men and women'
)

plot_year_double_event_study(subset(results,sex=='both'&
                              sample=='unbalanced'&
                              estimator=='C&S_with_PS'&
                              subset=='all'&
                              dependent_variable=='use_remote_work_incl_nw'),
                             'Use remote work, men and women'
)

#SF12
par(mar=c(4,4,2,2),mfrow=c(1,2))
plot_year_event_study(subset(results,sex=='both'&
                               sample=='unbalanced'&
                               estimator=='C&S_with_PS'&
                               dependent_variable=='SF12_mh'&
                               subset=='all'),
                      'Double-robust DiD'
)

plot_year_event_study(subset(results,sex=='both'&
                               sample=='balanced'&
                               estimator=='synth_dd'&
                               dependent_variable=='SF12_mh'&
                               subset=='all'),
                      'Synthetic DiD'
)

#A&D
par(mar=c(4,4,2,2),mfrow=c(1,2))
plot_year_event_study(subset(results,sex=='both'&
                               sample=='unbalanced'&
                               estimator=='C&S_with_PS'&
                               dependent_variable=='anxiety_and_depression'),
                      'Double-robust DiD'
)

plot_year_event_study(subset(results,sex=='both'&
                               sample=='balanced'&
                               estimator=='synth_dd'&
                               dependent_variable=='anxiety_and_depression'),
                      'Synthetic DiD'
)
par(mar=c(4,4,2,2),mfrow=c(1,2))
plot_year_event_study(subset(results,sex=='both'&
                               sample=='unbalanced'&
                               estimator=='C&S_with_PS'&
                               dependent_variable=='loss_of_confidence'),
                      'Double-robust DiD'
)

plot_year_event_study(subset(results,sex=='both'&
                               sample=='balanced'&
                               estimator=='synth_dd_contr'&
                               dependent_variable=='loss_of_confidence'),
                      'Synthetic DiD'
)

plot_year_double_event_study(subset(results,sex=='men'&
                               sample=='unbalanced'&
                               estimator=='C&S_with_PS'&
                                 subset=='all'&
                               dependent_variable=='use_remote_work_incl_nw'),
                      'Synthetic DiD'
)


#results table: remote work
remote.work.results<-
  cbind(return_results_table(subset(results,sex=='both'&
                                    sample=='unbalanced'&
                                    estimator=='C&S_with_PS'&
                                    dependent_variable=='work_mostly_from_home'&
                                    period_definition=='year'&
                                    subset=='all')),
  return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='work_mostly_from_home'&
                                      period_definition=='year'&
                                      subset=='all')),
  return_results_table(subset(results,sex=='women'&
                                sample=='unbalanced'&
                                estimator=='C&S_with_PS'&
                                dependent_variable=='work_mostly_from_home'&
                                period_definition=='year'&
                                subset=='all')))
  
remote.work.results.sd<-
  cbind(return_results_table(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='work_mostly_from_home'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='men'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='work_mostly_from_home'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='women'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='work_mostly_from_home'&
                                      period_definition=='year'&
                                      subset=='all')))


stargazer(remote.work.results,out='Table5')
stargazer(remote.work.results.sd,out='TableB1')

#Loneliness results
loneliness.main.results<-rbind(
  cbind(return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='all')),
  return_results_table(subset(results,sex=='both'&
                                sample=='unbalanced'&
                                estimator=='C&S_with_PS'&
                                dependent_variable=='is_often_lonely'&
                                period_definition=='year'&
                                subset=='all'))),
cbind(return_results_table(subset(results,sex=='men'&
                                    sample=='unbalanced'&
                                    estimator=='C&S_with_PS'&
                                    dependent_variable=='is_lonely'&
                                    period_definition=='year'&
                                    subset=='all')),
      return_results_table(subset(results,sex=='men'&
                                    sample=='unbalanced'&
                                    estimator=='C&S_with_PS'&
                                    dependent_variable=='is_often_lonely'&
                                    period_definition=='year'&
                                    subset=='all'))),
cbind(return_results_table(subset(results,sex=='women'&
                                    sample=='unbalanced'&
                                    estimator=='C&S_with_PS'&
                                    dependent_variable=='is_lonely'&
                                    period_definition=='year'&
                                    subset=='all')),
      return_results_table(subset(results,sex=='women'&
                                    sample=='unbalanced'&
                                    estimator=='C&S_with_PS'&
                                    dependent_variable=='is_often_lonely'&
                                    period_definition=='year'&
                                    subset=='all')))
)

stargazer(loneliness.main.results,out='Table6')

loneliness.main.results.sd<-rbind(
  cbind(return_results_table(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='all'))),
  cbind(return_results_table(subset(results,sex=='men'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='men'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='all'))),
  cbind(return_results_table(subset(results,sex=='women'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='women'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='all')))
)

stargazer(loneliness.main.results.sd,out='TableB2')

#Mental health results
mental.health.results<-
  rbind(
  cbind(return_results_table(subset(results,sex=='both'&
                sample=='unbalanced'&
                estimator=='C&S_with_PS'&
                dependent_variable=='GHQ12_caseness'&
                period_definition=='year'&
                  subset=='all')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='high_caseness'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='anxiety_and_depression'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='loss_of_confidence'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='social_dysfunction'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='SF12_mh'&
                                      period_definition=='year'&
                                      subset=='all'))
  ),
  cbind(return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='GHQ12_caseness'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='high_caseness'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='anxiety_and_depression'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='loss_of_confidence'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='social_dysfunction'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='SF12_mh'&
                                      period_definition=='year'&
                                      subset=='all'))
  ),
  cbind(return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='GHQ12_caseness'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='high_caseness'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='anxiety_and_depression'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='loss_of_confidence'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='social_dysfunction'&
                                      period_definition=='year'&
                                      subset=='all')),
        return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='SF12_mh'&
                                      period_definition=='year'&
                                      subset=='all'))
  ))

stargazer(mental.health.results,out='Table7')

mental.health.results.synth.dd<-
  rbind(
    cbind(return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='GHQ12_caseness'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='high_caseness'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='anxiety_and_depression'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='loss_of_confidence'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='social_dysfunction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='SF12_mh'&
                                        period_definition=='year'&
                                        subset=='all'))
    ),
    cbind(return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='GHQ12_caseness'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='high_caseness'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='anxiety_and_depression'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='loss_of_confidence'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='social_dysfunction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='SF12_mh'&
                                        period_definition=='year'&
                                        subset=='all'))
    ),
    cbind(return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='GHQ12_caseness'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='high_caseness'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='anxiety_and_depression'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='loss_of_confidence'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='social_dysfunction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='SF12_mh'&
                                        period_definition=='year'&
                                        subset=='all'))
    ))

stargazer(mental.health.results.synth.dd,out='TableB3')


#Other
other.results<-
  rbind(
    cbind(return_results_table(subset(results,sex=='both'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='job_satisfaction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='high_job_satisfaction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut1'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut2'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut3'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut4'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut5'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='low_autonomy_summary'&
                                        period_definition=='year_doubles'&
                                        subset=='all'))
    ),
    cbind(return_results_table(subset(results,sex=='men'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='job_satisfaction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='high_job_satisfaction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut1'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut2'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut3'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut4'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut5'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='low_autonomy_summary'&
                                        period_definition=='year_doubles'&
                                        subset=='all'))
    ),
    cbind(return_results_table(subset(results,sex=='women'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='job_satisfaction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='high_job_satisfaction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut1'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut2'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut3'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut4'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='wkaut5'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='unbalanced'&
                                        estimator=='C&S_with_PS'&
                                        dependent_variable=='low_autonomy_summary'&
                                        period_definition=='year_doubles'&
                                        subset=='all'))
    ))

stargazer(other.results,out='Table8')

other.results.synth.dd<-
  rbind(
    cbind(return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='job_satisfaction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='high_job_satisfaction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut1'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut2'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut3'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut4'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut5'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='both'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='low_autonomy_summary'&
                                        period_definition=='year_doubles'&
                                        subset=='all'))
    ),
    cbind(return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='job_satisfaction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='high_job_satisfaction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut1'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut2'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut3'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut4'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut5'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='men'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='low_autonomy_summary'&
                                        period_definition=='year_doubles'&
                                        subset=='all'))
    ),
    cbind(return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='job_satisfaction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='high_job_satisfaction'&
                                        period_definition=='year'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut1'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut2'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut3'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut4'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='wkaut5'&
                                        period_definition=='year_doubles'&
                                        subset=='all')),
          return_results_table(subset(results,sex=='women'&
                                        sample=='balanced'&
                                        estimator=='synth_dd_contr'&
                                        dependent_variable=='low_autonomy_summary'&
                                        period_definition=='year_doubles'&
                                        subset=='all'))
    ))

stargazer(other.results.synth.dd[,1:8],out='TableB4')

employment.results<-rbind(
cbind(
return_results_table(subset(results,sex=='both'&
                              sample=='unbalanced'&
                              estimator=='C&S_with_PS'&
                              dependent_variable=='unemployed'&
                              subset=='all'&
                              period_definition=='year')),
return_results_table(subset(results,sex=='both'&
                              sample=='unbalanced'&
                              estimator=='C&S_with_PS'&
                              dependent_variable=='retired'&
                              period_definition=='year')),
return_results_table(subset(results,sex=='both'&
         sample=='balanced'&
         estimator=='synth_dd_contr'&
         subset=='all'&
         dependent_variable=='unemployed'&
         period_definition=='year')),
return_results_table(subset(results,sex=='both'&
                              sample=='balanced'&
                              estimator=='synth_dd_contr'&
                              subset=='all'&
                              dependent_variable=='retired'&
                              period_definition=='year'))),
cbind(
  return_results_table(subset(results,sex=='men'&
                                sample=='unbalanced'&
                                estimator=='C&S_with_PS'&
                                dependent_variable=='unemployed'&
                                subset=='all'&
                                period_definition=='year')),
  return_results_table(subset(results,sex=='men'&
                                sample=='unbalanced'&
                                estimator=='C&S_with_PS'&
                                dependent_variable=='retired'&
                                period_definition=='year')),
  return_results_table(subset(results,sex=='men'&
                                sample=='balanced'&
                                estimator=='synth_dd_contr'&
                                subset=='all'&
                                dependent_variable=='unemployed'&
                                period_definition=='year')),
  return_results_table(subset(results,sex=='men'&
                                sample=='balanced'&
                                estimator=='synth_dd_contr'&
                                subset=='all'&
                                dependent_variable=='retired'&
                                period_definition=='year'))),
cbind(
  return_results_table(subset(results,sex=='women'&
                                sample=='unbalanced'&
                                estimator=='C&S_with_PS'&
                                dependent_variable=='unemployed'&
                                subset=='all'&
                                period_definition=='year')),
  return_results_table(subset(results,sex=='women'&
                                sample=='unbalanced'&
                                estimator=='C&S_with_PS'&
                                dependent_variable=='retired'&
                                period_definition=='year')),
  return_results_table(subset(results,sex=='women'&
                                sample=='balanced'&
                                estimator=='synth_dd_contr'&
                                subset=='all'&
                                dependent_variable=='unemployed'&
                                period_definition=='year')),
  return_results_table(subset(results,sex=='women'&
                                sample=='balanced'&
                                estimator=='synth_dd_contr'&
                                subset=='all'&
                                dependent_variable=='retired'&
                                period_definition=='year'))))

stargazer(employment.results,out='TableD1')

care.results<-rbind(
  cbind(
    return_results_table(subset(results,sex=='both'&
                                  sample=='unbalanced'&
                                  estimator=='C&S_with_PS'&
                                  dependent_variable=='cares'&
                                  subset=='all'&
                                  period_definition=='year')),
    return_results_table(subset(results,sex=='both'&
                                  sample=='unbalanced'&
                                  estimator=='C&S_with_PS'&
                                  dependent_variable=='cares_intensively'&
                                  period_definition=='year')),
    return_results_table(subset(results,sex=='both'&
                                  sample=='balanced'&
                                  estimator=='synth_dd_contr'&
                                  subset=='all'&
                                  dependent_variable=='cares'&
                                  period_definition=='year')),
    return_results_table(subset(results,sex=='both'&
                                  sample=='balanced'&
                                  estimator=='synth_dd_contr'&
                                  subset=='all'&
                                  dependent_variable=='cares_intensively'&
                                  period_definition=='year'))),
  cbind(
    return_results_table(subset(results,sex=='men'&
                                  sample=='unbalanced'&
                                  estimator=='C&S_with_PS'&
                                  dependent_variable=='cares'&
                                  subset=='all'&
                                  period_definition=='year')),
    return_results_table(subset(results,sex=='men'&
                                  sample=='unbalanced'&
                                  estimator=='C&S_with_PS'&
                                  dependent_variable=='cares_intensively'&
                                  period_definition=='year')),
    return_results_table(subset(results,sex=='men'&
                                  sample=='balanced'&
                                  estimator=='synth_dd_contr'&
                                  subset=='all'&
                                  dependent_variable=='cares'&
                                  period_definition=='year')),
    return_results_table(subset(results,sex=='men'&
                                  sample=='balanced'&
                                  estimator=='synth_dd_contr'&
                                  subset=='all'&
                                  dependent_variable=='cares_intensively'&
                                  period_definition=='year'))),
  cbind(
    return_results_table(subset(results,sex=='women'&
                                  sample=='unbalanced'&
                                  estimator=='C&S_with_PS'&
                                  dependent_variable=='cares'&
                                  subset=='all'&
                                  period_definition=='year')),
    return_results_table(subset(results,sex=='women'&
                                  sample=='unbalanced'&
                                  estimator=='C&S_with_PS'&
                                  dependent_variable=='cares_intensively'&
                                  period_definition=='year')),
    return_results_table(subset(results,sex=='women'&
                                  sample=='balanced'&
                                  estimator=='synth_dd_contr'&
                                  subset=='all'&
                                  dependent_variable=='cares'&
                                  period_definition=='year')),
    return_results_table(subset(results,sex=='women'&
                                  sample=='balanced'&
                                  estimator=='synth_dd_contr'&
                                  subset=='all'&
                                  dependent_variable=='cares_intensively'&
                                  period_definition=='year'))))

stargazer(care.results,out='TableE1')

setEPS(width=7,height=6)
postscript('labor_market_es.eps')
par(mfrow=c(2,2),mar=c(4,2,2,2))
plot_year_event_study(subset(results,sex=='both'&
                               sample=='unbalanced'&
                               estimator=='C&S_with_PS'&
                               dependent_variable=='unemployed'&
                               subset=='all'&
                               period_definition=='year'),
                      'unemployed doubly robust')
plot_year_event_study(subset(results,sex=='both'&
                        sample=='balanced'&
                      estimator=='synth_dd_contr'&
                      dependent_variable=='unemployed'&
                      subset=='all'&
                      period_definition=='year'),
                      'unemployed synth dd')
plot_year_event_study(subset(results,sex=='both'&
                               sample=='unbalanced'&
                               estimator=='C&S_with_PS'&
                               dependent_variable=='retired'&
                               subset=='all'&
                               period_definition=='year'),
                      'retired doubly robust')
plot_year_event_study(subset(results,sex=='both'&
                               sample=='balanced'&
                               estimator=='synth_dd_contr'&
                               dependent_variable=='retired'&
                               subset=='all'&
                               period_definition=='year'),
                      'retired synth dd')
dev.off()

loneliness.married.unmarried<-rbind(
  cbind(return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='not_married')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='not_married')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='married')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='married'))),
  cbind(return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='not_married')),
        return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='not_married')),
        return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='married')),
        return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='married'))),
  cbind(return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='not_married')),
        return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='not_married')),
        return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='married')),
        return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='married')))
)

stargazer(loneliness.married.unmarried,out='TableF1')

loneliness.married.unmarried.sd<-
  cbind(return_results_table(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='not_married')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='not_married')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='married')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='married')))

stargazer(loneliness.married.unmarried.sd,out='TableF2')

loneliness.kids.no.kids<-rbind(
  cbind(return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='no_kids')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='no_kids')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='kids')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='kids'))),
  cbind(return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='no_kids')),
        return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='no_kids')),
        return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='kids')),
        return_results_table(subset(results,sex=='men'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='kids'))),
  cbind(return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='no_kids')),
        return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='no_kids')),
        return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='kids')),
        return_results_table(subset(results,sex=='women'&
                                      sample=='unbalanced'&
                                      estimator=='C&S_with_PS'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='kids')))
)

loneliness.kids.no.kids.sd<-
  cbind(return_results_table(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='no_kids')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='no_kids')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_lonely'&
                                      period_definition=='year'&
                                      subset=='kids')),
        return_results_table(subset(results,sex=='both'&
                                      sample=='balanced'&
                                      estimator=='synth_dd_contr'&
                                      dependent_variable=='is_often_lonely'&
                                      period_definition=='year'&
                                      subset=='kids')))

stargazer(loneliness.kids.no.kids,out='TableG1')
stargazer(loneliness.kids.no.kids.sd,out='TableG2')

