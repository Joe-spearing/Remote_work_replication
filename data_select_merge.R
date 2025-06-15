rm(list=ls())
library(haven)

letters<-c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o',
           'p','q','r')

setwd('Z:\\Projects\\remote_work\\UKDA-6931-stata\\stata\\stata13_se\\ukhls')

for (val in seq(from=1,to=14,by=1)){
  
  file.name<-paste(letters[val],'_indresp_protect.dta',sep='')
  
  df<-read_dta(file.name)
  
  assign(paste('indresp_',val+18,sep=''),df)
  
  file.name<-paste(letters[val],'_hhresp_protect.dta',sep='')
  
  df<-read_dta(file.name)
  
  assign(paste('hhresp_',val+18,sep=''),df)
  
}

cross.wave.data<-read_dta('xwavedat_protect.dta')

setwd('C:\\Users\\zvh514\\OneDrive - University of York\\Documents\\remote_work')
cpi.data<-read.csv('CPI_Index.CSV')

indresp.list<-list(indresp_19,indresp_20,
                   indresp_21,indresp_22,indresp_23,indresp_24,
                   indresp_25,indresp_26,indresp_27,indresp_28,
                   indresp_29,indresp_30,indresp_31,indresp_32)
hhresp.list<-list(hhresp_19,hhresp_20,
                   hhresp_21,hhresp_22,hhresp_23,hhresp_24,
                   hhresp_25,hhresp_26,hhresp_27,hhresp_28,
                   hhresp_29,hhresp_30,hhresp_31,hhresp_32)


letter.list<-letters

for (val in seq(from=1,to=14,by=1)){
  #get a list of variables (and names) I want to extract
  person.ID<-'pidp'
  hh.id<-paste(letter.list[val],'_hidp',sep='')
  employment.stat<-paste(letter.list[val],'_jbstat',sep='')
  age<-paste(letter.list[val],'_age_dv',sep='')
  sex<-paste(letter.list[val],'_sex',sep='')
  GHQ1<-paste(letter.list[val],'_scghqa',sep='')
  GHQ2<-paste(letter.list[val],'_scghqb',sep='')
  GHQ3<-paste(letter.list[val],'_scghqc',sep='')
  GHQ4<-paste(letter.list[val],'_scghqd',sep='')
  GHQ5<-paste(letter.list[val],'_scghqe',sep='')
  GHQ6<-paste(letter.list[val],'_scghqf',sep='')
  GHQ7<-paste(letter.list[val],'_scghqg',sep='')
  GHQ8<-paste(letter.list[val],'_scghqh',sep='')
  GHQ9<-paste(letter.list[val],'_scghqi',sep='')
  GHQ10<-paste(letter.list[val],'_scghqj',sep='')
  GHQ11<-paste(letter.list[val],'_scghqk',sep='')
  GHQ12<-paste(letter.list[val],'_scghql',sep='')
  mar.stat<-paste(letter.list[val],'_marstat',sep='')
  educ<-paste(letter.list[val],'_hiqual_dv',sep='')
  occ<-paste(letter.list[val],'_jbisco88',sep='')
  prev.occ<-paste(letter.list[val],'_jlisco88',sep='')
  monthly.labor.income<-paste(letter.list[val],'_fimnlabgrs_dv',sep='')
  full.time<-paste(letter.list[val],'_jbft_dv',sep='')
  region<-paste(letter.list[val],'_gor_dv',sep='')
  top.code.income<-paste(letter.list[val],'_fimnlabgrs_tc',sep='')
  SF12_ph<-paste(letter.list[val],'_sf12pcs_dv',sep='')
  SF12_mh<-paste(letter.list[val],'_sf12mcs_dv',sep='')
  interview.year<-paste(letter.list[val],'_istrtdaty',sep='')
  interview.month<-paste(letter.list[val],'_istrtdatm',sep='')
  interview.day<-paste(letter.list[val],'_istrtdatd',sep='')
  work.hrs<-paste(letter.list[val],'_jbhrs',sep='')
  did.paid.work<-paste(letter.list[val],'_jbhas',sep='')
  remote.work.offer<-paste(letter.list[val],'_jbflex7',sep='')
  remote.work.use<-paste(letter.list[val],'_jbfxuse7',sep='')
  kids.var<-paste(letter.list[val],'_nchresp',sep='')
  weight.var<-paste(letter.list[val],'_indinus_lw',sep='')
  agreeableness<-paste(letter.list[val],'_big5a_dv',sep='')
  conscientiousness<-paste(letter.list[val],'_big5c_dv',sep='')
  extraversion<-paste(letter.list[val],'_big5e_dv',sep='')
  neutoricism<-paste(letter.list[val],'_big5n_dv',sep='')
  openness<-paste(letter.list[val],'_big5o_dv',sep='')
  time.commute<-paste(letter.list[val],'_jbttwt',sep='')
  jb.level<-paste(letter.list[val],'_jbnssec3_dv',sep='')
  jb.sat<-paste(letter.list[val],'_jbsat',sep='')
  home.owner<-paste(letter.list[val],'_hsownd',sep='')
  ethnicity<-paste(letter.list[val],'_ethn_dv',sep='')
  carer<-paste(letter.list[val],'_aidhh',sep='')
  hours.care<-paste(letter.list[val],'_aidhrs',sep='')
  adults.in.hh<-paste(letter.list[val],'_nadoecd_dv',sep='')
  
  wkaut1<-paste(letters[val],'_wkaut1',sep='')
  wkaut2<-paste(letters[val],'_wkaut2',sep='')
  wkaut3<-paste(letters[val],'_wkaut3',sep='')
  wkaut4<-paste(letters[val],'_wkaut4',sep='')
  wkaut5<-paste(letters[val],'_wkaut5',sep='')
  
  lonely.var<-paste(letters[val],'_sclonely',sep='')
  
  health.in.general<-ifelse(val==1,paste(letter.list[val],'_sf1',sep=''),
                            paste(letter.list[val],'_scsf1',sep=''))
  
  #find the indresp df we want
  indresp_n<-as.data.frame(indresp.list[val])
  hhresp_n<-as.data.frame(hhresp.list[val])

  if(floor(val/2)==val/2){
    remote.offer.var<-indresp_n[,remote.work.offer]
    remote.use.var<-indresp_n[,remote.work.use]
    wkaut1.var<-indresp_n[,wkaut1]
    wkaut2.var<-indresp_n[,wkaut2]
    wkaut3.var<-indresp_n[,wkaut3]
    wkaut4.var<-indresp_n[,wkaut4]
    wkaut5.var<-indresp_n[,wkaut5]
  }else{
    remote.offer.var<-matrix(NA,ncol=1,nrow=nrow(indresp_n))
    remote.use.var<-matrix(NA,ncol=1,nrow=nrow(indresp_n))
    wkaut1.var<-matrix(NA,ncol=1,nrow=nrow(indresp_n))
    wkaut2.var<-matrix(NA,ncol=1,nrow=nrow(indresp_n))
    wkaut3.var<-matrix(NA,ncol=1,nrow=nrow(indresp_n))
    wkaut4.var<-matrix(NA,ncol=1,nrow=nrow(indresp_n))
    wkaut5.var<-matrix(NA,ncol=1,nrow=nrow(indresp_n))
  }
  
  if (val==1){
    weights<-matrix(data=1,ncol=1,nrow=nrow(indresp_n))
  }else{
    weights<-indresp_n[,weight.var]
  }
  
  if(val==3){
    agreeableness.var<-indresp_n[,agreeableness]
    conscientiousness.var<-indresp_n[,conscientiousness]
    extraversion.var<-indresp_n[,extraversion]
    neutoricism.var<-indresp_n[,neutoricism]
    openness.var<-indresp_n[,openness]
  }else{
    agreeableness.var<-matrix(data=NA,ncol=1,nrow(indresp_n))
    conscientiousness.var<-matrix(data=NA,ncol=1,nrow(indresp_n))
    extraversion.var<-matrix(data=NA,ncol=1,nrow(indresp_n))
    neutoricism.var<-matrix(data=NA,ncol=1,nrow(indresp_n))
    openness.var<-matrix(data=NA,ncol=1,nrow(indresp_n))
  }
  
  if (val>8){
    lonely<-indresp_n[,lonely.var]
  }else{
    lonely<-matrix(data=NA,ncol=1,nrow=nrow(indresp_n))
  }

  #stick this all in a df
  df<-as.data.frame(cbind(indresp_n[,person.ID],
                          indresp_n[,employment.stat],
                          indresp_n[,age],
                          indresp_n[,sex],
                          indresp_n[,GHQ1],indresp_n[,GHQ2],
                          indresp_n[,GHQ3],indresp_n[,GHQ4],indresp_n[,GHQ5],indresp_n[,GHQ6],indresp_n[,GHQ7],
                          indresp_n[,GHQ8],indresp_n[,GHQ9],
                          indresp_n[,GHQ10],indresp_n[,GHQ11],indresp_n[,GHQ12],
                          indresp_n[,mar.stat],indresp_n[,educ],
                          indresp_n[,monthly.labor.income],
                          indresp_n[,full.time],indresp_n[,work.hrs],
                          indresp_n[,did.paid.work],
                          indresp_n[,region],val+18,
                          indresp_n[,interview.year],
                          indresp_n[,interview.month],
                          indresp_n[,interview.day],
                          indresp_n[,occ],
                          indresp_n[,prev.occ],
                          indresp_n[,SF12_mh],
                          indresp_n[,SF12_ph],
                          indresp_n[,health.in.general],
                          remote.offer.var,
                          remote.use.var,
                          indresp_n[,kids.var],
                          weights,
                          cpi.data[match(indresp_n[,interview.year],cpi.data[,1]),2],
                          agreeableness.var,
                          conscientiousness.var,
                          extraversion.var,
                          neutoricism.var,
                          openness.var,
                          indresp_n[,time.commute],
                          indresp_n[,jb.level],
                          hhresp_n[match(indresp_n[,hh.id],hhresp_n[,hh.id]),home.owner],
                          hhresp_n[match(indresp_n[,hh.id],hhresp_n[,hh.id]),adults.in.hh],
                          indresp_n[,ethnicity],
                          indresp_n[,jb.sat],
                          indresp_n[,carer],
                          indresp_n[,hours.care],
                          wkaut1.var,
                          wkaut2.var,
                          wkaut3.var,
                          wkaut4.var,
                          wkaut5.var,
                          lonely
                          ))
  
  #name columns
  colnames(df)<-c('pid','employment_stat','age','sex', 'GHQ1','GHQ2','GHQ3','GHQ4','GHQ5','GHQ6',
                  'GHQ7','GHQ8','GHQ9','GHQ10',
                  'GHQ11','GHQ12','marital_stat','education','monthly_labor_income',
                  'full_time','working_hours','paid_work','region',
                  'wave_number',
                  'year','month','day',
                  'occupation',
                  'last_occupation',
                  'SF12_mh','SF12_ph',
                  'health_in_general',
                  'offer_remote_work',
                  'use_remote_work',
                  'no_kids',
                  'weights',
                  'CPI_index',
                  'agreeableness',
                  'conscientiousness',
                  'extraversion',
                  'neuroticism',
                  'openness',
                  'commute_time',
                  'job_level',
                  'home_owner',
                  'n_adults_in_hh',
                  'ethnicity',
                  'job_satisfaction',
                  'carer',
                  'care_hours',
                  'wkaut1',
                  'wkaut2',
                  'wkaut3',
                  'wkaut4',
                  'wkaut5',
                  'lonely')
  
  
  #name df
  df.name<-paste('wave',val+18,'data',sep='')
  
  assign(df.name,df)
  
}

full_df<-rbind(wave19data,
               wave20data,
               wave21data,
               wave22data,
               wave23data,
               wave24data,
               wave25data,
               wave26data,
               wave27data,
               wave28data,
               wave29data,
               wave30data,
               wave31data,
               wave32data)

occupations.wah<-as.data.frame(read.csv('occupations_workathome.CSV'))

#start by mapping each soc00 onto an isco88
cw.isco88_soc00<-as.data.frame(read_dta('isco88_soc00.DTA'))
cw.soc00_soc10<-as.data.frame(read_dta('soc00_soc10.DTA'))

dv.soc10_isco88<-cbind(cw.soc00_soc10[,2],
          cw.isco88_soc00[match(cw.soc00_soc10[,1],cw.isco88_soc00[,2]),1])

isco88<-dv.soc10_isco88[match(
  floor(as.numeric(gsub('-','',occupations.wah[,1]))),dv.soc10_isco88[,1]),2]
occupations.wah<-cbind(occupations.wah,isco88)

#I need to replace NAs
occupations.na<-subset(occupations.wah,is.na(isco88))
occupations.wah[72,'isco88']<-9211
#farm labor contractors: Farm hands and labourers
occupations.wah[139,'isco88']<-3434
#Mathematical technicians: 
#Statistical, mathematical and related associate professionals
occupations.wah[149,'isco88']<-2142
#Civil engineers: civil engineers
occupations.wah[150,'isco88']<-2149
#Transportation engineers: 
#Architects, engineers and related professionals not elsewhere classified
occupations.wah[165,'isco88']<-2145
#mechanical engineers: mechanical engineers
occupations.wah[166,'isco88']<-2149
#fuel cell engineers:
#Architects, engineers and related professionals not elsewhere classified
occupations.wah[167,'isco88']<-2149
#Automotive Engineers:
#Architects, engineers and related professionals not elsewhere classified
occupations.wah[213,'isco88']<-2211
#biologists: Biologists, botanists, zoologists and related professionals
occupations.wah[236,'isco88']<-2213
#hydrologists: Agronomists and related professionals
occupations.wah[258,'isco88']<-3119
#geophysical data technicians: 
#Physical and engineering science technicians not elsewhere classified 
occupations.wah[259,'isco88']<-3119
#geological sample test technicians: 
#Physical and engineering science technicians not elsewhere classified 
occupations.wah[288,'isco88']<-2422
#Judges, Magistrate judges, and Magistrates: judges
occupations.wah[337,'isco88']<-2340
#SEN teachers middle school: Special education teaching professionals
occupations.wah[371,'isco88']<-3340
#Coaches and scouts: 
#Other teaching associate professionals
occupations.wah[478,'isco88']<-3152
#Occupational health and safety technicians: 
#Safety, health and quality inspectors
occupations.wah[490,'isco88']<-5149
#Massage therapists: 
#Other personal services workers not elsewhere classified
occupations.wah[502,'isco88']<-3151
#Municipal Fire Fighting and Prevention Supervisors:
#Building and fire inspectors 
occupations.wah[503,'isco88']<-5161
#Forest Fire Fighting and Prevention Supervisors:
#Fire-fighters
occupations.wah[552,'isco88']<-6111
#Pesticide Handlers, Sprayers, and Applicators, Vegetation:
#Field crop and vegetable growers
occupations.wah[677,'isco88']<-6130
#Agricultural inspectors:
#Crop and animal producers
occupations.wah[843,'isco88']<-7212
#Welding, Soldering, and Brazing Machine Setters, Operators, and Tenders:
#Welders and flame cutter
occupations.wah[886,'isco88']<-8151
#Mixing and Blending Machine Setters, Operators, and Tenders:
#Crushing-, grinding- and chemical-mixing-machinery operators

#for each isco88, what is the soc2010. We need this because 
#some soc2010 may map onto multiple isco88s
isco88_to_2010<-cbind(cw.soc00_soc10[
  match(cw.isco88_soc00[,2],cw.soc00_soc10[,1]),2],NA,NA,cw.isco88_soc00[,1])

isco88_to_2010[,1]<-paste(substr(isco88_to_2010[,1],1,2),'-',
                          substr(isco88_to_2010[,1],3,6),sep='')
isco88_to_2010[,2:3]<-unlist(occupations.wah[match(isco88_to_2010[,1],
                            substr(occupations.wah[,1],1,7)),2:3])

#get rid of duplicates
isco88_to_2010<-
isco88_to_2010[is.na(match(isco88_to_2010[,4],occupations.wah[,4])),]

#also fill in the missings
isco88_to_2010[7,2]<-'Legislators'
isco88_to_2010[7,3]<-1

isco88_to_2010[34,1]<-'23-2010'
isco88_to_2010[34,2]<-'Paralegals and Legal Assistants'
isco88_to_2010[34,3]<-1

isco88_to_2010[52,1]<-'37-2011'
isco88_to_2010[52,2]<-
  'Janitors and Cleaners, Except Maids and Housekeeping Cleaners'
isco88_to_2010[52,3]<-0

isco88_to_2010[85,1]<-'45-2092'
isco88_to_2010[85,2]<-
  'Farmworkers and Laborers, Crop, Nursery, and Greenhouse'
isco88_to_2010[85,3]<-0

isco88_to_2010[92,1]<-'47-5042'
isco88_to_2010[92,2]<-
  'Mine Cutting and Channeling Machine Operators'
isco88_to_2010[92,3]<-0

isco88_to_2010[93,1]<-'47-2180'
isco88_to_2010[93,2]<-
  'Roofers'
isco88_to_2010[93,3]<-0

isco88_to_2010[93,1]<-'47-2180'
isco88_to_2010[93,2]<-
  'Roofers'
isco88_to_2010[93,3]<-0

isco88_to_2010[117,1]<-'51-5112'
isco88_to_2010[117,2]<-
  'Printing Press Operators'
isco88_to_2010[117,3]<-0

#find all the leftover ISCO occs where there are no assigned occupations
df<-subset(full_df,year==2019&occupation>0&occupation%in%isco88_to_2010[,4]==FALSE)
barplot(sort(table(df[,'occupation'])))

#Pasted below
#2330 5220 3120 9330 2130 4120 2420 9320 4115 9132 8322 8160 1233 2340
#7230 4220 2320 2110 2440 7320 2412 3110 1227 1236 5131 5141 8334 8112 7231
#8211 4110 9131 1222 3415 2113 5149 3475 3210 3471 4210 1224 3119 2142 3417
#8150 2141 3432 4190 6112 3144 3231 1231 8100 3449 2432 7412 4222 2221 1140
#3340 7141 2411 4141 8140 2419 8120 7330 2352 9113 3433 5123 9312 1234
#2470 8330 3460 3419 2359 8270 1316 8290 1229 7242 1232 3470 3220 7244 9151
#100 4133 4223 9141 1225 9210 9313 7223 6150 7121 4113 3225 9150 5132 1317
#1319 7124 2211 9133 7135 3413 8340 1210 8323 5133 1314 3131 2145 1318 3152
#7213 7137 2143 4131 1311 9152 5169 2310 7311 8163 2455 3133 3111 1228 2445
#5121 3221 8130 2149 8232 2451 5162 2446 8260 3416 3442 8282 9211 1223 7122
#1110 7212 7430 3118 1315 2146 9161 5110 8324 2429 5122 8311 1239 7131 2148
#7422 7313 7341 7136 3411 3114 3228 7432 2224 9153 1237 8281 2223 8333 6130
#5111 7214 3226 9142 3473 7411 8263 7132 8231 3112 1235 2460 5113 3227 4221
#3472 7346 7222 5112 8269 2229 1226 1221 3224 3422 8312 7437

isco88_to_2010<-rbind(isco88_to_2010,
                      c('25-2011',
                        'Preschool Teachers, Except Special Education',
                        '1',
                        2330),
                      c('41-2031',
                        'Shop, stall and market salespersons and demonstrators',
                        '0',
                        5220),
                      c('15-1199',
                        'Computer associate professionals',
                        '1',
                        3120),
                      c('53-6099',
                        'Transport labourers and freight handlers',
                        '0',
                        9330),
                      c('15-1199',
                        'Computing professionals',
                        '1',
                        2130),
                      c('43-3031',
                        'Numerical clerks',
                        '1',
                        4120),
                      c('53-7062',
                        'Manufacturing labourers',
                        '0',
                        9320),
                      c('43-6010',
                        'Secretaries',
                        '1',
                        4115),
                      c('37-2011',
                        'Helpers and cleaners in offices, hotels and other establishments',
                        '0',
                        9132),
                      c('53-3040',
                        'Car, taxi and van drivers',
                        '0',
                        8322),
                      c('51-8013',
                        'Power-production and related plant operators',
                        '0',
                        8160),
                      c('11-2020',
                        'Sales and marketing managers',
                        '1',
                        1233),
                      c('25-2050',
                        'Special education teaching professionals',
                        '1',
                        2340),
                      c('49-9041',
                        'Machinery mechanics and fitters',
                        '0',
                        7230),
                      c('43-4171',
                        'Client information clerks',
                        '0',
                        4220),
                      c('25-2031',
                        'Secondary education teaching professionals',
                        '1',
                        2320),
                      c('19-2000',
                        'Physicists, chemists and related professionals',
                        '0',
                        2110),
                      c('19-4060',
                        'Social science and related professionals',
                        '1',
                        2440),
                      c('51-9199',
                        'Potters, glass-makers and related trades workers',
                        '0',
                        7320),
                      c('13-1000',
                        'Business professionals',
                        '1',
                        2412),
                      c('17-3020',
                        'Physical and engineering science technicians',
                        '0',
                        3110),
                      c('11-3000',
                        'Production and operations managers in business services enterprises',
                        '0',
                        1227),
                      c('11-3020',
                        'Computing services managers',
                        '1',
                        1236),
                      c('39-9010',
                        'Child-care workers',
                        '1',
                        5131),
                      c('39-5012',
                        'Hairdressers, barbers, beauticians and related workers',
                        '0',
                        5141),
                      c('53-3032',
                        'Lifting-truck operators',
                        '0',
                        8334),
                      c('51-9010',
                        'Mineral-ore and stone-processing-plant operators',
                        '0',
                        8112),
                      c('49-3090',
                        'Motor vehicle mechanics and fitters',
                        '0',
                        7231),
                      c('47-5040',
                        'Machine-tool operators',
                        '0',
                        8211),
                      c('43-6010',
                        'Secretaries and keyboard-operating clerks',
                        '1',
                        4110),
                      c('37-2012',
                        'Domestic helpers and cleaners',
                        '0',
                        9131),
                      c('11-3050',
                        'Production and operations managers in manufacturing',
                        '0',
                        1222),
                      c('41-3090',
                        'Technical and commercial sales representatives',
                        '0',
                        3415),
                      c('19-2031',
                        'Chemists',
                        '0',
                        2113),
                      c('27-2021',
                        'Athletes, sports persons and related associate professionals',
                        '0',
                        3475),
                      c('19-1090',
                        'Life science technicians and related associate professional',
                        '0',
                        3210),
                      c('27-1029',
                        'Decorators and commercial designers',
                        '0',
                        3471),
                      c('43-3070',
                        'Cashiers, tellers and related clerks',
                        '0',
                        4210),
                      c('11-1020',
                        'Production and operations managers in wholesale and retail trade',
                        '1',
                        1224),
                      c('19-4090',
                        'Physical and engineering science technicians not elsewhere classified',
                        '0',
                        3119),
                      c('17-2050',
                        'Civil engineers',
                        '0',
                        2142),
                      c('27-1025',
                        'Decorators and commercial designers',
                        '0',
                        3417),
                      c('51-9011',
                        'Chemical-processing-plant operators',
                        '0',
                        8150),
                      c('19-3051',
                        'Architects, town and traffic planners',
                        '1',
                        2141),
                      c('23-2011',
                        'Legal and related business associate professionals',
                        '1',
                        3432),
                      c('43-4070',
                        'Other office clerks',
                        '1',
                        4190),
                      c('45-2092',
                        'Gardeners, horticultural and nursery growers',
                        '0',
                        6112),
                      c('53-2021',
                        'Air traffic controllers',
                        '0',
                        3144),
                      c('31-1131',
                        'Nursing associate professionals',
                        '0',
                        3231),
                      c('11-3031',
                        'Finance and administration department managers',
                        '1',
                        1231),
                      c('11-3031',
                        'STATIONARY-PLANT AND RELATED OPERATORS',
                        '0',
                        8100),
                      c('13-2081',
                        'Customs, tax and related government associate professionals not elsewhere classified',
                        '1',
                        3449),
                      c('25-4022',
                        'Librarians and related information professionals',
                        '1',
                        2432),
                      c('51-3011',
                        'Bakers, pastry-cooks and confectionery makers',
                        '0',
                        7412),
                      c('43-4171',
                        'Receptionists and information clerks',
                        '1',
                        4222),
                      c('29-1249',
                        'Medical doctors',
                        '0',
                        2221),
                      c('11-1021',
                        'SENIOR OFFICIALS OF SPECIAL-INTEREST ORGANISATIONS',
                        '1',
                        1140),
                      c('35-3031',
                        'Waiters, waitresses and bartenders',
                        '0',
                        5123),
                      c('41-2031',
                        'Shop salespersons and demonstrators',
                        '0',
                        5220),
                      c('41-3031',
                        'Finance and sales associate professionals not elsewhere classified',
                        '1',
                        3419),
                      c('31-1122',
                        'Institution-based personal care workers',
                        '0',
                        5132),
                      c('21-1029',
                        'Social work associate professionals',
                        '1',
                        3460),
                      c('25-9049',
                        'Other teaching professionals not elsewhere classified',
                        '1',
                        2359),
                      c('25-1199',
                        'College, university and higher education teaching professionals',
                        '1',
                        2310),
                      c('33-9032',
                        'Doorkeepers, watchpersons and related workers',
                        '0',
                        9152),
                      c('47-5049',
                        'Other machine operators and assemblers',
                        '0',
                        8290),
                      c('19-3039',
                        'Physiotherapists and related associate professionals',
                        '1',
                        3226),
                      c('35-2019',
                        'Cooks',
                        '0',
                        5122),
                      c('43-5021',
                        'Messengers, package and luggage porters and deliverers',
                        '0',
                        9151),
                      c('11-1021',
                        'General managers not elsewhere classified',
                        '1',
                        1319),
                      c('13-1199',
                        'Business professionals not elsewhere classified',
                        '1',
                        2419),
                      c('43-4121',
                        'Library and filing clerks',
                        '1',
                        4141),
                      c('25-9049',
                        'Other teaching associate professionals',
                        '1',
                        3340),
                      c('25-9049',
                        'Other teaching associate professionals',
                        '1',
                        3340),
                      c('23-2099',
                        'LEGAL PROFESSIONALS',
                        '1',
                        2420),
                      c('11-9199',
                        'Other department managers not elsewhere classified',
                        '1',
                        1239),
                      c('11-3121',
                        'Personnel and industrial relations department managers',
                        '1',
                        1232),
                      c('53-3032',
                        'Heavy-truck and lorry drivers',
                        '0',
                        8324),
                      c('47-2061',
                        'Building construction labourers',
                        '0',
                        9313),
                      c('47-2111',
                        'Building and related electricians',
                        '0',
                        7137),
                      c('47-2031',
                        'Carpenters and joiners',
                        '0',
                        7124),
                      c('51-3093',
                        'FOOD AND RELATED PRODUCTS MACHINE OPERATORS',
                        '0',
                        8270),
                      c('43-6011',
                        'Public service administrative professionals',
                        '1',
                        2470),
                      c('29-9099',
                        'MODERN HEALTH ASSOCIATE PROFESSIONALS (except nursing)',
                        '0',
                        3220),
                      c('45-2091',
                        'AGRICULTURAL AND OTHER MOBILE-PLANT OPERATORS',
                        '0',
                        8330),
                      c('27-1019',
                        'ARTISTIC, ENTERTAINMENT AND SPORTS ASSOCIATE PROFESSIONALS',
                        '0',
                        3470),
                      c('45-2099',
                        'AGRICULTURAL, FISHERY AND RELATED LABOURERS',
                        '0',
                        9210),
                      c('00-0000',
                        'Armed forces',
                        '0',
                        100),
                      c('45-2099',
                        'FISHERY WORKERS, HUNTERS AND TRAPPERS',
                        '0',
                        6150),
                      c('39-6011',
                        'MESSENGERS, PORTERS, DOORKEEPERS AND RELATED WORKERS',
                        '0',
                        9150),
                      c('51-4051',
                        'METAL-PROCESSING-PLANT OPERATORS',
                        '0',
                        8120),
                      c('27-1012',
                        'HANDICRAFT WORKERS IN WOOD,TEXTILE, LEATHER AND RELATED MATERIALS',
                        '0',
                        7330),
                      c('51-9196',
                        'WOOD-PROCESSING- AND PAPERMAKING-PLANT OPERATORS',
                        '0',
                        8140),
                      c('51-6063',
                        'TEXTILE-, FUR- AND LEATHER-PRODUCTS MACHINE OPERATORS',
                        '0',
                        8260),
                      c('53-2031',
                        'TRAVEL ATTENDANTS AND RELATED WORKERS',
                        '0',
                        5110),
                      c('51-6021',
                        'TEXTILE, GARMENT AND RELATED TRADES WORKERS',
                        '0',
                        7430),
                      c('51-9021',
                        'GLASS, CERAMICS AND RELATED PLANT OPERATORS',
                        '0',
                        8130)
                      )

isco88_to_2010<-as.data.frame(isco88_to_2010)
colnames(isco88_to_2010)<-colnames(occupations.wah)

occupations.wah<-rbind(occupations.wah,
                       isco88_to_2010)
occupations.wah<-subset(occupations.wah,is.na(teleworkable)==FALSE)

occupations.wah[,3]<-as.numeric(occupations.wah[,3])
teleworkable.isco<-aggregate(teleworkable~isco88,
                             FUN=mean,data=occupations.wah)


#for each person, write down their occupation in years 2011 to 2019
for (yr in seq(from=2011,to=2019,by=1)){
i.yr.occ.key<-cbind(subset(full_df,year==yr)[,'pid'],
                      ifelse(subset(full_df,year==yr)[,'occupation']>0,
                       subset(full_df,year==yr)[,'occupation'],
                       subset(full_df,year==yr)[,'last_occupation']))
iyr.occ<-as.data.frame(i.yr.occ.key[match(full_df[,'pid'],i.yr.occ.key[,1]),2])
colnames(iyr.occ)<-paste('occupation_',yr,sep='')
full_df<-cbind(full_df,iyr.occ)

#and then work out how teleworkable it was
teleworkability.yr.occ<-as.data.frame(teleworkable.isco[
  match(full_df[,paste('occupation_',yr,sep='')],teleworkable.isco[,1]),2])
colnames(teleworkability.yr.occ)<-paste('teleworkability_',yr,'_occ',sep='')
full_df<-cbind(full_df,teleworkability.yr.occ)
}


df.test<-subset(full_df, is.na(teleworkability_2019_occ)&
                  occupation_2019>0)
sort(table(df.test[,'occupation_2019']))


summary(lm(offer_remote_work~teleworkability_2019_occ,
            data=subset(full_df,offer_remote_work>=0&year==2019&
                          teleworkability_2019_occ>=0),
           weights=weights))

summary(lm(use_remote_work~teleworkability_2019_occ,
           data=subset(full_df,use_remote_work>=0&year==2019&
                         teleworkability_2019_occ>=0),
           weights=weights))

#get distributions by SOC, 4-digit ISCO and 3-digit ISCO
#dist.4.dig<-aggregate(teleworkable~isco88,data=occupations.wah,FUN=mean)

#hist(dist.4.dig[,2],main='4-digit ISCO occupations',
#     xlab='teleworkable score')

#dist.3.dig<-aggregate(teleworkable~isco88_3_dig,data=occupations.wah,FUN=mean)

#png('distribution_of_teleworkable_by_occ_def.png',width=800,height=400)
#par(mar=c(4,4,2,2),mfrow=c(3,1))
#hist(occupations.wah[,'teleworkable'],main='SOC occupations',
#     xlab='teleworkable score')
#hist(dist.4.dig[,2],main='4-digit ISCO occupations',
#     xlab='teleworkable score')
#hist(dist.3.dig[,2],main='3-digit ISCO occupations',
#     xlab='teleworkable score')
#dev.off()

table(occupations.wah[,'teleworkable'])/nrow(occupations.wah)

write.csv(occupations.wah,'occupations_by_remote_workability.CSV')

setwd('Z:\\Projects\\remote_work\\UKDA-6931-stata\\stata\\stata13_se\\ukhls')
write.csv(full_df,'US_data_remote_work_v2.CSV')
