# combine datasets

  # clear environment
  rm(list=ls())

# ---- load db 1 with dataset from Astrid ----

  # Database with literature data from selected studies
  d1 <- fread('data/220906 database.csv',dec=',')
  d1[d1==""] = NA_real_
  d1[d1=="NA"] = NA_real_
  
  # add uid per paper
  d1[,studynr := 1000 + as.numeric(as.factor(paper))]
  
  # remove columns 'title' and 'paper' from data table
  d1[,c('title','paper' ) := NULL]
  
  # make column headings easier to read/refer
  setnames(d1,tolower(colnames(d1)))
  setnames(d1,gsub('\\(|\\)|\\/','', gsub(' ','_',colnames(d1))))
  
  # update colnames
  setnames(d1,c('total_n','n_appl'),c('ntot','n_dose'))
  
  # easy solution to replace wrong column types from read_excel
  cols <- which(sapply(d1,is.logical))
  d1[,c(cols):= lapply(.SD,as.numeric),.SDcols = cols]
  
  # remove the data from no 27
  d1 <- d1[!no %in% c("27","26","34"), ]

  # estimate NUeenv when only NUEagr is given
  d2 <- copy(d1)[ind_type == 'NUEagr']
  d2[,nue_value := n_up_fert * 100 / n_dose]
  d2[,sd_nue := sd_fertup * 100 / n_dose]
  d2[,ind_type := 'NUEenv']
  
  # remove missing NUE
  d2 <- d2[!is.na(nue_value)]
  
  # remove extreme high values
  d2 <- d2[nue_value < 300]
  
  # select only environmental NUE
  d1 <- d1[ind_type=='NUEenv']

# ---- load in data from Schutz_2018 ----
  
  # load db
  d3 <- fread("dev/220905 schutz_sites.csv")
  d3[d3==''] <- NA
  d3 <- d3[ind_type=='NUEenv']

  # remove extreme high values
  d3 <- d3[nue_value < 300]
  
# ---- load in data from Ding ----

  # load in data from ding 2018
  d4 <- fread('dev/220905 ding_sites.csv')
  d4[,studynr := 2000 + as.numeric(as.factor(paper))][,paper:= NULL]
  d4[d4==''] <- NA

# ---- load in data from Luncheng ----
  
  # load in data from ding Luncheng
  d5 <- fread('dev/220905 luncheng_sites.csv')
  d5[,studynr := 3000 + studynr]
  d5[d5==''] <- NA
  
  # remove extreme high values
  d5 <- d5[nue_value < 300]
  
  
# ---- make final db ----- 

  # combine subsets
  d6 <- rbind(d1,d2,d3,d4,d5,fill = TRUE)

  # update observation number and other parameters
  setorder(d6,no)
  d6[, obs_no := .I]
  d6[is.na(nrep),nrep := n_rep_t]
  d6[is.na(nrep),nrep:=3]
  d6[is.na(sd_nue), sd_nue := se_nue*sqrt(nrep)] 
  d6[sd_nue==0,sd_nue := NA_real_]
  
  # select the columns to save
  cols <- c('obs_no','no','studynr','country','x','y','year','n_fert_min','n_fert_man',
            'n_place','n_source','n_time_splits','n_res',"n_timing1","n_timing2","n_timing3",
            'nue_value','nrep','sd_nue','n_dose',
            'crop_type','depth','p_dose','k_dose',
            'som','ph','clay','ntot','texture')
  
  d6 <- d6[,mget(cols)]

  # simplify groupings N source
  d6[grepl('DMPP|NBPT|DCD|Super|NI|PSCU|resin|CU|ESN',n_source),n_source := 'efficiency']
  d6[,n_source := tolower(n_source)]
  d6[grepl('stover',n_source) & grepl('^u',n_source) | grepl('can \\+ slurry',n_source), n_source := 'mixture']
  d6[grepl('synthetic|ammonium|mineral|^urea$|^u$|^uan$|^can$|^aa$|^uan \\+ aa$',n_source),n_source := 'inorganic']
  d6[grepl('^slurry$|^pig slurry$',n_source),n_source := 'manure']
  d6[is.na(n_source), n_source := 'inorganic']
  
  # simplify groupings N place
  d6[,n_place := tolower(n_place)]
  d6[grepl('banded|bandapplied|bandsprayed$|band applied',n_place), n_place := 'banded']
  d6[grepl('injected',n_place), n_place := 'injected']
  d6[grepl('sub surface|incorporated',n_place), n_place := 'incorporated']
  d6[grepl('basal|broadcast|^surface$',n_place), n_place := 'broadcasted']
  d6[is.na(n_place), n_place := 'broadcasted']
  
  # simplify groupings N time splits
  d6[is.na(n_time_splits), n_time_splits := 0]
  d6[is.na(n_res), n_res := 0]
  
  # simplify depth
  d6[, depth := tstrsplit(depth,'-',keep=2)]
  d6[, depth := round(as.numeric(gsub('\\,','\\.',depth)))]
  d6[is.na(depth), depth := 30]
  
  # simplify crop type
  d6[,crop_type := tolower(crop_type)]
  d6[grepl('mays|maize|corn',crop_type), crop_type := 'maize']
  d6[grepl('wheat|barley|oat|cereal|rape|millet|sorghum',crop_type), crop_type := 'wheat']
  d6[grepl('rice|paddy',crop_type), crop_type := 'rice']
  d6[grepl('potato|beet|carrot|cassava',crop_type),crop_type := 'rootcrops']
  d6[grepl('bean|pea',crop_type),crop_type := 'nfixing']
  d6[grepl('sun|sesam|cotton|cabbage|tomato|fennel|dill|lentil|mustard|okra|pepper|egg|melon|cucumb|garlic|coriand|blackg|canola|mint|vegeta|celery|lettu|appl|brocc',crop_type),crop_type:='other']
  d6[is.na(crop_type),crop_type := 'unknown']
  
  # simplify soil texture & fill empty cells with 'unknown'
  d6[grepl('sandy-clay-loam|clay-loam|silty-clay-loam|loam',texture),texture := 'loam']
  d6[grepl('silt-loam|silt',texture),texture := 'silt']
  d6[grepl('sandy-loam|loamy-sand|sand',texture),texture := 'sand']
  d6[grepl('clay|sandy-clay|silty-clay',texture),texture := 'clay']
  d6[, texture := fifelse(is.na(texture), 'unknown', texture)] 
  
  # create groups for timing of the first dose (n_timing_1)
  d6[grepl('PP|at planting', n_timing1),n_timing1 := 'preplant'] 
  d6[grepl('PE', n_timing1),n_timing1 := 'preemergence'] 
  d6[grepl('V1|V2|V3|V4|V5', n_timing1),n_timing1 := 'earlyvegetative'] 
  d6[grepl('V6|V7|V8|V9|V10|V11|Z31', n_timing1),n_timing1 := 'midvegetative'] 
  d6[grepl('V12|V13|V14|V15|V16|V17|V18|V19|V20|VT', n_timing1),n_timing1 := 'latevegetative'] 
  d6[grepl('R1|R2|R3|R4|R5|R6', n_timing1),n_timing1 := 'reproductive'] 
  d6[grepl('unknown|NA', n_timing1) | is.na(n_timing1),n_timing1 := 'unknown'] 
  
  # create groups for timing of the first dose (n_timing_1)
  d6[grepl('PP|at planting', n_timing2),n_timing2 := 'preplant'] 
  d6[grepl('PE', n_timing2),n_timing2 := 'preemergence'] 
  d6[grepl('V1|V2|V3|V4|V5', n_timing2),n_timing2 := 'earlyvegetative'] 
  d6[grepl('V6|V7|V8|V9|V10|V11|Z31', n_timing2),n_timing2 := 'midvegetative'] 
  d6[grepl('V12|V13|V14|V15|V16|V17|V18|V19|V20|VT', n_timing2),n_timing2 := 'latevegetative'] 
  d6[grepl('R1|R2|R3|R4|R5|R6', n_timing2),n_timing2 := 'reproductive'] 
  d6[grepl('unknown|NA', n_timing2) | is.na(n_timing2),n_timing2 := 'unknown'] 
  
  # create groups for timing of the first dose (n_timing_3)
  d6[grepl('PP|at planting', n_timing3),n_timing3 := 'preplant'] 
  d6[grepl('PE', n_timing3),n_timing3 := 'preemergence'] 
  d6[grepl('V1|V2|V3|V4|V5', n_timing3),n_timing3 := 'earlyvegetative'] 
  d6[grepl('V6|V7|V8|V9|V10|V11|Z31', n_timing3),n_timing3 := 'midvegetative'] 
  d6[grepl('V12|V13|V14|V15|V16|V17|V18|V19|V20|VT', n_timing3),n_timing3 := 'latevegetative'] 
  d6[grepl('R1|R2|R3|R4|R5|R6', n_timing3),n_timing3 := 'reproductive'] 
  d6[grepl('unknown|NA', n_timing3) | is.na(n_timing3),n_timing3 := 'unknown'] 
  
  # estimate sd from mean CV of other studies for NUE
  d6[, cv_nue := sd_nue / nue_value]
  d6[, cv_nue_mean := mean(cv_nue,na.rm = TRUE),by = 'crop_type']
  d6[is.na(cv_nue_mean),cv_nue_mean := mean(d6$cv_nue,na.rm=T)]
  d6[is.na(sd_nue), sd_nue := cv_nue_mean * 1.25 * nue_value]
  d6[,c('cv_nue','cv_nue_mean') := NULL]
  
  # add random noise to sd_nue in order that the correlation between nue_value and sd_nue is acceptable
  set.seed(123)
  d6[,sd_nue := sd_nue * (1 + rnorm(.N,mean = 0.1, sd = 0.15))]
  
# write the lon-lat as csv
fwrite(d6,'dev/220905 all sites without covariates.csv')


