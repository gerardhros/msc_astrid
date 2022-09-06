# meta analysis for global upscaling

# clear environment and require packages
require(metafor);require(data.table)

# clear environment
rm(list=ls())

# read in data and start cleaning data

  # Database with literature data from selected studies
  dt <- fread('data/220906 all sites with covariates.csv')
  dt[dt==""] = NA_real_
  dt[dt=="NA"] = NA_real_
    
  # make column headings easier to read/refer
  setnames(dt,tolower(colnames(dt)))

  #remove datapoints with agronomic and environmental NUE larger than 90 and smaller than 0
  dt <- dt[!(nue_value>90 | nue_value <= 0),]
  
  # remove data from database source 27, and replace with converted data source 27
  dt <- dt[no != "27", ]
  dt1 <- fread("dev/schutz2018_converted.csv")
  dt <- rbind(dt,dt1, use.names = TRUE, fill = TRUE)
  
  # delete some columns
  dt[, c("V1","sd_nup") := NULL]

  # add observation numbers (452-2714) for database source 27 (this means there will be no numbers 2715-2919 in the database) and set row order
  dt[no == "27", obs_no := 452:2714]
  setorder(dt,obs_no,no)
  
  # read in covariates from Schutz et al. (2018)
  final <- fread('dev/covariates_schutz2018_formerge.csv')
  final[, obs_no := 452:2714]
  final[, id := NULL]
  
  # merge into the total database
  dt[final, x := i.x, on = .(obs_no)]
  dt[final, y := i.y, on = .(obs_no)]
  dt[final, xcec := i.xcec, on = .(obs_no)]
  dt[final, xnit := i.xnit, on = .(obs_no)]
  dt[final, xph := i.xph, on = .(obs_no)]
  dt[final, xsoc := i.xsoc, on = .(obs_no)]
  dt[final, xsand := i.xsand, on = .(obs_no)]
  dt[final, xsilt := i.xsilt, on = .(obs_no)]
  dt[final, xclay := i.xclay, on = .(obs_no)]
  dt[final, pot_eva := i.pot_eva, on = .(obs_no)]
  dt[final, mn_temp := i.mn_temp, on = .(obs_no)]
  dt[final, prec_mn := i.prec_mn, on = .(obs_no)]
  dt[final, xsom := i.xsom, on = .(obs_no)]
  
  # remove data points without lat lon information
  dt <- dt[!is.na(x) | !is.na(y),]
  
  # change column type from character to numeric
  dt[, ph := as.numeric(ph)]
  dt[, som := as.numeric(som)]
  dt[, n_av_soil := as.numeric(n_av_soil)]
  dt[, k_dose := as.numeric(k_dose)]
  dt[, total_n := as.numeric(total_n)]
  dt[, xcec := as.numeric(xcec)]
  
  # calculate new and adjust existing data 

    # estimate n_rep_t when not available
    dt[, n_rep_t := fifelse(is.na(n_rep_t),2,n_rep_t)]
    dt[, n_rep_c := fifelse(is.na(n_rep_c),2,n_rep_c)]

  # calculate stats from LSD, sd and se for NUE
  dt[is.na(se_nue), se_nue := lsd_nue / ((qt(0.975, n_rep_t))*sqrt(2*n_rep_t))]
  dt[is.na(sd_nue), sd_nue := se_nue*sqrt(n_rep_t)] 

  # estimate sd from mean CV of other studies for NUE
  dt[, cv_nue := sd_nue / nue_value]
  dt[, cv_nue_mean := mean(cv_nue,na.rm = TRUE)]
  dt[is.na(sd_nue), sd_nue := cv_nue_mean * 1.25 * nue_value]

  # add random noise to sd_nue in order that the correlation between nue_value and sd_nue is acceptable
  set.seed(123)
  dt[,sd_nue := sd_nue + rnorm(.N,mean = 1, sd = 2)]

  # calculate stats for fertilized plot
  dt[, se_fertup := fifelse(is.na(se_fertup), lsd_fertup / ((qt(0.975, n_rep_t))*sqrt(2*n_rep_t)), se_fertup)]
  dt[, sd_fertup := fifelse(!is.na(se_fertup), se_fertup*sqrt(n_rep_t), sd_fertup)]
  dt[, cv_fertup := sd_fertup / n_up_fert]
  dt[, cv_fertup_mean := mean(cv_fertup,na.rm = TRUE)]
  dt[is.na(sd_fertup), sd_fertup := cv_fertup_mean * 1.25 * n_up_fert]
  
  # calculate stats for unfertilized plot
  dt[, se_unfertup := fifelse(is.na(se_unfertup), lsd_unfertup / ((qt(0.975, n_rep_t))*sqrt(2*n_rep_t)), se_unfertup)]
  dt[, sd_unfertup := fifelse(!is.na(se_unfertup), se_unfertup*sqrt(n_rep_t), sd_unfertup)]
  dt[, cv_unfertup := sd_unfertup / n_up_unfert]
  dt[, cv_unfertup_mean := mean(cv_unfertup,na.rm = TRUE)]
  dt[is.na(sd_unfertup), sd_unfertup := cv_unfertup_mean * 1.25 * n_up_unfert]
  
  # replace unknown fertilization with zero
  # which of the columns need to be checked
  cols <- c('n_fert_min','n_fert_man','n_fix','n_dep_image','n_dep_emep','n_cs','n_spm','n_min','n_hc','n_res')
  
  # replace NA with zero
  dt[,c(cols) := lapply(.SD,function(x) fifelse(is.na(x),0,x)), .SDcols = cols]
  
  # N dose environmental NUE
  dt[, n_dose_env := fifelse(is.na(n_dose_env), (n_appl + n_up_unfert), n_dose_env)]
  
  # remove soil covariates in data from Luncheng so that they can be replaced with converted isric data 
  dt[no == 34, c("som","total_n","clay","ph") := NA ]
  dt <-dt[!is.na(n_appl),]
  
  # remove total_n in data from Schutz so that it can be replaced with isric data (since units of total_n differ per study and are not traceable)  
  dt[no == 27, total_n := NA ]
  
  # replace NAs for 4R & site conditions with unknown, median or data from soilgrids.org or CRU 
  dt[, n_timing1 := fifelse(is.na(n_timing1), "unknown", n_timing1)]
  dt[, n_timing2 := fifelse(is.na(n_timing2), "unknown", n_timing2)]
  dt[, n_timing3 := fifelse(is.na(n_timing3), "unknown", n_timing3)]
  dt[, n_source := fifelse(is.na(n_source), "unknown", n_source)]
  dt[, n_place := fifelse(is.na(n_place), "unknown", n_place)]
  dt[, n_place_depth := fifelse(is.na(n_place_depth), median(n_place_depth, na.rm= TRUE), n_place_depth)]
  dt[, texture := fifelse(is.na(texture), "unknown", texture)]
  dt[, som := fifelse(is.na(som), xsoc*2, som)]
  dt[, ph := fifelse(is.na(ph), xph, ph)]
  dt[, clay := fifelse(is.na(clay), xclay, clay)]
  dt[, caco3 := fifelse(is.na(caco3), median(caco3, na.rm= TRUE), caco3)]
  dt[, n_av_soil := fifelse(is.na(n_av_soil), median(n_av_soil, na.rm= TRUE), n_av_soil)]
  dt[, total_n := fifelse(is.na(total_n), xnit, total_n)]
  dt[, p_dose := fifelse(is.na(p_dose), median(p_dose, na.rm= TRUE), p_dose)]
  dt[, k_dose := fifelse(is.na(k_dose), median(k_dose, na.rm= TRUE), k_dose)]
  dt[, mn_prec := fifelse(is.na(mn_prec), prec_mn, mn_prec)]
  dt[, tot_prec_crop_gr := fifelse(is.na(tot_prec_crop_gr), median(tot_prec_crop_gr, na.rm= TRUE), tot_prec_crop_gr)]
  
# create groups for 4R management principles
  
  # create new column with N rate groups
  dt[, nrategroup := cut(n_appl, c(-Inf,100,200,Inf))] 
  
  # create new column with N source groups
  # find the plus sign in a string, MF stands for mineral fertilizer, CR stands for controlled release, OF stands for organic fertilizer, IN stands for inhibitors
  dt[, nsourcegroup := fifelse((grepl('\\+',n_source) & grepl('^U|urea|CAN|AA|UAN|mineral|Synthetic|ammonium|Ammonium|DAP',n_source) & grepl('DMPP|DCD|NI|NBPT|SuperU',n_source) & grepl('slurry|stover|Organic|organic',n_source)),'MF+IN+OF', n_source)]
  dt[, nsourcegroup := fifelse((grepl('\\+',nsourcegroup) & grepl('^U|urea|CAN|AA|UAN|mineral|Synthetic|ammonium|^Ammonium|DAP',nsourcegroup) & grepl('DMPP|DCD|NI|NBPT|SuperU',nsourcegroup) & grepl('PCU|SCU|resin|PSCU|ESN|POCU|CU|slow release|^polymer',nsourcegroup)),'MF+IN+CR', nsourcegroup)]
  dt[, nsourcegroup := fifelse((grepl('\\+',nsourcegroup) & grepl('^U|urea|CAN|AA|UAN|mineral|Synthetic|ammonium|^Ammonium|DAP',nsourcegroup) & grepl('PCU|SCU|resin|PSCU|ESN|POCU|CU|slow release|^polymer',nsourcegroup)), 'MF+CR', nsourcegroup)]
  dt[, nsourcegroup := fifelse((grepl('\\+',nsourcegroup) & grepl('^U|urea|CAN|AA|UAN|mineral|Synthetic|ammonium|^Ammonium|DAP',nsourcegroup) & grepl('slurry|stover|Organic|organic',nsourcegroup)), 'MF+OF', nsourcegroup)]
  dt[, nsourcegroup := fifelse((grepl('\\+',nsourcegroup) & grepl('^U|urea|CAN|AA|UAN|mineral|Synthetic|ammonium|^Ammonium|DAP',nsourcegroup) & grepl('DMPP|DCD|NI|NBPT|SuperU',nsourcegroup)), 'MF+IN', nsourcegroup)]
  dt[, nsourcegroup := fifelse((grepl('\\+',nsourcegroup) & grepl('PCU|SCU|resin|PSCU|ESN|POCU|CU|slow release|^polymer',nsourcegroup) & grepl('slurry|stover|Organic|organic',nsourcegroup)), 'CR+OF', nsourcegroup)]
  
  # divide into groups for N source 
  # MF stands for mineral fertilizer, CR stands for controlled release, OF stands for organic fertilizer, IN stands for inhibitors
  dt[grepl('^U|urea|CAN|AA|mineral|Synthetic|ammonium|^Ammonium|DAP',nsourcegroup), nsourcegroup := 'MF'] 
  dt[grepl('PCU|SCU|resin|PSCU|ESN|POCU|CU|slow release|^polymer',nsourcegroup), nsourcegroup := 'CR'] 
  dt[grepl('slurry|stover|Organic|organic',nsourcegroup),nsourcegroup := 'OF'] 
  dt[grepl('DMPP|DCD|NI|NBPT|SuperU', nsourcegroup),nsourcegroup := 'IN'] 
  dt[grepl('unknown|NA|no',nsourcegroup) | is.na(n_source),nsourcegroup := 'unknown']
  dt[grepl('mix',n_source),nsourcegroup := 'MF+OF']
  
  # create new columns for application timing 
  # if no splits are mentioned, number of splits is 1
  dt[, n_time_splits := fifelse(is.na(n_time_splits),1,n_time_splits)]
  
  # create groups for timing of the first dose (n_timing_1)
  dt[grepl('PP|at planting', n_timing1),n_timing1_group := 'preplant'] 
  dt[grepl('PE', n_timing1),n_timing1_group := 'preemergence'] 
  dt[grepl('V1|V2|V3|V4|V5', n_timing1),n_timing1_group := 'earlyvegetative'] 
  dt[grepl('V6|V7|V8|V9|V10|V11|Z31', n_timing1),n_timing1_group := 'midvegetative'] 
  dt[grepl('V12|V13|V14|V15|V16|V17|V18|V19|V20|VT', n_timing1),n_timing1_group := 'latevegetative'] 
  dt[grepl('R1|R2|R3|R4|R5|R6', n_timing1),n_timing1_group := 'reproductive'] 
  dt[grepl('unknown|NA', n_timing1) | is.na(n_timing1),n_timing1_group := 'unknown'] 
  
  # create groups for timing of the second dose (n_timing_2)
  dt[grepl('PP', n_timing2),n_timing2_group := 'preplant'] 
  dt[grepl('PE', n_timing2),n_timing2_group := 'preemergence'] 
  dt[grepl('V1|V2|V3|V4|V5', n_timing2),n_timing2_group := 'earlyvegetative'] 
  dt[grepl('V6|V7|V8|V9|V10|V11', n_timing2),n_timing2_group := 'midvegetative'] 
  dt[grepl('V12|V13|V14|V15|V16|V17|V18|V19|V20|VT', n_timing2),n_timing2_group := 'latevegetative'] 
  dt[grepl('R1|R2|R3|R4|R5|R6|EF', n_timing2),n_timing2_group := 'reproductive'] 
  dt[grepl('unknown|NA', n_timing2) | is.na(n_timing2),n_timing2_group := 'unknown'] 
  
  # create groups for timing of the third dose (n_timing_3)
  dt[grepl('PP', n_timing3),n_timing3_group := 'preplant'] 
  dt[grepl('PE', n_timing3),n_timing3_group := 'preemergence'] 
  dt[grepl('V1|V2|V3|V4|V5', n_timing3),n_timing3_group := 'earlyvegetative'] 
  dt[grepl('V6|V7|V8|V9|V10|V11', n_timing3),n_timing3_group := 'midvegetative'] 
  dt[grepl('V12|V13|V14|V15|V16|V17|V18|V19|V20|VT', n_timing3),n_timing3_group := 'latevegetative']
  dt[grepl('R1|R2|R3|R4|R5|R6|at silking', n_timing3),n_timing3_group := 'reproductive'] 
  dt[grepl('unknown|NA', n_timing3)| is.na(n_timing3),n_timing3_group := 'unknown'] 
  
  # create columns for placement
  dt[grepl('injected|sub|deep',n_place),nplacegroup := 'injected'] 
  dt[grepl('band',n_place),nplacegroup := 'bandsprayed']
  dt[grepl('broadcast|broadcasted|basal|topdress|surface',n_place),nplacegroup := 'broadcasted'] 
  dt[grepl('unknown', n_place), nplacegroup := 'unknown'] 
  
# create groups for soil and climate variables 
 
  # group texture & fill empty cells with 'unknown'
  dt[grepl('sandy-clay-loam|clay-loam|silty-clay-loam|loam',texture),gtexture := 'loamy']
  dt[grepl('silt-loam|silt',texture),gtexture := 'silty']
  dt[grepl('sandy-loam|loamy-sand|sand',texture),gtexture := 'sandy']
  dt[grepl('clay|sandy-clay|silty-clay',texture),gtexture := 'clayey']
  dt[, gtexture := fifelse(is.na(gtexture), 'unknown', gtexture)] 
  
  # group soil organic matter content
  dt[, gsom := cut(som, c(-Inf,50,Inf))]
  
  # group pH
  dt[, gph := cut(ph, c(-Inf, 6.5 ,Inf))]
  
  # group clay
  dt[, gclay := cut(clay, c(0, 25, 35, Inf))]
  
  # group mean temperature
  dt[, gmn_temp := cut(mn_temp, c(0,10,17,Inf))]
  
  # group mean precipitation
  dt[, gmn_prec := cut(mn_prec, c(0,500,800,1200,Inf))]
  
  # group potential evaporation
  dt[, gpot_eva := cut(pot_eva, c(0,800,1000,1300,Inf))]
  
  # group total N 
  dt[, gntot := cut(total_n, c(-Inf,2.5,Inf))]
  
  # group K dose
  dt[, gkdose := cut(k_dose, c(-Inf,50,Inf))]
  
  # group P dose
  dt[, pdose2:= as.factor(fifelse(p_dose<10|p_dose>50,'extreme','regular'))]
  
  # group CEC 
  dt[, gcec := cut(xcec, c(-Inf,200,Inf))]
  
  # convert column types from character to factor
  dt[,country:=as.factor(country)]
  dt[,n_timing1_group:=as.factor(n_timing1_group)]
  dt[,n_timing2_group:=as.factor(n_timing2_group)]
  dt[,n_timing3_group:=as.factor(n_timing3_group)]
  dt[,nsourcegroup:=as.factor(nsourcegroup)]
  dt[,nplacegroup:=as.factor(nplacegroup)]
  dt[,gtexture:=as.factor(gtexture)]
  
  # delete rows that have unreadable lat lon (expressed by the fact that xcec is missing or is 0)
  dt <- dt[!is.na(xcec) | xcec == 0,]
  
# calculate effect sizes and sampling variances
  
  # create new data table with relevant columns
  dt2 <- copy (dt)
  cols <- c('obs_no','no','paper','country', 'year', 'crop_type','ind_type', 'nue_value','sd_nue',
            'n_up_fert','sd_fertup','se_fertup','n_up_unfert', 'sd_unfertup','se_unfertup','n_appl',
            'n_rep_t','n_rep_c', 'n_time_splits', 'nrategroup', 'n_timing1_group', 
            'nsourcegroup', 'n_timing2_group','n_timing3_group', 'nplacegroup','gkdose',
            'n_place_depth','gtexture','gsom','som','gph','ph','gclay','clay','caco3',
            'total_n',	'p_dose',	'k_dose', 'xcec', 'xnit', 'xph', 'xsom','gntot',
            'xsand', 'xsilt', 'xclay', 'pot_eva', 'mn_temp', 'mn_prec', 'gpot_eva', 'gcec',
            'gmn_temp', 'gmn_prec', 'tot_prec_crop_gr', 'pdose2','n_dep_image', 'n_dose_env')
  dt2A <- dt2[,mget(cols)]
  
  # calculate log response ratio and corresponding sampling variance of N uptake
  es <- escalc(measure="ROM", m1i= n_up_fert, n1i= n_rep_t, sd1i= sd_fertup, m2i= n_up_unfert, n2i= n_rep_c, sd2i= sd_unfertup, data= dt2A, var.names = c("rr","rr_var"))
  
  # create new data table with columns rr, rr_var
  dt2_es <- as.data.table(es)
  
  # calculate sampling variance for NUEagr 
  dt2_es[, nue_var := (sd_nue^2)]
  
  # calculate value for NUEenv
  dt2_es[, es_env := ((n_up_fert)/(n_dose_env))*100] 
  
  # calculate variance for NUEenv
  dt2_es[, env_var := (((sd_fertup)/(n_dose_env))*100)^2] 
  
  # create new data table 
  dt3_es <- copy(dt2_es)
 
  # remove dt
  rm(dt,dt1,dt2,dt2_es,dt2A,es,final)

  # select data for either NUE_env or NUE_agr 
  dt3_es_env <- dt3_es[!is.na(es_env)|ind_type == "NUEenv" | ind_type == "nueenv",]
  dt3_es_agr <- dt3_es[ind_type == "NUEagr" | ind_type == "nueagr",]
  
  # data normalization and removal of outliers for nue agronomic
  dt3_es_agr[n_appl != 0, n_appl := log(n_appl)]
  dt3_es_agr[som != 0, som := log(som)] #2 groups, probably due to ph (mean ph is 6.6 vs 7.2)
  dt3_es_agr[p_dose!=0, p_dose := log(p_dose)]
  dt3_es_agr[total_n != 0, total_n := log(total_n)]
  dt3_es_agr[k_dose != 0, k_dose := log(k_dose)]
  dt3_es_agr[clay != 0, clay := log(clay)]
  dt3_es_agr[pot_eva !=0, pot_eva := log(pot_eva)]
  
  # data normalization and removal of outliers for nue environmental
  dt3_es_env[, n_appl := log(n_appl)]
  dt3_es_env[, som := log(som)] #2 groups, probably due to ph (mean ph is 6.3 vs 7.6)
  dt3_es_env[, p_dose := log(p_dose)]
  dt3_es_env[, xcec := log(xcec)]
  dt3_es_env[, total_n := log(total_n)]
  dt3_es_env[, k_dose := log(k_dose)]
 
  
  dt3_es_env[, mn_prec := log(mn_prec)]
  dt3_es_env <- dt3_es_env[!(ph <= 4),]
  
  # update crop groups
  nsim
  dt3_es_agr[grepl('maize|corn',crop_type), crop_type := 'maize']
  dt3_es_agr[grepl('paddy|rice',crop_type), crop_type := 'rice']
  dt3_es_agr[grepl('wheat|millet|cereal|barley',crop_type), crop_type := 'cereal']
  dt3_es_agr[!grepl('maize|rice|cereal',crop_type), crop_type := 'other']
  boxplot(dt3_es_agr$nue_value~dt3_es_agr$crop_type)
  boxplot(dt3_es_agr$nue_value~dt3_es_agr$gclay,main = 'effect clay',xlab = 'group',ylab='nue')
  boxplot(dt3_es_agr$nue_value~dt3_es_agr$gmn_prec,main = 'effect climate (precipitation)',xlab = 'group',ylab='nue')
  boxplot(dt3_es_agr$nue_value~dt3_es_agr$gmn_temp,main = 'effect climate (temperature)',xlab = 'group',ylab='nue')
  boxplot(dt3_es_agr$nue_value~dt3_es_agr$gtexture,main = 'effect texture',xlab = 'group',ylab='nue')
  boxplot(dt3_es_agr$nue_value~dt3_es_agr$pdose2,main = 'effect P dose class',xlab = 'group',ylab='nue')
  
  summary(lm(dt3_es_agr$nue_value~dt3_es_agr$n_appl))
  
  # make spatial map
  mp <- dt[,.(x,y,obs_no)]
  mp.sf <- sf::st_as_sf(mp,coords = c('x','y'),crs = 4326)
  
  ## meta models
  dt3_es_agr
  
  
  # model without moderator for NUEagr
  sel <- sample(1:nrow(dt3_es_agr),500)
  a = Sys.time()
  modelC1=rma.mv(nue_value,nue_var, random= list(~1|no),data=dt3_es_agr[sel,], method="ML", sparse = TRUE)
  Sys.time()-a
  
  # ML meta-models for main factor analysis with paper as ML component for NUEagr
  
  # create vector with main factors
  cols <- c('nrategroup', 'n_time_splits','n_timing1_group',  
            'n_timing2_group','nsourcegroup',
            'nplacegroup', 'n_place_depth','gtexture','gsom','gph',
            'pdose2','gntot','gkdose') 
  
  # create an output list with stats from moderator models of each factor (output summary of each model)
  out.list_C <- list()
  
  for(i in 1:length(cols)){
    
    mod <- rma.mv(nue_value,nue_var,mods=~factor(get(cols[i]))-1,random=list(~1| no),
                  data=dt3_es_agr[sel,], method="ML", sparse = TRUE) 
    modtest <- anova(mod,modelC1)
    
    out_C <- data.table(parm = cols[i],
                        AIC = mod$fit.stats$ML[3],
                        pdif = round(modtest$pval,4)) 
    out.list_C[[i]] <- copy(out_C)
    print(i)
  }
  
  out_C <- rbindlist(out.list_C)
  
  
  # model with random component but without moderator for NUEenv
  modelG1=rma.mv(es_env,env_var, random=~1| paper,data=dt3_es_env, method="ML", sparse = TRUE)
  
  # ML meta-models for main factor analysis with no as ML component for NUEenv
  
  # model with random component but without moderator
  modelG1=rma.mv(es_env,env_var, random=~1| no,data=dt3_es_env, method="ML")

  # create output list with stats from moderator models of each factor (output summary of each model)
  out.list_G <- list()
  
  for(i in 1:length(cols)){
    
    mod <- rma.mv(es_env,env_var,mods=~factor(get(cols[i]))-1,random=~1| no,data=dt3_es_env, method="ML", sparse = TRUE) 
    modtest <- anova(mod,modelG1)
    
    out_G <- data.table(parm = cols[i],
                        AIC = mod$fit.stats$ML[3],
                        pdif = round(modtest$pval,4)) 
    out.list_G[[i]] <- copy(out_G)
  }
  
  out_G <- rbindlist(out.list_G)
  
  
  # create vector with main numerical factors
  cols <- c('n_appl',
            'xcec', 'som','total_n','k_dose',
            'pot_eva', 'mn_temp', 'mn_prec', 'clay', 
            'p_dose','ph') 
  

  # create output list with stats from moderator models of each factor (output summary of each model) for NUE agr
  out.list_E <- list()
  
  for(i in 1:length(cols)){
    
    mod <- rma.mv(nue_value,nue_var,mods=~(get(cols[i])),random=list(~1| no),
                  data=dt3_es_agr[sel,], method="ML", sparse = TRUE) 
    #modtest <- anova(mod,modelC1)
    
    out_E <- data.table(parm = cols[i],
                        AIC = mod$fit.stats$ML[3],
                        mean = as.numeric(mod$b[2,]),
                        se = as.numeric(mod$se[2]),
                        pval = mod$pval[2]) 
    out.list_E[[i]] <- copy(out_E)
    print(i)
  }
  
  out_E <- rbindlist(out.list_E)
  

  # create output list with stats from moderator models of each factor (output summary of each model) for NUE env
  out.list_H <- list()
  
  for(i in 1:length(cols)){
    
    mod <- rma.mv(es_env,env_var,mods=~(get(cols[i])),random=~1| no,data=dt3_es_env, method="ML",control=list(optimizer="optim", optmethod="Nelder-Mead")) 
    modtest <- anova(mod,modelG1)
    
    out_H <- data.table(parm = cols[i],
                        AIC = mod$fit.stats$ML[3],
                        pdif = round(modtest$pval,4)) 
    out.list_H[[i]] <- copy(out_H)
  }
  
  out_H <- rbindlist(out.list_H) 
  
  # bind numerical and categorical data together for each response variable 
  outNUEagr <- rbind(out_C, out_E)
  outNUEenv <- rbind(out_G, out_H) 

  
  
  # model development NUE agronomic
  
  # model with moderators
  modelA1 = rma.mv(nue_value,nue_var,mods=~n_appl+xcec*som+total_n,random=~1| no,data=dt3_es_agr, method="ML", sparse = TRUE)
  # check whether model A1 is a better fit than model C1
  anova(modelC1, modelA1)
  # percentage variance explained:
  evp_A1 = 100*(1-modelA1$QE/modelC1$QE)  
  
  
  # model development NUEenv
  
  # model with moderators: nappl, xcec, nsourcegroup, nplacegroup, n_timing1_group, 
  modelE1 = rma.mv(es_env,env_var,mods=~n_appl+xcec+factor(nsourcegroup)+factor(nplacegroup)+factor(n_timing1_group),random=~1| no,data=dt3_es_env, method="ML", sparse = TRUE)
  # check whether model E1 is a better fit than model G1
  anova(modelG1, modelE1)
  # percentage variance explained:
  evp_E1 = 100*(1-modelA1$QE/modelG1$QE)  
  
  
  # QQ plots
  
  # create qqplot for NUEagr
  qqnorm(residuals.rma(modelA1), main= "(a) NUEagr")
  qqline(residuals.rma(modelA1)) 
  
  # create qqplot for NUEenv
  # qqnorm(rstandard.rma.mv(modelE1))
  qqnorm(residuals.rma(modelE1), main= "(b) NUEenv")
  qqline(residuals.rma(modelE1)) 
  
  