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
    
  # update NUE given deposition and NLV
  dt[,cn := xsoc / xntot]
  dt[is.na(cn),cn := median(dt$cn,na.rm=T)]
  dt[,cn := pmin(quantile(cn,0.95),pmax(quantile(cn,0.05),cn))]
  dt[,bd := 100 / ((xsoc * 0.1 * 1.72) / 0.244 + (100 - xsoc * 0.1 * 1.72)/1.64)]
  dt[,nlv := 0.025 * (xsoc * 0.1 / cn) * (100 * 100 * 0.3) * (1000 * bd) * 0.001]
  dt[,depntot := pmax(quantile(depntot,0.05),depntot)]
  dt[,n_dose := pmax(1,n_dose,na.rm=T)]
  dt[,fnue := nue_value * n_dose / (n_dose + nlv + depntot)]
  dt[,fvnue := (sd_nue * n_dose / (n_dose + nlv + depntot))^2]
  
  # replace missing ones with median
  dt[is.na(p_dose), p_dose := median(dt$p_dose,na.rm=T)]
  dt[is.na(k_dose), k_dose := median(dt$k_dose,na.rm=T)]
  dt[is.na(pot_eva), pot_eva := median(dt$pot_eva,na.rm=T)]
  dt[is.na(pet_sd), pet_sd := median(dt$pet_sd,na.rm=T)]
  dt[is.na(tmp_sd), tmp_sd := median(dt$tmp_sd,na.rm=T)]
  dt[is.na(tmp_mean), tmp_mean := median(dt$tmp_mean,na.rm=T)]
  
  # transform or cutoff relevant variables
  dt[,n_dose := pmin(n_dose,quantile(n_dose,0.99))]
  dt[,p_dose := pmin(p_dose,quantile(p_dose,0.99))]
  dt[,k_dose := pmin(k_dose,quantile(k_dose,0.99))]
  
  # create new data table with relevant columns
  dt2 <- copy(dt)
  
  # adjust a few exceptional variance estimates (visible by plotting var~value)
  dt2[fnue <= 150,fvnue := pmin(500,pmin(4 * fnue,fvnue))]
  dt2[fnue > 150,fvnue := pmin(500,pmin(9 * fnue,fvnue))]
 
  # investigate influence of factors
  boxplot(dt2$fnue~dt2$n_place,main = 'effect of placement', ylab='NUE (%)',xlab = '')
  boxplot(dt2$fnue~dt2$n_source,main = 'effect of N source', ylab='NUE (%)',xlab = '')
  boxplot(dt2$fnue~dt2$n_time_splits,main = 'effect of N timing', ylab='NUE (%)',xlab = '')
  boxplot(dt2$fnue~dt2$crop_type,main = 'effect of crop type', ylab='NUE (%)',xlab = '')
  boxplot(dt2$fnue~dt2$texture,main = 'effect of soiltype', ylab='NUE (%)',xlab = '')
  plot(dt2$fnue~dt2$n_dose,main = 'effect of N dose', ylab='NUE (%)',xlab = '')
  plot(dt2$fnue~dt2$xclay,main = 'effect of clay content', ylab='NUE (%)',xlab = '')
  plot(dt2$fnue~dt2$xph,main = 'effect of pH', ylab='NUE (%)',xlab = '')
  plot(dt2$fnue~dt2$xntot,main = 'effect of total N', ylab='NUE (%)',xlab = '')
  plot(dt2$fnue~dt2$depntot,main = 'effect of N deposition', ylab='NUE (%)',xlab = '')
  plot(dt2$fnue~dt2$pre_mean,main = 'effect of precipitation', ylab='NUE (%)',xlab = '')
  
  
  # baseline model
  a = Sys.time()
  mC1=rma.mv(nue_value,nue_var, random= list(~1|studynr),data=dt2, method="ML", sparse = TRUE)
 Sys.time()-1
 
  # test first main glm models
  m1 = lm(fnue ~ n_place + n_source + n_time_splits + crop_type + n_dose : I(n_dose^2) + xclay*pre_mean + xsom*tmp_mean, data = dt2)
 
  p1 <- predict(m1,newdata = dt2)
  plot(p1,dt2$fnue)
  
  # model with moderators
  modelA1 = rma.mv(fnue,fvnue,mods=~n_place + n_source + n_time_splits + crop_type + n_dose + I(n_dose^2) + p_dose + k_dose + 
                     tmp_mean + pre_mean + pot_eva + depntot + xntot + xclay + bd,random=~1| no,data=dt2, method="ML", sparse = TRUE)
  
  # check whether model A1 is a better fit than model C1
  anova(modelC1, modelA1)
  # percentage variance explained:
  evp_A1 = 100*(1-modelA1$QE/modelC1$QE)  
  
  
  
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
  
  