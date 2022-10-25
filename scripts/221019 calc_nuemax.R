require(metafor)
require(data.table)

rm(list=ls())

  # require rasters Nuptake arable crops and target yield n uptake
  r1 <- terra::rast('D:/DATA/06 inputs/nup_ara_ph.asc')
  r2 <- terra::rast('D:/DATA/06 inputs/nup_tar_ara_ph.asc')

  # totale N input from fertilizer and manure minus nh3
  r3 <- terra::rast('D:/DATA/06 inputs/nfer_man_eff_ara_ph.asc')
  
  # totale N input na aanwendingsemissies (kg N / ha)
  r4 <- terra::rast('D:/DATA/06 inputs/nin_eff_ara_ph.asc')

  # convert to data.table
  r1.p <- as.data.table(as.data.frame(r1,xy=TRUE))
  r2.p <- as.data.table(as.data.frame(r2,xy=TRUE))
  r3.p <- as.data.table(as.data.frame(r3,xy=TRUE))
  r4.p <- as.data.table(as.data.frame(r4,xy=TRUE))

  # merge data into one data.table
  dt <- merge(r1.p,r2.p,by= c('x','y'),all = TRUE)
  dt <- merge(dt,r3.p,by= c('x','y'),all = TRUE)
  dt <- merge(dt,r4.p,by= c('x','y'),all = TRUE)

  # save coordinates to extract relevant covariates (also used in metafor model)
  # d1 <- dt[,.(x,y)]
  # fwrite(d1,'data/221019 world loc predictions.csv')
  
  # read in the covariates
  d1 <- fread('data/221019 global covariates.csv')
  
  # merge covariates with N data Lena
  dt <- merge(dt,d1[,.(x,y,tmp_mean, pre_mean,xsom,xclay)],by=c('x','y'))
  
  # replace two missing values (with nearby estimates)
  dt[is.na(xsom), c('xsom','xclay') := list(13.95,4.1)]
  
  # predict NUE for BAU using metafor model, independent of crop, given current ninput
  d2 <- pred_nue(clay = dt$xclay,
                 som = dt$xsom,
                 temp = dt$tmp_mean,
                 prec = dt$pre_mean,
                 n_dose = dt$nfer_man_eff_ara_ph)
  d2[,impr := best - bau]
  
  # merge with dt
  dt <- cbind(dt,d2)
 
  
# load prediction model
m1 <- readRDS('products/nue_meta5.rds')
summary(m1)

pred_nue <- function(clay,som,temp,prec, n_dose){
  
  # make internal table for default situation
  d1 <- data.table(id= 1: length(clay),
                   clay = clay,
                   som = som,
                   temp = temp,
                   prec = prec,
                   n_dose = n_dose)
  
  # predict baseline NUE
  d1[,nue := -125.8874 + 1.6673 * clay + 0.2138 * som + 2.1131 * temp + 0.0541 * prec + 0.1281 * n_dose + 0.001 * n_dose^2]
  
  # run some simulations for NUE options
  out <- list()
  
  for(i in c('bau','best')) {
    
    if(i == 'best'){
      d1[,c('n_source','n_place','n_splits') := list('efficiency','broadcasted',1)]
    } else {
      d1[,c('n_source','n_place','n_splits') := list('mixture','broadcasted',0)]
    }
    # add correction factor for n source
    d1[n_source == 'inorganic', cf1 := 40.4728 - 0.2415 * n_dose]
    d1[n_source == 'manure', cf1 := 54.9891 - 0.3974 * n_dose]
    d1[n_source == 'mixture', cf1 := 21.6771- 0.2620 * n_dose]
    d1[n_source =='organic', cf1 := 33.3699 - 0.3172 * n_dose]
    d1[n_source =='efficiency', cf1 := 0]
    
    # add correction foractor for n place
    d1[n_place == 'broadcasted', cf2 := 56.657 - 0.2236 * n_dose]
    d1[n_place == 'injected', cf2 := 53.8909 - 0.6297 * n_dose]
    d1[n_place == 'incorporated', cf2 := 70.8318 - 0.4885 * n_dose]
    d1[n_place == 'topdressed', cf2 := 116.7911 - 1.0821 * n_dose]
    d1[n_place == 'banded', cf2 := 0]
    
    # add correction for n splits
    d1[n_splits == 0, cf3 := 0]
    d1[n_splits == 1, cf3 := 44.6394 - 0.0313 * n_dose]
    d1[n_splits == 2, cf3 := 39.4335 - 0.0346 * n_dose]
    d1[n_splits == 3, cf3 := 27.8434 + 0.0594 * n_dose]
    
    # estimate baseline: inorganic N fertilizers, 0 splits, broadcasted, 
    d1[,var := nue + cf1 + cf2 + cf3]
    
    # retreive output
    out[[i]] <- d1[,.(id,var,type = i)]
  }
  
  out <- rbindlist(out)
  out <- dcast(out,id~type, value.var = 'var')
  
  return(out)
}

# run a solver to find NUEmax with yield target 
nue_nup_tar <- list()

# solve NUE max for each situation
for(i in 1:nrow(dt)){
  
  # do a check when nuptake is 0 of NA, set NUEmax then to NA
  check <- dt[i,nup_tar_ara_ph]
  
  if(check ==0 | is.na(check)){
    nue_nup_tar[[i]] <- data.table(bau=NA_real_,best = NA_real_)
  } else{
    
    # estimate max NUE for business as usual
    nue_max_bau = optim(par= dt[i,nue],pred_nue_solver,
                    nup = dt[i,nup_tar_ara_ph], 
                    clay= dt[i,xclay],
                    som= dt[i,xsom],
                    temp= dt[i,tmp_mean],
                    prec= dt[i,pre_mean],
                    type='bau',
                    method = 'Brent',lower=0,upper = 300, control = list(abstol = 1,reltol = 0.1))
    
    # estimate max NUE for best practice
    nue_max_best = optim(par= dt[i,nue],pred_nue_solver,
                        nup = dt[i,nup_tar_ara_ph], 
                        clay= dt[i,xclay],
                        som= dt[i,xsom],
                        temp= dt[i,tmp_mean],
                        prec= dt[i,pre_mean],
                        type='best',
                        method = 'Brent',lower=0,upper = 300, control = list(abstol = 1,reltol = 0.1))
    
    # save both predictions
    nue_nup_tar[[i]] <- data.table(bau = nue_max_bau$par,
                                   best = nue_max_best$par)
  }
 
  print(i)
  # save data during running
  if(i %in% round(seq(1000,39683,length.out = 30))){
    saveRDS(nue_nup_tar,'dev/tmp_solver.rds')
  }
}

# 
out <- rbindlist(nue_nup_tar)
setnames(out,paste0('nuemax_',colnames(out)))
saveRDS(out,'dev/tmp_solver_total.rds')
 
dfin <- cbind(dt,out)
saveRDS(dfin,'dev/tmp_db_total.rds')

    # a =Sys.time()
    # optim(par= dt[10,nue],pred_nue_solver,
    #       nup = dt[10,nup_tar_ara_ph], 
    #       clay= dt[10,xclay],
    #       som= dt[10,xsom],
    #       temp= dt[10,tmp_mean],
    #       prec= dt[10,pre_mean],type='bau',
    #       method = 'Brent',lower=0,upper = 300, control = list(abstol = 1,reltol = 0.1))
    # Sys.time() -a

pred_nue_solver <- function(nue_max,nup, clay,som,temp,prec, type = 'bau',pred = FALSE){
  
  # make internal table for default situation
  d1 <- data.table(id= 1: length(clay),
                   clay = clay,
                   som = som,
                   temp = temp,
                   prec = prec,
                   n_dose = nup/nue_max)
  
  # predict baseline NUE
  d1[,nue := -125.8874 + 1.6673 * clay + 0.2138 * som + 2.1131 * temp + 0.0541 * prec + 0.1281 * n_dose + 0.001 * n_dose^2]
  
  # set management
  if(type == 'best'){
      d1[,c('n_source','n_place','n_splits') := list('efficiency','broadcasted',1)]
    } else {
      d1[,c('n_source','n_place','n_splits') := list('mixture','broadcasted',0)]
    }
  
  # add correction factor for n source
  d1[n_source == 'inorganic', cf1 := 40.4728 - 0.2415 * n_dose]
  d1[n_source == 'manure', cf1 := 54.9891 - 0.3974 * n_dose]
  d1[n_source == 'mixture', cf1 := 21.6771- 0.2620 * n_dose]
  d1[n_source =='organic', cf1 := 33.3699 - 0.3172 * n_dose]
  d1[n_source =='efficiency', cf1 := 0]
  
  # add correction foractor for n place
  d1[n_place == 'broadcasted', cf2 := 56.657 - 0.2236 * n_dose]
  d1[n_place == 'injected', cf2 := 53.8909 - 0.6297 * n_dose]
  d1[n_place == 'incorporated', cf2 := 70.8318 - 0.4885 * n_dose]
  d1[n_place == 'topdressed', cf2 := 116.7911 - 1.0821 * n_dose]
  d1[n_place == 'banded', cf2 := 0]
  
  # add correction for n splits
  d1[n_splits == 0, cf3 := 0]
  d1[n_splits == 1, cf3 := 44.6394 - 0.0313 * n_dose]
  d1[n_splits == 2, cf3 := 39.4335 - 0.0346 * n_dose]
  d1[n_splits == 3, cf3 := 27.8434 + 0.0594 * n_dose]
  
  # estimate baseline: inorganic N fertilizers, 0 splits, broadcasted, 
  d1[,var := nue + cf1 + cf2 + cf3]
    
  if(pred == FALSE){
    
    out <- abs(d1[,var] - nue_max)
  } else {
    out <- d1[,var]
  }
  

  return(out)
}





broadcsat > incorporated > injected
1 > 0

d1 <- CJ(n_dose = seq(50,400,50), 
           nue = 45, 
           n_source = c('inorganic','manure','mixture','organic','efficiency'), 
           n_place = c('broadcasted','injected','incorporated','topdressed','banded'), 
           n_splits = c(0,1,2,3))

require(ggplot2)
ggplot(data = d1,aes(x = n_dose, y = nue_0, group = n_splits, colour = as.factor(n_splits))) + 
  geom_boxplot() + theme_bw()

