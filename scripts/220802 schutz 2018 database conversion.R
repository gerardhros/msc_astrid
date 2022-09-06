
# Script for data structure conversion and estimation of NUE's from database of Schütz et al. (2018)
# Astrid Berndsen
# 02-08-2022

# clean environment
rm(list=ls())

# require packages
require(readxl);require(metafor);require(data.table);require(patchwork);require(maps);require(ggplot2);library(parzer);


# import data set 
schutz.dt <- as.data.table(readxl::read_excel("dev/schutz_2018_original.xlsx"))

# make column headings easier to read/refer
setnames(schutz.dt,tolower(colnames(schutz.dt)))
setnames(schutz.dt,gsub('\\(|\\)|\\/|\\=|_|\\%|\\+','', gsub(' ','_',colnames(schutz.dt))))

schutz.dt[schutz.dt=='NA'] <- NA

# what are numeric columns
cols <- c('om','ceccmolckg','totalnkghamerged','nappliedkgha','kappliedkgha','pappliedkgha',
          'nuentot','sdnuentot','yielddrykgha','sddry','sd')
schutz.dt[,c(cols) := lapply(.SD,as.numeric),.SDcols = cols]

# make data table with N-contents (g / kg) after Bouwman, Van Drecht & Van der Hoek (2004) table 3
ncontents <- data.table(crop_group = c("wheat","paddy rice","maize","barley","millet","sorghum","other cereals","potatoes","sweet potatoes","cassava",
                                       "other root crops","plantains","sugar beets","sugar cane","pulses,total","vegetables and melons","bananas",
                                       "citrus fruit","fruit excluding melons","cocoa beans","rapeseed","oil palm fruit","soybeans","groundnuts in shell",
                                       "sunflower seed","sesame seed","other oilseeds","coconuts","coffee, green","tea", "tobacco leaves","seed cotton",
                                       "fibre crops primary","rubber"),
                        nc = c(19,13,14,17,15,15,16,3,3,2,3,2,2,2,35,2,2,1,1,14,35,15,35,40,34,33,30,0,24,78,3,29,81,0))


# add Nuptake column based on yield and N content

  # set all crop types to lower in schutz.dt
  schutz.dt[, crop := tolower(crop)]
  
  # N content of the harvested product based on Bouwman, Van Drecht & Van der Hoek (2004) table 3
  schutz.dt[, ncontent:= NA_real_]
  schutz.dt[, ncontent:= fifelse(grepl("maize",crop), ncontents[grepl("maize",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("sorghum",crop), ncontents[grepl("sorghum",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("paddy|rice",crop), ncontents[grepl("paddy",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("wheat|triticum",crop), ncontents[grepl("wheat",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("sunflower",crop), ncontents[grepl("sunflower",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("rapeseed",crop), ncontents[grepl("rapeseed",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("sesame",crop), ncontents[grepl("sesame",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("cotton",crop), ncontents[grepl("cotton",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("tomato|okra|pepper|melon|cucumber|eggplant|cabbage",crop), ncontents[grepl("vegetables and melons",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("chickpea|pigeon|lentil|snap|runner|kidney|horse|mung|blackgram",crop), ncontents[grepl("pulses",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("sugar beet",crop), ncontents[grepl("sugar beet",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("sugarcane",crop), ncontents[grepl("sugar cane",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("barley",crop), ncontents[grepl("barley",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("soybean",crop), ncontents[grepl("soybean",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("peanut",crop), ncontents[grepl("groundnuts",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("tumeric",crop), ncontents[grepl("root",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("potato",crop), ncontents[grepl("^potato",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("millet",crop), ncontents[grepl("millet",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("cassava",crop), ncontents[grepl("cassava",crop_group), nc], ncontent)]
  
  # N content of the harvested product based on 220210 gewas database provided by Gerard Ros
  schutz.dt[, ncontent:= fifelse(grepl("ryegrass",crop), 20, ncontent)] # ndicea
  schutz.dt[, ncontent:= fifelse(grepl("fennel",crop), 44.8, ncontent)] # ndicea
  schutz.dt[, ncontent:= fifelse(grepl("garlic",crop), 19.1, ncontent)]  # eurostat
  schutz.dt[, ncontent:= fifelse(grepl("mustard",crop), 40, ncontent)] # ndicea
  
  # NEED TO BE ADDED CORRECTLY LATER, for now N content based on Bouwman, Van Drecht & Van der Hoek (2004) table 3
  schutz.dt[, ncontent:= fifelse(grepl("dill|coriander",crop), ncontents[grepl("vegetables and melons",crop_group), nc], ncontent)]
  schutz.dt[, ncontent:= fifelse(grepl("fenugreek",crop), ncontents[grepl("vegetables and melons",crop_group), nc], ncontent)] 
  
  # calculate N uptake in kg/ha
  schutz.dt[, nup := ncontent * yieldfreshkgha / 1000]
  
  # ratio dry versus moist
  schutz.dt[,dmratio := yieldfreshkgha/yielddrykgha]
  schutz.dt[is.na(sd),sd:= sddry * dmratio]
  
  # calculate the sd of the N uptake in kg/ha
  schutz.dt[, sd_nup := ncontent * sd /1000]

  # make column heading easier to read/refer and set class to numeric
  setnames(schutz.dt,"totalninput-mineralorganic","ndose_tot")
  schutz.dt[, ndose_tot := as.numeric(ndose_tot)]
 
  # update total N input when missing
  schutz.dt[is.na(ndose_tot) & !is.na(nappliedkgha), ndose_tot := nappliedkgha]

# estimate the agronomic nitrogen use efficiency (NUEagr)
  
  # remove rows with NA for total N dose, N uptake and study
  schutz.dt <- schutz.dt[!is.na(ndose_tot) & !is.na(nup) & !is.na(study),]
  
  # calculate nue agr ((nup fert - nup cont) / ndose) * 100 
  # select nuptake for each row
  # minus the nuptake that corresponds with the lowest N dose within the study (and crop type) 
  # divided by the difference in N dose in the row with the lowest N dose within the study (and crop type)
  setnames(schutz.dt,"first,second,thirdorforthyeardata0123",'exp_year')
  setnames(schutz.dt,"nfixing0,psolubilizing1,both2,unclear3,sorksolubilizer4,amf5",'exp_type')
  schutz.dt[,uid := .GRP, by = .(study,crop,exp_year,exp_type,location,strain)]
  schutz.dt[, nueagr := ((nup - mean(nup[ndose_tot == min(ndose_tot)])) / (ndose_tot - min(ndose_tot)))*100, by = 'uid']
  
# estimate the environmental nitrogen use efficiency (NUEenv)
  
  # calculate nue env ((N uptake/dose) * 100)
  schutz.dt[, nueenv := ((nup/ndose_tot)*100)]
  
  # calculate sd of nueagr based on the sd of the nup 
  schutz.dt[,v1 := sd_nup / sqrt(replicates)]
  schutz.dt[, sd_nue_agr := (sqrt(v1^2 + mean(v1[ndose_tot == min(ndose_tot)])^2) * 100 * sqrt(replicates)/ (ndose_tot - min(ndose_tot))), by = 'uid']
  
  # calculate sd of nueenv based on the sd of the nup
  schutz.dt[, sd_nue_env := ((sd_nup/ndose_tot)*100)]
  
  # rename column names
  setnames(schutz.dt,c("crop","longitude","latitude","soiltexture","om","phwater","0-30cmtopsoilsamplingyesno", "ceccmolckg","availablenkghamerged",
                       "totalnkghamerged","kappliedkgha","pappliedkgha","ndose_tot","replicates","study"),
           c("crop_type","x","y","texture","som","ph", "depth","xcec","n_av_soil",
             "total_n","k_dose","p_dose","n_appl","n_rep_t","paper"))
  
  # convert "yes" into depth of 0-30 cm 
  schutz.dt[, depth := fifelse(depth == "yes", "0-30", depth)]
  
  # replace infinite with NA
  schutz.dt[!is.finite(nueenv), nueenv := NA_real_]
  schutz.dt[!is.finite(nueagr), nueagr := NA_real_]
  schutz.dt[!is.finite(sd_nue_agr), sd_nue_agr := NA_real_]
  schutz.dt[!is.finite(sd_nue_env), sd_nue_env := NA_real_]
  
  # convert ntot from kg/ha to g/kg
  schutz.dt[,bd := as.numeric(bulkdensitymergedkgdm3)]
  schutz.dt[is.na(bd),bd := mean(schutz.dt$bd,na.rm=T)]
  schutz.dt[,depth2 := tstrsplit(depth,'-',keep=2)]
  schutz.dt[,depth2 := as.numeric(depth2)]
  schutz.dt[is.na(depth2),depth2 := 30]
  schutz.dt[,total_n := total_n * 1000 / (depth2 * 0.01 * 100 * 100 * bd * 1000)]
  
# select columns to be stored
  schutz.dt <- schutz.dt[,.(studynr,x,y,uid,year,nrep = n_rep_t,crop_type,country,
                            n_fert_min = nappliedkgha,
                            n_fert_man = nutrientcontentorganicamendmentnkgha,
                            nsource,
                            texture,depth,som,ph,ntot = total_n,
                            bd,
                            k_dose,p_dose,n_dose = n_appl, 
                            typeoffertilization,yieldfreshkgha, ncontent,nup,sd_nup,nueagr,sd_nue_agr,nueenv,sd_nue_env)]
  
  # add column with study no 27
  schutz.dt[, no := 27]
  
# set lon-lat correct
  
  # remove missing coordinates
  schutz.dt<- schutz.dt[!is.na(x)]
  
  # convert comma's to dots
  schutz.dt[, x:= parse_lon(x)]
  schutz.dt[, y:= parse_lat(y)]
  
# rbind NUE-agri en NUE-env
  
  schutz.dt.agri <- copy(schutz.dt)[,c('nueenv', 'sd_nue_env') := NULL][,ind_type := 'NUEagr']
  schutz.dt.agri <- schutz.dt.agri[!is.na(nueagr)]
  schutz.dt.agri[,ind_unit := '%']
  setnames(schutz.dt.agri,c('nueagr','sd_nue_agr'),c('nue_value','sd_nue'))
  schutz.dt.agri[,se_nue := sd_nue / sqrt(nrep)]
  
  schutz.dt.env <- copy(schutz.dt)[,c('nueagr', 'sd_nue_agr') := NULL][,ind_type := 'NUEenv']
  schutz.dt.env <- schutz.dt.env[!is.na(nueenv)]
  schutz.dt.env[,ind_unit := '%']
  setnames(schutz.dt.env,c('nueenv','sd_nue_env'),c('nue_value','sd_nue'))
  schutz.dt.env[,se_nue := sd_nue / sqrt(nrep)]
  
  schutz.dt <- rbind(schutz.dt.agri,schutz.dt.env)
  
# write the lon-lat as csv
  fwrite(schutz.dt,'dev/220905 schutz_sites.csv')
  
  
  
# # reshape data table in order to merge into the database
#   
#   # melt nue_agr and nue_env into the same column 
#   schutz.dt <- melt(schutz.dt, id.vars = c("study","year","country","latitude","longitude","soiltexture","om","phwater","ceccmolckg",
#                                       "availablenkghamerged","totalnkghamerged","0-30cmtopsoilsamplingyesno","application","nsource","kappliedkgha",
#                                       "pappliedkgha","typeoffertilization","replicates","crop","ndose_tot","sd_nup","uid"),
#                                measure.vars = c("nueagr","nueenv"), variable.name = "ind_type", value.name = c("nue_value"))
#   
 
  

# estimate the standard of the nue values
  
    

  
  
  
# remove unreliable and infinite datapoints  
  
  # remove datapoints with nue values that are infinite (since these are the rows used to determine the control uptake)
  # schutz.dt <- schutz.dt[!is.infinite(nue_value),]
 
  # remove datapoints with nueagr or nueenv smaller than 0 and larger than 90
  # schutz.dt <- schutz.dt[!(nue_value <= 0| nue_value > 90),]
  
  # set datapoints larger than 90 to a value between 90 and 95
  #set.seed(111)
  #schutz.dt <- schutz.dt[nue_value > 90, nue_value := rnorm(.N,mean = 91, sd = 1)]
  #the values larger than 90 appear the be a large group compared to the other data (see histogram), so therefore still removed all nue_values larger than 90
  #hist(schutz.dt$nue_value,  breaks = seq(0,100,5) )

  
# save new database schutz.dt in dev
  #fwrite(schutz.dt,"dev/schutz2018_converted.csv")
  
  
  