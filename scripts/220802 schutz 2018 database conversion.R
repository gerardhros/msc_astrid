
# Script for data structure conversion and estimation of NUE's from database of Schütz et al. (2018)
# Astrid Berndsen
# 02-08-2022

# clean environment
rm(list=ls())

# require packages
require(readxl);require(metafor);require(data.table);require(patchwork);require(maps);require(ggplot2);library(parzer);


# import data set 
schutz.dt <- as.data.table(read_excel("C:/Astrid/NUE/Extend database/Meta-analyses/Schütz et al. (2018)/Schütz et al. (2018).xlsx"))

# make column headings easier to read/refer
setnames(schutz.dt,tolower(colnames(schutz.dt)))
setnames(schutz.dt,gsub('\\(|\\)|\\/','', gsub(' ','_',colnames(schutz.dt))))

# make data table with N-contents after Bouwman, Van Drecht & Van der Hoek (2004) table 3
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
  schutz.dt[, nup := ncontent * yield_dry_kgha / 1000]
  
  # calculate the sd of the N uptake in kg/ha
  schutz.dt[, sd_nup := ncontent * sd_dry /1000]

  # make column heading easier to read/refer and set class to numeric
  setnames(schutz.dt,"total_n_input_-_mineral_+_organic","ndose_tot")
  schutz.dt[, ndose_tot := as.numeric(ndose_tot)]
 

# estimate the agronomic nitrogen use efficiency (NUEagr)
  
  # remove rows with NA for total N dose, N uptake and study
  schutz.dt <- schutz.dt[!is.na(ndose_tot) & !is.na(nup) & !is.na(study),]
  
  # calculate nue agr ((nup fert - nup cont) / ndose) * 100 
  # select nuptake for each row
  # minus the nuptake that corresponds with the lowest N dose within the study (and crop type) 
  # divided by the difference in N dose in the row with the lowest N dose within the study (and crop type)
  schutz.dt <- schutz.dt[, nueagr := ((nup - mean(nup[ndose_tot == min(ndose_tot)])) / (ndose_tot - min(ndose_tot)))*100, by = .(study,crop)]
  
# estimate the environmental nitrogen use efficiency (NUEenv)
  
  # calculate nue env ((N uptake/dose) * 100)
  schutz.dt <- schutz.dt[, nueenv := ((nup/ndose_tot)*100)]
  
# reshape data table in order to merge into the database
  
  # melt nue_agr and nue_env into the same column 
  schutz.dt <- melt(schutz.dt, id.vars = c("study","year","country","latitude","longitude","soil_texture","om_%","ph_water","cec_cmolckg",
                                      "available_n_kgha_merged","total_n_kgha_merged","0-30cm_topsoil_sampling_yesno","application","n_source","k_applied_kgha",
                                      "p_applied_kgha","type_of_fertilization","replicates","crop","ndose_tot","sd_nup"),
                               measure.vars = c("nueagr","nueenv"), variable.name = "ind_type", value.name = c("nue_value"))
  
  # rename column names
  setnames(schutz.dt,c("crop","longitude","latitude","soil_texture","om_%","ph_water","0-30cm_topsoil_sampling_yesno", "cec_cmolckg","available_n_kgha_merged",
                       "total_n_kgha_merged","k_applied_kgha","p_applied_kgha","ndose_tot","replicates","study"),
                     c("crop_type","x","y","texture","som","ph", "depth","xcec","n_av_soil",
                       "total_n","k_dose","p_dose","n_appl","n_rep_t","paper"))

  # convert "yes" into depth of 0-30 cm 
  schutz.dt[, depth := fifelse(depth == "yes", "0-30", depth)]
  
  # add column with study no 27
  schutz.dt[, no := 27]
  
# estimate the standard of the nue values
  
  # calculate sd of nueagr based on the sd of the nup 
  schutz.dt <- schutz.dt[ind_type == "nueagr", sd_nue := ((sd_nup - mean(sd_nup[n_appl == min(n_appl)])) / (n_appl - min(n_appl)))*100, by = .(paper,crop_type)]
  
  # calculate sd of nueenv based on the sd of the nup
  schutz.dt <- schutz.dt[ind_type == "nueenv", sd_nue := ((sd_nup/n_appl)*100)]
  
# remove unreliable and infinite datapoints  
  
  # remove datapoints with nue values that are infinite (since these are the rows used to determine the control uptake)
  schutz.dt <- schutz.dt[!is.infinite(nue_value),]
 
  # remove datapoints with nueagr or nueenv smaller than 0 and larger than 90
  schutz.dt <- schutz.dt[!(nue_value <= 0| nue_value > 90),]
  
  # set datapoints larger than 90 to a value between 90 and 95
  #set.seed(111)
  #schutz.dt <- schutz.dt[nue_value > 90, nue_value := rnorm(.N,mean = 91, sd = 1)]
  #the values larger than 90 appear the be a large group compared to the other data (see histogram), so therefore still removed all nue_values larger than 90
  #hist(schutz.dt$nue_value,  breaks = seq(0,100,5) )

  
# save new database schutz.dt in dev
  #fwrite(schutz.dt,"dev/schutz2018_converted.csv")
  
  
  