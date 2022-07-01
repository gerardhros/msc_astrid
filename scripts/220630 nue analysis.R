# meta analysis for global upscaling

# clear environment and require packages
require(data.table);require(metafor)

# clear environment
rm(list=ls())

  # read in data and start cleaning data

  # Database with literature data from selected studies
  dt <- fread('data/220630 database.csv',dec=',')

  # remove data from database source 27, not yet converted to the right nue type
  dt <- dt[no != "27", ]

  # remove column 'title' from data table
  dt[,c('title','paper' ) := NULL]

  # make column headings easier to read/refer
  setnames(dt,tolower(colnames(dt)))
  setnames(dt,gsub('\\(|\\)|\\/','', gsub(' ','_',colnames(dt))))

  # easy solution to replace wrong column types from read_excel
  cols <- which(sapply(dt,is.logical))
  dt[,c(cols):= lapply(.SD,as.numeric),.SDcols = cols]

  # change column type from character to numeric
  dt[, ph := as.numeric(ph)]
  dt[, som := as.numeric(som)]
  dt[, n_av_soil := as.numeric(n_av_soil)]
  dt[, k_dose := as.numeric(k_dose)]

  # Calculate new and adjust existing data 

    # estimate n_rep_t when not available
    dt[, n_rep_t := fifelse(is.na(n_rep_t),2,n_rep_t)]
    dt[, n_rep_c := fifelse(is.na(n_rep_c),2,n_rep_c)]

  # calculate stats from LSD, sd and se for NUE
  dt[is.na(se_nue), se_nue := lsd_nue / ((qt(0.975, n_rep_t))*sqrt(2*n_rep_t))]
  dt[is.na(sd_nue), sd_nue := se_nue*sqrt(n_rep_t)] 

  #remove datapoints with agronomic and environmental NUE larger than 90 and smaller than 0
  dt <- dt[!(nue_value>90 | nue_value <= 0),]

  # estimate sd from mean CV of other studies for NUE
  dt[, cv_nue := sd_nue / nue_value]
  dt[, cv_nue_mean := mean(cv_nue,na.rm = TRUE)]
  dt[is.na(sd_nue), sd_nue := cv_nue_mean * 1.25 * nue_value]

  # add random noise to sd_nue in order that the correlation between nue_value and sd_nue is acceptable
set.seed(123)
dt[,sd_nue := sd_nue + rnorm(.N,mean = 1, sd = 2)]

#dt[is.na(sd_nue), sd_nue1 := cv_nue_mean * abs(rnorm(1, 1.25, 0.5))] #added an random rnorm instead of 1.25
#dt[is.na(sd_nue), sd_nue := sd_nue1 * nue_value]

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

# Replace unknown fertilization with zero
# which of the columns need to be checked
cols <- c('n_fert_min','n_fert_man','n_fix','n_dep_image','n_dep_emep','n_cs','n_spm','n_min','n_hc','n_res')

# Replace NA with zero
dt[,c(cols) := lapply(.SD,function(x) fifelse(is.na(x),0,x)), .SDcols = cols]

#N dose environmental NUE
#dt[, n_dose_env := fifelse(is.na(n_dose_env), (n_fert_min + n_fert_man + n_fix + n_dep_image + n_cs), n_dose_env)]
dt[, n_dose_env := fifelse(is.na(n_dose_env), (n_appl + n_up_unfert), n_dose_env)]

## Replace NAs for 4R & site conditions with unknown, median or data from soilgrids.org
#which of the columns need to be checked
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
