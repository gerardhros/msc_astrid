# luncheng database

# load in db of luncheng
d1 <- readxl::read_xlsx('data/luncheng_2022_nitrogen.xlsx',sheet = 'database')
d1 <- as.data.table(d1)

d1 <- d1[,list(obs_no = id, no = 34,studynr = studyid,
               x = lon, y = lat, 
               crop_type,
               treatment = tillage,
               n_dose, p_dose, k_dose,
               nuec_type,
               nuet_mean,nuet_sd,nrep = replication,
               nuec_mean,nuec_sd,
               nut_mean,nut_sd,nut_0N,
               nuc_mean,nuc_sd,nuc_0N,
               soc,
               cn = soc * 10/total_nitrogen,
               bd = 100 / ((soc * 0.1 * 1.72) / 0.244 + (100 - soc * 0.1 * 1.72)/1.64)
               )]

d1[,nlv := 0.02 * (soc * 0.1 / cn) * (100 * 100 * 0.3) * (1000 * bd) * 0.001]

d1[!is.na(nut_mean), nue_value_t := nut_mean *100/ n_dose]
d1[!is.na(nut_mean), nue_value_c := nuc_mean *100/ n_dose]
d1[!is.na(nut_mean), nue_sd_t := nuet_sd *100/ n_dose]
d1[!is.na(nut_mean), nue_sd_c := nuec_sd *100/ n_dose]
d1[is.na(nue_value_t),nue_sd_t := (nuet_sd * 0.01 * n_dose + 0) * 100 / n_dose]
d1[is.na(nue_value_c),nue_sd_c := (nuec_sd * 0.01 * n_dose + 0) * 100 / n_dose]
d1[is.na(nue_value_t),nue_value_t := (nuet_mean * 0.01 * n_dose + nlv) * 100 / n_dose]
d1[is.na(nue_value_c),nue_value_c := (nuec_mean * 0.01 * n_dose + nlv) * 100 / n_dose]


d1.p1 <- copy(d1)
setnames(d1.p1,c('nue_value_t','nue_sd_t'),c('nue_value','sd_nue'))
d1.p1[,c('nue_value_c','nue_sd_c') := NULL]
d1.p1[treatment=='EE',n_source := 'efficiency']
d1.p1[treatment=='CC',n_res := TRUE]
d1.p1[treatment=='OF', n_source := 'organic']
d1.p1[treatment=='CF', n_source := 'mixture']
d1.p1[treatment=='RFP', n_place := 'deepbanded']
d1.p1[treatment=='RFT', n_time_splits := 2]
d1.p1[treatment=='ROT', rotation := 'diverse_crops']
d1.p1[is.na(n_source), n_source := 'mineral']
d1.p1[is.na(n_res), n_res := FALSE]
d1.p1[is.na(n_place), n_place := 'broadcast']
d1.p1[is.na(n_time_splits), n_time_splits := 1]
d1.p1[is.na(rotation), rotation := 'unknown']
d1.p1 <- d1.p1[!is.na(nue_value)]

d1.p2 <- copy(d1)
setnames(d1.p2,c('nue_value_c','nue_sd_c'),c('nue_value','sd_nue'))
d1.p2[,c('nue_value_t','nue_sd_t') := NULL]
d1.p2[, n_source := 'mineral']
d1.p2[, n_res := FALSE]
d1.p2[, n_place := 'broadcast']
d1.p2[, n_time_splits := 1]
d1.p2[, rotation := 'unknown']
d1.p2 <- d1.p2[!is.na(nue_value)]

d1 <- rbind(d1.p1,d1.p2)

cols <- c("obs_no","no","studynr","x","y","crop_type","n_dose","p_dose" , "k_dose", "nrep" , 
          "soc","cn","bd","nlv","nue_value" ,"sd_nue","n_source","n_res","n_place" ,"n_time_splits", "rotation")

d2 <- d1[,mget(cols)]
  
fwrite(d2,'dev/220905 luncheng_sites.csv')

