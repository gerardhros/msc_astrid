# dataset for Ding_2018_improving

d1 <- readxl::read_xlsx('data/ding_2018_improving.xlsx',sheet = 'Secondary and micronutrients')
d1 <- as.data.table(d1)


# what is the N content of rice (derived from shen_guo_2015
nc_rice = function(nrate){
  
  # estimate yield (kg/ha)
  yield = (-0.0212 * 0.001 * nrate^2 + 0.016807 * nrate + 5.8659)*1000
  yield = (-0.00002849*nrate^2 + 0.02402583 * nrate + 4.13037020)*1000
  # estimate nuptake (kg N /ha)
  nup = 0.3471 * nrate + 5.8659
  nup = 67.5053 + 0.4035 * nrate
  
  # estimate N content  (g/kg)
  ncontent = nup * 1000 / yield
  
  return(ncontent)
}

setnames(d1,c('paper','province','crop_type','treatment','nrate','yield_treat','yield_control',
              'ren_treat','ren_control','aen_treat','aen_control','pfpn_treat','pfpn_control','lon','lat'))
d1[,nc := nc_rice(nrate)]

#pfpn = yield (kg / ha) per kg N/ha added
d1.p1 <- copy(d1)[,nsource := 'mineral_plusmicro'][,nue_value := pfpn_treat * nc * 0.001 * 100]
d1.p2 <- copy(d1)[,nsource := 'mineral'][,nue_value := pfpn_control * nc * 0.001 * 100]
d1 <- rbind(d1.p1,d1.p2)

# load sheet with treatments with greenmanure
d2 <- readxl::read_xlsx('data/ding_2018_improving.xlsx',sheet = 'Green manure')
d2 <- as.data.table(d2)

setnames(d2,c('paper','province','crop_type','treatment','nrate','yield_treat','yield_control',
              'aen_treat','aen_control','pfpn_treat','pfpn_control','lon','lat'))
d2[,nc := nc_rice(nrate)]

#pfpn = yield (kg / ha) per kg N/ha added
d2.p1 <- copy(d2)[,nsource := 'mineral'][,n_res := TRUE][,nue_value := pfpn_treat * nc * 0.001 * 100]
d2.p2 <- copy(d2)[,nsource := 'mineral'][,n_res := FALSE][,nue_value := pfpn_control * nc * 0.001 * 100]
d2 <- rbind(d2.p1,d2.p2)



# load sheet with treatments with straw return
d3 <- readxl::read_xlsx('data/ding_2018_improving.xlsx',sheet = 'Straw return')
d3 <- as.data.table(d3)

setnames(d3,c('number','year','u1','u2','paper','province','crop_type','nrate','yield_treat','yield_control',
              'ren_treat','ren_control','aen_treat','aen_control','pfpn_treat','pfpn_control','lon','lat'))
d3[,nc := nc_rice(nrate)]
d3[,c('u1','u2','number') := NULL]

#pfpn = yield (kg / ha) per kg N/ha added
d3.p1 <- copy(d3)[,nsource := 'mineral'][,n_res := TRUE][,nue_value := pfpn_treat * nc * 0.001 * 100]
d3.p2 <- copy(d3)[,nsource := 'mineral'][,n_res := FALSE][,nue_value := pfpn_control * nc * 0.001 * 100]
d3 <- rbind(d3.p1,d3.p2)

# load sheet with treatments with organic fertilizer
d4 <- readxl::read_xlsx('data/ding_2018_improving.xlsx',sheet = 'Organic fertilizer')
d4 <- as.data.table(d4)

setnames(d4,c('paper','province','crop_type','manure','nrate','yield_treat','yield_control','orgn_fraction',
              'ren_treat','ren_control','aen_treat','aen_control','pfpn_treat','pfpn_control','lon','lat'))
d4[,nc := nc_rice(nrate)]

#pfpn = yield (kg / ha) per kg N/ha added
d4.p1 <- copy(d4)[,nsource := 'mixed'][,n_res := FALSE][,nue_value := pfpn_treat * nc * 0.001 * 100]
d4.p2 <- copy(d4)[,nsource := 'mineral'][,n_res := FALSE][,nue_value := pfpn_control * nc * 0.001 * 100]
d4 <- rbind(d4.p1,d4.p2)


# load sheet with treatments with slow release fertilizer
d5 <- readxl::read_xlsx('data/ding_2018_improving.xlsx',sheet = 'Slow-release fertilizer')
d5 <- as.data.table(d5)

setnames(d5,c('paper','province','crop_type','nrate','yield_treat','yield_control','fr_slowrelease','red_slowrelease',
              'ren_treat','ren_control','aen_treat','aen_control','pfpn_treat','pfpn_control','lon','lat'))
d5[,nc := nc_rice(nrate)]

#pfpn = yield (kg / ha) per kg N/ha added
d5.p1 <- copy(d5)[,nsource := 'slowrelease'][,n_res := FALSE][,nue_value := pfpn_treat * nc * 0.001 * 100]
d5.p2 <- copy(d5)[,nsource := 'mineral'][,n_res := FALSE][,nue_value := pfpn_control * nc * 0.001 * 100]
d5 <- rbind(d5.p1,d5.p2)

# combine all
dt <- rbind(d1,d2,d3,d4,d5,fill=TRUE)
cols <- colnames(dt)[grepl('pfpn|nc|slow|aen|ren|yield',colnames(dt))]
dt[,c(cols) := NULL]
dt[,crop_type := 'rice']

# remove missing values
dt <- dt[!is.na(nue_value)]

# add original publication number
dt[, no := 26]

# add country
dt[,country := 'China']

# add some properties
setnames(dt,c('lon','lat','nrate'),c('x','y','n_dose'))

fwrite(dt,'dev/220905 ding_sites.csv')

