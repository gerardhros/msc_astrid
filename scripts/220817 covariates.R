library(parzer);library(data.table);library(readxl);
library(terra);library(data.table);library(sf);
library(stringr);library(foreign);library(mapview);

# load data
lonlat <- fread("dev/test_nieuwe_covariates2.csv")
as.data.table(lonlat)

# convert comma's to dots
lonlat[, x:= gsub(",", ".", gsub("\\.", "", x))]
lonlat[, y:= gsub(",", ".", gsub("\\.", "", y))]

# transform from DSC to dec
lon <- parse_lon(lonlat$x)
lat<- parse_lat(lonlat$y)

# create data table 
lon <- as.vector(lon)
lat <- as.vector(lat)
d1 <- data.table(y = lat, x = lon, obs_no = lonlat$obs_no)

# assign unique ID to each row
d1[, ID := (1:nrow(d1))]

# remove observations without X Y data from d1
d2 <- d1[!is.na(y) & !is.na(x),]

# convert to spatial object
s1 <- st_as_sf(d2,coords = c('x','y'),crs = 4326)
s1 <- vect(s1)

# read in dbf file for metzger climatic regions
s2 <- foreign::read.dbf('C:/Astrid/NUE/NUE/data/dataNUE/03 metzger/GenS_v3.dbf')

# what rasters are available
# downloaded via QGIS for ISRIC, 0.5 degrees resolution, https://maps.isric.org/
# downloaded via CRU, https://catalogue.ceda.ac.uk/uuid/89e1e34ec3554dc98594a5732622bce9
# downloaded via https://datashare.ed.ac.uk/handle/10283/3089

# read in the rasters via hard drive
r1 <- list.files('C:/Astrid/NUE/NUE/data/dataNUE/01 soil',pattern = 'tif|nc',full.names = T)
r2 <- list.files('C:/Astrid/NUE/NUE/data/dataNUE/02 climate',pattern = 'tif|nc',full.names = T)
r3 <- list.files('C:/Astrid/NUE/NUE/data/dataNUE/03 metzger',pattern = 'tif|nc',full.names = T)

# read in the raster files and convert to spatrasters
isric <- sds(r1)
climate <- sds(r2)
metzger <- rast(r3)
isric <- rast(isric)
climate <- rast(climate)

# --- extract isric data ----

# update names of isric raster to avoid duplication in names
names(isric) <- str_split_fixed(names(isric),"_isric_",2)[,2]

# extract data for the spatial objects
d1.isric <- terra::extract(x = isric, y = s1)
d2.isric <- terra::extract(x = isric, y = buffer(s1,width = 10000), fun = mean, na.rm=T)
d3.isric <- terra::extract(x = isric, y = buffer(s1,width = 20000), fun = mean, na.rm=T)
d4.isric <- terra::extract(x = isric, y = buffer(s1,width = 30000), fun = mean, na.rm=T)
d5.isric <- terra::extract(x = isric, y = buffer(s1,width = 50000), fun = mean, na.rm=T)

# convert to data.table to facilitate re-arranging
setDT(d1.isric);setDT(d2.isric);setDT(d3.isric);setDT(d4.isric);setDT(d5.isric)

# function to adapt colnames
acn <- function(x,var='or'){c('ID',paste0(var,'_',gsub('isric_|_mean_|','',x[-1])))}

# adapt colnames
setnames(d1.isric,acn(colnames(d1.isric)))
setnames(d2.isric,acn(colnames(d2.isric),'e1'))
setnames(d3.isric,acn(colnames(d3.isric),'e2'))
setnames(d4.isric,acn(colnames(d4.isric),'e3'))
setnames(d5.isric,acn(colnames(d5.isric),'e4'))

c1.isric <- merge(d1.isric,d2.isric,by = "ID")
c1.isric <- merge(c1.isric,d3.isric,by = "ID")
c1.isric <- merge(c1.isric,d4.isric,by = "ID")
c1.isric <- merge(c1.isric,d5.isric,by = "ID")

c1.isric <- melt(c1.isric,id = 'ID',
                 measure=patterns("or_", "e1_","e2_","e3_","e4_"),
                 variable.factor = FALSE,
                 value.name = c("or", "e1","e2","e3","e4"))
c1.isric[,variable := sort(names(isric))[as.integer(variable)]]
c1.isric[,value := as.numeric(or)]
c1.isric[or < 1 & e1 > 1,value := e1]
c1.isric[or < 1 & e2 > 1,value := e2]
c1.isric[or < 1 & e3 > 1,value := e3]
c1.isric[or < 1 & e4 > 1,value := e4]

c2.isric <- dcast(c1.isric,ID~variable, value.var = 'value')


# --- extract climate data ----


# extract climate data nc files
d1.climate <- terra::extract(x = climate, y = s1)

# convert to data.table
d1.climate <- as.data.table(d1.climate)

# rearrange data
d2.climate <- melt(d1.climate,id.vars = 'ID', variable.name = 'variable')
d2.climate <- d2.climate[!grepl('_stn_',variable)]
d2.climate[, cvar :=  stringr::str_extract_all(variable,"(?<=[0-9]{4}\\.[0-9]{4}\\.).+(?=\\.dat_)",simplify = T)]
d2.climate[, years :=  stringr::str_extract_all(variable,"[0-9]{4}\\.[0-9]{4}",simplify = T)]
d2.climate[, month :=  stringr::str_extract_all(variable,"(?<=[a-z]{3}_)\\d+",simplify = T)]

# estimate mean global climate properties over 1991-2019
# temperature in degrees (mean = tmp, max = tmx, min = tmn)
# potential evaporation in mm/day
# precipitation in mm/month
d3.climate <- dcast(d2.climate,ID+years+month~cvar,value.var = 'value')

# derive the mean and SD per gridcel over period 1991-2019
c1.climate <- d3.climate[,list(pre_mean = mean(pre),
                               pre_sd = sd(pre),
                               tmp_mean = mean(tmp),
                               tmp_sd = sd(tmp),
                               pet_mean = mean(pet),
                               pet_sd = sd(pet)
),by='ID']
c2.climate <- copy(c1.climate)

# --- extract metzger data

# extract for measurement points
d1.metzger <- terra::extract(x = metzger, y = s1)

# read metzger decription
s2.dt <- as.data.table(s2)

# merge description
d1.metzger <- as.data.table(d1.metzger)
c2.metzger <- merge(d1.metzger,s2.dt,by.x = 'gens_v3', by.y = 'GEnS_seq')

# subset
c2.metzger <- c2.metzger[,.(ID,GEnZname,GEnZ,GEnS)]

# rename columns
dt <- d2[,.(obs_no,lon = x,lat = y)]

# reassign unique ID to each row
dt[, ID := (1:nrow(dt))]

# merge the data files
dt <- merge(dt,c2.isric, by='ID')
dt <- merge(dt,c2.climate, by='ID')
#dt <- merge(dt,c2.metzger, by='ID')

# save the file
#fwrite(dt,'dev/covariates_schutz2018.csv', dec=',', sep=';')


### --- prepare data for data merge ---- ###

# average ISRIC values for 0-5 cm, 5-15 cm and 15-30 cm over 0-30 cm
dt[, XCEC := ((cec_mean_0_5 + cec_mean_5_15*2 + cec_mean_15_30*3)/6)]
dt[, XCLAY := ((clay_mean_0_5 + clay_mean_5_15*2 + clay_mean_15_30*3)/6)] 
dt[, xnit := ((ntot_mean_0_5 + ntot_mean_5_15*2 + ntot_mean_15_30*3)/6)]  
dt[, XPH := ((phw_mean_0_5 + phw_mean_5_15*2 + phw_mean_15_30*3)/6)]  
dt[, XSAND := ((sand_mean_0_5 + sand_mean_5_15*2 + sand_mean_15_30*3)/6)]   
dt[, XSILT := ((silt_mean_0_5 + silt_mean_5_15*2 + silt_mean_15_30*3)/6)] 
dt[, XSOC := ((soc_mean_0_5 + soc_mean_5_15*2 + soc_mean_15_30*3)/6)] 
dt[, xsom := (XSOC*2)]

# rename climate columns
setnames(dt,"pet_mean","pot_eva")
setnames(dt,"tmp_mean","mn_temp")  
setnames(dt,"pre_mean","prec_mn") 

# convert covariables to the right units
dt[, pot_eva := pot_eva*365] # convert from mm/d to mm/y
dt[, prec_mn := prec_mn*12] # convert from mm/month to mm/y
dt[, XCLAY := XCLAY/10] # convert from gkg to %
dt[, XSILT := XSILT/10] # convert from gkg to %
dt[, XSAND := XSAND/10] # convert from gkg to %
dt[, xnit := xnit/100] # convert from cg/kg to g/kg
dt[, XPH := XPH/10] # convert from pHx10 to pH
dt[, XSOC := XSOC/10 ] # convert from dg/kg to g/kg 
dt[, xsom := xsom/10 ] # convert from dg/kg to g/kg 

# extract columns to be used
final <- dt[,.(obs_no,lon,lat,XCEC,xnit,XPH,XSOC,XSAND,XSILT,XCLAY,pot_eva,mn_temp,prec_mn,xsom)]

# round values and set names to lower
cols <- colnames(final[,!c("obs_no")])
final[, c(cols) := round(.SD,2), .SDcols = cols] 
setnames(final,tolower(colnames(final)))

# save the file  
fwrite(final,'dev/covariates_data266-451.base.csv', dec=',', sep=';')



