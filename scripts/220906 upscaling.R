# predict plots

  # remove global environment
  rm(list = ls())
  
  # read the precipitation and temperature,mean, 0.5 x 0.5 degrees
  r.clim <- terra::rast('data/climate.tif')
  
  # read in ph, clay and soc, 0.5 x 0.5 degrees
  r.soil <- terra::rast('data/soil.tif')
  
  # read in inorganic N input
  r.nfert <- terra::rast('data/nifert.tif')
  
  # load crop area harvested as raster
  if(FALSE){
    
    # get the file names of the tiffs
    rfiles <- list.files('D:/DATA/04 crop/spam2010', pattern = '_H.tif$',full.names = TRUE)
    
    # read in all files and convert to spatrasters
    r.crop <- terra::sds(rfiles)
    r.crop <- terra::rast(r.crop)
    
    # aggregate to 0.5 x 0.5 degree
    r.crop <- terra::aggregate(r.crop,fact = 0.5/0.083333,fun = "sum", na.rm=T)
    
    # adjust names
    names(r.crop) <- stringr::str_extract(names(r.crop),'[A-Z]{4}')
    
    # crop names to combine in other
    rcrop <- 'RICE'
    mcrop <- 'MAIZ'
    ccrop <- c('WHEA','BARL','SMIL','PMIL','SORG','OCER')
    rcrop <- c('POTA','SWPO','CASS','ORTS','SUGB')
    ncrop <- c('BEAN','COWP','CHIC','PIGE','OPUL','SOYB')
    other <- names(r.crop)[!names(r.crop) %in% c(rcrop,mcrop,ccrop,rcrop,ncrop)]
    
    # sum all other crops
    r.crop.rice = terra::app(r.crop[[rcrop]],fun = sum,na.rm=T)
    names(r.crop.rice) <- 'rice'
    r.crop.maize = terra::app(r.crop[[mcrop]],fun = sum,na.rm=T)
    names(r.crop.maize) <- 'maize'
    r.crop.cereal = terra::app(r.crop[[ccrop]],fun = sum,na.rm=T)
    names(r.crop.cereal) <- 'cereal'
    r.crop.rcrop = terra::app(r.crop[[rcrop]],fun = sum,na.rm=T)
    names(r.crop.rcrop) <- 'rootcrop'
    r.crop.nfix = terra::app(r.crop[[ncrop]],fun = sum,na.rm=T)
    names(r.crop.nfix) <- 'nfix'
    r.crop.other = terra::app(r.crop[[other]],fun = sum,na.rm=T)
    names(r.crop.other) <- 'other'
  
    # combine again
    r.crop <- c(r.crop.rice,
                r.crop.maize,
                r.crop.cereal,
                r.crop.rcrop,
                r.crop.nfix,
                r.crop.other)
    
    # reproject to r.clim
    r.crop <- terra::resample(r.crop,r.clim,method='bilinear')
    
    # write raster
    terra::writeRaster(r.crop,'data/ma_crops.tif', overwrite = TRUE)
    
    
  } else {
    
    # read the raster with cropping data
    r.crop <- terra::rast('data/ma_crops.tif')
    
  }
  
# ---- add all rasters ----
  
  # clear environment
  rm(list= ls())
  
  # what rasters are in data
  rfiles <- list.files('data', pattern = 'tif$',full.names = TRUE)
  
  # read in raster files
  r.ma <- terra::sds(rfiles)
  
  # convert to raster
  r.ma <- terra::rast(r.ma)
  
  # convert rasters to data.table
  
    # set first to xy data.frame (NA=FALSE otherwise gridcels are removed)
    r.df <- as.data.frame(r.ma,xy = TRUE, na.rm = FALSE)
  
    # convert to data.table
    r.dt <- as.data.table(r.df)
  
  # setnames
  setnames(r.dt,old = c('climate_mat', 'climate_pre',
                        'soil_isric_phw_mean_0_5','soil_isric_clay_mean_0_5','soil_isric_soc_mean_0_5',
                        'nifert_nfert_nh4','nifert_nfert_no3'),
           new = c('mat','pre','ph','clay','soc','nh4','no3'),skip_absent = T)
  
  # select only land area
  r.dt <- r.dt[!(is.na(mat)|is.na(pre))]
  r.dt <- r.dt[!(is.na(ma_crops_rice) & is.na(ma_crops_maize) & is.na(ma_crops_rootcrop) 
                 & is.na(ma_crops_other) & is.na(ma_crops_nfix)& is.na(ma_crops_cereal))]
  
  # replace area with 0 when missing
  cols <- colnames(r.dt)[grepl('^ma_|nh4|no3|nam',colnames(r.dt))]
  r.dt[,c(cols) := lapply(.SD,function(x) fifelse(is.na(x),0,x)),.SDcols = cols]
  
  # update units
  r.dt[,som := 2 * soc / 10]
  r.dt[,clay := clay / 10]
  r.dt[,ph := ph / 10]
  
  # melt the data.table
  r.dt.melt <- melt(r.dt,
                    id.vars = c('x','y','mat', 'pre','ph','clay','nh4','no3','soc'),
                    measure=patterns(area="^ma_crops"),
                    variable.factor = FALSE,
                    variable.name = 'croptype')
  
  # make prediction dataset for current situation
  dt.new <- copy(r.dt.melt)

  dt.new[, n_place := 'broadcasted']
  dt.new[, n_source := 'inorganic']
  dt.new[, n_timing := 0]
  dt.new[, n_dose := nh4 + no3]
  dt.new[grepl('rice',croptype), nue := 60.46 + 9.205 + 0.5486 - 7.514 * n_timing  - 6.33e-7 * n_dose * n_dose^2 - 3.004]
  dt.new[grepl('maiz',croptype), nue := 60.46 + 9.205 + 0.5486 - 7.514 * n_timing  - 6.33e-7 * n_dose * n_dose^2 - 0]
  dt.new[grepl('rootcrop',croptype), nue := 60.46 + 9.205 + 0.5486 - 7.514 * n_timing  - 6.33e-7 * n_dose * n_dose^2 - 18.85]
  dt.new[grepl('other',croptype), nue := 60.46 + 9.205 + 0.5486 - 7.514 * n_timing  - 6.33e-7 * n_dose * n_dose^2 - 19.27]
  dt.new[grepl('cereal',croptype), nue := 60.46 + 9.205 + 0.5486 - 7.514 * n_timing  - 6.33e-7 * n_dose * n_dose^2 - 2.749]
  dt.new[grepl('nfix',croptype), nue := 60.46 + 9.205 + 0.5486 - 7.514 * n_timing  - 6.33e-7 * n_dose * n_dose^2 - 3.962]
  
  # subset 
  dt.fin <- dt.new[,.(x,y,nue,area = value)]
  
  # estimate area weighted mean NUE
  dt.fin <- dt.fin[,list(nue = weighted.mean(nue,w = area,na.rm = TRUE)),by = c('x','y')]
  
  # convert to spatial raster
  r.fin <- terra::rast(dt.fin,type='xyz')
  terra::crs(r.fin) <- 'epsg:4326'
  
  # write as output
  terra::writeRaster(r.fin,'products/nue_curr.tif', overwrite = TRUE)
  