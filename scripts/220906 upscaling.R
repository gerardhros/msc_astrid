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
  
  dt.n2 <- copy(r.dt.melt)
  dt.n2[, n_place := 'broadcasted']
  dt.n2[, n_source := 'inorganic']
  dt.n2[, n_timing := 0]
  dt.n2[, n_dose := nh4 + no3]
  dt.n2[, n_time_splits := 0]
  dt.n2[, xclay := clay]
  dt.n2[, pre_mean := pre]
  dt.n2[, tmp_mean := mat]
  dt.n2[, xsom := soc * 2 / 10]
  dt.n2[grepl('rice',croptype),crop_type := 'rice']
  dt.n2[grepl('maiz',croptype),crop_type := 'maize']
  dt.n2[grepl('rootcrop',croptype),crop_type := 'rootcrops']
  dt.n2[grepl('cereal',croptype),crop_type := 'wheat']
  dt.n2[grepl('other',croptype),crop_type := 'other']
  dt.n2[grepl('nfix',croptype),crop_type := 'nfixing']
  m1 <- readRDS('products/nue_m1.rds')
  p1 <- predict(m1,newdata = dt.n2, interval="confidence",se.fit = TRUE)
  dt.n2[,pred := as.numeric(p1$fit[,1])]
  dt.n2[,predse := as.numeric(p1$se.fit)]
  
  # subset, estimate mean per area, and write as geotiff
  dt.fin <- dt.n2[,.(x,y,pred,area = value)]
  dt.fin <- dt.fin[,list(nue = weighted.mean(pred,w = area,na.rm = TRUE)),by = c('x','y')]
  r.fin <- terra::rast(dt.fin,type='xyz')
  terra::crs(r.fin) <- 'epsg:4326'
  terra::writeRaster(r.fin,'products/nue_curr_v2.tif', overwrite = TRUE)
 
  # subset, estimate mean per area, and write as geotiff
  dt.fin <- dt.n2[,.(x,y,n_dose,area = value)]
  dt.fin <- dt.fin[,list(n_dose = weighted.mean(n_dose,w = area,na.rm = TRUE)),by = c('x','y')]
  r.fin <- terra::rast(dt.fin,type='xyz')
  terra::crs(r.fin) <- 'epsg:4326'
  terra::writeRaster(r.fin,'products/ndose_v1.tif', overwrite = TRUE)
  
  dt.n3 <- copy(r.dt.melt)
  dt.n3[, n_place := 'injected']
  dt.n3[, n_source := 'manure']
  dt.n3[, n_timing := 2]
  dt.n3[, n_dose := 0.9*(nh4 + no3)]
  dt.n3[, n_time_splits := 3]
  dt.n3[, xclay := clay]
  dt.n3[, pre_mean := pre]
  dt.n3[, tmp_mean := mat]
  dt.n3[, xsom := soc * 2 / 10]
  dt.n3[grepl('rice',croptype),crop_type := 'rice']
  dt.n3[grepl('maiz',croptype),crop_type := 'maize']
  dt.n3[grepl('rootcrop',croptype),crop_type := 'maize']
  dt.n3[grepl('cereal',croptype),crop_type := 'wheat']
  dt.n3[grepl('other',croptype),crop_type := 'maize']
  dt.n3[grepl('nfix',croptype),crop_type := 'nfixing']
  m1 <- readRDS('products/nue_m1.rds')
  p1 <- predict(m1,newdata = dt.n3, interval="confidence",se.fit = TRUE)
  dt.n3[,pred := as.numeric(p1$fit[,1])]
  dt.n3[,predse := as.numeric(p1$se.fit)]
  
  # subset, estimate mean per area, and write as geotiff
  dt.fin <- dt.n3[,.(x,y,pred,area = value)]
  dt.fin <- dt.fin[,list(nue = weighted.mean(pred,w = area,na.rm = TRUE)),by = c('x','y')]
  r.fin <- terra::rast(dt.fin,type='xyz')
  terra::crs(r.fin) <- 'epsg:4326'
  terra::writeRaster(r.fin,'products/nue_impr_v2.tif', overwrite = TRUE)

  # change due to upgrade
  dt.n2[,uid := .I]
  dt.n3[,uid := .I]
  dt.n4 <- merge(dt.n2[,.(x,y,uid,curr = pred)],
                 dt.n3[,.(x,y,uid,impr = pred,value)],by=c('uid', 'x','y'))
  dt.n4[,diff := impr - curr]
    
  # subset, estimate mean per area, and write as geotiff
  dt.fin <- dt.n4[,.(x,y,dnue = diff,area = value)]
  dt.fin <- dt.fin[,list(dnue = weighted.mean(dnue,w = area,na.rm = TRUE)),by = c('x','y')]
  r.fin <- terra::rast(dt.fin,type='xyz')
  terra::crs(r.fin) <- 'epsg:4326'
  terra::writeRaster(r.fin,'products/nue_diff_v2.tif', overwrite = TRUE)
  