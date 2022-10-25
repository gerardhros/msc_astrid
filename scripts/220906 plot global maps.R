# plot figures

  # plotting
  
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(terra)

  # set theme
  theme_set(theme_bw())

  # get the raster to plot
  r1 <- terra::rast('products/nue_curr.tif')

  # convert to data.frame
  r1.p <- as.data.frame(r1,xy=TRUE)

  # get base world map
  world <- ne_countries(scale = "medium", returnclass = "sf")

  # plot a basic world map plot
  p1 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
    geom_tile(data = r1.p,aes(x=x,y=y,fill= nue)) +
    scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean current NUE (%)") +
    coord_sf(crs = 4326)
  ggsave(plot = p1, filename = 'products/nue_current.jpg')

  # plot model 1
  
  # convert to data.frame
  r2.p <- as.data.frame(terra::rast('products/nue_curr_V2.tif'),xy=TRUE)
  # plot a basic world map plot
  p2 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
    geom_tile(data = r2.p,aes(x=x,y=y,fill= nue)) +
    scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean current NUE (%)") +
    coord_sf(crs = 4326)
  ggsave(plot = p2, filename = 'products/nue_current_v2.jpg')
  
  # plot N dose used
  r3.p <- as.data.frame(terra::rast('products/ndose_v1.tif'),xy=TRUE)
  # plot a basic world map plot
  p3 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
    geom_tile(data = r3.p,aes(x=x,y=y,fill= n_dose)) +
    scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean N dose (kg N / ha)") +
    coord_sf(crs = 4326)
  ggsave(plot = p3, filename = 'products/ndose_v1.jpg')
    
  # plot N dose after improvement
  r4.p <- as.data.frame(terra::rast('products/nue_diff_v2.tif'),xy=TRUE)
  # plot a basic world map plot
  p4 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
    geom_tile(data = r4.p,aes(x=x,y=y,fill= dnue)) +
    scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean change in NUE (%)") +
    coord_sf(crs = 4326)
  ggsave(plot = p4, filename = 'products/nue_diff_v2.jpg')
  
  
  
  # plot NUE for BAU
  db <- readRDS('dev/tmp_db_total.rds')

  p5 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
          geom_tile(data = db,aes(x=x,y=y,fill= bau)) +
          scale_fill_viridis_c()+ theme_void() +
          theme(legend.position = 'bottom') +
          xlab("Longitude") + ylab("Latitude") +
          ggtitle("World map", subtitle = "Mean NUE (%) for BAU") +
          coord_sf(crs = 4326)
  ggsave(plot = p5, filename = 'products/nue_bau.jpg')
  
  
  p6 <- visualize(db$bau,db$x,db$y,
                  breaks = c(-100,10,40,70,100,1000),
                  labels = c('<10','10-40','40-70','70-100','>100'),
                  ftitle = 'mean NUE (%) for BAU')
  ggsave(plot = p6, filename = 'products/nue_bau.jpg', width = 14, height = 8, units = 'cm')
  
  p6 <- visualize(db$best,db$x,db$y,
                  breaks = c(-100,10,40,70,100,1000),
                  labels = c('<10','10-40','40-70','70-100','>100'),
                  ftitle = 'mean NUE (%) for 4R')
  ggsave(plot = p6, filename = 'products/nue_best.jpg', width = 14, height = 8, units = 'cm')
  
  p6 <- visualize(db$nuemax_bau,db$x,db$y,
                  breaks = c(-100,10,40,70,100,1000),
                  labels = c('<10','10-40','40-70','70-100','>100'),
                  ftitle = 'mean NUE (%) for BAU')
  ggsave(plot = p6, filename = 'products/nue_maxbau.jpg', width = 14, height = 8, units = 'cm')
  
  p6 <- visualize(db$nuemax_best,db$x,db$y,
                  breaks = c(-100,10,40,70,100,1000),
                  labels = c('<10','10-40','40-70','70-100','>100'),
                  ftitle = 'mean NUE (%) for 4R')
  ggsave(plot = p6, filename = 'products/nue_maxbest.jpg', width = 14, height = 8, units = 'cm')
  
  p6 <- visualize(db$nue,db$x,db$y,
                  breaks = c(-100,10,40,70,100,1000),
                  labels = c('<10','10-40','40-70','70-100','>100'),
                  ftitle = 'mean NUE (%) for IMAGE')
  ggsave(plot = p6, filename = 'products/nue_image.jpg', width = 14, height = 8, units = 'cm')
  
  p6 <- visualize(db$impr,db$x,db$y,
                  breaks = c(-100,20,30,40,50,1000),
                  labels = c('<20','20-30','30-40','40-50','>50'),
                  ftitle = 'improvement NUE (%) by 4R')
  ggsave(plot = p6, filename = 'products/nue_improvement.jpg', width = 14, height = 8, units = 'cm')
  
  p6 <- visualize(db$nuemax_best - db$nuemax_bau,db$x,db$y,
                  breaks = c(-100,20,25,30,35,1000),
                  labels = c('<20','20-25','25-30','30-35','>35'),
                  ftitle = 'improvement max NUE (%) by 4R')
  ggsave(plot = p6, filename = 'products/nue_maximprovement.jpg', width = 14, height = 8, units = 'cm')
  
  
  
  visualize <- function(variable,x,y, name = 'NUE (%)',breaks, labels, ftitle){
    
    df <- data.table(x=x,y=y,variable = variable)
    plotcrs <- coord_sf(crs = 4326, lims_method = "box")
    #plot
    ggplot() + 
      geom_sf(data = world, color = "black", fill = "white",show.legend = FALSE) +
      geom_tile(data = df, aes(x = x, y = y,fill = cut(variable, breaks,labels = labels))) +
      plotcrs +
      scale_fill_viridis_d(na.translate = F) +
      xlab("") + ylab("")+
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("World map", subtitle = ftitle) +
      labs(fill = name) + 
      theme(text = element_text(size = 10), 
            legend.position = c(0.15,0.4),
            legend.text = element_text(size=8),
            legend.title = element_text(size=8),
            legend.key.height= unit(0.3, 'cm'),
            legend.key.width= unit(0.3, 'cm'),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank(),
            plot.title = element_text(hjust = 0)) 
  }
  