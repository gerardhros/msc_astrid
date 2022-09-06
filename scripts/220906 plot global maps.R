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
  