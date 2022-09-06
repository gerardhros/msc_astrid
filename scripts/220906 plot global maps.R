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
