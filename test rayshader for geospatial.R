# test rayshader for geospatial
# This script is combining satelite imagery with elevation models to make sweet 3d plots, 
# and even a video of Ruapehu! Based on this tutorial: 
# https://www.tylermw.com/a-step-by-step-guide-to-making-3d-maps-with-satellite-imagery-in-r/

## Load packages
library(rayshader)
library(sp)
library(raster)
library(scales)
library(ggplot2)

## Get data from LINZ (Hat tip Matthew Skiffington Uni of Waikato)
# https://www.reddit.com/r/gis/comments/dj6pph/animation_of_mt_taranaki_nz_using_rayshader_in_r/
# Data:
#   https://data.linz.govt.nz/layer/93652-nz-10m-satellite-imagery-2017/
#   https://data.linz.govt.nz/layer/51768-nz-8m-digital-elevation-model-2012/

## Download relevant tiles
# BJ34_2016-12-06_2017-12-01.tif #image
# IL.tif #Elevation

elevation1 = raster::raster("IL.tif")
class(elevation1)
crs(elevation1) <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"

## If multiple tiles are required, download then merge
#elevation2 = raster::raster("~/Desktop/LC08_L1TP_038034_20191117_20191202_01_T1/N37W114.hgt")

#elevation = raster::merge(elevation1,elevation2)
#elevation = raster::merge(elevation1)

## Resample raster to reduce size (so R doesn't run out of memory)
# https://stackoverflow.com/questions/32278825/how-to-change-the-resolution-of-a-raster-layer-in-r

res(elevation1)
saveRDS(elevation1, "elevation1.RDS") # Doesn't seem to resave the data as a new RDS file

elevation1_midres <- aggregate(elevation1, fact=2)
res(elevation1_midres)
saveRDS(elevation1_midres, "elevation1_midres.RDS")

elevation1_lores <- aggregate(elevation1, fact=10)
res(elevation1_lores)
saveRDS(elevation1_lores, "elevation1_lores.RDS")

#restart R session to clear up?

#elevation1_midres <- readRDS("elevation1_midres.RDS")
height_shade(raster_to_matrix(elevation1_midres)) %>%
  plot_map()

#elevation1_lores <- readRDS("elevation1_lores.RDS")
height_shade(raster_to_matrix(elevation1_lores)) %>%
  plot_map()

gc()

rbg = raster::stack("BJ34_2016-12-06_2017-12-01.tif")
class(rbg)
#plot(rbg)
rbg
rbg <- dropLayer(rbg,c(4))

plot(rbg)
raster::plotRGB(rbg)


## Crop_region

## Relevant region guestimated from google
# top left
# -39.204305, 175.404060
#bottom right
#-39.372443, 175.655970

bottom_left = c(y=-39.372443, x=175.655970)
top_right   = c(y=-39.204305, x=175.404060)

## which CRS?
# crs(elevation1_lores)
# crs(elevation1)
# print("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m")

## Couldn't get this to work
# extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# extent_utm = sp::spTransform(extent_latlong, raster::crs(elevation1_lores))

## Instead: Find overlapping area to make extent
bbox(elevation1)
bbox_elevation <- bbox(elevation1_lores)
bbox_image <- bbox(rbg)
class(bbox_elevation)
bottom_left = c(x=max(bbox_elevation["s1","min"],bbox_image["s1","min"]),
                y=max(bbox_elevation["s2","min"],bbox_image["s1","min"])) 
bottom_left
top_right = c(x=min(bbox_elevation["s1","max"],bbox_image["s1","max"]),
              y=min(bbox_elevation["s2","max"],bbox_image["s2","max"])) 
top_right

extent_utm = sp::SpatialPoints(rbind(bottom_left, top_right), proj4string=raster::crs(elevation1_lores))

e = raster::extent(extent_utm)
e

## crop to extent 

rgb_cropped = raster::crop(rbg, e)
elevation_cropped = raster::crop(elevation1_lores, e)

names(rgb_cropped) = c("r","g","b")

r_cropped = rayshader::raster_to_matrix(rgb_cropped$r)
g_cropped = rayshader::raster_to_matrix(rgb_cropped$g)
b_cropped = rayshader::raster_to_matrix(rgb_cropped$b)

el_matrix = rayshader::raster_to_matrix(elevation_cropped)

rgb_array = array(0,dim=c(nrow(r_cropped),ncol(r_cropped),3))

rgb_array[,,1] = r_cropped/255 #Red layer
rgb_array[,,2] = g_cropped/255 #Blue layer
rgb_array[,,3] = b_cropped/255 #Green layer

rgb_array = aperm(rgb_array, c(2,1,3))

plot_map(rgb_array)

## rescale for colour
## Actually the LINZ imagery was already pretty good

rgb_contrast = scales::rescale(rgb_array,to=c(0,1))

plot_map(rgb_contrast)

## plot 3d!
plot_3d(rgb_contrast, el_matrix, windowsize = c(1100,900), zscale = 50, shadowdepth = -50,
        zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
render_snapshot(title_text = "Ruapehu | Imagery: Linz | DEM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)

## Video of 3d!
  
angles= seq(0,360,length.out = 1441)[-1]
for(i in 1:1440) {
  render_camera(theta=-45+angles[i])
  render_snapshot(filename = sprintf("Ruapehu%i.png", i), 
                  title_text = "Ruapehu | Imagery: Linz | DEM",
                  title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
}
rgl::rgl.close()

#av::av_encode_video(sprintf("zionpark%d.png",seq(1,1440,by=1)), framerate = 30,
# output = "zionpark.mp4")

rgl::rgl.close()

# To use ffmpeg, you'll need to install it and add the directory where ffmpeg lives to the path, 
# and if the path to your files have any spaces, enclose with double quotes (for windows)
system("ffmpeg -framerate 60 -i \"C:/Users/nealm/OneDrive - DairyNZ Limited/Desktop/dairynz/rayshader/Ruapehu%d.png\" -pix_fmt yuv420p Ruapehu.mp4")
# or just use his at cmd prompt:
#ffmpeg -framerate 60 -i "C:/Users/nealm/OneDrive - DairyNZ Limited/Desktop/dairynz/rayshader/Ruapehu%d.png" -pix_fmt yuv420p Ruapehu.mp4


