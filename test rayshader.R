#test rayshader
#https://www.rayshader.com/ 

install.packages("rayshader") # Note: Need v0.26.1 or greater to avoid an error, CRAN has 0.24 (3/9/21)
# devtools::install_github("tylermorganwall/rayshader") #Need Rtools to build
# my Rtools playing up, used this technique to build latest version:
# https://stackoverflow.com/a/65161599/4927395

library(rayshader)
library(rayrender)
library(ggplot2)

# Here, I load a map with the raster package. ####
loadzip = tempfile()
download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
localtif = raster::raster(unzip(loadzip, "dem_01.tif"))
unlink(loadzip)

#And convert it to a matrix:
elmat = raster_to_matrix(localtif)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

#sphere_shade can shift the sun direction:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  plot_map()

#detect_water and add_water adds a water layer to the map:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  plot_map()

#And we can add a raytraced layer from that sun direction as well:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(elmat) %>%
  plot_map()
# Error in hillshade[, , 1] * scales::rescale(shadowmap, c(max_darken, 1)) : 
#   non-conformable arrays

dim(elmat)
dim(ray_shade(elmat))

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_map()


elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot()

## Monterey bay example ####

montshadow = ray_shade(montereybay, zscale = 50, lambert = FALSE)
montamb = ambient_shade(montereybay, zscale = 50)
montereybay %>%
  sphere_shade(zscale = 10, texture = "imhof1") %>%
  add_shadow(montshadow, 0.5) %>%
  add_shadow(montamb, 0) %>%
  plot_3d(montereybay, zscale = 50, fov = 0, theta = -45, phi = 45,
          windowsize = c(1000, 800), zoom = 0.75,
          water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
          waterlinecolor = "white", waterlinealpha = 0.5)
Sys.sleep(0.2)
render_snapshot(clear=TRUE)


montereybay %>%
  sphere_shade(zscale = 10, texture = "imhof1") %>%
  plot_3d(montereybay, zscale = 50, fov = 70, theta = 270, phi = 30,
          windowsize = c(1000, 800), zoom = 0.6,
          water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "#233aa1",
          waterlinecolor = "white", waterlinealpha = 0.5)
Sys.sleep(0.2)
render_highquality(lightdirection = c(-45,45), lightaltitude  = 30, clamp_value = 10,
                   samples = 200, camera_lookat= c(0,-50,0),
                   ground_material = diffuse(color="grey50",checkercolor = "grey20", checkerperiod = 100))

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 30, theta = -225, phi = 25, windowsize = c(1000, 800), zoom = 0.3)
Sys.sleep(0.2)
render_depth(focus = 0.6, focallength = 200, clear = TRUE)

## ggplot example ####

a = data.frame(x = rnorm(20000, 10, 1.9), y = rnorm(20000, 10, 1.2))
b = data.frame(x = rnorm(20000, 14.5, 1.9), y = rnorm(20000, 14.5, 1.9))
c = data.frame(x = rnorm(20000, 9.5, 1.9), y = rnorm(20000, 15.5, 1.9))
data = rbind(a, b, c)

#Lines
pp = ggplot(data, aes(x = x, y = y)) +
  geom_hex(bins = 20, size = 0.5, color = "black") +
  scale_fill_viridis_c(option = "C")

par(mfrow = c(1, 2))
plot_gg(pp, width = 5, height = 4, scale = 300, raytrace = FALSE, preview = TRUE)
plot_gg(pp, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1000, 800))
render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)
Sys.sleep(0.2)
render_snapshot(clear = TRUE) 
