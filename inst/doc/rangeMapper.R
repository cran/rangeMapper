## ----setup, echo=FALSE--------------------------------------------------------
library(knitr)
opts_chunk$set(
  warning = FALSE, 
  message = FALSE, 
  fig.height = 5,   
  collapse = TRUE,
  comment = "#>"
  )

## -----------------------------------------------------------------------------
require(rangeMapper)
require(sf)
require(data.table)
require(ggplot2)
require(viridis)

data(dem)
data(wrens)
wrens$breeding_range_area = st_area(wrens)


## -----------------------------------------------------------------------------
# path is not specified so an in-memory file is created.
con = rmap_connect()


## -----------------------------------------------------------------------------
rmap_add_ranges(con, x = wrens, ID = 'sci_name')
rmap_prepare(con, 'hex', cellsize = 500)
rmap_add_bio(con, wrens, 'sci_name')

## -----------------------------------------------------------------------------
lm(clutch_size~log(body_mass), wrens)  %>%  summary

## -----------------------------------------------------------------------------
rmap_save_map(con)

## -----------------------------------------------------------------------------
rmap_save_subset(con,'sset1', species_richness = 'species_richness > 10')

## -----------------------------------------------------------------------------
linmod = function(x) {
  lm(clutch_size ~ log(body_mass), x) %>% 
    summary %>% coefficients %>% data.table %>% .[-1] }

rmap_save_map(con, fun= linmod, subset= 'sset1', src='wrens', dst='slope_clutch_size')

## -----------------------------------------------------------------------------
x = rmap_to_sf(con)

ggplot() + 
  geom_sf(data = x, aes(fill = Estimate),  size= 0.05) + 
  scale_fill_gradientn(colours =  viridis(10, option = 'E'), na.value= 'grey80') + 
  theme_bw()

## -----------------------------------------------------------------------------
xy = st_centroid(x)  %>% st_coordinates
x = cbind(x, xy )

ggplot(x , aes(y = Estimate, x = Y) ) + 
  geom_point() + 
  geom_smooth() +
  theme_bw() + 
  ylab('Clutch size ~ Body mass slope') + 
  xlab('Distance from equator (km)')


