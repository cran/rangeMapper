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
require(glue)
require(ggplot2)
require(viridis)

data(wrens)
wrens$breeding_range_area = st_area(wrens)

con = rmap_connect()

rmap_add_ranges(con, x = wrens, ID = 'sci_name')
rmap_prepare(con, 'hex', cellsize = 500, chunksize = 1)
rmap_add_bio(con, wrens, 'sci_name')


## -----------------------------------------------------------------------------

ggplot() + 
  geom_sf(data = rmap_to_sf(con, 'wkt_canvas') , color = 'grey80', fill = NA) +
  geom_sf(data = wrens, fill = NA) 

head(wrens,3)


## -----------------------------------------------------------------------------
P_richness  = 0.75 # species richness quantile
P_bodymass  = 0.50 # CV body mass quantile
P_endemics  = 0.35 # endemic species richness quantile


## -----------------------------------------------------------------------------
rmap_save_map(con)  
# rmap_save_map with no arguments other than `con` saves a species_richness map.

CV_Mass <- function(x) (sd(log(x),na.rm = TRUE)/mean(log(x),na.rm = TRUE))
rmap_save_map(con, fun = CV_Mass, src='wrens',v = 'body_mass', dst='CV_Mass')


## -----------------------------------------------------------------------------
sr = rmap_to_sf(con, "species_richness")
sr_threshold = quantile(sr$species_richness, probs = P_richness, na.rm = TRUE)

es_threshold = quantile(wrens$breeding_range_area, probs = P_endemics, na.rm = TRUE)

bmr = rmap_to_sf(con, "CV_Mass")
bmr_threshold = quantile(bmr$V1_body_mass, probs = P_bodymass, na.rm = TRUE)


## -----------------------------------------------------------------------------
rmap_save_subset(con,'sr_threshold', species_richness = paste('species_richness     >', sr_threshold) )
rmap_save_subset(con,'es_threshold', wrens            = paste('breeding_range_area <=', es_threshold) )
rmap_save_subset(con,'bmr_threshold', CV_Mass         = paste('V1_body_mass        >=', bmr_threshold))

rmap_save_subset(con, "cumul_congruence_threshold",
    species_richness = paste('species_richness >', sr_threshold),
    wrens            = paste('breeding_range_area <=', es_threshold), 
    CV_Mass          = paste('body_mass >=', bmr_threshold) 
    )

## -----------------------------------------------------------------------------
rmap_save_map(con, subset = 'sr_threshold', dst = 'Species_richness_hotspots')
rmap_save_map(con, subset = 'es_threshold', dst = 'Endemics_hotspots')
rmap_save_map(con, subset = 'bmr_threshold', dst = 'Body_mass_diversity_hotspots')
rmap_save_map(con, subset = 'cumul_congruence_threshold', dst = 'Cumul_congruence_hotspots')



## ---- fig.height = 10---------------------------------------------------------
study_area = rmap_to_sf(con, 'species_richness')  %>% st_union
bmr = rmap_to_sf(con, pattern = 'hotspots')  %>% 
      melt(id.vars = c('geometry', 'cell_id') )  %>% 
      st_as_sf
bmr$variable = bmr$variable %>% gsub('species_richness_|_hotspots', '', .)      

ggplot() + 
  facet_wrap(~variable) + 
  geom_sf(data = study_area ) + 
  geom_sf(data = bmr, aes(fill = value),  size= 0.05) + 
  scale_fill_gradientn(colours = viridis(10, option = 'E'), na.value= 'grey80') + 
  guides(fill=guide_legend(title='Wren\nspecies')) +
  ggtitle("Hotspots") +
  theme_bw()



## -----------------------------------------------------------------------------

lm_slope = function (x) {
  lm(scale(log(breeding_range_area)) ~ scale(male_tarsus), x)  %>% 
  summary %>% coefficients %>% data.frame %>% .[-1, ] 
  }


rmap_save_map(con, fun = lm_slope, src='wrens', dst='slope_area_body_mass')


## -----------------------------------------------------------------------------
m = rmap_to_sf(con, 'slope_area_body_mass')

ggplot(m) + 
  geom_sf(aes(fill = Estimate),  size= 0.05, show.legend = TRUE) + 
  scale_fill_gradientn(colours = viridis(10, option = 'E') ) + 
  ggtitle("Range size ~ Body size slope")


## -----------------------------------------------------------------------------

cellSizes = seq(from = 700, to = 1500, length.out = 5)

FUN = function(g) {
  con = rmap_connect()
  rmap_add_ranges(con, x = wrens, ID = 'sci_name')
  rmap_prepare(con, 'hex', cellsize=g, chunksize = 1, verbose = FALSE)
  rmap_add_bio(con, wrens, 'sci_name')
  rmap_save_map(con)
  rmap_save_map(con, fun = 'median', src='wrens', v = 'male_tarsus', dst='median_male_tarsus')
  m = rmap_to_sf(con)

  # lm at assemblage level
  o = lm(scale(log(median_male_tarsus)) ~ sqrt(species_richness), m)  %>% 
        summary %>% coefficients %>% data.frame %>% .[-1, ]

  o$cell_size = g
  o

  }


o = lapply(cellSizes, FUN)   %>% rbindlist


## ---- , fig.height = 4--------------------------------------------------------

ggplot(o, aes(x = cell_size, y = Estimate)) +
    geom_point() +
    theme_bw()


## -----------------------------------------------------------------------------
quant = seq(0.05, 1, 0.1) 
Q = quantile(wrens$breeding_range_area,  probs = quant)
range_classes = data.frame(area = Q, quant =  quant )
W = 4   # size of the moving window
maxn = nrow(range_classes) - W

range_classes


## -----------------------------------------------------------------------------

subsets = paste0('area_subset_',1:maxn )

for(i in 1:maxn ) {
  rmap_save_subset(con, subsets[i], 
    wrens = glue("breeding_range_area BETWEEN 
            {range_classes[i,     'area']} AND 
            {range_classes[i+W, 'area']  }") )
  }


## -----------------------------------------------------------------------------

maps = paste0('body_size_', subsets)

for(i in 1:maxn ) {
  rmap_save_map(con, subset = subsets[i], dst = maps[i],  
                fun = 'median', src='wrens', v = 'male_tarsus')
    }


## -----------------------------------------------------------------------------

m = rmap_to_sf(con, pattern = 'richness|body')  %>% setDT
m = melt(m, measure.vars = patterns("median") )

x = m[, { 
      
      fm = lm( log10(value) ~  sqrt(species_richness) )
      data.table( Estimate = coefficients(fm)[2], t( confint(fm)[2, ]) )
      
      } , by = variable]

x[, Quantiles :=  quant[1:maxn]  ]


## ---- fig.height = 4----------------------------------------------------------
ggplot(x, aes(x = Quantiles, y = Estimate) ) +
    geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`), width= 0) +
    geom_line() +
    geom_point() +
    theme_bw()


