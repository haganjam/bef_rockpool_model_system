
# Title: Examining bef relationships at local and regional scales using a rock pool model system

# load relevant libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(here)

# load the regional species composition data
reg.dat <- read_csv(here("data/reg_spp_site_dat.csv") )
View(reg.dat)

# load the environmental data
env.dat <- read_csv(here("data/env_data.csv") )
View(env.dat)

# examine the reg.dat
glimpse(reg.dat)

# Caeculidae
reg.dat$Caeculidae[1] <- 0
reg.dat$Caeculidae <- as.numeric(reg.dat$Caeculidae )

# rename the Species column as site
reg.dat <- 
  reg.dat %>%
  rename(site = Species)

# subset the site column
reg.site <- 
  reg.dat %>%
  select(site)

# get a species by site matrix
sp.site <- 
  reg.dat %>%
  select(-site)

# calculate diversity indices and add to reg.site

# calculate species richness
reg.site$species_richness <- apply(sp.site, MARGIN = 1, function(x){
  
  y <- ifelse(x > 0, 1, 0)
  
  sum(y)
  }
  )

# calculate shannon diversity (effective number of species equivalent)
reg.site$shannon_ENS <- exp(vegan::diversity(x = sp.site, index = "shannon"))


# examine the environmental data
glimpse(env.dat)

# rename certain columns
env.dat <- 
  env.dat %>%
  rename(site = Species,
         cluster_size = `Cluster size`,
         rocks_10_km = `Rocks 10km`,
         elevation_ggearth = `Elevation (ggearth)`)

names(env.dat)


# join the environmental data to the community data
any(sort(reg.site$site) != sort(env.dat$site))
reg.env <- full_join(reg.site, env.dat, by = "site")


# check cluster size range
range(reg.env$cluster_size)
nrow(reg.env[reg.env$cluster_size < 12, ])

# remove the six sites with fewer than 12 pools
reg.env <- 
  reg.env %>%
  filter(cluster_size > 12)

# plot the data
names(reg.env)

ggplot(data = reg.env,
       mapping = aes(x = cluster_size, y = species_richness)) +
  geom_point()

ggplot(data = reg.env,
       mapping = aes(x = Avgdistothermountains, y = species_richness)) +
  geom_point()






