
# Title: Examining bef relationships at local and regional scales using a rock pool model system

# load relevant libraries
library(tidyverse)
library(here)
library(broom)
library(corrplot)
library(viridis)
library(vegan)

# make a folder to export figures or tables
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# load the data
gam_dat <- read_csv(here("data/metacomm_gamma_richness.csv"))

# check variable structure
str(gam_dat)

# check summary statistics
summary(gam_dat)

# check variable distributions
gam_dat %>%
  gather(Average_dist_other_mountains_km,
         Cluster_size,
         Tot_Rich_kwant_PAS,
         key = "var", value = "val") %>%
  ggplot(data = .,
         mapping = aes(x = val)) +
  geom_histogram() +
  facet_wrap(~var, scales = "free") +
  theme_classic()

gam_dat %>%
  filter(Cluster_size < 10)

# remove the rocks with cluster size of less than 10 because that's how many pools were sampled
gam_dat <-
  gam_dat %>%
  filter(Cluster_size >= 10)

# check summary variables
summary(gam_dat)


### plot the relationship between cluster size and richness

# set-up a list of different models to fit to the curve
mod_list <- 
  list(mod1 = c("Cluster_size"),
       mod2 = c("Cluster_size", "I(Cluster_size^2)")
       )

# set-up a range of predictor variable values
preds <- 
  tibble(Cluster_size = seq(min(gam_dat$Cluster_size), 
                            max(gam_dat$Cluster_size), 
                            by = 1),
         Average_dist_other_mountains_km = seq(min(gam_dat$Average_dist_other_mountains_km), 
                                               max(gam_dat$Average_dist_other_mountains_km), 
                                               by = 0.05) %>%
           sample(., size = (max(gam_dat$Cluster_size) - min(gam_dat$Cluster_size) + 1)))


mod_preds <- 
  
  lapply(mod_list, function(x) {
  
  x <- 
    lm(reformulate(x, response =  c("Tot_Rich_kwant_PAS")), data = gam_dat) %>%
    predict(., preds)
  
  preds %>%
    mutate(fitted = x)
  
})

mod_preds <- 
  mod_preds %>%
  bind_rows(.id = "model")

p_g <- 
  ggplot() +
  geom_point(data = gam_dat,
             mapping = aes(x = Cluster_size, y = Tot_Rich_kwant_PAS, size = Average_dist_other_mountains_km),
             alpha = 0.5, colour = "black") +
  geom_line(data = mod_preds,
            mapping = aes(x = Cluster_size, y = fitted, colour = model),
            size = 1) +
  scale_colour_viridis_d() +
  theme_classic() +
  ylab("gamma diversity") +
  xlab("number of pools") +
  theme(legend.position = "none")
  
ggsave(filename = here("figures/intro_sem_y.png"), plot = p_g,
         width = 12, height = 12, units = "cm", dpi = 300)







