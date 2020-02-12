library(tidyverse)
library(faux) ## devtools::install_github("debruine/faux")
library(broom)

theme_set(theme_light())

# Function to generate dataset ------------------------------------------------------

# This dataset has a Likert scale explicit measure and a go-nogo based d score. The two measures are correlated by a known value.
generate_dataset <- function(explicit_levels = 6,
                             n = 100,
                             explicit_mean = 4.24,
                             explicit_sd = .72,
                             implicit_mean = 0,
                             implicit_sd = .33,
                             r = .4){
  
tibble(explicit = sample.int(explicit_levels, 
                             size = n, 
                             replace = TRUE, 
                             prob = dnorm(1:explicit_levels, 
                                          mean = explicit_mean, 
                                          sd = explicit_sd)),
       implicit = rnorm_pre(explicit, 
                            mu = implicit_mean, 
                            sd = implicit_sd, r = r))
}


# Define parameters -----------------------------------------------------------------

samples = 1000 # Number of samples for each n
sample_increment = 10 # The increment of cell sizes for the simulation
max_n = 120 # The maximum cell size
explicit_levels = 6 # The numner of levels the explicit measure has
explicit_mean = 4.24 # The mean of the explicit measure
explicit_sd = .72 # The sd of the explicit measure
implicit_mean = 0 # The mean of the explicit measure
implicit_sd = .33 # The sd of the explicit measure
r = .4 # Correlation between explicit and implicit measures
significance = .05 # The level of significance

# Simulate samples ------------------------------------------------------------------

# Generate a design matrix that defines the samples that will be simulated
power_matrix <-
  crossing(cell_size = seq.int(sample_increment, max_n, sample_increment),
           sample = 1:samples) 

# Generate data (may take a while)
simulation_data <-
  power_matrix %>% 
  as.list() %>% 
  pmap(~generate_dataset(n = ..1))

# Calculate correlation for each sample
simulation_result <- 
  simulation_data %>% 
  map(., ~cor.test(.x$explicit, .x$implicit, method = "spearman") %>% 
        broom::tidy()) %>% 
  bind_rows() %>% 
  bind_cols(power_matrix, .)

# Count the proportion of significant correlations
result_summary <-
  simulation_result %>% 
  group_by(cell_size) %>% 
  summarise(avg_r = mean(estimate),
            power = mean(p.value < significance))

# Visualize
result_summary %>% 
  ggplot() +
  aes(x = cell_size, y = power) +
  geom_hline(yintercept = c(.8, .95), linetype = "dashed", color = "red") +
  geom_point() +
  geom_line()

# Intrapolate between the numbers to calculate the exact sample size
tibble(cell_size = 1:max_n) %>% 
  left_join(select(result_summary, -avg_r), by = "cell_size") %>% 
  mutate(power = stats::approx(x = cell_size,
                               y = power,
                               xout = cell_size)$y) %>% 
  mutate(power_80 = power >= .8,
         power_95 = power >= .95) %>% 
  gather(power, value, -cell_size, -power, convert = TRUE) %>% 
  filter(value) %>% 
  group_by(power) %>% 
  summarise(required_cell_size = first(cell_size))
   

