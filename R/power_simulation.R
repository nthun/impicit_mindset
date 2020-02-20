library(tidyverse)
library(faux) ## devtools::install_github("debruine/faux")
library(truncnorm)
library(broom)

theme_set(theme_light())

# Function to generate dataset ------------------------------------------------------

# This dataset has a Likert scale explicit measure and a go-nogo based d score. The two measures are correlated by a known value.
generate_dataset <- function(n = NULL,
                             explicit_max,
                             explicit_mean,
                             explicit_sd,
                             implicit_mean,
                             implicit_sd,
                             behavioral_odds,
                             implicit_explicit_r,
                             implicit_behavioral_r){
  
tibble(explicit = truncnorm::rtruncnorm(n = n, 
                                        a = 1, 
                                        b = explicit_max, 
                                        mean = explicit_mean,
                                        sd = explicit_sd),
       implicit = faux::rnorm_pre(explicit, 
                                  mu = implicit_mean, 
                                  sd = implicit_sd, 
                                  r = implicit_explicit_r),
       behavioral = faux::rnorm_pre(explicit, 
                                    mu = 0, 
                                    sd = implicit_sd, 
                                    r = implicit_behavioral_r) %>% 
                    faux::norm2binom(x = ., 
                                     size = 1, # To make bernoulli distribution
                                     prob = behavioral_odds))
}


# Define parameters -----------------------------------------------------------------

samples = 10000 # Number of samples for each n
sample_increment = 30 # The increment of cell sizes for the simulation
max_n = 300 # The maximum cell size
explicit_max = 6 # The maximum value of the explicit measure (minimum is set to 1)
explicit_mean = 4.24 # The mean of the explicit measure
explicit_sd = .72 # The sd of the explicit measure
implicit_mean = 0 # The mean of the implicit measure
implicit_sd = .285 # The sd of the implicit measure
behavioral_odds = 3/4 # The success ratio
implicit_explicit_r = .36 # Correlation between implicit and the explicit measure
implicit_behavioral_r = .27 # Correlation between implicit and the behavioral measure
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
  pmap(~generate_dataset(n = ..1,
                         explicit_max = explicit_max,
                         explicit_mean = explicit_mean,
                         explicit_sd = explicit_sd,
                         implicit_mean = implicit_mean,
                         implicit_sd = implicit_sd,
                         behavioral_odds = behavioral_odds,
                         implicit_explicit_r = implicit_explicit_r,
                         implicit_behavioral_r = implicit_behavioral_r))

# Calculate correlation for each sample
simulation_result_explicit <- 
  simulation_data %>% 
  map(., ~cor.test(.x$explicit, .x$implicit, 
                   method = "spearman", 
                   alternative = "greater") %>% 
          broom::tidy()) %>% 
  bind_rows() %>% 
  bind_cols(power_matrix, .)

# Calculate correlation for each sample
simulation_result_behavioral <- 
  simulation_data %>% 
  map(., ~cor.test(.x$explicit, .x$behavioral, 
                   method = "spearman", 
                   alternative = "greater") %>% 
        broom::tidy()) %>% 
  bind_rows() %>% 
  bind_cols(power_matrix, .)

# Count the proportion of significant correlations
result_summary_explicit <-
  simulation_result_explicit %>% 
  group_by(cell_size) %>% 
  summarise(avg_r = mean(estimate),
            power = mean(p.value < significance)) %>% 
  mutate(correlate = "explicit")

result_summary_behavioral <-
  simulation_result_behavioral %>% 
  group_by(cell_size) %>% 
  summarise(avg_r = mean(estimate),
            power = mean(p.value < significance)) %>% 
  mutate(correlate = "behavioral")

result_summary <-
  bind_rows(result_summary_explicit, result_summary_behavioral)  

# Visualize
result_summary %>% 
  ggplot() +
  aes(x = cell_size, y = power, color = correlate) +
  geom_hline(yintercept = c(.8, .95), linetype = "dashed", color = "red") +
  geom_point(size = 2) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Statistical power as function of sample size for explicit and behavioral correlates of the implicit measure",
       x = "Sample size")

# Intrapolate between the numbers to calculate the exact sample size
crossing(cell_size = 1:max_n,
         correlate = c("explicit", "behavioral")) %>% 
  left_join(select(result_summary, -avg_r), by = c("cell_size","correlate")) %>% 
  group_by(correlate) %>% 
  mutate(power = stats::approx(x = cell_size,
                               y = power,
                               xout = cell_size)$y) %>% 
  mutate(power_80 = power >= .8,
         power_90 = power >= .9,
         power_95 = power >= .95,
         power_99 = power >= .99) %>% 
  gather(power, value, -cell_size, -power, -correlate, convert = TRUE) %>% 
  filter(value) %>% 
  group_by(correlate, power) %>% 
  summarise(required_cell_size = first(cell_size)) %>% 
  ungroup()
   

