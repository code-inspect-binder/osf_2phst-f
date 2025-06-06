# Plot the mean and standard deviation of the attitude scales used
# Author: Jake Fisher

# Set up workspace
rm(list = ls())
setwd("C:/Users/jakef/Google Drive/Papers/Social space diffusion")
library(tidyverse)
library(prosperHelper2)
library(forcats)

# Load the data
data("PROSPER.survey")

# Plot the mean and SD of the variables
p <- PROSPER.survey %>% 
  select(cattus, cexpt, csumscab, cavgdev, wave) %>% 
  gather(key = "varname", value = "value", -wave) %>%
  filter(!is.na(value)) %>% 
  mutate(varname = fct_relevel(varname, "cattus", "cexpt", "csumscab", "cavgdev"),
         varname = fct_recode(
           varname,
           "Attitudes towards substance use" = "cattus",
           "Expectations about substance use" = "cexpt",
           "School adjustment and bonding" = "csumscab",
           "Deviance" = "cavgdev"
         )) %>% 
  ggplot(aes(x = wave, y = value, group = interaction(wave, varname))) + 
  stat_summary(fun.data = "mean_sdl") + 
  facet_wrap(~ varname) + 
  theme_bw() + 
  labs(
    x = "Survey wave",
    y = "Scale mean (range gives 2 x SD)",
    title = "Appendix Figure 1: Mean and standard deviation of scales"
  )

ggsave(p, file = "appendix_mean_sd_scales.pdf", paper = "USr", width = 10,
       height = 7.5, units = "in")
