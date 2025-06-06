# Comparison of predicted values from full posterior vs. other approaches
# Author: Jake Fisher

# Set up workspace
rm(list = ls())
setwd("C:/Users/jakef/Google Drive/Papers/Social space diffusion")
library(tidyverse)
library(data.table)
library(dtplyr)
library(magrittr)
library(forcats)

# Load the data
data.loc <- "D:/Duke backup/DissertationShare/social_space_diffusion_r_and_r/"
load(paste0(data.loc, "all_mses.Rdata"))
# load("full_post_mses_combined.Rdata")  # later, to save memory

# Combine the relevant data
# In data.table to reduce memory use
mses[, c("prediction.number", "simulated") := NULL]
gc()
mses %<>%
#   select(-prediction.number, -simulated) %>% 
  filter(!grepl(model.type, pattern = "fit")) 
gc()

load(paste0(data.loc, "full_post_mses_combined.Rdata"))
full.post.mses %<>% 
  mutate(iteration = iteration - 1) %>% 
  filter(wave - pred.from.wave == 1) %>% 
  select(school, cohort, wave = pred.from.wave, attitude = varname, iteration,
         model.type, diag.weights = sw, next.wave.mse = mse)

both <- bind_rows(mses, full.post.mses) %>% 
  group_by(school, cohort, wave, attitude, iteration, diag.weights) %>% 
  mutate(diff.from.fit2d = next.wave.mse - next.wave.mse[which(model.type == "fit2d")]) %>% 
  filter(model.type %in% c("fit2d", "locf", "obs.network", "density_weight_0.2",
                           "density_weight_0.5", "density_weight_0.7"))

# Base plot
p <- both %>% 
  filter(iteration == 1, diag.weights == 1) %>% 
  ggplot(aes(x = wave, group = model.type, color = model.type)) + 
  scale_color_brewer(palette = "Paired") + 
  facet_wrap(~ attitude) + 
  theme_bw()

pr <- p +   # pointrange version
  stat_summary(fun.data = mean_se, position = position_dodge(.1),
               fatten = .25)

# Plot the comparison of the mean fit2d vs. other approaches
pr + aes(y = next.wave.mse)

# Difference from fit2d
pr + aes(y = diff.from.fit2d)
  
# Frequency with which other approaches do worse
p %+% {
  both %>% 
    mutate(fit2d.better = diff.from.fit2d > 0) %>% 
    group_by(wave, attitude, iteration, diag.weights, model.type) %>% 
    summarize(pct.fit2d.better = mean(fit2d.better, na.rm = T)) %>% 
    filter(iteration == 1, diag.weights == 1, 
           model.type %in% c("locf", "obs.network", "density_weight_0.2",
                             "density_weight_0.5", "density_weight_0.7")) 
} + 
  aes(y = pct.fit2d.better) + 
  geom_line() +
  geom_hline(yintercept = .5, lty = 2, color = "black")

# Plots for article
duke.colors <- c(
  brown = "#988675",
  secondary.blue.3 = "#0680CD",
  secondary.blue.2 = "#235F9C",
  secondary.blue.1 = "#0736A4",
  red = "#CC3300",
  orange = "#F09905"
  # purple = "#993399"
  # duke.blue = "#001A57" 
  # black = "#262626"
  )

# Clean the data for plotting
plot.df <- both %>% 
  filter(iteration == 1,  
          # $!*%^^$% floating point errors
         diag.weights %in% c(.5, .7, 1) | abs(diag.weights - .7) < .001,
         # diag.weights %in% c(.2, .5, .7, 1) | abs(diag.weights - .7) < .001,
         attitude %in% c("cattus", "cavgdev", "cexpt", "csumscab"),
         # attitude %in% c("csuatt_r", "cavgdev", "csumscab"),
         # In a number of cases, the 2-d model fit failed, meaning we can't do
         # comparisons.  Let's drop those cases.
         !is.nan(next.wave.mse), !is.na(diff.from.fit2d)
         ) %>% 
  tbl_df %>% 
  mutate(
    attitude = fct_relevel(attitude, "cattus", "cexpt", "csumscab", "cavgdev"),
    attitude = fct_recode(
      attitude,
      "Attitudes towards substance use" = "cattus",
      "Expectations about substance use" = "cexpt",
      "School adjustment and bonding" = "csumscab",
      "Deviance" = "cavgdev"
      ),
    model.type = fct_relevel(model.type, "fit2d"),
    pct.better = diff.from.fit2d / next.wave.mse)

pp <- ggplot(plot.df, aes(x = wave, group = model.type, color = model.type)) + 
  scale_color_manual(
    "",
    values = setNames(duke.colors, nm = NULL),
    limits = c("fit2d", "density_weight_0.2", "density_weight_0.5",
               "density_weight_0.7", "locf", "obs.network"),
    labels = c("Latent space model\n(predicted probabilities)",
               expression(paste("Density smoother (", delta, " = 0.2)")),
               expression(paste("Density smoother (", delta, " = 0.5)")),
               expression(paste("Density smoother (", delta, " = 0.7)")),
               "Class average",
               "Observed network")
    ) +
  facet_grid(diag.weights ~ attitude, 
             labeller = label_bquote(rows = alpha == .(diag.weights))) + 
  theme_bw() + 
  theme(legend.position = "bottom", legend.text.align = 0) + 
  xlab("Survey wave") +
  stat_summary(fun.data = mean_cl_normal, position = position_dodge(.65),
  # stat_summary(fun.data = mean_se, position = position_dodge(.65),
               fatten = .1) 

(fig4 <- pp + 
  aes(y = next.wave.mse) +
  labs(
    title = "Figure 4: Mean squared error of next survey wave predictions",
    y = "MSE of prediction of attitudes in the subsequent wave"
    ))
ggsave("figure_4.pdf", plot = fig4, width = 10, height = 7.5, units = "in",
       paper = "USr")

# Percentage improvement:
(fig5 <- pp + 
  aes(y = pct.better) +
  scale_y_continuous(labels = scales::percent) + 
  geom_hline(yintercept = 0, lty = 3) + 
  labs(
    title = "Figure 5: Percentage improvement in MSE",
    y = "% difference from latent space model MSE"
  ))

ggsave("figure_5.pdf", plot = fig5, width = 10, height = 7.5, units = "in", 
       paper = "USr")
