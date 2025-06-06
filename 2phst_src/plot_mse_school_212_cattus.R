# Plot MSE for school 212 for cattus, specifically.
# Author: Jake Fisher

# Set up workspace
rm(list = ls())
setwd("C:/Users/jakef/Google Drive/Papers/Social space diffusion")
library(data.table)
library(tidyverse)
library(dtplyr)

# Load the MSE data
load("D:/Duke backup/DissertationShare/social_space_diffusion_r_and_r/all_mses.Rdata")

school.212 <- mses %>%
  filter(school == 212, cohort == 1, wave == 1, attitude == "cattus")

sch.212.all <- filter(mses, school == 212, cohort == 1)
rm(mses)
  
duke.blue <- "#001A57"
duke.background <- "#0680CD"
duke.gray <- "#666666"
duke.lightgray <- "#E8E5E2"  #"#E5E5E5"
white <- "#FFFFFF"
duke.brown <- "#988675"
duke.brown2 <- "#7B4500"
duke.lightyellow <- "#FFD960"
duke.lightgreen <- "#BED1A1"
duke.graybrown <- "#DAD0C6"
duke.purple <- "#4D005E"
duke.lightblue <- "#CFEDED"
duke.red <- "#CC3300"
duke.yellow <- "#F09905"
duke.green <- "#A1B70D"
duke.orange <- "#F09905"

# Original plot
# IMPORTANT NOTE: this uses a non-standard version of ggplot2.
# There is a bug in vanilla ggplot2, where stat_summary(geom = "smooth") does
# not plot both lines and fill colors.  See bug report:
# https://stackoverflow.com/questions/43433783/why-doesnt-the-fill-scale-show-up-in-a-ggplot-legend?noredirect=1#comment73925299_43433783
# https://github.com/tidyverse/ggplot2/issues/1546
# However, this seems to be solved by an OR statement identified in this pull
# request:
# https://github.com/tidyverse/ggplot2/pull/1978
# So, as a workaround, I downloaded ggplot2, added the OR statement, rebuilt the
# package, and used that.  Hopefully they will fix this soon, so that it will
# be fixed in a future release of ggplot2, and this will no longer be a problem.
plot.df <- school.212 %>% 
  filter(diag.weights %in% c(.2, .5, .7, 1) | abs(diag.weights - .7) < .001,
         grepl(x = model.type, pattern = "fit2d|obs|locf")) %>% 
  mutate(type = paste(model.type, simulated, sep = "."),
         type = forcats::fct_recode(
           type,
           "Predicted probabilities" = "fit2d.FALSE",
           "Posterior predictive" = "fit2d.TRUE",
           "Observed network" = "obs.network.NA",
           "Class average" = "locf.NA"
           ),
         iteration = iteration + 1) 

# Recreate the original plot from the paper
orig <- plot.df %>% 
  filter(abs(diag.weights - .7) < .001,
         grepl(x = model.type, pattern = "fit2d|obs")) %>% 
  ggplot(aes(x = iteration, y = next.wave.mse, group = type,
             fill = type, alpha = type, lty = type)) + 
  stat_summary(fun.data = median_hilow, geom = "smooth", color = duke.blue,
               size = 1.1) +
  scale_linetype_manual(values = 3:1, name = "Network type") + 
  scale_alpha_manual(values = c(.9, .7, 0), name = "Network type") + 
  scale_fill_manual(values = c(duke.brown, duke.lightblue, white),
                    name = "Network type") +
  labs(
    x = "Number of iterations",
    y = "Mean squared error of simulations versus\nactual attitudes at wave 2"
    ) +
  ylim(c(0, .75)) + 
  theme_bw() 

fig3 <- orig + 
  theme_classic() + 
  theme(legend.position = c(.75, .25),
        legend.background = element_rect(color = "black")) + 
  labs(title = "Figure 3: Mean square error of predictions, School 212") 

ggsave(fig3, file = "figure_3.pdf", height = 7, width = 7, units = "in", 
       paper = "USr")

# Faceted by diag.weight
orig %+%
  plot.df + 
  facet_grid(diag.weights ~ .)

# Include alternative smoothing mechanisms
ggplot(plot.df, aes(x = iteration, y = next.wave.mse, group = type,
             fill = type, alpha = type, lty = type, color = type)) + 
  stat_summary(fun.data = median_hilow, geom = "smooth", size = 1.1) +
  scale_linetype_manual(values = c(3:1, 1), name = "Network type") +
  scale_alpha_manual(values = c(.9, .7, 0, 0), name = "Network type") +
  scale_fill_manual(values = c(duke.brown, duke.lightblue, white, white),
                    name = "Network type") +
  scale_color_manual(values = c(duke.blue, duke.blue, duke.red, duke.orange),
                     name = "Network type") + 
  labs(
    x = "Number of iterations",
    y = "Mean squared error of simulations versus\nactual attitudes at wave 2"
  ) +
  theme_bw() + 
  facet_grid(diag.weights ~ .)


# Save the summaries for the latent space model
ls.summaries <- school.212 %>%
  filter(grepl(x = model.type, pattern = "fit2d")) %>%
  group_by(iteration, simulated) %>%
  # Added SE to see if that would make the rectangles fill in geom_smooth...
  do(data.frame(median_hilow(.$next.wave.mse), se = sd(.$next.wave.mse))) 

# Plot the alternative models with facets
pdf("school_212_diffusion_cattus_compared_with_alt_models.pdf", 
    paper = "letter")
school.212 %>%
  filter(grepl(x = model.type, pattern = "obs|0.2|0.5|.7|locf")) %>%
  mutate(
    is.density = grepl(x = model.type, pattern ="density_weight"),
    density.weight = if_else(is.density, parse_number(model.type), NA_real_),
    model.type.clean = factor(
      if_else(is.density, "density_weight", model.type),
      levels = c("obs.network", "locf", "density_weight"),
      labels = c("Observed network", "Mean", "Density")
    )
  ) %>%
  select(-is.density) %>%
  
  ggplot(aes(x = iteration)) + 
  facet_grid(model.type.clean ~ .) +
  
  # geom_smooth has a bug (cf. http://stackoverflow.com/questions/43433783/why-doesnt-the-fill-scale-show-up-in-a-ggplot-legend?noredirect=1#comment73925299_43433783
  # so use a hack of geom_ribbon() and geom_line() instead)
  geom_smooth(aes(y = y, ymin = ymin, ymax = ymax, fill = simulated),
              stat = "identity", lty = 1, color = duke.gray,
              data = ls.summaries) +
  
  # geom_line(data = ls.summaries, aes(y = y, group = simulated),
  #           color = duke.gray, lty = 1, show.legend = T) +
  # geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = simulated), alpha = 0.5,
  #             data = ls.summaries) +

  geom_line(aes(y = next.wave.mse, lty = factor(density.weight)),
            color = duke.blue) + 
  
  scale_fill_manual("Social space diffusion\napproach",
                    values = c(duke.brown, duke.lightblue),
                    labels = c("Predicted probabilities", 
                               "Posterior predictive"),
                    guide = guide_legend(fill = "black")) + 
  
  scale_linetype_manual(values = 4:2, na.value = 1) + 
  # scale_color_manual(values = duke.blue) + 
  
  labs(
    x = "Number of iterations",
    y = "Mean squared error of simulations versus\nactual attitudes at wave 2",
    title = "Figure 3: School 212 MSE's compared across many models"
    ) + 
  theme_bw() 
dev.off()
school.212 %>%
  filter(grepl(x = model.type, pattern = "fit2d|obs|0.2|0.5|.7|locf")) %>%
  mutate(model.type.adj = factor(
    if_else(model.type == "fit2d", paste(model.type, simulated, sep = ".sim."),
            model.type),
    levels = c("fit2d.sim.FALSE", "fit2d.sim.TRUE", "obs.network",
               "density_weight_0.2", "density_weight_0.5", "density_weight_0.7",
               "locf"),
    labels = c("Predicted probabilities", "Posterior predictive", 
               "Observed network", "Density (0.2)", "Density (0.5)",
               "Density (0.7)", "Mean")
    )) %>%
  ggplot(aes(x = iteration, y = next.wave.mse, group = model.type.adj,
             fill = model.type.adj, lty = model.type.adj,
             color = model.type.adj)) +
  stat_summary(fun.data = "median_hilow", geom = "smooth", size = 1.1) + #,
               # color = duke.blue) +
  theme_classic() + 
  labs(
    x = "Number of iterations",
    y = "Mean squared error of simulations versus\nactual attitudes at wave 2"
    ) + 
  scale_fill_manual(name = "Network type",
                    values = c(duke.brown, duke.lightblue, rep(NA, 5))) + 
  scale_linetype_manual(name = "Network type", values = c(1, 1, 2, 3, 3, 3,
                                                          4)) + 
  scale_color_manual(name = "Network type",
                     values = c(duke.blue, duke.blue, duke.blue, duke.red,
                                duke.yellow, duke.background, duke.blue))
                    
                    ,
                    , labels = c("Predicted probabilities",
                                                      "Posterior predictive", 
                                                      "Observed network")) + 
  scale_linetype_manual(, 
                        labels = c("Predicted probabilities",
                                   "Posterior predictive",
                                   "Observed network")) +
  scale_alpha_manual(values = c(.9, .7, 0), name = "Network type",
                     labels = c("Predicted probabilities", 
                                "Posterior predictive", "Observed network")) + 
  
school.212 %>%
  filter(grepl(x = model.type, pattern = "fit2d|obs|0.2|0.5|.7|locf")) %>%
  mutate(model.type.adj = 
    if_else(model.type == "fit2d", paste(model.type, simulated, sep = ".sim."),
            model.type)) %>%
  select(model.type.adj) %>%
  unlist %>%
  unique %>%
  dput

school.212 %>%
  filter(grepl(x = model.type, pattern = "fit2d")) %>%
  group_by(iteration) %>%
  do(median_hilow(.$next.wave.mse)) %>%
  inner_join(y = filter(school.212, grepl(x = model.type, pattern = "obs")))
  
  median_hilow
  