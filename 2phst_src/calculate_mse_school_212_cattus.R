# Plot MSE values for school 212
# Author: Jake Fisher

# Set up workspace
rm(list = ls())
setwd("C:/Users/jakef/Google Drive/Papers/Social space diffusion")
library(tidyverse)
library(statnet)
library(latentnet)
library(latentnetDiffusion)
library(prosperHelper2)
library(magrittr)r

meanImputation <- function(x) {
  # Replaces missing values of X with the mean of X
  #
  # Args:
  #   x: a numeric vector, not enforced
  #
  # Returns:
  #   a numeric vector of length x
  replace(x, is.na(x), mean(x, na.rm = T))
}

#
load("D:/Duke Backup/DissertationShare/social_space_diffusion_r_and_r/all_mses.Rdata")

# Load PROSPER data and calculate 2D latent space model
data(PROSPER.networks)
data(PROSPER.survey)

# Constants
first.wave <- 1
sch <- 212
ch <- 1

sch.212 <- subsetPROSPERNetworks(school = sch, cohort = ch, wave = first.wave)
Y1 <- getPROSPERVariableByNetwork(sch.212, "cattus") %>% 
  as.matrix %>% 
  set_colnames(., NULL) %>% 
  meanImputation(.)

set.seed(734)
fit.2d <- ergmm(sch.212 ~ euclidean(d = 2))

# Get predicted probabilities after each iteration
set.seed(48106)
ls.pred <- ergmmDegroot(fit.2d, Y1, draws = 100, all.iter = T) %>% 
  tidyDegrootList(id = as.integer(network.vertex.names(sch.212)))

ls.sim <- ergmmDegroot(fit.2d, Y1, draws = 100, simulate = T, all.iter = T) %>% 
  tidyDegrootList(id = as.integer(network.vertex.names(sch.212)))

net.pred <- degroot()

# Bring MSE values together
both <- ls.sim %>% 
  rename(sim.pred.value = pred.value) %>% 
  full_join(ls.pred)

# Now pull in the nth future wave
n <- first.wave + 1

plot.df <- PROSPER.survey %>% 
  filter(id %in% unique(ls.pred$id), school == sch, cohort == ch,
         wave == first.wave + 1) %>% 
         # wave > first.wave) %>%   # all future waves
  select(id, cohort, wave, cattus) %>% 
  tbl_df %>% 
  left_join(y = both) %>% 
  # filter(iteration %in% c(1, 2)) %>% 
  group_by(wave, iteration, draws) %>% 
  summarize(pred.mse = hydroGOF::mse(pred.value, cattus),
            sim.mse = hydroGOF::mse(sim.pred.value, cattus)) 

# Create named colors
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

# First, create a baseline plot
base.plot <- ggplot(plot.df, aes(x = iteration, y = change, linetype = type, 
                                 alpha = type, fill = type)) + 
  stat_summary(fun.data = "CI", geom = "smooth", color = duke.blue, 
               size = 1.1) + 
  scale_linetype_manual(values = 3:1, name = "Network type", 
                        labels = c("Predicted probabilities", 
                                   "Posterior predictive", 
                                   "Observed network")) + 
  scale_alpha_manual(values = c(.9, .7, 0), name = "Network type",
                     labels = c("Predicted probabilities", 
                                "Posterior predictive", "Observed network")) + 
  scale_fill_manual(values = c(duke.brown, duke.lightblue, white),
                    name = "Network type", labels = c("Predicted probabilities",
                                                      "Posterior predictive", 
                                                      "Observed network")) +
  xlab("Number of iterations") +
  theme_classic() + 
  theme(legend.position = c(.75, .8), 
        legend.background = element_rect(color = "black"))

# Now plot it with the figure 2 (change by iteration) options
pdf("figure_2.pdf", paper = "a4r")
base.plot + scale_y_log10() +
  ylab(expression(paste(log, bgroup("(",bgroup("||", A^{(t)} - A^{(t-1)}, "||"), ")")))) +
  ggtitle("Figure 2: Change in simulated attitudes after each iteration")
dev.off()

# And again with the MSE options
pdf("figure_4.pdf", paper = "a4r")
base.plot %+% subset(mse.df, iteration <= 6) + aes(y = mse) + ylim(0, .75) +
  theme(legend.position = c(.8, .25)) + 
  ggtitle("Figure 4: Mean squared error of predictions") + 
  ylab("Mean squared error of simulations versus\nactual attitudes at wave 2")
dev.off()




ggplot(mse, aes(x = iteration, y = pred.mse)) + 
  stat_summary(geom = "ribbon")

%>% 
  summarize(mean.mse = mean(mse)) %>% 
  ggplot(aes(y = mean.mse, x = wave, group = iteration, color = iteration)) + 
  geom_line() + 
  scale_color_distiller(palette = "YlGnBu", direction = -1)



sch.212 %<>% addPROSPERVertexAttribute(., "cattus")
