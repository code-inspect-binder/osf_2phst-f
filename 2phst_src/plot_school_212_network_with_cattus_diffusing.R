# Figure 1: Plot of networks under different diffusion mechanisms
# Author: Jake Fisher

# Set up workspace
rm(list = ls())
set.seed(919)
setwd("C:/Users/jakef/Google Drive/Papers/Social space diffusion")
library(latentnetDiffusion)
library(prosperHelper2)
library(ggnetwork)
library(dplyr)
library(dtplyr)
library(tidyr)

meanImputation <- function(x) {
  # Replaces missing values with the mean value
  # 
  # Args:
  #   x: a numeric vector
  #
  # Returns:
  #   x, with missing values replaced by the mean
  return(replace(x, is.na(x), mean(x, na.rm = T)))
}

clean_ggnet <- function(ggnet) {
  # Converts a ggnetwork data.frame into a tbl_df and renames the id variable
  #
  # Args:
  #   ggnet: output from ggnetwork
  #
  # Returns:
  #   tbl_df, with vertex.names converted into a character variable
  lapply(ggnet, unname) %>%
    as.data.frame %>%
    tbl_df %>%
    mutate(id = as.character(vertex.names)) %>%
    select(-vertex.names) %>%
    return
}

# Load the relevant data
load("ergmm_212.Rdata")
predict.212.2d <- latentnet:::predict.ergmm(ergmm.212.2d)


# Pull cattus variable from the main PROSPER dataset
data(PROSPER.survey)
cattus <- getPROSPERVariableByNetwork(school.212, "cattus") %>%
  unlist(use.names = F) %>%  # converts from 1 col. data.table to vector
  meanImputation %>%
  as.matrix  # degroot fails unless "Y" is a matrix

# Get ID values for school.212 to use with tidyDegroot
sid <- network.vertex.names(school.212)

# Calculate diffusion predictions
Y <- degroot(school.212, cattus, 5, all.iter = T) %>%
  tidyDegroot(id = sid)

Y.pred <- degroot(predict.212.2d, cattus, 5, all.iter = T) %>%
  tidyDegroot(id = sid)

sim <- simulate(ergmm.212.2d, 5)$networks
Y.sims <- degrootList(sim, cattus, all.iter = T) %>%
  tidyDegroot(id = sid)  # TODO(jcf26@): why isn't this tidyDegrootList?

# Get base layout.  All of the "unname" stuff is from a bug in ggnetwork -- it
# produces a data.frame where the columns have unused attributes, which make it
# not play nicely with dplyr
ag <- .01  # arrow.gap parameter
main.ggnet <- ggnetwork(school.212, layout = ergmm.212.2d$mcmc.mle$Z,
                   arrow.gap = ag) %>%
  clean_ggnet

# Get layouts for each of the simulated networks
# Also convert ID values, which were provided before by tidyDegroot, to the 
# PROSPER ID values
sims.ggnet <- append(sim, list(school.212), after = 0) %>%
  lapply(ggnetwork, layout = ergmm.212.2d$mcmc.mle$Z,
                     arrow.gap = ag) %>%
  lapply(FUN = clean_ggnet) %>%
  lapply(FUN = function(x) mutate(x, id = if_else(as.numeric(id) > 1000, id,
                                                  sid[as.numeric(id)])))

# Join the layouts to each of the diffusion predictions.  Easier just to do this
# separately, because you have to do the Y.sims join separately anyway
obs <- split(Y, f = Y$iteration) %>%
  lapply(right_join, y = main.ggnet, by = "id") %>%
  bind_rows

pred.prob <- split(Y.pred, f = Y.pred$iteration) %>%
  lapply(right_join, y = main.ggnet, by = "id") %>%
  bind_rows

pred.dist <- split(Y.sims, f = Y.sims$iteration) %>%
  Map(right_join, x = ., y = sims.ggnet, by = "id") %>%
  bind_rows

# Put all of the diffusion predictions in a list, and combine them with the
# network dataset so that you can plot everything
diffusion.predictions <- list(
  `(a)\nObserved network` = obs,
  `(b)\nPredicted probabilities` = pred.prob,
  `(c)\nPredictive distribution` = pred.dist
) %>%
  bind_rows(.id = "network.type") 

# Plotting with ggnetwork
iteration.prefix <- as_labeller(
  function(string) if_else(string == "1", "Observed values", 
                           paste0("t = ", as.numeric(string) - 1))
)

pdf("figure_1_gray.pdf", height = 10, width = 7.5, paper = "letter")

ggplot(diffusion.predictions, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(1, "pt"), type = "closed", angle = 15),
             color = "lightgray") + 
  geom_nodes(aes(color = pred.value), size = 1) + 
  scale_color_distiller(
    "Predicted value of \"How wrong do you think it is to smoke?\"",
    # palette = "YlGnBu",
    palette = "Greys",
    direction = 1
    ) + 
  facet_grid(iteration ~ network.type, switch = "y",
             labeller = labeller(iteration = iteration.prefix)) + 
  theme_facet() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = .5),
    # Reset axis text to default
    axis.title.y = element_text(family = "", face = "plain", colour = "black",
                                size = 12, hjust = 0.5, vjust = 0.5,
                                angle = 90, lineheight = 0.9, debug = F)
    ) +
  ggtitle("Figure 1: Visualization of weighted averaging diffusion processes") + 
  ylab("Iteration")

dev.off()
