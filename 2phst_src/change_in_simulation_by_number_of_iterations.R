# Author: Jake Fisher
# See how the changes in the network over time vary with number of interations

rm(list = ls())
setwd("C:/Users/Jake/Documents/Dissertation")
library(dissertation1Helper)
library(latentnet)
library(statnet)
library(sfsmisc)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(ggplot2)
library(grid)

getChanges <- function(mat) {
  # A function to get the norm of the difference between each two successive
  # columns of a matrix
  # 
  # Args:
  #   mat: the matrix whose rows are to be compared
  #
  # Returns:
  #   A vector where each element is the norm of columns i and i + 1 of mat
  return(
    sapply(
        1:(ncol(mat) - 1),
        function(i) 
          norm(matrix(mat[, i] - mat[, (i + 1)]), ncol = 1)
        )
    )
}

CI <- function(x) {
  # Calculates 2-tailed 95% credible interval for ggplot
  #
  # Args:
  #   x: a numeric vector
  #
  # Returns:
  #   a named vector of .025, .5, and .975 quantiles (named ymin, y, and ymax)
  quantiles <- quantile(x, c(0.025, 0.5, 0.975))
  names(quantiles) <- c("ymin", "y", "ymax")
  return(quantiles)
}

# Load data
data(school.212)
data(ergmm.212)

# Save the diffusing variable
A.1 <- meanImputation(school.212[[1]] %v% "cattus1")

# Calculate the diffusion simulations
obs <- degroot(school.212[[1]][, ], A.1, 20, all.iter = T)
pred <- ergmmDegroot(ergmm.212[[1]], A.1, draws = 100, iterations = 20,
                     all.iter = T)
sims <- ergmmDegroot(ergmm.212[[1]], A.1, draws = 100, iterations = 20, 
                     simulate = T, all.iter = T)

# Put everything into a single data.frame for ggplot
pred.df <- ldply(pred, getChanges) %>%
  melt %>%
  transmute(type = "pred", iteration = as.numeric(variable), change = value)

sims.df <- ldply(sims, getChanges) %>%
  melt %>%
  transmute(type = "sims", iteration = as.numeric(variable), change = value)

obs.df <- data.frame(type = "obs", iteration = 1:20, change = getChanges(obs))

diff.df <- rbind(pred.df, sims.df, obs.df)
diff.df$type <- factor(diff.df$type, levels = c("pred", "sims", "obs"))


# Plot it

# colors <- rev(brewer.pal(3, "Greys"))
# WU.colors <- c("#D3BC8D", "#000000", "#C8102E", "#006747")
#names(WU.colors) <- c("background", "black", "red", "green")
duke.blue <- "#001A57"
duke.background <- "#0680CD"
duke.gray <- "#666666"

base.plot <- ggplot(diff.df, aes(x = iteration, y = change, linetype = type)) + 
  scale_y_log10() + 
  stat_summary(fun.data = "CI", geom = "smooth", color = duke.blue,
               fill = duke.gray, alpha = .25, size = 1.1) +
  scale_linetype_manual(values = 3:1, name = "Network type",
                        labels = c("Predicted probabilities",
                                   "Posterior predictive", "Observed network")) + 
  labs(x = "Number of iterations",
       y = expression(paste(log, bgroup("(",bgroup("||", A^{(t)} - A^{(t-1)}, "||"), ")")))) +
  ggtitle("Appendix Figure 2: Change in simulated attitudes after each iteration") +
  theme_classic() + 
  theme(legend.position = c(.75, .8), 
        legend.background = element_rect(color = "black"))
  
base.plot
base.plot + 
  


  theme_classic()

  



# Plot it all with ggplot
colors <- c("#001A57", "#E8E5E2", "#CC3300", "#666666")
ggplot(diff.df, aes(x = iteration, y = diff.obs)) + 
  geom_line() + 
 # theme_bw() + 
#  scale_y_log10() +
  geom_crossbar(aes(ymin = diff.pred.025, y = diff.pred.5,
                    ymax = diff.pred.97.5))
  
  geom_ribbon(aes(ymin = diff.pred.025, ymax = diff.pred.97.5),
              fill = colors[1]) +
  geom_ribbon(aes(ymin = diff.sims.025, ymax = diff.sims.97.5), fill = colors[2],
              alpha = .7) + 
  geom_line(aes(iteration, diff.pred.5), colour = colors[3], size = 1) +
  geom_line(aes(iteration, diff.sims.5), colour = colors[3], size = 1) +
  geom_line(aes(iteration, diff.obs), colour = colors[4], size = 1) + 


##### Old version, without ggplot #####

# # Get the changes after each iteration
# diff.obs <- getChanges(obs)
# diff.pred.list <- sapply(pred, getChanges)
# diff.sims.list <- sapply(sims, getChanges)
# 
# # Get the quantiles for the latent space simulations
# diff.pred.quantiles <- data.frame(t(apply(diff.pred.list, 1, quantile,
#                                           c(.025, .5, .975))))
# names(diff.pred.quantiles) <- c("diff.pred.025", "diff.pred.5", 
#                                 "diff.pred.97.5")
# 
# diff.sims.quantiles <- data.frame(t(apply(diff.sims.list, 1, quantile,
#                                           c(.025, .5, .975))))
# names(diff.sims.quantiles) <- c("diff.sims.025", "diff.sims.5", 
#                                 "diff.sims.97.5")
# 
# # Put everything in a data.frame for ggplot
# diff.df <- data.frame(
#   iteration = 1:20,
#   diff.obs = diff.obs
# )
# diff.df <- cbind(diff.df, diff.pred.quantiles, diff.sims.quantiles)
# 
# 
# colors <- brewer.pal(3, "Set3")
# 
# with(
#     diff.pred.quantiles, 
#     plotShadedCI(
#         x = 1:20,
#         median = X50.,
#         upper = X2.5.,
#         lower = X97.5.,
# #         median = log(X50.),
# #         upper = log(X2.5.),
# #         lower = log(X97.5.),
#        color = colors[1],
#        outer.lines = F,
#         log = "y"
#         )
#     )
# with(
#     diff.sims.quantiles,
#     plotSecondCI(
#         x = 1:20,
#         median = X50.,
#         upper = X2.5.,
#         lower = X97.5.#,
#        hashed = F,
#        color = colors[2],
#        border = NA
# #         median = log(X50.),
# #         upper = log(X2.5.),
# #         lower = log(X97.5.)
#         )
#     )
# lines(
#     x = 1:20,
#     y = diff.obs,
#     col = colors[3],
#     lwd = 2
#     )
# 
# 
# png("figure_2.png", width = 5, height = 4, units = "in", res = 300)
# par(mar = c(5, 5, 4, 2) + .1)
# plot(x = 1:50, y = log(diff.pred[1:50]), type = "l", lty = 2,
#      xlab = "Number of iterations",
#      main = "Figure 2: Change in simulated attitudes\nafter each iteration",
#      ylab = expression(paste(log, bgroup("(",bgroup("||", A^{(t)} - A^{(t-1)}, "||"), ")"))))
# # TODO(jcf26@): Make the logged axes look good
# lines(x = 1:50, y = log(diff.obs[1:50]), lty = 1)
# lines(x = 1:50, y = log(diff.sims[1:50]), lty = 3)
# legend("topright", inset = .05, title = "Network type:",
#        c("Observed", "Predicted probability", "Posterior predictive"),
#        lty = 1:3)
# 
# # TODO(jcf26@): Add error bounds to the latent space simulations
# 
# dev.off()
