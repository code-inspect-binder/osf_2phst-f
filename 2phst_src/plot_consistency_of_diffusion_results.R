# Author: Jake Fisher
# Look at the distribution of outcomes for diffusion simulations with random
# intial values

# Set up workspace
rm(list = ls())
set.seed(919)
# setwd("C:/Users/Jake/Documents/Dissertation")
setwd("C:/Users/jakef/Google Drive/Papers/Social space diffusion")
library(dissertation1Helper)
library(prosperHelper)
library(RColorBrewer)
library(latentnet)
library(statnet)

# Load the data
data(school.212)
data(ergmm.212)
data(predict.212)

# Create a vector of simulated initial values
# We're not averaging over many initial values because that conflates the
# properties of averages with the properties of the diffusion process
A.1 <- sample(1:5, size = 160, replace = T)

# Simulate diffusion over 10 steps
A.obs <- degroot(school.212[[1]], A.1, 10, all.iter = T)
A.pred <- ergmmDegroot(ergmm.212[[1]], A.1, draws = 1, iterations = 10,
                       all.iter = T)[[1]]
A.nets <- ergmmDegroot(ergmm.212[[1]], A.1, draws = 1, simulate = T,
                       iterations = 10, all.iter = T)[[1]]

# Now plot the diffusion results
cols <- brewer.pal(5, "Blues")
columns <- c(1, 2, 3, 7, 11)

pdf("figure_3.pdf", height = 4, width = 10, paper = "a4r")
par(mfrow = c(1, 3), oma = c(2, 0, 2, 0) + .1)
plot(density(A.obs[, 1]), lwd = 2, col = cols[1], xlim = c(1, 5),
     ylim = c(0, 5), xlab = "", main = "(a)\nObserved network")
for (i in 2:5) {
  lines(density(A.obs[, columns[i]]), lwd = 2, col = cols[i])
}
legend("topleft", inset = .05, lwd = 2, col = cols, title = "Iteration",
       legend = c(expression(paste(t == 1)), expression(paste(t == 2)), 
                  expression(paste(t == 3)), expression(paste(t == 7)),
                  expression(paste(t == 11))))

plot(density(A.pred[, 1]), lwd = 2, col = cols[1], xlim = c(1, 5),
     ylim = c(0, 5), xlab = "", ylab = "", 
     main = "(b)\nPredicted probabilities")
for (i in 2:5) {
  lines(density(A.pred[, columns[i]]), lwd = 2, col = cols[i])
}

plot(density(A.nets[, 1]), lwd = 2, col = cols[1], xlim = c(1, 5),
     ylim = c(0, 5), xlab = "", ylab = "",
     main = "(c)\nPosterior predictive")
for (i in 2:5) {
  lines(density(A.nets[, columns[i]]), lwd = 2, col = cols[i])
}
mtext("Figure 3: Distribution of attitude values by simulated iteration", outer = T)
mtext("Simulated attitude values", side = 1, outer = T)
dev.off()

# Create a matrix of simulated initial values
nsim <- 1000
A.1 <- Matrix(sample(1:5, size = 160 * nsim, replace = T), ncol = nsim)

# Get the results for the observed network
A.obs <- degroot(school.212[[1]], A.1, 100)

# Get the results for the predicted probabilities
predicted <- lapply(1:100, function(i) predict(ergmm.212[[1]], type = i))
A.pred <- lapply(predicted, degroot, Y = A.1, iterations = 100)

# Get the results for the posterior predictive distribution
pred.nets <- simulate(ergmm.212[[1]], nsim = 100)$networks
A.nets <- degrootList(pred.nets, A.1, all.iter = F)

# Get the results for all the iterations
A.1.df <- data.frame(as.matrix(A.1))
test <- lapply(A.1.df, degrootList, mat.list = pred.nets)
test.2 <- lapply(A.1.df, degroot, W = school.212, iterations = 100, all.iter = T)
test <- degrootList(pred.nets, A.1)
test.2 <- degroot(school.212[[1]], A.1, 100, all.iter = T)

# Average over the posterior
A.pred.avg <- (Reduce(`+`, A.pred) / length(A.pred))





# Average over the simulated values by iteration
# Not sure if these do what you think they do.
test.avg <- sapply(
    seq(from = 1, to = ncol(test), by = ncol(A.1)),
    function(i) 
      rowMeans(test[, i:(i + ncol(A.1) - 1)])
    )

test.2.avg <- sapply(
    seq(from = 1, to = ncol(test.2), by = ncol(A.1)),
    function(i)
      rowMeans(test.2[, i:(i + ncol(A.1) - 1)])
    )

plot(density(test.avg[, 1]), ylim = c(0, 10))
lines(density(test.avg[, 2]), lty = 2)
lines(density(test.avg[, 3]), lty = 3)
lines(density(test.avg[, 4]), lty = 4)
lines(density(test.avg[, 5]), lty = 5)

plot(density(test.2.avg[, 1]), ylim = c(0, 10))
lines(density(test.2.avg[, 2]), lty = 2)
lines(density(test.2.avg[, 3]), lty = 3)
lines(density(test.2.avg[, 4]), lty = 4)
lines(density(test.2.avg[, 5]), lty = 5)



# Plot the distribution of final results
plot.new()
plot.window(xlim = c(1, 5), ylim = c(0, 5))
# apply(A.obs, 2, function(i) lines(density(i), col = "lightgray"))
apply(A.nets, 2, function(i) lines(density(i), col = "lightgray"))
plot(density(A.nets[, 1]))

plot.new()
plot.window(xlim = c(1, 5), ylim = c(0, 5))
# apply(A.obs, 2, function(i) lines(density(i), col = "lightgray"))
apply(test[, seq(from = 1, to = ncol(test), by = 1000)], 2, function(i) lines(density(i), col = "lightgray"))
plot(density(A.nets[, 1]))

hist(A.obs[,1])
lines(density(A.pred[[1]][, 1]), add = T)
