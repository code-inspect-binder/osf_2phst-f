# Plots distribution of attitudes across several iterations of the model
# Author: Jake Fisher

# Set up workspace
rm(list = ls())
set.seed(919)
# setwd("I:/Dissertation")
setwd("~/lanhome/DissertationShare")
# library(latentnetDiffusion)
library(prosperHelper2)
library(latentnet)
library(parallel)

# TODO(jcf26@): bundle sample data with latentnet diffusion and use sample data
# instead of relying on full PROSPER data

data(PROSPER.networks)
school.212 <- subsetPROSPERNetworks(school = 212, cohort = 1, wave = 1)

model.specs <- c(
  school.212 ~ euclidean(d = 2),
  school.212 ~ euclidean(d = 3),
  school.212 ~ euclidean(d = 4),
  school.212 ~ euclidean(d = 5)
)

ergmm.212 <- mclapply(model.specs, function(x) ergmm(x),
                      mc.cores = length(model.specs))

ergmm.212.2d <- ergmm.212[[1]]
ergmm.212.3d <- ergmm.212[[2]]
ergmm.212.4d <- ergmm.212[[3]]
ergmm.212.5d <- ergmm.212[[4]]

save(school.212, ergmm.212.2d, ergmm.212.3d, ergmm.212.4d, ergmm.212.5d,
     file = "ergmm_212.Rdata")