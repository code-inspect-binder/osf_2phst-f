# Get MSE of prediction of next wave using weighted average of network tie and
# density of network
# TODO(jcf26@): bring all of these "mse_values*" files into a single R script
# Author: Jake Fisher

# Set up workspace
rm(list = ls())
setwd("/misc/utopia3/jcf26/lanhome/DissertationShare/social_space_diffusion_r_and_r")
library(prosperHelper2)
library(hydroGOF)  # for mse
library(latentnet)
library(latentnetDiffusion)
library(dplyr)
library(dtplyr)
library(tidyr)
library(parallel)

meanImputation <- function(x) {
  # Mean-imputes missing values for a vector x
  #
  # NOTE: see http://www.mail-archive.com/r-help@r-project.org/msg58289.html
  #
  # Args:
  #   x: numeric vector
  #
  # Returns:
  #   x, with missing values replaced by mean(x)
  return(replace(x, is.na(x), mean(x, na.rm = T)))
}

makeConstructMatrix <- function(net, a) {
  # Calculates Friedkin's construct matrix (C) from pg. 40 of his 2011 book
  #
  # Args:
  #   net: statnet network object
  #   a: numeric scalar between 0 and 1, indicates how heavily to weight network
  #      vs. individual (a = 1 is all individual, a = 0 is all network)
  # 
  # TODO(jcf26@): allow a to be a vector, such that each person gets his or her
  #               own weight
  #
  # Returns:
  #   A reweighted square matrix
  
  # Check a
  stopifnot(length(a) == 1)
  stopifnot(is.numeric(a))
  stopifnot(a <= 1 | a >= 0)
  
  # Check net, and convert it to an adjacency matrix if necessary
  if (is.network(net)) {
    mat <- net[, ]
  } else if (is.matrix(net)){
    stopifnot(nrow(net) == ncol(net))
    mat <- net
  } else {
    stop("net must be a statnet network or a square matrix object")
  }
  
  diag(mat) <- 0
  C <- rowNormalize(mat)
  
  rounded.rowsums <- round(rowSums(C), floor(-log10(.Machine$double.eps)))
  
  if (!all(rounded.rowsums %in% c(0, 1)))
    warning("C matrix values do not sum to 0 or 1")
  
  # Create diagonal matrix A and identity matrix I
  I <- diag(nrow(C))
  A <- I * a
  
  # Fix isolated dyads -- definitionally (my defn.) assigned to weight the last
  # observation at 1
  isos <- which(rounded.rowsums == 0)
  A[isos, isos] <- 0
  
  # Convert to a weight matrix and return
  return(A %*% C + I - A)
}

getMSE <- function(net, attitude, diag.weight, gam, iter = 5,
                   dat = wv.minus.1) {
  # Calculates the MSE value for 5 iterations on a given network and attitude,
  # with a given diagonal weight
  #
  # Args:
  #   net: a statnet network object, with attitude as a vertex attribute
  #   attitude: a character variable denoting a vertex attribute on net, which 
  #             should be a numeric vector (i.e., the v. attr. should be num.)
  #   diag.weight: parameter passed as "a" to makeConstructMatrix
  #   gam: "gamma" parameter in [0, 1] that indicates how much weight we should
  #        give to the observed network ties (1) versus network density (0)
  #   iter: number of iterations to run
  #   dat: data where the "true" values of the attitude should be looked up
  #        (expectation is that we have PROSPER.survey[, wave := wave + 1])
  #   smoother: character string indicating which smoother we should apply to 
  #             the network
  #
  # TODO(jcf26@): break this into two smaller functions
  #
  # NOTE: none of these parameters are tested; use with care
  # 
  # Returns:
  #   a scalar value
  
  
  # Make sure we're not pulling more than one attitude, or else this will break
  stopifnot(length(attitude) == 1)
  
  # Create weighted matrix balancing network ties and network density
  if (gam == 0) {
    w <- matrix(gden(net), ncol = network.size(net), nrow = network.size(net))
  } else {
    mat <- net[, ]
    w <- gam * mat + (1 - gam) * gden(net)
  }
  # 
  # w <- matrix(0, ncol = network.size(net), nrow = network.size(net)) +
  #   gam * mat + (1 - gam) * gden(net)
  
  # First calculate predicted diffusion values using net and attitude
  pred <- degroot(
    makeConstructMatrix(w, diag.weight),
    matrix(meanImputation(net %v% attitude), ncol = 1),
    iterations = iter,
    all.iter = T,
    row.normalize = F  # NOTE: will warn, that's okay
  )
  
  # Next pull the "correct" data using prosperHelper2
  Y.obs <- unlist(getPROSPERVariableByNetwork(net, attitude, dat))
  
  # Finally, return the MSE's -- return them in a format that matches other
  # approaches
  net.name <- net %n% "title"
  out <- data.frame(
    school = getSchool(net.name),
    cohort = getCohort(net.name),
    wave = getWave(net.name),
    attitude = attitude,
    prediction.number = NA,
    iteration = 0:iter,
    model.type = paste0("density_weight_", gam),
    simulated = NA,
    diag.weights = diag.weight,
    next.wave.mse = apply(pred, 2, mse, obs = Y.obs)
  )
  
  return(out)
}

# Load data
data(PROSPER.networks)
data(PROSPER.survey)

# Construct a network object that we can loop over
attitudes <- c("cattus", "cavgdev", "cexpt", "cfmre", "csuatt_r", "csumscab")
nets <- subsetPROSPERNetworks(wave = 1:7) %>%
  lapply(FUN = addPROSPERVertexAttribute.dt, attribute = attitudes)

# Adjust PROSPER.survey so that we can use prosperHelper2 functions to subset it
wv.minus.1 <- copy(PROSPER.survey)
wv.minus.1[, wave := wave - 1]
setkey(wv.minus.1, id, cohort, wave)

# Now loop through all of the possible attitudes and diagonal weightings
permutations <- expand.grid(attitudes, seq(from = 0, to = 1, by = .1),
                            seq(from = 0, to = 1, by = .1),
                            stringsAsFactors = F) %>%
  setNames(nm = c("attitudes", "diag.weights", "gamm")) 

density.mse <- with(permutations, mcMap(
  function(i, j, k) bind_rows(lapply(nets, getMSE, attitude = i,
                                     diag.weight = j, gam = k)),
  i = attitudes,
  j = diag.weights,
  k = gamm,
  mc.cores = 24,
  mc.preschedule = F  #why...?
)
) %>%
  bind_rows %>%
  tbl_dt

# Save the output
save(density.mse, file = "density_smoother_mses.Rdata")

