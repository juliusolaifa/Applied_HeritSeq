set.seed(234)
real_fake <- function(iteration, strainSizes) {
  sig11 <- rgamma(iteration,2,1)
  sig22 <- rexp(iteration)
  rho <- runif(iteration,-1,1)
  sig12 <- rho * sqrt(sig11) * sqrt(sig22)
  phi <- rchisq(iteration,3)
  alpha <- rnorm(iteration,5.5, 2)
  beta <- rnorm(iteration,0,0.5)

  sigma <- lapply(1:length(sig11), function(i)
    matrix(c(sig11[i], sig12[i], sig12[i], sig22[i]), nrow = 2))
  X <- rgen01(strainSizes)

  true_param = matrix(c(alpha, beta, sig11, sig22,sig12, phi), ncol=6)
  countMatrix <- getNBReadMatrix(strainSizes, alpha, beta, sigma, phi, X)
  fit <- fitNBModel(countMatrix, "glmmTMB", X, TRUE)
  vpc0 <- computeNBVPC(fit,0)
  vpc1 <- computeNBVPC(fit,1)
  true_vpc0 <- computeNBVPC(true_param,0)
  true_vpc1 <- computeNBVPC(true_param,1)
  vpcs <- cbind(true_vpc0, vpc0,true_vpc1,vpc1)
  colnames(vpcs) <- c('true_0', 'est_0', 'true_1', 'est_1')
  return(list(true=true_param,fit = fit, vpcs = vpcs))
}

strainSizes = rep(6,40)
iteration = 10000
result <- real_fake(iteration,strainSizes)
