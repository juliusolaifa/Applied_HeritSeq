rm(list=ls())
library(HeritSeq2)


realLife <- function(data, covariate, group, fittingModel="NB", slope=FALSE) {
  fitModel <- ifelse(fittingModel == "NB", fitNBmodel, fitCPmodel)
  computeVPCModel <- ifelse(fittingModel == "NB", computeNBVPC, computeCPVPC)
  fit <- fitModel(data, covariate, slope)
  vpc <- computeVPCModel(fit, group)
  colnames(vpc) <- "vpc"
  rownames(vpc) <- paste("Gene", 1:nrow(vpc))
  vpc
}

#setup
load("C:/Users/JuliusOlaifa/Applied_HeritSeq/LXSmRNA.rda")
X <- extractGroupAsBinary(LXSmRNA, 2)

LXSmRNA <- LXSmRNA[1:2,]

fitnb_noslope0 <- realLife(LXSmRNA, X, 0)
fitnb_noslope1 <- realLife(LXSmRNA, X, 1)
fitnb_slope0 <- realLife(LXSmRNA, X, 0, slope = TRUE)
fitnb_slope1 <- realLife(LXSmRNA, X, 1, slope = TRUE)

fitcp_noslope0 <- realLife(LXSmRNA, X, 0, fittingModel = "CP")
fitcp_noslope1 <- realLife(LXSmRNA, X, 1, fittingModel = "CP")
fitcp_slope0 <- realLife(LXSmRNA, X, 0, fittingModel = "CP", slope = TRUE)
fitcp_slope1 <- realLife(LXSmRNA, X, 1, fittingModel = "CP", slope = TRUE)
