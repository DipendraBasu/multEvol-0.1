pc.aicc <- function(phy, dat)
{
  dat <- as.data.frame(dat)
  pc.models <- list(BM=list(), EB=list(), OU=list(), lambda=list(), kappa=list(), delta=list())
  for(i in 1:length(pc.models))
  {
    print(names(pc.models[i]))
    pc.models[[i]] <- geiger::fitContinuous(phy, dat, model = names(pc.models[i]))
  }
  # pc.models
  out1 <- lapply(pc.models, function(x){sapply (x, function(y){y$opt$aicc})})
  out2 <- mapply(c, out1$BM, out1$EB, out1$OU, out1$lambda, out1$kappa, out1$delta)
  rownames(out2) <- names(out1)
  out2
}