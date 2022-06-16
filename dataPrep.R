data.prep <- function(phy, dat, colnum=NULL, scale=NULL, method="BM", mode="cov")
{
  '%notin%' <- Negate('%in%')
  phy <- ape::drop.tip(phy, which(phy$tip.label %notin% rownames(dat)))
  pca.out <- function(x, y)
  {
    dat.pca <- phytools::phyl.pca(x, y, method, mode)
    par(mfrow=c(1, 2))
    biplot(dat.pca)
    plot(diag(dat.pca$Eval))
    return(dat.pca$S)
  }
  if(is.null(colnum))
  {
    if(missing(scale))
    {
      pca.out(phy, dat)
    }
    else
    {
      if(scale == TRUE)
      {
        dat <- scale(dat, center = T, scale = T)
        pca.out(phy, dat)
      }
      else
      {
        if(scale == FALSE)
        {
          pca.out(phy, dat)
        }
      }
    }
  }
  else
  {
    dat.resid <- phytools::phyl.resid(phy, dat[, as.numeric(colnum)], dat1[, -as.numeric(colnum)])$resid
    if(missing(scale))
    {
      pca.out(phy, dat.resid)
    }
    else
    {
      if(scale == TRUE)
      {
        dat.resid <- scale(dat.resid, center = T, scale = T)
        pca.out(phy, dat.resid)
      }
      else
      {
        if(scale == FALSE)
        {
          pca.out(phy, dat.resid)
        }
      }
    }
  }
}
