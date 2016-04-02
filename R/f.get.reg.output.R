#' Take output from lm function and put relevant info into a dataframe
#'
#' Returns a dataframe with predictors and coefficients listed along with model statistics 
#' F,df,p, and r-squred values.
#'
#'
#' @param out.lm results of a linear regression from lm()
#' @author Adam Meade \email{awmeade@@ncsu.edu}
#' @export 
#' @examples
#' R = matrix(cbind(1,.80,.2,.80,1,.7,.2,.7,1),nrow=3)
#' U = t(chol(R))
#' set.seed(1)
#' random.normal <- matrix(rnorm(dim(U)[1]*100,0,1), nrow=dim(U)[1], ncol=100);
#' X <- as.data.frame(t(U %*% random.normal))
#' model.out <- lm(X[,1]~X[,2]+X[,3])
#' f.get.reg.output(model.out)
f.get.reg.output <- local(function(out.lm){
  the.model <- summary(out.lm)$call
  get.model <- toString(the.model)
  coef <- round(summary(out.lm)$coefficients,2)
  f <- as.matrix(round(summary(out.lm)$fstatistic,2))
  p <- round(pf(f[1],f[2],f[3],lower.tail=F),3)
  r2 <- round(summary(out.lm)$r.squared,2)
  r <- round(sqrt(r2),2)
  f.stats <- rbind(f,p,r2,r)
  x <- matrix(NA, ncol = ncol(coef), nrow = 6)
  x[,1] <- f.stats
  row.names(x) <- c("F value","df1","df2","p","r-sq","r")
  return.me <- rbind(coef,x)
  return.me <- cbind(return.me,get.model)
  return(return.me)
})
