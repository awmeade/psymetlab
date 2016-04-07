#' Deletes multivariate outliers using Mahalanobis distance
#'
#' Accepts a dataframe and optional list of variables within that dataframe for which to screen the data.
#' The function computes mahalanobis distace and associated chi-square on the screening data and then
#' returns a dataframe that is a subset of the original all.data dataframe based on non-significant 
#' chi-square values.
#'
#' @param add.data is the database from which to remove outliers
#' @param screening.vars is an optional array of variable names on which the screening should be based.
#   default is to use all variables in the database for screening.
#' @param p.val is the cutoff value of the chi-square distribution to use. default is .05.
#' @author Adam Meade \email{awmeade@@ncsu.edu}
#' @export 
#' @examples
#' \dontrun{
#' nrow(trees)
#' new.data <- f.screen.outliers(trees)
#' nrow(new.data)
#' }

f.screen.outliers <- local(function(all.data, screening.vars = NULL, p.val = .05){
  p.val = 1 - p.val
  if(is.null(screening.vars)){
    d.screening <- all.data
  }else{
    d.screening <- all.data[screening.vars]
  }
  m.dist <- mahalanobis(d.screening,colMeans(d.screening,na.rm=TRUE), cov(d.screening ,use="pairwise.complete.obs"))
  cut.score <- qchisq(p=p.val,df=ncol(d.screening))
  flag <- ifelse(m.dist > cut.score,1,0)
  table(flag)
  r.df <- all.data[flag==0,]
  return(r.df)
})
