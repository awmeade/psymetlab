#' Add asterisk(s) to significant correlations 
#'
#' Uses an output object from the Psych package and adds 1 or 2 askterisks (stars) to the 
#' correlation for APA table output. Returns object as dataframe.
#'
#'
#' @param corr.obj is the output list object from the corr.test function in the Psych package
#' @param is.triangle is whether the correlation is a symetric matrix (default) or a rectangular 
#' matrix in which one set of variables is correlated with a different set.
#' @param p.val.1 is the p-value desired to trigger a single astrisk (default = .05)
#' @param p.val.2 is the p-value desired to trigger a second astrisk added to the first
#' default = .01. p.val.2 should be a smaller p val than p.val.1. set p.val.2 = FALSE if
#' no second astrisk is desired
#'
#' @author Adam Meade \email{awmeade@@ncsu.edu}
#' @import psych
#' @export 
#' @examples
#' \dontrun{
#' require('psych')
#'  corrs.1 = corr.test(sat.act)
#'  f.corstar(corrs.1)
#'  f.corstar(corrs.1, p.val.1 = .01, p.val.2 = FALSE)
#'  corrs.2 = corr.test(sat.act[3:5],sat.act[6])
#'  f.corstar(corrs.2,is.triangle = FALSE)
#'  }

f.corstar <- local(function(corr.obj, is.triangle = TRUE, p.val.1 = .05, p.val.2 = .01){
  require('psych')
  R <- corr.obj$r
  p <- corr.obj$p
  ## define notions for significance levels; spacing is important.
  if(p.val.2){
    mystars <- ifelse(p < p.val.1,ifelse(p<p.val.2,"** ","* "), " ")
  }else{
    mystars <- ifelse(p < p.val.1,"* ", " ")
  }
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(R, 2))
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(R))
  rownames(Rnew) <- rownames(R)
  colnames(Rnew) <- paste(colnames(R), "", sep="")
  ## if triangle, remove upper triangle
  if(is.triangle==TRUE){
    diag(Rnew) <- paste(diag(R), " ", sep="")
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
    ## remove last column and return the matrix (which is now a data frame)
    Rnew <- cbind(Rnew[1:length(Rnew)-1])
  }
  return(Rnew)
}) 
