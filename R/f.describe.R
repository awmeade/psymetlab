#' Simplify output of R Psych package describe function
#'
#' Accepts dataframe as arguement and returns dataframe table
#'
#'
#' @param f.d is the dataframe object
#'
#' @author Adam Meade \email{awmeade@@ncsu.edu}
#' @import psych
#' @export
#' @examples
#' \dontrun{
#' require('psych')
#'  f.describe(sat.act)
#'  }

f.describe <- function(f.d){
  f.table <- describe(f.d)
  f.keep.cols <- c("n","mean","sd","min","max","skew")
  f.out <- f.table[f.keep.cols]
  f.out <- round(f.out,2)
  return(f.out)
}

