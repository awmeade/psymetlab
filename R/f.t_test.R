#' Returns the results of a t-test along with a Cohen's D effect size estimate 
#'
#' Requires two dataframes with continuous variables, variance on variables, and matched in variable order. 
#' Requires use of describe from 'psych' package. Requires use of cohen.d from 'effsize' package
#'
#'
#' @param df1 is the majority group dataframe 
#' @param df2 is the minority group dataframe
#'
#' @author Adam Meade \email{awmeade@@ncsu.edu}
#' @importFrom psych describe
#' @importFrom effsize cohen.d

#' @export 
#' @examples
#' \dontrun{
#'  require('psych')
#'  require('effsize')
#'  automatic  <- mtcars[which(mtcars$am == 0),]
#'  manual <- mtcars[which(mtcars$am == 1),]
#'  f.t_test(automatic,manual)
#' }


f.t_test <- local(function(df1, df2){
  out.table <- data.frame()
  out.i <- 1
  library('effsize')
  for(i in 1:ncol(df1)){
    out.table[out.i,1] <- describe(df1[,i])$mean
    out.table[out.i,2] <- describe(df1[,i])$sd
    out.table[out.i,3] <- nrow(df1)
    out.table[out.i,4] <- describe(df2[,i])$mean
    out.table[out.i,5] <- describe(df2[,i])$sd
    out.table[out.i,6] <- nrow(df2)
    if(describe(df1[,i])$sd > 0 && describe(df2[,i])$sd > 0 ){
      out.table[out.i,7] <- cohen.d(df1[,i],df2[,i])$estimate
      temp.out <- t.test(df1[,i],df2[,i])
      out.table[out.i,8] <- temp.out$statistic
      out.table[out.i,9] <- temp.out$parameter
      out.table[out.i,10] <- temp.out$p.value      
    }
    
    out.i = out.i + 1
  
  }
  rm(i,out.i,temp.out)
  out.table
  
  out.table[11] <- .05/ncol(d.ps)
  
  colnames(out.table) <- c("Mean Majority","SD Majority","N Majority","Mean Minority","SD Minority",
                           "N Minority","cohens.d","t","df","p","Bon Ferroni Critical P")
  row.names(out.table)<-names(d.ps)
  
  out.table <- round(out.table,digits=3)
  return(out.table)
})
