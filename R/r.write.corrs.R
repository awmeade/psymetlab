#' Formats in APA format and writes correlation matrices to an excel sheet
#'
#' Uses an output object from the Psych package and adds 1 or 2 askterisks (stars) to the 
#' correlation for APA table output. Returns object as dataframe. Writes correlational 
#' data for r, n, and pvalues to three consecutively named Excel sheets.
#'
#'
#' @param f.name is the file name to which to write the data. default = 'output.xlsx'
#' @param s.name is the name of the Excel sheet(s) to which to write the data
#' @param obj.corrs is the correlation object from the corr.test function in the Psych package
#' @param print.p requests printing of p values. default = TRUE
#' @param print.n requests printing of sample size (n). default = TRUE
#' @param ... parameters passed to the f.corstar function
#' 
#' @author Adam Meade \email{awmeade@@ncsu.edu}
#' @import xlsx
#' @export 
#' @examples
#' library('psych')
#' R = matrix(cbind(1,.80,.2,.80,1,.7,.2,.7,1),nrow=3)
#' U = t(chol(R))
#' set.seed(1)
#' random.normal = matrix(rnorm(dim(U)[1]*100,0,1), nrow=dim(U)[1], ncol=100);
#' X = as.data.frame(t(U %*% random.normal))
#' corrs = corr.test(X)
#' f.write.corrs(s.name='example',obj.corrs=corrs)
#' f.write.corrs(f.name='example.xlsx',s.name='example',obj.corrs=corrs,p.val.2=FALSE)

f.write.corrs <- local(function(f.name = 'output.xlsx', s.name, obj.corrs, 
                                print.p = TRUE, print.n = TRUE, ...){
  require('xlsx')
  corr.sheet <- paste0(s.name,'.r')
  n.sheet <- paste0(s.name,'.n')
  p.sheet <- paste0(s.name,'.p')
  write.xlsx(f.corstar(obj.corrs,...), f.name, sheetName = corr.sheet, append=TRUE)
  if(print.n == TRUE){
    write.xlsx(obj.corrs$n,f.name, sheetName = n.sheet, append=TRUE)
  }
  if(print.p == TRUE){
    write.xlsx(round(obj.corrs$p,2),f.name, sheetName = p.sheet, append=TRUE)
  }
  return(TRUE)
})
