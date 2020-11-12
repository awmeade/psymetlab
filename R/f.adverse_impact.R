#' Compute adverse impact statistics
#'
#' Accepts grouping vector d.group, vector of outcome (e.g., pass/fail) as d.pass, 
#' and value for majority group (e.g., "male"), assumes only two groups
#'
#' @param d.group is a vector of grouping variable values
#' @param d.pass is a vector of outcome values
#' @param majority.group is the label of value assigned to the majority group
#' @param min_percent_minority is the minimum percent of the sample that must be in the minority group in order to report results
#' @return Returns a dataframe with columns... 
#' @return total.n = overall sample size
#' @return majority.n = sample size of the majority group
#' @return minority.n = sample size of the minority group
#' @return percent.minority = percentage of sample in minority group
#' @return SR.total = selection ratio of the entire sample
#' @return SR.majority = selection ratio of the majority group
#' @return SR.minority = selection ratio of the minority group
#' @return impact.ratio = SR.minority / SR.majority
#' @return fishers.p = p value of the Fisher Exact Test
#' @return SD2.test = results of the 2 Standard Deviation Test
#' @return lower_95CI, upper_95CI = lower and upper confidence intervals around Impact Ratio
#' @return shortfall = the number of additional minority group members needed to pass to have an impact ratio = 1
#' @author Adam Meade \email{awmeade@@ncsu.edu}
#' @export
#' @examples
#' require('psych')
#' pass <- ifelse(sat.act$ACT > mean(sat.act$ACT) , 1, 0)
#' table(sat.act$gender)
#' f.adverse_impact(sat.act$gender, pass, majority.group = 2, minority.group = 1)




f.adverse_impact <- function(d.group, d.pass, majority.group, minority.group, min_percent_minority = .05){
  if( length(d.group) != length(d.pass) ){
    stop('grouping variable and outcome must be of equal length.')
  } 
  
  is.majority <- d.group==majority.group
  is.minority <- d.group==minority.group
  is.either <-  ifelse(d.group==majority.group, TRUE, 
                       ifelse(d.group==minority.group, TRUE, 
                              FALSE))
  d.majority.pass <- d.pass[is.majority]
  d.minority.pass <- d.pass[is.minority]
  
  d.just.maj.min <- d.pass[is.either]
  d.group.just.maj.min <- d.group[is.either]
  #d.majority.pass <- d.pass[d.group==majority.group]
  #d.minority.pass <- d.pass[d.group != majority.group]
  
  #total.n <- length(d.group)
  
  majority.n  <- length(d.majority.pass)
  minority.n  <- length(d.minority.pass)
  total.n <- length(d.just.maj.min)
  percent.minority <- round( minority.n / total.n,3)
  
  majority.pass.n <- sum(d.majority.pass)
  minority.pass.n <- sum(d.minority.pass)
  
  SR.total <-  round( sum(d.just.maj.min) / total.n,3)
  SR.majority <- round(sum(d.majority.pass) / length(d.majority.pass),3)
  SR.minority <- round(sum(d.minority.pass) / length(d.minority.pass),3)
  
  impact.ratio <- round(SR.minority / SR.majority,3)
  impact.ratio <- ifelse(percent.minority < min_percent_minority, NA, impact.ratio)
  
  ### fischer exact test using grouping variable and pass score
  #fishers.p <- fisher.test(d.group,d.pass)$p.value
  fishers.p <- fisher.test(d.group.just.maj.min,d.just.maj.min)$p.value
  fishers.p <- ifelse(percent.minority < min_percent_minority, NA, round(fishers.p,3))
  
  #2SD test
  SD2.test <- (SR.majority - SR.minority) / 
    sqrt(SR.total*(1-SR.total)*(1/majority.n + 1/minority.n)) # negative if protected group has higher rate
  SD2.test <- round(SD2.test,3)
  #95% CI interval
  lower_95CI <- exp(log(SR.minority/SR.majority)-
                      1.96*sqrt(((1-SR.minority)/(minority.n*SR.minority))+
                                  ((1-SR.majority)/(majority.n*SR.majority))))
  lower_95CI <- round(lower_95CI, 3)
  upper_95CI <- exp(log(SR.minority/SR.majority)+
                      1.96*sqrt(((1-SR.minority)/(minority.n*SR.minority))+
                                  ((1-SR.majority)/(majority.n*SR.majority))))
  upper_95CI <- round(upper_95CI, 3)
  
  #shortfall
  shortfall <-  ceiling(minority.n*SR.total - sum(d.minority.pass))  
  
  out.table <- data.frame(cbind(total.n, majority.n, minority.n, percent.minority, 
                                majority.pass.n, minority.pass.n, SR.total, 
                                SR.majority, SR.minority, impact.ratio,
                                fishers.p, SD2.test, lower_95CI, upper_95CI, shortfall ))
  return(out.table)  
}
