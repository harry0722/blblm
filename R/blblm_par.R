#' Title
#' @param formula the regression formula
#' @param data user-specified datasets
#' @param B the number of bootstraps
#' @param cl the number of clusters that the users choose
#'
#' @export
#'
#' @examples
#' blblm.par(distance ~ air_time, c("flights/1.csv","flights/2.csv"), B = 100,cl=4)
#' @method blblm par
blblm.par<-function(formula, data, B=5000, cl){
  suppressWarnings(plan(multiprocess, workers=cl))
  estimates<-future_map(data,~{
    df<-read.csv(.)
    lm_each_subsample(formula=formula, data=df, n=nrow(df), B=B)
  })

  res<-list(estimates=estimates, formula=formula)
  class(res)<-"blblm"
  invisible(res)
}
