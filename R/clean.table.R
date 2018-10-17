#' aTitle
#'
#' @param data a
#' @param variable.number a
#' @param max.levels.of.factor a
#'
#' @return a
#' @export
#'
#' 
clean.table <-
function(data,variable.number,max.levels.of.factor=7) {
  max.levels.of.factor<-max(max.levels.of.factor,6)
  ds <- summary.data.frame(data)
  d <- matrix(ds,nrow = max.levels.of.factor,ncol=variable.number,dimnames = dimnames(ds))
  a <- apply(d,2, FUN=as.data.frame)
  d <- do.call("cbind",a)
  colnames(d)<- unlist(dimnames(ds)[2])
  return(d)
}
