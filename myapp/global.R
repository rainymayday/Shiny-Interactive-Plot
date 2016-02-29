
factor2numeric <- function(x){
  x.new <- as.numeric(gsub(",","",as.character(x)))
  return (x.new)
}
merge.all <- function(x, y) {
  merge(x, y, all=TRUE, by=c("Created.By","Segment"))
}