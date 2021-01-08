circle_apply <- function(
  values,
  n,
  fun = "mean"
) {
  valuesex <- c(
    values[ (length(values)-floor(n/2)+1) : length(values) ], # last n/2 values of the vector at the beginning
    values,
    values[ 1 : (floor(n/2)) ] # first n/2 values of the vector added to the end
  )
  
  smooth <- zoo::rollapply(valuesex, width = n, FUN = fun, fill = NA)[
    (floor(n/2)+1):(length(valuesex)-floor(n/2)) ]
  
  return(smooth)
}

