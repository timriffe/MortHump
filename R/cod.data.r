# function to isolate and/or group cod from the Mxc matrix
cod.data <- function(k, Mxc, exp){
   # TR: 2023-10-06 this break causes a warning in check()
   # it expects to be used in a loop setting, but it is instead used in an lapply()
   # not sure now what behavior to aim for
  if(!(all(k < 0) | all(k > 0))){break ; warning("Cod must be all included or all excluded")}

  if(length(k) == 1){if(k > 0){m <- Mxc[,k]}else{m <- rowSums(Mxc[,k])}}else{
    m <- rowSums(Mxc[,k])}

  if(!is.null(dim(m))){warning("Error")}

  d <- m * exp

  return(data.frame(m,d,exp))
}
