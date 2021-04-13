
media_mobile <- function(val, k){

    appo <- vector()
    
    for(i in 1:length(val)){
      if(i < k){
        pippo <- 0
        for(j in 1:i){pippo <- pippo + val[j]}
        appo[i] <- pippo/i
      }else{
        pluto <- 0
        for(h in 0:(k-1)){pluto <- pluto + val[i-h]}
        appo[i] <- pluto/k
      }
    }
    
    return(appo)
    
}







