# model for inorganic fertilization
prednue <- function(ndose,clay,som,tmp,pre) {-68.6658 + 1.863 * clay + 0.2269 * som + 2.1827 * tmp + 0.0566 * pre +
    34.2542 - 0.1275 * ndose + 0.001 * ndose^2 - 0.2213 * ndose}

# example for a situation where input values (clay, som, tmp, pre) are fixed. assume x = 56
x <- 56.9244

# simplify the function to estimate NUEmax
nue_max = 56.9244 - (0.1275 + 0.2213) * ndose + 0.001 * ndose^2

# the input to achieve a given n uptake can be estimated (with NUE in %)
ndose = (nup * 100/ nue_max)

nue_max = 56.9244 - (0.1275 + 0.2213) * ndose + 0.001 * ndose^2

nue_max = 56.9244 - 0.0938 * (nup * 100/ nue_max) + 0.001 * (nup * 100/ nue_max)^2



nuesolve <- function(nue_max,nup,pred = FALSE){ 
  
  # predict the NUE max for a given N uptake
  if(pred){
    
    out = 56.9244 - 0.0938 * (nup * 100/ nue_max) + 0.001 * (nup * 100/ nue_max)^2 
    
  } else {
    
    # find the NUE max where polynomial equation gives zero
    out = abs(56.9244 - 0.0938 * (nup * 100/ nue_max) + 0.001 * (nup * 100/ nue_max)^2 - nue_max)
    
  }
  
  return(out)
}

a = optim(par=c(70),nuesolve,nup=2000,method = 'CG')

nuesolve(nue_max = a$par,nup = 2000, pred = TRUE)
