## Use optim 

# simple example

test.x2 <- function(x){
  
  y = x*x
  
  return (-y)
}

test.optimx <- optim(par = 4, fn = test.x2)

# output: par = best x, value = best y, counts = number of guesses, convergence = status (0 means success)









# create small test data set to do faster calculations in preparation of the real calculations 

test.dt <- dt2[0:1000,]

# metafor model with selection of covariates
metatest = rma.mv(fnue,fvnue,mods = ~ xclay + xsom + tmp_mean + pre_mean + factor(n_source)*n_dose + I(n_dose^2),random=~1| no,data=test.dt,method="ML", sparse = TRUE)

# prediction

test <- predict.rma(metatest, newmods=cbind(c(25,25,25,25,25,25,25,25,25,25,25,25),# xclay
                                            c(35,35,35,35,35,35,35,35,35,35,35,35),# xsom
                                            c(15,15,15,15,15,15,15,15,15,15,15,15), # tmp_mean
                                            c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000), # pre_mean
                                            c(1,1,1,1,1,1,1,1,1,1,1,1),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0), # nsource (inorganic)
                                            c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose
                                            c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose^2
                                            c(25,50,75,100,125,150,175,200,225,250,275,300),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0))) # nsource (inorganic)



# input 
# xclay = test.dt$xclay
# xsom = test.dt$xsom
# tmp_mean = test.dt$tmp_mean
# per_mean = test.dt$pre_mean
# n_source = test.dt$n_source
# n_dose = test.dt$n_dose
# fnue = test.dt$fnue
# fvnue = test.dt$fvnue
# test.dt = test.dt 



test.function <- function(xclay, xsom, tmp_mean, pre_mean, n_source, n_dose, fnue, fvnue, test.dt){ 
  
  out <- predict.rma(metatest, newmods = cbind(xclay,
                                            xsom,
                                            tmp_mean,
                                            pre_mean,
                                            n_source,
                                            n_source,
                                            n_source,
                                            n_dose,
                                            n_dose,
                                            n_dose,
                                            n_dose,
                                            n_dose,
                                        ))

  return(-out)
  
}



test.optim <- optim( c(25,35,15,1000,1,0,0,25,25,25,0,0), 
              fn = test.function, 
              method = "L-BFGS-B",
              upper = 100)



