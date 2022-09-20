# predictions for NUE with metafor model

# clear environment and require packages
require(metafor);require(data.table);require(ggplot2)

# clear environment
#rm(list=ls())

# load metafor model: meta2 = rma.mv(fnue,fvnue,mods = ~ xclay + xsom + tmp_mean + pre_mean + factor(n_source)*n_dose + I(n_dose^2),random=~1| no,data=dt2,method="ML", sparse = TRUE)
#meta2 <- readRDS('products/nue_meta2.rds')


# predict NUE under varying N dose and varying n source
  p0_ndose <- predict.rma(meta2, newmods=cbind(c(25,25,25,25,25,25,25,25,25,25,25,25),# xclay
                                               c(35,35,35,35,35,35,35,35,35,35,35,35),# xsom
                                               c(15,15,15,15,15,15,15,15,15,15,15,15), # tmp_mean
                                               c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000), # pre_mean
                                               c(1,1,1,1,1,1,1,1,1,1,1,1),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0), # nsource (inorganic)
                                               c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose
                                               c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose^2
                                               c(25,50,75,100,125,150,175,200,225,250,275,300),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0))) # nsource (inorganic)
                                               
  # bind model estimates together
  
    # create dataframes for the predicted estimates
    e0_ndose <- as.data.frame(p0_ndose)
   
    # extract vectors 
    
    # for nsource: efficiency
    # extract vectors pred, ci.lb and ci.ub from the model estimates with n source 'efficiency'
    e0_ndose.df <- data.frame(e0_ndose)
    nue <- unlist(e0_ndose.df$pred, use.names=FALSE)
    cil <- unlist(e0_ndose.df$ci.lb, use.names=FALSE)
    ciu <- unlist(e0_ndose.df$ci.ub, use.names=FALSE)
    se <- unlist(e0_ndose.df$se, use.names=FALSE)
    
    # create vectors for the other covariables
    xclay <- rep(25,12)
    xsom <- rep(35,12)
    tmp <- rep(15,12)
    pre <- rep(1000,12)
    nsource <- rep("inorganic",12)
    ndose <- seq(25,300,25) 
    
    # combine extractions and vectors
    nue_e0_ndose <- data.table(nue,se,cil,ciu,xclay,xsom,tmp,pre,nsource,ndose)
    
  # visualise the predictions for nue_e1_all 
  plot_nue_ndose <- ggplot(data=nue_e0_ndose,aes(x=(ndose),y=nue)) +
                           geom_point(color='blue') + geom_line(show.legend=FALSE, color='blue') + theme_bw() +
                           xlab("N dose (kg/ha)") + ylab("NUE (%)") +
                           theme(legend.title = element_text(size=10),
                                 legend.position = c(0.10,.15), 
                                 axis.text.x = element_text(color="black", size = 11), 
                                 axis.text.y = element_text(color="black", size = 11),
                                 axis.title = element_text(color="black", size = 15)) + ylim(0,101)
                           
  # save plot
  #ggsave(plot = plot_nue_ndose, filename = 'products/nue_ndose.jpg')


# predict NUE under varying N dose and varying n source
  p1_eff <- predict.rma(meta2, newmods=cbind(c(25,25,25,25,25,25,25,25,25,25,25,25),# xclay
                                             c(35,35,35,35,35,35,35,35,35,35,35,35),# xsom
                                             c(15,15,15,15,15,15,15,15,15,15,15,15), # tmp_mean
                                             c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000), # pre_mean
                                             c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0), # nsource(efficiency)
                                             c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose
                                             c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose
                                             c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0))) # nsource(efficiency)
                                             
  
  p1_inorg <- predict.rma(meta2, newmods=cbind(c(25,25,25,25,25,25,25,25,25,25,25,25),# xclay
                                               c(35,35,35,35,35,35,35,35,35,35,35,35),# xsom
                                               c(15,15,15,15,15,15,15,15,15,15,15,15), # tmp_mean
                                               c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000), # pre_mean
                                               c(1,1,1,1,1,1,1,1,1,1,1,1),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0), # nsource(inorganic)
                                               c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose
                                               c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose
                                               c(25,50,75,100,125,150,175,200,225,250,275,300),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0))) # nsource(inorganic)
                                               
  
  p1_man <- predict.rma(meta2, newmods=cbind(c(25,25,25,25,25,25,25,25,25,25,25,25),# xclay
                                             c(35,35,35,35,35,35,35,35,35,35,35,35),# xsom
                                             c(15,15,15,15,15,15,15,15,15,15,15,15), # tmp_mean
                                             c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000), # pre_mean
                                             c(0,0,0,0,0,0,0,0,0,0,0,0),c(1,1,1,1,1,1,1,1,1,1,1,1),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0), # nsource(manure)
                                             c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose
                                             c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose
                                             c(0,0,0,0,0,0,0,0,0,0,0,0),c(25,50,75,100,125,150,175,200,225,250,275,300),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0))) # nsource(manure)
  
  p1_mix <- predict.rma(meta2, newmods=cbind(c(25,25,25,25,25,25,25,25,25,25,25,25),# xclay
                                             c(35,35,35,35,35,35,35,35,35,35,35,35),# xsom
                                             c(15,15,15,15,15,15,15,15,15,15,15,15), # tmp_mean
                                             c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000), # pre_mean
                                             c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(1,1,1,1,1,1,1,1,1,1,1,1),c(0,0,0,0,0,0,0,0,0,0,0,0), # nsource(mixture)
                                             c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose
                                             c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose
                                             c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(25,50,75,100,125,150,175,200,225,250,275,300),c(0,0,0,0,0,0,0,0,0,0,0,0))) # nsource(mixture)
  
  p1_org <- predict.rma(meta2, newmods=cbind(c(25,25,25,25,25,25,25,25,25,25,25,25),# xclay
                                             c(35,35,35,35,35,35,35,35,35,35,35,35),# xsom
                                             c(15,15,15,15,15,15,15,15,15,15,15,15), # tmp_mean
                                             c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000), # pre_mean
                                             c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(1,1,1,1,1,1,1,1,1,1,1,1), # nsource(organic)
                                             c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose
                                             c(25,50,75,100,125,150,175,200,225,250,275,300), # n_dose
                                             c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0),c(25,50,75,100,125,150,175,200,225,250,275,300))) # nsource(organic)
  
                                             
  
# bind model estimates together
  
  # create dataframes for the predicted estimates
  e1_eff <- as.data.frame(p1_eff)
  e1_inorg <- as.data.frame(p1_inorg)
  e1_man <- as.data.frame(p1_man)
  e1_mix <- as.data.frame(p1_mix)
  e1_org <- as.data.frame(p1_org)

  # extract vectors 
  
  # for nsource: efficiency
    # extract vectors pred, ci.lb and ci.ub from the model estimates with n source 'efficiency'
    e1_eff.df <- data.frame(e1_eff)
    nue <- unlist(e1_eff.df$pred, use.names=FALSE)
    cil <- unlist(e1_eff.df$ci.lb, use.names=FALSE)
    ciu <- unlist(e1_eff.df$ci.ub, use.names=FALSE)
    se <- unlist(e1_eff.df$se, use.names=FALSE)
    
    # create vectors for the other covariables
    xclay <- rep(25,12)
    xsom <- rep(35,12)
    tmp <- rep(15,12)
    pre <- rep(1000,12)
    nsource <- rep("efficiency",12)
    ndose <- seq(25,300,25) 
  
    # combine extractions and vectors
    nue_e1_eff <- data.table(nue,se,cil,ciu,xclay,xsom,tmp,pre,nsource,ndose)
    
  # for nsource: inorganic
    # extract vectors pred, ci.lb and ci.ub from the model estimates with n source 'inorganic'
    e1_inorg.df <- data.frame(e1_inorg)
    nue <- unlist(e1_inorg.df$pred, use.names=FALSE)
    cil <- unlist(e1_inorg.df$ci.lb, use.names=FALSE)
    ciu <- unlist(e1_inorg.df$ci.ub, use.names=FALSE)
    se <- unlist(e1_inorg.df$se, use.names=FALSE)
  
    # create vector for n source
    nsource <- rep("inorganic",12)
    
    # combine extractions and vectors
    nue_e1_inorg <- data.table(nue,se,cil,ciu,xclay,xsom,tmp,pre,nsource,ndose)
  
  # for nsource: manure
    # extract vectors pred, ci.lb and ci.ub from the model estimates with n source 'manure'
    e1_man.df <- data.frame(e1_man)
    nue <- unlist(e1_man.df$pred, use.names=FALSE)
    cil <- unlist(e1_man.df$ci.lb, use.names=FALSE)
    ciu <- unlist(e1_man.df$ci.ub, use.names=FALSE)
    se <- unlist(e1_man.df$se, use.names=FALSE)
    
    # create vector for n source
    nsource <- rep("manure",12)
    
    # combine extractions and vectors
    nue_e1_man <- data.table(nue,se,cil,ciu,xclay,xsom,tmp,pre,nsource,ndose)
    
  # for nsource: mixture
    # extract vectors pred, ci.lb and ci.ub from the model estimates with n source 'mixture'
    e1_mix.df <- data.frame(e1_mix)
    nue <- unlist(e1_mix.df$pred, use.names=FALSE)
    cil <- unlist(e1_mix.df$ci.lb, use.names=FALSE)
    ciu <- unlist(e1_mix.df$ci.ub, use.names=FALSE)
    se <- unlist(e1_mix.df$se, use.names=FALSE)
    
    # create vector for n source
    nsource <- rep("mixture",12)
    
    # combine extractions and vectors
    nue_e1_mix <- data.table(nue,se,cil,ciu,xclay,xsom,tmp,pre,nsource,ndose)
    
  # for nsource: organic
    # extract vectors pred, ci.lb and ci.ub from the model estimates with n source 'organic'
    e1_org.df <- data.frame(e1_org)
    nue <- unlist(e1_org.df$pred, use.names=FALSE)
    cil <- unlist(e1_org.df$ci.lb, use.names=FALSE)
    ciu <- unlist(e1_org.df$ci.ub, use.names=FALSE)
    se <- unlist(e1_org.df$se, use.names=FALSE)
    
    # create vector for n source
    nsource <- rep("organic",12)
    
    # combine extractions and vectors
    nue_e1_org <- data.table(nue,se,cil,ciu,xclay,xsom,tmp,pre,nsource,ndose)  
  
  # combine estimates and variables for the five levels of n source
  nue_e1_all <- rbind(nue_e1_eff,nue_e1_inorg,nue_e1_man,nue_e1_mix,nue_e1_org)
    
  # visualise the predictions for nue_e1_all 
  plot_nue_source <- ggplot(data=nue_e1_all,aes(x=(ndose),y=nue, color = nsource, group=nsource)) +
                     geom_point() + geom_line(show.legend=FALSE) + theme_bw() +
                     scale_color_manual(values=c('chartreuse3','deepskyblue','orange','darkolivegreen','blue')) +
                     xlab("N dose (kg/ha)") + ylab("NUE (%)") +
                     theme(legend.title = element_text(size=10),
                           legend.position = c(0.10,.15), 
                           axis.text.x = element_text(color="black", size = 11), 
                           axis.text.y = element_text(color="black", size = 11),
                           axis.title = element_text(color="black", size = 15)) + ylim(0,120)
  # save plot
  #ggsave(plot = plot_nue_source, filename = 'products/nue_source.jpg')
                     
    
 # predict NUE under varying clay content
 p2_clay <- predict.rma(meta2, newmods=cbind(c(5,10,15,20,25,30,35,40,45,50,55),# xclay
                                            c(35,35,35,35,35,35,35,35,35,35,35),# xsom
                                            c(15,15,15,15,15,15,15,15,15,15,15), # tmp_mean
                                            c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000), # pre_mean
                                            c(1,1,1,1,1,1,1,1,1,1,1),c(0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0), # nsource (inorganic)
                                            c(200,200,200,200,200,200,200,200,200,200,200), # n_dose
                                            c(200,200,200,200,200,200,200,200,200,200,200), # n_dose
                                            c(200,200,200,200,200,200,200,200,200,200,200),c(0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0))) # nsource (inorganic)
                                            
 # bind model estimates together

   # create dataframe for the predicted estimates
   e2_clay <- as.data.frame(p2_clay)
   
   # extract vectors 
   
   # for nsource: efficiency
   # extract vectors pred, ci.lb and ci.ub from the model estimates with n source 'efficiency'
   e2_clay.df <- data.frame(e2_clay)
   nue <- unlist(e2_clay.df$pred, use.names=FALSE)
   cil <- unlist(e2_clay.df$ci.lb, use.names=FALSE)
   ciu <- unlist(e2_clay.df$ci.ub, use.names=FALSE)
   se <- unlist(e2_clay.df$se, use.names=FALSE)
   
   # create vectors for the other covariables
   xclay <- seq(5,55,5)
   xsom <- rep(35,11)
   tmp <- rep(15,11)
   pre <- rep(1000,11)
   nsource <- rep("inorganic",11)
   ndose <- rep(200,11) 
   
   # combine extractions and vectors
   nue_e2_clay <- data.table(nue,se,cil,ciu,xclay,xsom,tmp,pre,nsource,ndose)
 
   # visualise the predictions for nue_e1_all 
   plot_nue_clay <- ggplot(data=nue_e2_clay,aes(x=(xclay),y=nue)) +
                      geom_point(color='blue') + geom_line(show.legend=FALSE, color='blue') + theme_bw() +
                      xlab("Clay (%)") + ylab("NUE (%)") +
                      theme(legend.title = element_text(size=10),
                            legend.position = c(0.10,.15), 
                            axis.text.x = element_text(color="black", size = 11), 
                            axis.text.y = element_text(color="black", size = 11),
                            axis.title = element_text(color="black", size = 15)) + ylim(0,100)
   # save plot
   #ggsave(plot = plot_nue_clay, filename = 'products/nue_clay.jpg')
   
  
 # predict NUE under varying som content
 p3_som <- predict.rma(meta2, newmods=cbind(c(25,25,25,25,25,25,25,25,25,25,25,25,25,25,25), # xclay
                                            c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150), # xsom
                                            c(15,15,15,15,15,15,15,15,15,15,15,15,15,15,15), # tmp_mean
                                            c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000), # pre_mean
                                            c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), # nsource (inorganic)
                                            c(200,200,200,200,200,200,200,200,200,200,200,200,200,200,200), # n_dose
                                            c(200,200,200,200,200,200,200,200,200,200,200,200,200,200,200), # n_dose
                                            c(200,200,200,200,200,200,200,200,200,200,200,200,200,200,200),c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))) # nsource (inorganic)
                                         
   # bind model estimates together
   
   # create dataframe for the predicted estimates
   e3_som <- as.data.frame(p3_som)
   
   # extract vectors 
   
   # for nsource: efficiency
   # extract vectors pred, ci.lb and ci.ub from the model estimates with n source 'efficiency'
   e3_som.df <- data.frame(e3_som)
   nue <- unlist(e3_som.df$pred, use.names=FALSE)
   cil <- unlist(e3_som.df$ci.lb, use.names=FALSE)
   ciu <- unlist(e3_som.df$ci.ub, use.names=FALSE)
   se <- unlist(e3_som.df$se, use.names=FALSE)
   
   # create vectors for the other covariables
   xclay <- rep(25,15)
   xsom <- seq(10,150,10)
   tmp <- rep(15,15)
   pre <- rep(1000,15)
   nsource <- rep("inorganic",15)
   ndose <- rep(200,15) 
   
   # combine extractions and vectors
   nue_e3_som <- data.table(nue,se,cil,ciu,xclay,xsom,tmp,pre,nsource,ndose)
   
   # visualise the predictions for nue_e1_all 
   plot_nue_som <- ggplot(data=nue_e3_som,aes(x=(xsom),y=nue)) +
                             geom_point(color='blue') + geom_line(show.legend=FALSE, color='blue') + theme_bw() +
                             xlab("SOM (g/kg)") + ylab("NUE (%)") +
                             theme(legend.title = element_text(size=10),
                                   legend.position = c(0.10,.15), 
                                   axis.text.x = element_text(color="black", size = 11), 
                                   axis.text.y = element_text(color="black", size = 11),
                                   axis.title = element_text(color="black", size = 15)) + ylim(0,100)
   
   # save plot
   #ggsave(plot = plot_nue_som, filename = 'products/nue_som.jpg')
       
                       
 # predict NUE under varying mean temperature 
 p4_temp <- predict.rma(meta2, newmods=cbind(c(25,25,25,25,25,25,25,25,25,25), # xclay
                                             c(35,35,35,35,35,35,35,35,35,35), # xsom
                                             c(3,6,9,12,15,18,21,24,27,30), # tmp_mean
                                             c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000), # pre_mean
                                             c(1,1,1,1,1,1,1,1,1,1),c(0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0), # nsource (inorganic)
                                             c(200,200,200,200,200,200,200,200,200,200), # n_dose
                                             c(200,200,200,200,200,200,200,200,200,200), # n_dose
                                             c(200,200,200,200,200,200,200,200,200,200),c(0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0))) # nsource (inorganic)
                                             
                                             
                                             
   # bind model estimates together
   
   # create dataframe for the predicted estimates
   e4_temp <- as.data.frame(p4_temp)
   
   # extract vectors 
   
   # for nsource: efficiency
   # extract vectors pred, ci.lb and ci.ub from the model estimates with n source 'efficiency'
   e4_temp.df <- data.frame(e4_temp)
   nue <- unlist(e4_temp.df$pred, use.names=FALSE)
   cil <- unlist(e4_temp.df$ci.lb, use.names=FALSE)
   ciu <- unlist(e4_temp.df$ci.ub, use.names=FALSE)
   se <- unlist(e4_temp.df$se, use.names=FALSE)
   
   # create vectors for the other covariables
   xclay <- rep(25,10)
   xsom <- rep(35,10)
   tmp <- seq(3,30,3)
   pre <- rep(1000,10)
   nsource <- rep("inorganic",10)
   ndose <- rep(200,10) 
   
   # combine extractions and vectors
   nue_e4_temp <- data.table(nue,se,cil,ciu,xclay,xsom,tmp,pre,nsource,ndose)
   
   # visualise the predictions for nue_e1_all 
   plot_nue_temp <- ggplot(data=nue_e4_temp,aes(x=(tmp),y=nue)) +
                            geom_point(color='blue') + geom_line(show.legend=FALSE, color='blue') + theme_bw() +
                            xlab("Mean temperature (*C)") + ylab("NUE (%)") +
                            theme(legend.title = element_text(size=10),
                                  legend.position = c(0.10,.15), 
                                  axis.text.x = element_text(color="black", size = 11), 
                                  axis.text.y = element_text(color="black", size = 11),
                                  axis.title = element_text(color="black", size = 15)) + ylim(0,100)
                          
   # save plot
   #ggsave(plot = plot_nue_temp, filename = 'products/nue_temp.jpg') 
 
   
 # predict NUE under varying mean precipitation
 p5_prec <- predict.rma(meta2, newmods=cbind(c(25,25,25,25,25,25,25,25,25,25), # xclay
                                             c(35,35,35,35,35,35,35,35,35,35), # xsom
                                             c(15,15,15,15,15,15,15,15,15,15), # tmp_mean
                                             c(200,400,600,800,1000,1200,1400,1600,1800,2000), # pre_mean
                                             c(1,1,1,1,1,1,1,1,1,1),c(0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0), # nsource (inorganic)
                                             c(200,200,200,200,200,200,200,200,200,200), # n_dose
                                             c(200,200,200,200,200,200,200,200,200,200), # n_dose
                                             c(200,200,200,200,200,200,200,200,200,200),c(0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0))) # nsource (inorganic)

   # bind model estimates together
   
   # create dataframe for the predicted estimates
   e5_prec <- as.data.frame(p5_prec)
   
   # extract vectors 
   
   # for nsource: efficiency
   # extract vectors pred, ci.lb and ci.ub from the model estimates with n source 'efficiency'
   e5_prec.df <- data.frame(e5_prec)
   nue <- unlist(e5_prec.df$pred, use.names=FALSE)
   cil <- unlist(e5_prec.df$ci.lb, use.names=FALSE)
   ciu <- unlist(e5_prec.df$ci.ub, use.names=FALSE)
   se <- unlist(e5_prec.df$se, use.names=FALSE)
   
   # create vectors for the other covariables
   xclay <- rep(25,10)
   xsom <- rep(35,10)
   tmp <- rep(15,10)
   pre <- seq(200,2000,200)
   nsource <- rep("inorganic",10)
   ndose <- rep(200,10) 
   
   # combine extractions and vectors
   nue_e5_prec <- data.table(nue,se,cil,ciu,xclay,xsom,tmp,pre,nsource,ndose)
   
   # visualise the predictions for nue_e1_all 
   plot_nue_prec <- ggplot(data=nue_e5_prec,aes(x=(pre),y=nue)) +
                             geom_point(color='blue') + geom_line(show.legend=FALSE, color='blue') + theme_bw() +
                             xlab("Mean precipitation (mm/y)") + ylab("NUE (%)") +
                             theme(legend.title = element_text(size=10),
                                   legend.position = c(0.10,.15), 
                                   axis.text.x = element_text(color="black", size = 11), 
                                   axis.text.y = element_text(color="black", size = 11),
                                   axis.title = element_text(color="black", size = 15)) + ylim(0,100)
                           
   # save plot
   #ggsave(plot = plot_nue_prec, filename = 'products/nue_prec.jpg') 
                           
 
 
 
 
