# calculate proportional recruitment means and SDs
# from penguin diets (U.S. AMLR)
# 'diet.len.meas' (dim = 3130 x  67) is a data.frame of individual
#  diet samples
# 'spSite' calculates mean and SD for each species and site
# 'propRec' (dim = 234 x  6) combines all individuals for each
# species, site and year

plot.mos <- 1 # 1 to plot only January, 2 to plot January and February
if(nareas == 4){
for(i.r in 1:length(unique(diet.len.meas$region.id)))
  for(i.sp in 1:length(unique(diet.len.meas$spp.id))){
    spp.region <- diet.len.meas[diet.len.meas$region.id == 
                  as.character(unique(diet.len.meas$region.id)[i.r]) & 
                  diet.len.meas$spp.id == as.character(unique(diet.len.meas$spp.id)[i.sp]),]
    if(nrow(spp.region) > 0){ # if the species occurs at the site
        prMean <- as.data.frame(aggregate(spp.region$juvl,
                                        list(spp.region$yr.id,spp.region$mo.id),mean))
        class(prMean$Group.1) <- class(prMean$Group.2) <- 'numeric'
        pMean <- prMean$x
        prSD <- aggregate(spp.region$juvl,list(spp.region$yr.id,spp.region$mo.id),sd)
      class(prSD$Group.1) <- class(prSD$Group.2) <- 'numeric'
      pSD <- prSD$x
      spSite <- as.data.frame(cbind(pMean=as.numeric(round(pMean,3)),
                  pSD=round(pSD,3),
                  yr=as.numeric(levels(prMean$Group.1))[prMean$Group.1],
                  mo=as.numeric(levels(prMean$Group.2))[prMean$Group.2],
                  spp = as.character(unique(diet.len.meas[,'spp.id'])[i.sp]), 
                  site = as.character(unique(diet.len.meas[,'region.id'])[i.r]))
                  )
      spSite$pMean <- as.numeric(spSite$pMean) # change columns from character to numeric
      spSite$pSD <- as.numeric(spSite$pSD)
      spSite$yr <- as.numeric(spSite$yr)
      spSite$mo <- as.numeric(spSite$mo)

      ifelse (i.r==1 & i.sp==1,
        propRec <-spSite,
      if(nrow(spSite)>0)
        propRec <- rbind(propRec,spSite))
    } # end nrow(spp.region) > 0
    } # end for loops
write.csv(propRec,paste('propRec_csvs/pengDiets_',juv.l,'mm.csv',sep=''))

# extract named subsets of each predator, survey, and fisheries dataset
# by month (Jan, Feb) and area ('COPA', 'CS' for AMLR pengins;
# '481N', '481S' for the fishery; Western area ('WA') and Southern Area
# ('SA') for AMLR surveys
##############################
# AMLR penguin diets
gepe.copa.m1 <- propRec[propRec$spp == 'GEPE' & propRec$site =='COPA' & propRec$mo == 1,]
gepe.copa.m2 <- propRec[propRec$spp == 'GEPE' & propRec$site =='COPA' & propRec$mo == 2,]
gepe.cs.m1 <- propRec[propRec$spp == 'GEPE' & propRec$site =='CS' & propRec$mo == 1,]
gepe.cs.m2 <- propRec[propRec$spp == 'GEPE' & propRec$site =='CS' & propRec$mo == 2,]
adpe.copa.m1 <- propRec[propRec$spp == 'ADPE' & propRec$site =='COPA' & propRec$mo == 1,]
adpe.copa.m2 <- propRec[propRec$spp == 'ADPE' & propRec$site =='COPA' & propRec$mo == 2,]
chpe.copa.m1 <- propRec[propRec$spp == 'CHPE' & propRec$site =='COPA' & propRec$mo == 1,]
chpe.copa.m2 <- propRec[propRec$spp == 'CHPE' & propRec$site =='COPA' & propRec$mo == 2,]
chpe.cs.m1 <- propRec[propRec$spp == 'CHPE' & propRec$site =='CS' & propRec$mo == 1,]
chpe.cs.m2 <- propRec[propRec$spp == 'CHPE' & propRec$site =='CS' & propRec$mo == 2,]
# fishery observer data
fsh.481N.m1 <- fshry[fshry$M==1 & fshry$ASD=='481N',]
fsh.481N.m2 <- fshry[fshry$M==2 & fshry$ASD=='481N',]
fsh.481S.m1 <- fshry[fshry$M==1 & fshry$ASD=='481S',]
fsh.481S.m2 <- fshry[fshry$M==2 & fshry$ASD=='481S',]
ade.m1 <- ade[ade$mo==1,] # LTER adelies
ade.m2 <- ade[ade$mo==2,]
ade.m12<- ade[ade$mo==12,]
lter.trwl <- lter.pRec     # LTER trawl surveys 2009-2019
# AMLR tawl surveys
amlr.cs.1 <- propRec.amlr[propRec.amlr$area == 'WA' & propRec.amlr$leg == 'A',]
amlr.cs.2 <- propRec.amlr[propRec.amlr$area == 'WA' & propRec.amlr$leg == 'D',]
amlr.copa.1 <- propRec.amlr[propRec.amlr$area == 'SA' & propRec.amlr$leg == 'A',]
amlr.copa.2 <- propRec.amlr[propRec.amlr$area == 'SA' & propRec.amlr$leg == 'D',]

############################## 
# plot proportional recruitment means
# from penguin diets (U.S. AMLR and Palmer LTER), U.S.AMLR surveys,
# and the fishery observer data
  plt.name <- paste('plots_',juv.l,'mm/','Penguin Diets ',juv.l,'mm.pdf',sep='')
    pdf(file = plt.name)
    par(cex=1.3)
  plot(1992:2020,gepe.copa.m1$pMean[match(1992:2020,gepe.copa.m1$yr)],
  main=paste('Predators (COPA) & fishery (481.S)',sep=''), 
    xlab='Year',ylab = 'Proportional recruitment',ylim = c(0,1),xlim=c(1992,2020),col='red',pch=19)
  lines(1992:2020,gepe.copa.m1$pMean[match(1992:2020,gepe.copa.m1$yr)],col='red',lwd=2)
  points(1992:2020,chpe.copa.m1$pMean[match(1992:2020,chpe.copa.m1$yr)],col='blue',pch=19)
  lines(1992:2020,chpe.copa.m1$pMean[match(1992:2020,chpe.copa.m1$yr)],col='blue',lwd=2)
  points(1992:2020,adpe.copa.m1$pMean[match(1992:2020,adpe.copa.m1$yr)],col='green',pch=19)
  lines(1992:2020,adpe.copa.m1$pMean[match(1992:2020,adpe.copa.m1$yr)],col='green',lwd=2)
  points(yrs,ade.m1.m,col='brown',pch=19)
  lines(yrs,ade.m1.m,col='brown',lwd=2)
  points(fsh.481S.m1$Y,fsh.481S.m1$f,col='black',pch=17)
  lines(fsh.481S.m1$Y,fsh.481S.m1$f,col='black',lwd=3)

  if(plot.mos == 2){
  points(1992:2020,chpe.copa.m2$pMean[match(1992:2020,chpe.copa.m2$yr)],col='blue',pch=19)
  lines(1992:2020,chpe.copa.m2$pMean[match(1992:2020,chpe.copa.m2$yr)],col='blue',lwd=2,lty=2)
  points(1992:2020,gepe.copa.m2$pMean[match(1992:2020,gepe.copa.m2$yr)],col='red',pch=19)
  lines(1992:2020,gepe.copa.m2$pMean[match(1992:2020,gepe.copa.m2$yr)],col='red',lwd=2,lty=2)
  points(1992:2020,adpe.copa.m2$pMean[match(1992:2020,adpe.copa.m2$yr)],col='green',pch=19)
  lines(1992:2020,adpe.copa.m2$pMean[match(1992:2020,adpe.copa.m2$yr)],col='green',lwd=2,lty=2)
  points(yrs,ade.m2.m,col='brown',pch=19)
  lines(yrs,ade.m2.m,col='brown',lwd=2,lty=2)
  points(fsh.481S.m2$Y,fsh.481S.m2$f,col='black',pch=17)
  lines(fsh.481S.m2$Y,fsh.481S.m2$f,col='black',lwd=3,lty=2)
  }

  legend.placement <- c(2014,1.03)
  legend(legend.placement[1],legend.placement[2],
    c('gepe.copa','chpe.copa','adpe.copa','adpe.lter',
    'fshry.481S'),lty=c(1),pch=c(19,19,19,19,17),merge=T,
    lwd=c(2,2,2,2,3),col=c('red','blue','green','brown','black'),cex=0.65)
  graphics.off()

  plt.name <- paste('plots_',juv.l,'mm/','Predators (CAPE & LTER) & fishery (481.N) ',juv.l,'mm.pdf',sep='')
    pdf(file = plt.name)
    par(cex=1.3)
  plot(1992:2020,gepe.cs.m1$pMean[match(1992:2020,gepe.cs.m1$yr)],
  main=paste('Predators (CS) & fishery (481.N)',sep=''), 
    xlab='Year',ylab = 'Proportional recruitment',ylim = c(0,1),xlim=c(1992,2020),col='red',pch=19)
  lines(1992:2020,gepe.cs.m1$pMean[match(1992:2020,gepe.cs.m1$yr)],col='red',lwd=2)
  points(1992:2020,chpe.cs.m1$pMean[match(1992:2020,chpe.cs.m1$yr)],col='blue',pch=19)
  lines(1992:2020,chpe.cs.m1$pMean[match(1992:2020,chpe.cs.m1$yr)],col='blue',lwd=2)
  points(yrs,ade.m1.m,col='brown',pch=19)
  lines(yrs,ade.m1.m,col='brown',lwd=2)
  points(fsh.481N.m1$Y,fsh.481N.m1$f,col='black',pch=17)
  lines(fsh.481N.m1$Y,fsh.481N.m1$f,col='black',lwd=3)

  if(plot.mos == 2){
  points(1992:2020,gepe.cs.m2$pMean[match(1992:2020,gepe.cs.m2$yr)],col='red',pch=19)
  lines(1992:2020,gepe.cs.m2$pMean[match(1992:2020,gepe.cs.m2$yr)],col='red',lwd=2,lty=2)
  points(1992:2020,chpe.cs.m2$pMean[match(1992:2020,chpe.cs.m2$yr)],col='blue',pch=19)
  lines(1992:2020,chpe.cs.m2$pMean[match(1992:2020,chpe.cs.m2$yr)],col='blue',lwd=2,lty=2)
  points(yrs,ade.m2.m,col='brown',pch=19)
  lines(yrs,ade.m2.m,col='brown',lwd=2,lty=2)
  points(fsh.481.m2$Y,fsh.481N.m2$f,col='black',pch=17)
  lines(fsh.481.m2$Y,fsh.481N.m2$f,col='black',lwd=3,lty=2)
  }

  #if(nareas == 4 & juv.l == 44){
  legend.placement <- c(2014,1.03)
  legend(legend.placement[1],legend.placement[2],
    c('gepe.cape','chpe.cape','adpe.lter',
    'fshry'),lty=c(1),pch=c(19,19,19,19,17),merge=T,
    lwd=c(2,2,2,2,3),col=c('red','blue','brown','black'),cex=0.7)
  graphics.off()
 # }

  plt.name <- paste('plots_',juv.l,'mm/','AMLR trawls, LTER trawls, and Fishery.pdf',sep='')
    pdf(file = plt.name)
    par(cex=1.3)
  plot(1992:2020,amlr.cs.1$mean[match(1992:2020,amlr.cs.1$yr)],
  main=paste('AMLR trawls, LTER trawls and Fishery',sep=''), 
    xlab='Year',ylab = 'Proportional recruitment',ylim = c(0,1),xlim=c(1991,2020),col='red',pch=19)
  lines(1992:2020,amlr.cs.1$mean[match(1992:2020,amlr.cs.1$yr)],col='red',lwd=2)
  points(1992:2020,amlr.copa.1$mean[match(1992:2020,amlr.copa.1$yr)],col='blue',pch=19)
  lines(1992:2020,amlr.copa.1$mean[match(1992:2020,amlr.copa.1$yr)],col='blue',lwd=2)
  points(1992:2020,lter.trwl[match(1992:2020,names(lter.trwl))],col='brown',pch=19)
  lines(1992:2020,lter.trwl[match(1992:2020,names(lter.trwl))],col='brown',lwd=2,lty=1)
  points(fshry$Y,fshry$f,col='black',pch=17)
  lines(fshry$Y,fshry$f,col='black',lwd=3)

  if(plot.mos == 2){
  points(1992:2020,amlr.copa.2$mean[match(1992:2020,amlr.copa.2$yr)],col='blue',pch=19)
  lines(1992:2020,amlr.copa.2$mean[match(1992:2020,amlr.copa.2$yr)],col='blue',lwd=2,lty=2)
  points(1992:2020,amlr.cs.2$mean[match(1992:2020,amlr.cs.2$yr)],col='red',pch=19)
  lines(1992:2020,amlr.cs.2$mean[match(1992:2020,amlr.cs.2$yr)],col='red',lwd=2,lty=2)
  points(1992:2020,lter.trwl[match(1992:2020,names(lter.trwl))],col='brown',pch=19)
  lines(1992:2020,lter.trwl[match(1992:2020,names(lter.trwl))],col='brown',lwd=2,lty=1)
  points(fshry$Y,fsh.481N.m1$f,col='black',pch=17)
  lines(fshry$Y,fsh.481N.m1$f,col='black',lwd=3)
  }

  legend.placement <- c(2014,1.0)
  legend(legend.placement[1],legend.placement[2],
    c('amlr.cape','amlr.copa','lter.trwls','fshry'),
    lty=c(1),pch=c(19,19,19,19),merge=T,
    lwd=c(2),col=c('red','blue','brown','black'),cex=0.7)
  graphics.off()
} # end nareas == 4
#####################################

if(nareas == 1){
  for(i.sp in 1:length(unique(diet.len.meas$spp.id))){
    spp.region <- diet.len.meas[ 
                  diet.len.meas$spp.id == as.character(unique(diet.len.meas$spp.id)[i.sp]),]
    if(nrow(spp.region) > 0){ # if the species occurs at the site
        prMean <- as.data.frame(aggregate(spp.region$juvl,
                                        list(spp.region$yr.id,spp.region$mo.id),mean))
            class(prMean$Group.1) <- class(prMean$Group.2) <- 'numeric'
        pMean <- prMean$x
        prSD <- aggregate(spp.region$juvl,list(spp.region$yr.id,spp.region$mo.id),sd)

    class(prSD$Group.1) <- class(prSD$Group.2) <- 'numeric'
      pSD <- prSD$x
      spSite <- as.data.frame(cbind(pMean=as.numeric(round(pMean,3)),
                  pSD=round(pSD,3),
                  yr=as.numeric(levels(prMean$Group.1))[prMean$Group.1],
                  mo=as.numeric(levels(prMean$Group.2))[prMean$Group.2],
                  spp = as.character(unique(diet.len.meas[,'spp.id'])[i.sp])))
                  
      spSite$pMean <- as.numeric(spSite$pMean) # change columns from character to numeric
      spSite$pSD <- as.numeric(spSite$pSD)
      spSite$yr <- as.numeric(spSite$yr)
      spSite$mo <- as.numeric(spSite$mo)

      ifelse (i.sp==1,
        propRec <-spSite,
      if(nrow(spSite)>0)
        propRec <- rbind(propRec,spSite))
    } # end nrow(spp.region) > 0
    } # end for loops
write.csv(propRec,paste('propRec_csvs/pengDiets_',juv.l,'mm_agg.csv',sep=''))

# extract named subsets of each predator, survey, and fisheries dataset
# by month (Jan, Feb) and area ('COPA', 'CS' for AMLR pengins;
# '481N', '481S' for the fishery; Western area ('WA') and Southern Area
# ('SA') for AMLR surveys
############################## 
# AMLR penguins
gepe.m1 <- propRec[propRec$spp == 'GEPE' & propRec$mo == 1,]
gepe.m2 <- propRec[propRec$spp == 'GEPE' & propRec$mo == 2,]
adpe.m1 <- propRec[propRec$spp == 'ADPE' & propRec$mo == 1,]
adpe.m2 <- propRec[propRec$spp == 'ADPE' & propRec$mo == 2,]
chpe.m1 <- propRec[propRec$spp == 'CHPE' & propRec$mo == 1,]
chpe.m2 <- propRec[propRec$spp == 'CHPE' & propRec$mo == 2,]
# fishery observer data
fsh.m1 <- fshry[fshry$M==1,]
fsh.m2 <- fshry[fshry$M==2,]
# LTER penguins
ade.m1 <- ade[ade$mo==1,]
ade.m2 <- ade[ade$mo==2,]
ade.m12<- ade[ade$mo==12,]
lter.trwl <- lter.pRec     # LTER trawl surveys 2009-2019
# AMLR trawls
amlr.1 <- propRec.amlr[propRec.amlr$leg == 'A',]
amlr.2 <- propRec.amlr[propRec.amlr$leg == 'D',]

############################## 
# plot proportional recruitment means
# from penguin diets (U.S. AMLR and Palmer LTER), U.S. AMLR trawl surveys,
# Palmer LTER trawl surveys, and the fishery observer data
  plt.name <- paste('plots_',juv.l,'mm/','Penguin Diets ',juv.l,'mm_agg.pdf',sep='')
    pdf(file = plt.name)
    par(cex=1.3)
  plot(1992:2020,gepe.m1$pMean[match(1992:2020,gepe.m1$yr)],
  main=paste('Penguin Diets',sep=''), 
    xlab='Year',ylab = 'Proportional recruitment',ylim = c(0,1),xlim=c(1992,2020),col='red',pch=19)
  lines(1992:2020,gepe.m1$pMean[match(1992:2020,gepe.m1$yr)],col='red',lwd=2)
  points(1992:2020,chpe.m1$pMean[match(1992:2020,chpe.m1$yr)],col='blue',pch=19)
  lines(1992:2020,chpe.m1$pMean[match(1992:2020,chpe.m1$yr)],col='blue',lwd=2)
  points(1992:2020,adpe.m1$pMean[match(1992:2020,adpe.m1$yr)],col='green',pch=19)
  lines(1992:2020,adpe.m1$pMean[match(1992:2020,adpe.m1$yr)],col='green',lwd=2)
  points(yrs,ade.m1.m,col='brown',pch=19)
  lines(yrs,ade.m1.m,col='brown',lwd=2)

  if(plot.mos == 2){
  points(1992:2020,chpe.m2$pMean[match(1992:2020,chpe.m2$yr)],col='blue',pch=19)
  lines(1992:2020,chpe.m2$pMean[match(1992:2020,chpe.m2$yr)],col='blue',lwd=2,lty=2)
  points(1992:2020,gepe.m2$pMean[match(1992:2020,gepe.m2$yr)],col='red',pch=19)
  lines(1992:2020,gepe.m2$pMean[match(1992:2020,gepe.m2$yr)],col='red',lwd=2,lty=2)
  points(1992:2020,adpe.m2$pMean[match(1992:2020,adpe.m2$yr)],col='green',pch=19)
  lines(1992:2020,adpe.m2$pMean[match(1992:2020,adpe.m2$yr)],col='green',lwd=2,lty=2)
  points(yrs,ade.m2.m,col='brown',pch=19)
  lines(yrs,ade.m2.m,col='brown',lwd=2,lty=2)
  }

  legend.placement <- c(2014,1.03)
  legend(legend.placement[1],legend.placement[2],
    c('gepe','chpe','adpe','adpe.lter'),
     lty=c(1),pch=c(19,19,19,19,17),merge=T,
    lwd=c(2,2,2,2,3),col=c('red','blue','green','brown','black'),cex=0.65)
  graphics.off()


  plt.name <- paste('plots_',juv.l,'mm/AMLR trawls, LTER trawls, and Fishery agg.pdf',sep='')
    pdf(file = plt.name)
    par(cex=1.3)
  plot(1992:2020,amlr.1$mean[match(1992:2020,amlr.1$yr)],
  main=paste('AMLR trawls, LTER trawls, and Fishery',sep=''), 
    xlab='Year',ylab = 'Proportional recruitment',ylim = c(0,1),xlim=c(1991,2020),col='blue',pch=19)
  lines(1992:2020,amlr.1$mean[match(1992:2020,amlr.1$yr)],col='blue',lwd=2)
  points(1992:2020,lter.trwl[match(1992:2020,names(lter.trwl))],col='brown',pch=19)
  lines(1992:2020,lter.trwl[match(1992:2020,names(lter.trwl))],col='brown',lwd=2,lty=1)
  points(fsh.m1$Y,fsh.m1$f,col='black',pch=17)
  lines(fsh.m1$Y,fsh.m1$f,col='black',lwd=3)

  if(plot.mos == 2){
  points(1992:2020,amlr.2$mean[match(1992:2020,amlr.2$yr)],col='blue',pch=19)
  lines(1992:2020,amlr.2$mean[match(1992:2020,amlr.2$yr)],col='blue',lwd=2,lty=2)
  points(1992:2020,amlr.2$mean[match(1992:2020,amlr.2$yr)],col='red',pch=19)
  lines(1992:2020,amlr.2$mean[match(1992:2020,amlr.2$yr)],col='red',lwd=2,lty=2)
  points(yrs,lter.trwl[match(1992:2020,names(lter.trwl))],col='brown',pch=19)
  lines(yrs,lter.trwl[match(1992:2020,names(lter.trwl))],col='brown',lwd=2,lty=2)
  points(fsh.m1$Y,fsh.m2$f,col='black',pch=17)
  lines(fsh.m1$Y,fsh.m2$f,col='black',lwd=3)
 }

  legend.placement <- c(2014,1.0)
  legend(legend.placement[1],legend.placement[2],
    c('amlr.trwl','lter.trwl','fshry'),
    lty=c(1),pch=c(19,19,19),merge=T,
    lwd=c(2),col=c('blue','brown','black'),cex=0.7)
  graphics.off()
} # end nareas == 1
