# calculate proportional recruitment means and SDs
# from penguin diets (U.S. AMLR and Palmer LTER), U.S.AMLR surveys,
# and the fishery observer data

for(i.r in 1:length(unique(diet.len.meas$region.id)))
  for(i.sp in 1:length(unique(diet.len.meas$spp.id))){
    spp.region <- diet.len.meas[diet.len.meas$region.id == 
                  as.character(unique(diet.len.meas$region.id)[i.r]) & 
                  diet.len.meas$spp.id == as.character(unique(diet.len.meas$spp.id)[i.sp]),]
    if(nrow(spp.region) > 0){
      prMean <- as.data.frame(aggregate(spp.region$p41,list(spp.region$yr.id,spp.region$mo.id),mean))
      class(prMean$Group.1) <- class(prMean$Group.2) <- 'numeric'
      pMean <- prMean$x
      prSD <- aggregate(spp.region$p41,list(spp.region$yr.id,spp.region$mo.id),sd)
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
      }
    }


############################## 
gepe.copa.m1 <- propRec[propRec$spp == 'GEPE' & propRec$site =='COPA' & propRec$mo == 1,]
gepe.copa.m2 <- propRec[propRec$spp == 'GEPE' & propRec$site =='COPA' & propRec$mo == 2,]
gepe.cs.m1 <- propRec[propRec$spp == 'GEPE' & propRec$site =='CS' & propRec$mo == 1,]
gepe.cs.m2 <- propRec[propRec$spp == 'GEPE' & propRec$site =='CS' & propRec$mo == 2,]
adpe.copa.m1 <- propRec[propRec$spp == 'ADPE' & propRec$site =='COPA' & propRec$mo == 1,]
adpe.copa.m2 <- propRec[propRec$spp == 'ADPE' & propRec$site =='COPA' & propRec$mo == 2,]
adpe.cs.m1 <- propRec[propRec$spp == 'ADPE' & propRec$site =='CS' & propRec$mo == 1,]
adpe.cs.m2 <- propRec[propRec$spp == 'ADPE' & propRec$site =='CS' & propRec$mo == 2,]
chpe.copa.m1 <- propRec[propRec$spp == 'CHPE' & propRec$site =='COPA' & propRec$mo == 1,]
chpe.copa.m2 <- propRec[propRec$spp == 'CHPE' & propRec$site =='COPA' & propRec$mo == 2,]
chpe.cs.m1 <- propRec[propRec$spp == 'CHPE' & propRec$site =='CS' & propRec$mo == 1,]
chpe.cs.m2 <- propRec[propRec$spp == 'CHPE' & propRec$site =='CS' & propRec$mo == 2,]
fsh.481N.m1 <- fshry[fshry$M==1 & fshry$ASD=='481N',]
fsh.481N.m2 <- fshry[fshry$M==2 & fshry$ASD=='481N',]
fsh.481S.m1 <- fshry[fshry$M==1 & fshry$ASD=='481S',]
fsh.481S.m2 <- fshry[fshry$M==2 & fshry$ASD=='481S',]
ade.m1 <- ade[ade$mo==1,]
ade.m2 <- ade[ade$mo==2,]
ade.m12<- ade[ade$mo==12,]
amlr.cs.1 <- propRec.amlr[propRec.amlr$area == 'WA' & propRec.amlr$leg == 'A',]
amlr.cs.2 <- propRec.amlr[propRec.amlr$area == 'WA' & propRec.amlr$leg == 'D',]
amlr.copa.1 <- propRec.amlr[propRec.amlr$area == 'SA' & propRec.amlr$leg == 'A',]
amlr.copa.2 <- propRec.amlr[propRec.amlr$area == 'SA' & propRec.amlr$leg == 'D',]


  plt.name <- paste('plots/','Predators (COPA & LTER) & fishery (481.S).pdf',sep='')
    pdf(file = plt.name)
    par(cex=1.3)
  plot(1992:2020,gepe.copa.m1$pMean[match(1992:2020,gepe.copa.m1$yr)],
  main=paste('Predators (COPA) & fishery (481.S)',sep=''), 
    xlab='Year',ylab = 'Proportional recruitment',ylim = c(0,1),xlim=c(1992,2020),col='red',pch=19)
  lines(1992:2020,gepe.copa.m1$pMean[match(1992:2020,gepe.copa.m1$yr)],col='red',lwd=2)
  points(1992:2020,gepe.copa.m2$pMean[match(1992:2020,gepe.copa.m2$yr)],col='red',pch=19)
  lines(1992:2020,gepe.copa.m2$pMean[match(1992:2020,gepe.copa.m2$yr)],col='red',lwd=2,lty=2)
  points(1992:2020,chpe.copa.m1$pMean[match(1992:2020,chpe.copa.m1$yr)],col='blue',pch=19)
  lines(1992:2020,chpe.copa.m1$pMean[match(1992:2020,chpe.copa.m1$yr)],col='blue',lwd=2)
  points(1992:2020,chpe.copa.m2$pMean[match(1992:2020,chpe.copa.m2$yr)],col='blue',pch=19)
  lines(1992:2020,chpe.copa.m2$pMean[match(1992:2020,chpe.copa.m2$yr)],col='blue',lwd=2,lty=2)
  points(1992:2020,adpe.copa.m1$pMean[match(1992:2020,adpe.copa.m1$yr)],col='green',pch=19)
  lines(1992:2020,adpe.copa.m1$pMean[match(1992:2020,adpe.copa.m1$yr)],col='green',lwd=2)
  points(1992:2020,adpe.copa.m2$pMean[match(1992:2020,adpe.copa.m2$yr)],col='green',pch=19)
  lines(1992:2020,adpe.copa.m2$pMean[match(1992:2020,adpe.copa.m2$yr)],col='green',lwd=2,lty=2)
  points(yrs,ade.m1.m,col='brown',pch=19)
  lines(yrs,ade.m1.m,col='brown',lwd=2)
  points(yrs,ade.m2.m,col='brown',pch=19)
  lines(yrs,ade.m2.m,col='brown',lwd=2,lty=2)
  points(fsh.481S.m1$Y,fsh.481S.m1$f,col='black',pch=17)
  lines(fsh.481S.m1$Y,fsh.481S.m1$f,col='black',lwd=3)
  points(fsh.481S.m2$Y,fsh.481S.m2$f,col='black',pch=17)
  lines(fsh.481S.m2$Y,fsh.481S.m2$f,col='black',lwd=3,lty=2)

  legend.placement <- c(2014,1.03)
  legend(legend.placement[1],legend.placement[2],
    c('gepe.co','chpe.co','adpe.co','adpe.lter',
    'fshry'),lty=c(1),pch=c(19,19,19,19,17),merge=T,
    lwd=c(2,2,2,2,3),col=c('red','blue','green','brown','black'),cex=0.65)
  graphics.off()

  plt.name <- paste('plots/','Predators (CAPE & LTER) & fishery (481.N).pdf',sep='')
    pdf(file = plt.name)
    par(cex=1.3)
  plot(1992:2020,gepe.cs.m1$pMean[match(1992:2020,gepe.cs.m1$yr)],
  main=paste('Predators (CS) & fishery (481.N)',sep=''), 
    xlab='Year',ylab = 'Proportional recruitment',ylim = c(0,1),xlim=c(1992,2020),col='red',pch=19)
  lines(1992:2020,gepe.cs.m1$pMean[match(1992:2020,gepe.cs.m1$yr)],col='red',lwd=2)
  points(1992:2020,gepe.cs.m2$pMean[match(1992:2020,gepe.cs.m2$yr)],col='red',pch=19)
  lines(1992:2020,gepe.cs.m2$pMean[match(1992:2020,gepe.cs.m2$yr)],col='red',lwd=2,lty=2)
  points(1992:2020,chpe.cs.m1$pMean[match(1992:2020,chpe.cs.m1$yr)],col='blue',pch=19)
  lines(1992:2020,chpe.cs.m1$pMean[match(1992:2020,chpe.cs.m1$yr)],col='blue',lwd=2)
  points(1992:2020,chpe.cs.m2$pMean[match(1992:2020,chpe.cs.m2$yr)],col='blue',pch=19)
  lines(1992:2020,chpe.cs.m2$pMean[match(1992:2020,chpe.cs.m2$yr)],col='blue',lwd=2,lty=2)
  points(1992:2020,adpe.cs.m1$pMean[match(1992:2020,adpe.cs.m1$yr)],col='green',pch=19)
  lines(1992:2020,adpe.cs.m1$pMean[match(1992:2020,adpe.cs.m1$yr)],col='green',lwd=2)
  points(1992:2020,adpe.cs.m2$pMean[match(1992:2020,adpe.cs.m2$yr)],col='green',pch=19)
  lines(1992:2020,adpe.cs.m2$pMean[match(1992:2020,adpe.cs.m2$yr)],col='green',lwd=2,lty=2)
  points(yrs,ade.m1.m,col='brown',pch=19)
  lines(yrs,ade.m1.m,col='brown',lwd=2)
  points(yrs,ade.m2.m,col='brown',pch=19)
  lines(yrs,ade.m2.m,col='brown',lwd=2,lty=2)
  points(fsh.481N.m1$Y,fsh.481N.m1$f,col='black',pch=17)
  lines(fsh.481N.m1$Y,fsh.481N.m1$f,col='black',lwd=3)
  points(fsh.481N.m2$Y,fsh.481N.m2$f,col='black',pch=17)
  lines(fsh.481N.m2$Y,fsh.481N.m2$f,col='black',lwd=3,lty=2)

  legend.placement <- c(2014,1.03)
  legend(legend.placement[1],legend.placement[2],
    c('gepe.cs','chpe.cs','adpe.cs','adpe.lter',
    'fshry'),lty=c(1),pch=c(19,19,19,19,17),merge=T,
    lwd=c(2,2,2,2,3),col=c('red','blue','green','brown','black'),cex=0.7)
  graphics.off()

  plt.name <- paste('plots/','AMLR and LTER surveys.pdf',sep='')
    pdf(file = plt.name)
    par(cex=1.3)
  plot(1992:2020,amlr.cs.1$mean[match(1992:2020,amlr.cs.1$yr)],
  main=paste('AMLR surveys and LTER adelies',sep=''), 
    xlab='Year',ylab = 'Proportional recruitment',ylim = c(0,1),xlim=c(1991,2020),col='red',pch=19)
  lines(1992:2020,amlr.cs.1$mean[match(1992:2020,amlr.cs.1$yr)],col='red',lwd=2)
  points(1992:2020,amlr.cs.2$mean[match(1992:2020,amlr.cs.2$yr)],col='red',pch=19)
  lines(1992:2020,amlr.cs.2$mean[match(1992:2020,amlr.cs.2$yr)],col='red',lwd=2,lty=2)
  points(1992:2020,amlr.copa.1$mean[match(1992:2020,amlr.copa.1$yr)],col='blue',pch=19)
  lines(1992:2020,amlr.copa.1$mean[match(1992:2020,amlr.copa.1$yr)],col='blue',lwd=2)
  points(1992:2020,amlr.copa.2$mean[match(1992:2020,amlr.copa.2$yr)],col='blue',pch=19)
  lines(1992:2020,amlr.copa.2$mean[match(1992:2020,amlr.copa.2$yr)],col='blue',lwd=2,lty=2)
  points(yrs,ade.m1.m,col='brown',pch=19)
  lines(yrs,ade.m1.m,col='brown',lwd=2)
  points(yrs,ade.m2.m,col='brown',pch=19)
  lines(yrs,ade.m2.m,col='brown',lwd=2,lty=2)

  legend.placement <- c(2014,1.0)
  legend(legend.placement[1],legend.placement[2],
    c('amlr.cs','amlr.co','adpe.lter'),
    lty=c(1),pch=c(19,19,19),merge=T,
    lwd=c(2),col=c('red','blue','brown'),cex=0.7)
  graphics.off()

