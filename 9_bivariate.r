if(juv.l==30) adult.age <- 1.4 # von Bert age at length
if(juv.l==35) adult.age <- 1.8
if(juv.l==38) adult.age <- 2.1
if(juv.l==40) adult.age <- 2.3
if(juv.l==44) adult.age <- 2.8

###################
if(nareas == 1){
  data.sources <- list(gepe.m1$pMean,chpe.m1$pMean,adpe.m1$pMean,
        ade.m1.m,fsh.m1$f,amlr.1$mean,lter.trwl)
  names(data.sources) <- c('gepe','chpe','adpe',
    'adpe.LTER','fsh.481','amlr.trwl','lter.trwl')

  plot.x <- plot.y <- vector()
  for (i in 1:length(data.sources)){
    plot.x[i] <- mean(data.sources[[i]])
    plot.y[i] <- sd(data.sources[[i]])
    }
  plot.xy <- cbind(c(1:length(data.sources)),plot.x,plot.y)

  c7 <- c('red','blue','green','brown','black','blue','purple') # plot colors for 7 data sources
  plt.name <- paste('plots_',juv.l,'mm/propRec_nareas_',nareas,'_agg.pdf',sep='')
  pdf(file = plt.name)
  par(cex=1.3)
  plot(plot.x,plot.y, main = paste('Juveniles <= ',juv.l,
    ' mm','\n(age ',adult.age,' years)',sep=''),lwd=3,xlim = c(0.1,0.77),
    cex = 1.5,ylim=c(0,0.3),ylab = 'SD', 
    xlab = 'mean proportional recruitment',pch=1:10,col=c7
    )
  if(juv.l == 30)
    legend(0.3,0.16,names(data.sources),pch=1:8,col=c7,cex=1,pt.lwd=2,text.font=2)
  if(juv.l == 35)
    legend(0.4,0.19,names(data.sources),pch=1:8,col=c7,cex=1,pt.lwd=2,text.font=2)
  if(juv.l == 38)
    legend(0.5,0.15,names(data.sources),pch=1:8,col=c7,cex=1,pt.lwd=2,text.font=2)
  if(juv.l == 40)
    legend(0.55,0.19,names(data.sources),pch=1:8,col=c7,cex=1,pt.lwd=2,text.font=2)
  if(juv.l == 44)
    legend(0.5,0.15,names(data.sources),pch=1:8,col=c7,cex=1,pt.lwd=2,text.font=2)
  graphics.off()
  }# end if(nareas == 1)
##############################

if(nareas == 4){
  data.sources <- list(gepe.copa.m1$pMean,chpe.copa.m1$pMean,adpe.copa.m1$pMean,
    gepe.cs.m1$pMean,chpe.cs.m1$pMean,
    ade.m1.m,fsh.481N.m1$f,fsh.481N.m1$f,
    amlr.cs.1$mean,amlr.copa.1$mean,lter.trwl)
    
  names(data.sources) <- c('gepe.copa','chpe.copa','adpe.copa',
    'gepe.cape','chpe.cape','adpe.LTER',
    'fsh.481S','fsh.481N','amlr.trwl.cape','amlr.trwl.copa','lter.trwl')

  #if(nlegs == 2){
  #    data.sources <-c(data.sources,list(fsh.481S.m2=fsh.481S.m2$f,fsh.481N.m2=fsh.481N.m2$f))
  #    }
  plot.x <- plot.y <- vector()
  for (i in 1:length(data.sources)){
    plot.x[i] <- mean(data.sources[[i]])
    plot.y[i] <- sd(data.sources[[i]])
    }
  plot.xy <- cbind(c(1:length(data.sources),plot.x,plot.y))

  c11 <- c('red','blue','green','red','blue','brown','black','black',
         'red','blue','purple') # plot colors for 11 data sources
  if(nlegs==2){
  plt.name <- paste('plots_',juv.l,'mm/propRec_nareas_',nareas,'_m1&2.pdf',sep='')
  pdf(file = plt.name)
  par(cex=1.3)
  plot(plot.x,plot.y, main = paste('Juveniles <= ',juv.l,' mm',
    '\n(age ',adult.age,' years)',sep=''),lwd=2,
    ylab = 'SD', xlab = 'mean proportional recruitment',pch=1:11,col=c11 ,ylim=c(0,0.3)
    )
  if(juv.l == 30)
    legend(0.3,0.17,names(data.sources),pch=1:length(c11),
    col=c11,cex=0.8,pt.lwd=2,text.font=2)
  if(juv.l == 35)
    legend(0.4,0.19,names(data.sources),pch=1:length(c11),
    col=c11,cex=0.8,pt.lwd=2,text.font=2)
  if(juv.l == 38)
    legend(0.5,0.19,names(data.sources),pch=1:length(c11),
    col=c11,cex=0.8,pt.lwd=2,text.font=2)
  if(juv.l == 40)
    legend(0.53,0.19,names(data.sources),pch=1:length(c11),
    col=c11,cex=0.8,pt.lwd=2,text.font=2)
  if(juv.l == 44)
    legend(0.63,0.19,names(data.sources),pch=1:length(c11),
    col=c11,cex=0.8,pt.lwd=2,text.font=2)
  }
  graphics.off()
  }# end if(nareas == 4)

