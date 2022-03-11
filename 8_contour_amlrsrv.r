#was R2_contour_amlr2.txt on feb8
# from CCAMLR_2021/SAM/Kinzey_code_locations
# modified from may5_mac

######## combi
#oc.srv.meas <- read.csv('/users/noaa/GRYM/GYM_2021/5may/may19/oc_srv_meas.csv',
#               header=TRUE)
if(nlegs == 1 & nareas == 1){
if(!dir.exists(paste('plots_',juv.l,'mm/1area1leg',sep=''))){
    dir.create(paste('plots_',juv.l,'mm/1area1leg',sep=''))
    }
  l.min <- 2 # start of length data columns
  plt.name <- paste('plots_',juv.l,'mm/1area1leg/AMLR LFs measured.pdf',sep='')
  pdf(file = plt.name)
  par(cex=1.4) 
  contour(as.matrix(oc.srv.meas[,l.min:ncol(oc.srv.meas)]),
         x=oc.srv.meas[,'year'],y=13:60,
         xlab = "Year",labcex=1.3,ylim=c(20,60), 
         ylab= "Krill length (mm)",
         nlevels=20,main = 'AMLR LFs measured')
         abline(h=38,col='blue',lwd=3,lty=2)
         abline(h=44,col='blue',lwd=3,lty=2)
	 
  plt.name <- paste('plots_',juv.l,'mm/1area1leg/AMLR LFs scaled.pdf',sep='')
  pdf(file = plt.name)
  par(cex=1.4) 
  contour(as.matrix(oc.srv[,l.min:ncol(oc.srv)]),
         x=oc.srv[,'year'],y=13:60,
         xlab = "Year",labcex=1.3,ylim=c(20,60),
         ylab= "Krill length (mm)",
         nlevels=20,main = 'AMLR LFs scaled')
         abline(h=38,col='blue',lwd=3,lty=2)
         abline(h=44,col='blue',lwd=3,lty=2)
  }
  graphics.off()
######## 
if(nareas==1 & nlegs==2){
if(!dir.exists(paste('plots_',juv.l,'mm/1area2legs',sep=''))){
    dir.create(paste('plots_',juv.l,'mm/1area2legs',sep=''))
    }

  l.min <- 3
  amlr.set <- list()
  amlr.set[[1]] <- oc.srv[oc.srv$leg == 'A',]
  amlr.set[[2]] <- oc.srv[oc.srv$leg == 'D',]
  
for(leg in 1:length(amlr.set)){
    plt.name <- paste('plots_',juv.l,'mm/1area2legs/AMLR leg',
                      leg,' LFs scaled.pdf',sep='')
  pdf(file = plt.name)
  par(cex=1.4) 
  contour(as.matrix(amlr.set[[leg]][,l.min:ncol(amlr.set[[leg]])]),
         x=as.numeric(as.character(amlr.set[[leg]][,'year'])),
         y=13:60,xlab = "Year",labcex=1.3,
         ylab= "Krill length (mm)",ylim=c(20,60),
         nlevels=20,main = paste('AMLR leg ',leg,' LFs scaled'))
         abline(h=38,col='blue',lwd=3,lty=2)
         abline(h=44,col='blue',lwd=3,lty=2)
  }
  graphics.off()
  }
######## 
if(nareas==4 & nlegs==1){
if(!dir.exists(paste('plots_',juv.l,'mm/4areas1leg',sep=''))){
    dir.create(paste('plots_',juv.l,'mm/4areas1leg',sep=''))
    }
  l.min <- 4
  amlr.set <- list()
  amlr.set[[1]] <- oc.srv[oc.srv$area == 'EI',]
  amlr.set[[2]] <- oc.srv[oc.srv$area == 'JI',]
  amlr.set[[3]] <- oc.srv[oc.srv$area == 'SA',]
  amlr.set[[4]] <- oc.srv[oc.srv$area == 'WA',]
  
strata.nm <- c('EI','JI','SA','WA')
for(stratum in 1:length(amlr.set)){
  plt.name <- paste('plots_',juv.l,'mm/4areas1leg/AMLR ',
                    strata.nm[stratum],' LFs scaled.pdf',sep='')
  pdf(file = plt.name)
  par(cex=1.4) 
  contour(as.matrix(amlr.set[[stratum]][,l.min:ncol(amlr.set[[stratum]])]),
         x=as.numeric(as.character(amlr.set[[stratum]][,'year'])),
         y=13:60,xlab = "Year",labcex=1.3,ylim=c(20,60),
         ylab= "Krill length (mm)",
         nlevels=20,main = paste('AMLR area',strata.nm[stratum],'LFs scaled'))
         abline(h=38,col='blue',lwd=3,lty=2)
         abline(h=44,col='blue',lwd=3,lty=2)
  }
  graphics.off()
}
######## 
if(nareas==4 & nlegs==2){
if(!dir.exists(paste('plots_',juv.l,'mm/4areas2legs',sep=''))){
    dir.create(paste('plots_',juv.l,'mm/4areas2legs',sep=''))
    }
  l.min <- 4
  amlr.set <- list()
  amlr.set[[1]] <- oc.srv[oc.srv$area == 'EI' & oc.srv$leg == 'A',]
  amlr.set[[2]] <- oc.srv[oc.srv$area == 'JI' & oc.srv$leg == 'A',]
  amlr.set[[3]] <- oc.srv[oc.srv$area == 'SA' & oc.srv$leg == 'A',]
  amlr.set[[4]] <- oc.srv[oc.srv$area == 'WA' & oc.srv$leg == 'A',]
  amlr.set[[5]] <- oc.srv[oc.srv$area == 'EI' & oc.srv$leg == 'D',]
  amlr.set[[6]] <- oc.srv[oc.srv$area == 'JI' & oc.srv$leg == 'D',]
  amlr.set[[7]] <- oc.srv[oc.srv$area == 'SA' & oc.srv$leg == 'D',]
  amlr.set[[8]] <- oc.srv[oc.srv$area == 'WA' & oc.srv$leg == 'D',]
  
strata.nm <- c('EI Jan ','JI Jan ','SA Jan ','WA Jan ',
  'EI Feb ','JI Feb ','SA Feb ','WA Feb ')
for(stratum in 1:length(amlr.set)){
  plt.name <- paste('plots_',juv.l,'mm/4areas2legs/AMLR ',
                    strata.nm[stratum],' LFs scaled.pdf',sep='')
    pdf(file = plt.name)
  par(cex=1.4) 
  contour(as.matrix(amlr.set[[stratum]][,l.min:ncol(amlr.set[[stratum]])]),
         x=as.numeric(as.character(amlr.set[[stratum]][,'year'])),
         y=13:60,xlab = "Year",labcex=1.3,
         ylab= "Krill length (mm)",
         nlevels=20,main = paste('AMLR area',strata.nm[stratum],'LFs scaled'))
         abline(h=38,col='blue',lwd=3,lty=2)
         abline(h=44,col='blue',lwd=3,lty=2)
  }
  graphics.off()
  }

