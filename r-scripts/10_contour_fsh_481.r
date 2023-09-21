path.out <- paste(path.nm,'fshry/',sep='')
Kmv=read.csv(paste(path.nm,'data/KobsLFDs.csv',sep=''))
dim(Kmv)
if(!dir.exists('fshry')){
    dir.create('fshry')
    }
#######################
# month 1
fsh.len.scaled <- tapply(Kmv$fr,                 
                     list(Kmv$ASD,Kmv$L,Kmv$Y,Kmv$M==1), 
                     mean,na.rm=T)
#dim(fsh.len.scaled)
#[1]  4 51 11 2
s481N <- fsh.len.scaled[3,,,1] # 481N
s481S <- fsh.len.scaled[4,,,2] # 481S
s481_m1 <- (s481N + s481S)/2
oc.fsh.scaled <- as.data.frame(as.matrix(s481_m1))
 oc.fsh.scaled<-as.data.frame(as.matrix(t(oc.fsh.scaled)))
 oc.fsh.scaled <- oc.fsh.scaled[,-1] #zero length column
 
 plt.name <- paste('fshry/','481all_m1_juvl_.pdf',sep='')
 pdf(file = plt.name)
 par(cex=1.4) 
 contour(as.matrix(oc.fsh.scaled[,1:35]),x=as.numeric(rownames(oc.fsh.scaled)),
         y=as.numeric(colnames(oc.fsh.scaled[1:35])),xlab = "Year", 
         ylab= "Krill length (mm)",labcex=1.3,ylim=c(20,60),
         nlevels=20,main = 'Fishery month 1 ')
         abline(h=30,col='blue',lwd=3,lty=2)
         abline(h=44,col='blue',lwd=3,lty=2)
graphics.off()


#######################
# month 2
fsh.len.scaled <- tapply(Kmv$fr,                 
                     list(Kmv$ASD,Kmv$L,Kmv$Y,Kmv$M==2), 
                     mean,na.rm=T)
#dim(fsh.len.scaled)
#[1]  4 51 11 2 
s481N <- fsh.len.scaled[3,,,1] # 481N
s481S <- fsh.len.scaled[4,,,2] # 481S
s481_m1 <- (s481N + s481S)/2
oc.fsh.scaled <- as.data.frame(as.matrix(s481_m1))
 oc.fsh.scaled<-as.data.frame(as.matrix(t(oc.fsh.scaled)))
 oc.fsh.scaled <- oc.fsh.scaled[,-1] #zero length column
 
 plt.name <- paste('fshry/','481all_m2_juvl_.pdf',sep='')
 pdf(file = plt.name)
 par(cex=1.4) 
 contour(as.matrix(oc.fsh.scaled[,1:35]),x=as.numeric(rownames(oc.fsh.scaled)),
         y=as.numeric(colnames(oc.fsh.scaled[1:35])),xlab = "Year", 
         ylab= "Krill length (mm)",labcex=1.3,ylim=c(20,60),
         nlevels=20,main = 'Fishery month 2 ')
         abline(h=30,col='blue',lwd=3,lty=2)
         abline(h=44,col='blue',lwd=3,lty=2)
graphics.off()

#######################
# month 1 & 2
fsh.len.scaled <- tapply(Kmv$fr,                 
                     list(Kmv$ASD,Kmv$L,Kmv$Y,Kmv$M==1 | Kmv$M==2), 
                     mean,na.rm=T)
#dim(fsh.len.scaled)
#[1]  4 51 11 2
s481N <- fsh.len.scaled[3,,,1] # 481N
s481S <- fsh.len.scaled[4,,,2] # 481S
s481_m1 <- (s481N + s481S)/2
oc.fsh.scaled <- as.data.frame(as.matrix(s481_m1))
 oc.fsh.scaled<-as.data.frame(as.matrix(t(oc.fsh.scaled)))
 oc.fsh.scaled <- oc.fsh.scaled[,-1] #zero length column
 
 plt.name <- paste('fshry/','481all_m1&2_juvl.pdf',sep='')
 pdf(file = plt.name)
 par(cex=1.4) 
 contour(as.matrix(oc.fsh.scaled[,1:35]),x=as.numeric(rownames(oc.fsh.scaled)),
         y=as.numeric(colnames(oc.fsh.scaled[1:35])),xlab = "Year", 
         ylab= "Krill length (mm)",labcex=1.3,ylim=c(20,60),
         nlevels=20,main = 'Fishery months 1&2 combined ')
         abline(h=30,col='blue',lwd=3,lty=2)
         abline(h=44,col='blue',lwd=3,lty=2)

# bubble plot
 plt.name <- paste('fshry/','481all_m1&2_juvl_bubble.pdf',sep='')
 pdf(file = plt.name)
 par(cex=1.4) 
oc.fsh.scaled <- as.data.frame(as.matrix(fsh.len.scaled[2,,,2]))
 oc.fsh.scaled<-as.data.frame(as.matrix(t(oc.fsh.scaled)))
 oc.fsh.scaled <- oc.fsh.scaled[,-1] #zero length column


tmp <- cbind(year=rownames(oc.fsh.scaled[,1:35]),as.data.frame(oc.fsh.scaled[,1:35]))
long.fsh <- melt(setDT(tmp), 
           id.vars = c('year'), variable.name = "mm")

par(cex=1.3)
plot(long.fsh$year,as.numeric(as.character(long.fsh$mm)),cex=long.fsh$value*30,pch=19,
     col=ifelse(long.fsh$value>0,'black','white'),
     main= 'Fishery',
     #'plot(long.fsh$year,long.fsh$mm,cex=long$value*30)',
     ylim=c(0,60),ylab= 'Krill length (mm)', xlab= 'Year')
abline(h=30,col='blue',lwd=3,lty=2)
abline(h=44,col='blue',lwd=3,lty=2)

graphics.off()
