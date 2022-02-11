# Palmer LTER adelie penguin diet data from
# https://oceaninformatics.ucsd.edu/datazoo/catalogs/pallter/datasets/89
ade <- read.csv(paste(path.dat,'Adelie Penguin Diet Composition, Krill.csv',sep=''))
ade <- ade[ade$Total.Number>0,] # remove rows with all lengths = 0
ade$Sample.Collection.Date <- as.Date(ade$Sample.Collection.Date,format='%m/%d/%y')
ade$yr <- as.numeric(substr(ade$Sample.Collection.Date,1,4))
ade$mo <- as.numeric(substr(ade$Sample.Collection.Date,6,7))
yrs <- unique(ade$yr)

ade.m1 <- subset(ade[ade$mo == 1,])
ade.m2 <- subset(ade[ade$mo == 2,])

if(juv.l==30) juv.lter <- 5:7 # translate juvenile size limits to LTER 5 mm bins
if(juv.l==35) juv.lter <- 5:8
if(juv.l==40) juv.lter <- 5:9
ade.m1.m <- ade.m2.m <- ade.m1.sd <- ade.m2.sd <- vector()
  for(iyr in 1:length(yrs)){
    ade.m1.m[iyr] <- mean(apply(ade.m1[ade.m1$yr == yrs[iyr],juv.lter],1,sum,na.rm=TRUE)/
        apply(ade.m1[ade.m1$yr == yrs[iyr],5:14],1,sum,na.rm=TRUE))
    ade.m2.m[iyr] <- mean(apply(ade.m2[ade.m2$yr == yrs[iyr],juv.lter],1,sum,na.rm=TRUE)/
        apply(ade.m2[ade.m2$yr == yrs[iyr],5:14],1,sum,na.rm=TRUE))
    ade.m1.sd[iyr] <- sd(apply(ade.m1[ade.m1$yr == yrs[iyr],juv.lter],1,sum,na.rm=TRUE)/
        apply(ade.m1[ade.m1$yr == yrs[iyr],5:14],1,sum,na.rm=TRUE))
    ade.m2.sd[iyr] <- sd(apply(ade.m2[ade.m2$yr == yrs[iyr],juv.lter],1,sum,na.rm=TRUE)/
        apply(ade.m2[ade.m2$yr == yrs[iyr],5:14],1,sum,na.rm=TRUE))
    }

plt.name <- paste('plots/','LTER_adelie_Jan.pdf',sep='')
pdf(file = plt.name)
y.lim <- c(min(ade.m1.m-ade.m1.sd),
           max(ade.m1.m+ade.m1.sd))
plot(yrs,ade.m1.m,ylim=y.lim,ylab='proportional recruitment',
     xlab='year',main='January LTER adelie')
segments(
         yrs,ade.m1.m+ade.m1.sd,
         yrs,ade.m1.m-ade.m1.sd
        )

plt.name <- paste('plots/','LTER_adelie_Feb.pdf',sep='')
pdf(file = plt.name)
y.lim <- c(min(ade.m2.m-ade.m2.sd,na.rm=TRUE),
           max(ade.m2.m+ade.m2.sd,na.rm=TRUE))
plot(yrs,ade.m2.m,ylim=c(0,1),ylab='proportional recruitment',
     xlab='year',main = 'February LTER adelie')
segments(
         yrs,ade.m2.m+ade.m2.sd,
         yrs,ade.m2.m-ade.m2.sd
        )
graphics.off()
