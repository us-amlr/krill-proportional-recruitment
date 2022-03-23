
lter.srv <- read.csv(paste(path.dat,'lter_srv.csv',sep=''),
            header = TRUE)
lter.srv$yr <- as.numeric(paste(20,substr(lter.srv$cruise,4,5),sep=''))

pRec.lter.srv <- tapply(lter.srv$count[lter.srv$count<=juv.l],
                 c(lter.srv$cruise[lter.srv$count<=juv.l],
		 lter.srv$round_SL[lter.srv$count<=juv.l]),sum)/
		 tapply(lter.srv$count,
                 c(lter.srv$cruise,
		 lter.srv$round_SL),sum)
mean(pRec.lter.srv)
sd(pRec.lter.srv)

lter.smpl <- tapply(lter.srv$count,list(lter.srv$yr,lter.srv$sl),sum,na.rm=TRUE)

lter.pRec <- apply(lter.smpl[,as.character(3:juv.l)],1,sum,na.rm=TRUE)/
             apply(lter.smpl[,as.character(3:65)],1,sum,na.rm=TRUE)
