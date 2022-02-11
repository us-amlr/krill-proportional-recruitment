dat.all <- # net length frequencies including zero hauls
  read.csv(file=paste(path.dat,"AMLR_Krill_LFD_data.csv",sep=""),sep=",",
           header=T,stringsAsFactors=F)
dat.l <- subset(dat.all,dat.all$leg=="A" | dat.all$leg =="D") # A (Jan) and D (Feb) legs only
dat.l <- subset(dat.l,dat.l$amlr_area=="EI" | dat.l$amlr_area =="SA" |
                dat.l$amlr_area=="WA" | dat.l$amlr_area =="JI")
dat.l <- dat.l[,-c(2,3,7,8)] # removed unused columns
dat.l <-cbind(Year=substr(dat.l$AMLR_Station,5,8),dat.l)

dat.l.A <- dat.l[dat.l$leg=="A",] # Jan samples
dat.l.D <- dat.l[dat.l$leg=="D",] # Feb samples
dat.l.day <- dat.l[dat.l$time_local=="D",]
dat.l.night <- dat.l[dat.l$time_local=="N" |
                     dat.l$time_local=="n",]
dat.l.twil <- dat.l[dat.l$time_local=="T",]

################
n.y <- tapply(dat.l$AMLR_Station,list(dat.l$Year),
                  function(x){length(unique(x))})
n.y.leg <- tapply(dat.l$AMLR_Station,list(dat.l$Year,dat.l$leg),
                  function(x){length(unique(x))})
n.y.area.leg <- tapply(dat.l$AMLR_Station,list(dat.l$Year,dat.l$amlr_area,dat.l$leg),
                  function(x){length(unique(x))})
sqkm.areas <- c(43865,18151,24479,38524)
if (nareas ==1 & nlegs == 1) names.area <- names.leg <- "combi"
if (nareas == 4)
  names(sqkm.areas) <- names.area <- c("EI","JI","SA","WA")
if (nlegs == 2) names.leg <- c("A","D")
names.yrs <- 1992:2012
sqkm.all <- sum(sqkm.areas)
################
# abundance scaled up from individuals measured to captured
haul.totadults <- tapply(dat.l$Krill_total,list(dat.l$AMLR_Station),
                  unique)
haul.len.meas <- tapply(dat.l$amount,                 # Numbers measured in each length-class
                   list(dat.l$AMLR_Station,dat.l$length),  # by sample.
                   sum,na.rm=T)                       # (dim = c(2415,51)) 
haul.len.meas[is.na(haul.len.meas)] <- 0  
haul.meas <- tapply(dat.l$amount,                     # Total numbers measured for length
                   list(dat.l$AMLR_Station),               # in each of  (2415 non-water-haul) samples.
                   sum,na.rm=T)                       # (dim = 2415)  
haul.scalor <- array(dim=dim(haul.meas))              # Number by which to scale up measured
                 names(haul.scalor) <-                # individuals in each sample.(dim = 2415)
                 names(haul.meas)
  for(i.id in 1:length(haul.meas))
    ifelse(haul.totadults[i.id]>0,
           haul.scalor[i.id] <- haul.totadults[i.id]/haul.meas[i.id],
           haul.scalor[i.id] <- NA
           )         
################
# Length-weight relationships
# length-wt from Farber-Lorda, 1994:
# wt_len <- 0.00000503*(13:60)^3.283
#
# length-wt from Hewitt et al 2004:
wt_len = 0.000002236*(13:60)^3.314; names(wt_len) <- 13:60

################
# LENGTH totals, scaled up from measured individuals per haul
haul.len.scaled <- 
                   array(dim=dim(haul.len.meas))      # Scaled-up numbers of individuals by length
  dimnames(haul.len.scaled) <-                        # for each sample.(dim = c(2415,51))
                   dimnames(haul.len.meas)
  for(i.id in 1:length(haul.meas))
    haul.len.scaled[i.id,] <- haul.len.meas[i.id,] * haul.scalor[i.id]
  haul.len.scaled[is.na(haul.len.scaled)] <- 0  

region.id <- tapply(as.character(dat.l$amlr_area),dat.l$AMLR_Station,paste)
leg.id <- tapply(as.character(dat.l$leg),dat.l$AMLR_Station,paste)
  for(i.id in 1:length(region.id)){
    region.id[i.id] <- region.id[[i.id]][1]
    leg.id[i.id] <- leg.id[[i.id]][1]
    }
  region.id <- as.character(region.id) # change from array to character
  leg.id <- as.character(leg.id)
  haul.len.meas <- 
               cbind(region.id,leg.id,as.data.frame(haul.len.meas),
               stringsAsFactors=T)                      # (dim = c(3011,52))
  haul.len.meas <- haul.len.meas[,-c(3,4)] # strip out 2mm, 4mm length columns
  haul.len.scaled <- 
               cbind(region.id,leg.id,as.data.frame(haul.len.scaled),
               stringsAsFactors=T)                      # (dim = c(3011,52))
  haul.len.scaled <- haul.len.scaled[,-c(3,4)] # strip out 2mm, 4mm length columns
  #rm(i.id,region.id,leg.id)

haul.biom.scaled <- cbind(haul.len.scaled[,1:2],        # tons/km^2
                          biom.tons.km2 = apply(t(t(haul.len.scaled[,
                          as.character(13:60)])*wt_len),1,sum))
haul.biom.scaled <- cbind(haul.biom.scaled,
                            biom.area =haul.biom.scaled$biom.tons.km2*
                            sqkm.all)
haul.vols <- tapply(dat.l$Volume/dat.l$depth_fished,list(dat.l$AMLR_Station),
                  function(x){sum(unique(x))}) # haul.vols are depth-integrated volumes (m^2)
                  # by haul (haul.vols[,,1])
haul.len.dens <- cbind(haul.len.scaled[,1:2],
                       haul.len.scaled[,as.character(13:60)] / haul.vols)
haul.len.biom <- cbind(haul.len.scaled[,1:2],t(t(haul.len.dens[,as.character(13:60)]) * wt_len))
haul.biom <- cbind(haul.len.scaled[,1:2],apply(haul.len.biom[,as.character(13:60)],1,sum))
             # haul.biom is the total biomass captured in a single haul
             # units of haul.biom are g/sq m (or metric tons/sq km)
haul.biom.area <-cbind(haul.biom[,1:2],
                       apply(haul.len.biom[,as.character(13:60)],1,sum)*
                       sqkm.areas[haul.biom$leg.id])
             # this is the total biomass extrapolated from each
	     # individual haul (units are metric tons)
##############
if(nareas==4 & nlegs==1){
  yal.biom.norm <- 
      cbind(aggregate(haul.biom.scaled$biom.area,
          by = list(substr(rownames(haul.biom.scaled),5,8),
               haul.biom.scaled$region.id),
          mean)[c(1,2,3)],
       aggregate(haul.biom.scaled$biom.area,
          by = list(substr(rownames(haul.biom.scaled),5,8),
               haul.biom.scaled$region.id),sd)[3])
  dimnames(yal.biom.norm) <- list(rownames(yal.biom.norm),c("year","area",
       "biom.m","biom.sd"))
  yal.lens.meas <-  # new calculation May 4, 2021 -dhk
  aggregate(haul.len.meas[,as.character(13:60)],
          by = list(substr(rownames(haul.len.meas),5,8),haul.len.meas$region.id
                 ), sum,na.rm=TRUE)
          dimnames(yal.lens.meas) <-
                          list(rownames(yal.lens.meas),
                          c("year","area",colnames(yal.lens.meas[as.character(13:60)])))
  yal.lens.scaled <-
    aggregate(haul.len.scaled[,3:ncol(haul.len.scaled)],
          by = list(substr(rownames(haul.len.scaled),5,8),haul.len.scaled$region.id
                 ), sum)
          dimnames(yal.lens.scaled) <-
                          list(rownames(yal.lens.scaled),
                          c("year","area",colnames(yal.lens.scaled[as.character(13:60)])))
  yal.vols <- tapply(dat.l$Volume/dat.l$depth_fished,list(dat.l$Year,dat.l$amlr_area),
                  function(x){sum(unique(x))}) # yal.vols are depth-integrated volumes (m^2)
                  # by year, area, and leg (yal.vols[,,1])
  yal.lens.dens <- yal.lens.scaled
  yal.lens.dens[,as.character(13:60)] <-  
             yal.lens.scaled[,as.character(13:60)]/
             as.vector(yal.vols[!is.na(yal.vols)])
  yal.dens <- cbind(yal.lens.scaled[,1],dens.m2=apply(yal.lens.scaled[,as.character(13:60)],1,sum) /
                  as.vector(yal.vols[!is.na(yal.vols)]))
  oc.srv <- cbind(yal.lens.scaled[,1:3], # observed survey compositions scaled to total catch
                prop.table(as.matrix(yal.lens.scaled[,as.character(13:60)]),1))
  oc.srv.meas <- cbind(yal.lens.meas[,1:3], # unscaled to total catch
                prop.table(as.matrix(yal.lens.meas[,as.character(13:60)]),1))
  net.dens.SD <- aggregate(haul.biom[,3],  # SD of densities, not of weights
                list(substr(rownames(haul.biom),5,8),haul.biom$region.id,
                    haul.biom$leg.id),sd,na.rm=T) # gm/m2 = metric tons/km2
  for(iyal in 1:nrow(net.dens.SD))
     if (is.na(net.dens.SD[iyal,"x"])) # if only one yal net sample, use average SD
         net.dens.SD[iyal,"x"] <-
	   mean(net.dens.SD$x[net.dens.SD$Group.2==net.dens.SD[iyal,"Group.2"] &
	   net.dens.SD$Group.3==net.dens.SD[iyal,"Group.3"]],na.rm=T)
  area.net.biom.SD <- aggregate(haul.biom.area[,3],
                    list(substr(rownames(haul.biom.area),5,8),haul.biom.area$region.id
                    ),sd,na.rm=T)
  colnames(area.net.biom.SD) <-c("year","area","area.SD")
  # NET BIOMASS
  yal.biom.nets <- cbind(yal.lens.scaled[,1:3],
                       biom.tons.km2=apply(t(t(yal.lens.dens[,as.character(13:60)])*wt_len),1,sum))
  match.area.index <- match(yal.biom.nets$area,names(sqkm.areas))
  yal.biom.nets <- cbind(yal.biom.nets,
                       biom = yal.biom.nets$biom.tons.km2*
                       sqkm.areas[match.area.index],
                       biom_sd = area.net.biom.SD$area.SD)
  }
##############
if(nareas==4 & nlegs==2){
  yal.biom.norm <- 
      cbind(aggregate(haul.biom.scaled$biom.area,
          by = list(substr(rownames(haul.biom.scaled),5,8),
               haul.biom.scaled$leg.id ,haul.biom.scaled$region.id),
          mean)[c(1,3,2,4)],
       aggregate(haul.biom.scaled$biom.area,
          by = list(substr(rownames(haul.biom.scaled),5,8),
               haul.biom.scaled$leg.id ,haul.biom.scaled$region.id),
          sd)[4])
  dimnames(yal.biom.norm) <- list(rownames(yal.biom.norm),c("year","area","leg",
       "biom.m","biom.sd"))
  yal.lens.meas <-  # new calculation May 4, 2021 -dhk
  aggregate(haul.len.meas[,3:ncol(haul.len.meas)],
          by = list(substr(rownames(haul.len.meas),5,8),haul.len.meas$region.id,
                 haul.len.meas$leg.id), sum,na.rm=TRUE)
                 dimnames(yal.lens.meas) <-
                          list(rownames(yal.lens.meas),
                          c("year","area","leg",colnames(yal.lens.meas[4:ncol(yal.lens.meas)])))
  yal.lens.scaled <-
  aggregate(haul.len.scaled[,3:ncol(haul.len.scaled)],
          by = list(substr(rownames(haul.len.scaled),5,8),haul.len.scaled$region.id,
                 haul.len.scaled$leg.id), sum)
                 dimnames(yal.lens.scaled) <-
                          list(rownames(yal.lens.scaled),
                          c("year","area","leg",colnames(yal.lens.scaled[as.character(13:60)])))
  yal.vols <- tapply(dat.l$Volume/dat.l$depth_fished,list(dat.l$Year,dat.l$amlr_area,dat.l$leg),
                  function(x){sum(unique(x))}) # yal.vols are depth-integrated volumes (m^2)
                  # by year, area, and leg (yal.vols[,,1])
  yal.lens.dens <- yal.lens.scaled
  yal.lens.dens[,as.character(13:60)] <-  
             yal.lens.scaled[,as.character(13:60)]/
             as.vector(yal.vols[!is.na(yal.vols)])
  yal.dens <- cbind(yal.lens.scaled[,1],dens.m2=apply(yal.lens.scaled[,as.character(13:60)],1,sum) /
                  as.vector(yal.vols[!is.na(yal.vols)]))
  oc.srv <- cbind(yal.lens.scaled[,1:3],
                prop.table(as.matrix(yal.lens.scaled[,as.character(13:60)]),1))
  oc.srv.meas <- cbind(yal.lens.meas[,1:3],
                prop.table(as.matrix(yal.lens.meas[,as.character(13:60)],1)))
  net.dens.SD <- aggregate(haul.biom[,3],  # SD of densities, not of weights
                list(substr(rownames(haul.biom),5,8),haul.biom$region.id,
                    haul.biom$leg.id),sd,na.rm=T) # gm/m2 = metric tons/km2
  for(iyal in 1:nrow(net.dens.SD))
     if (is.na(net.dens.SD[iyal,"x"])) # if only one yal net sample, use average SD
         net.dens.SD[iyal,"x"] <-
	 mean(net.dens.SD$x[net.dens.SD$Group.2==net.dens.SD[iyal,"Group.2"] &
	 net.dens.SD$Group.3==net.dens.SD[iyal,"Group.3"]],na.rm=T)
  area.net.biom.SD <- aggregate(haul.biom.area[,3],
                    list(substr(rownames(haul.biom.area),5,8),haul.biom.area$region.id,
                    haul.biom.area$leg.id),sd,na.rm=T)
  colnames(area.net.biom.SD) <-c("year","area","area.SD")
  # NET BIOMASS
  yal.biom.nets <- cbind(yal.lens.scaled[,1:2],
                       biom.tons.km2=apply(t(t(yal.lens.dens[,as.character(13:60)])*wt_len),1,sum))
  match.area.index <- match(yal.biom.nets$area,names(sqkm.areas))
  yal.biom.nets <- cbind(yal.biom.nets,
                       biom = yal.biom.nets$biom.tons.km2*sqkm.areas[match.area.index],
                       biom_sd = area.net.biom.SD$area.SD)
}
##############
if(nareas==1 & nlegs == 2){
  yal.biom.norm <- 
      cbind(aggregate(haul.biom.scaled$biom.area,
          by = list(substr(rownames(haul.biom.scaled),5,8),
               haul.biom.scaled$leg.id),
          mean),
       aggregate(haul.biom.scaled$biom.area,
          by = list(substr(rownames(haul.biom.scaled),5,8),
               haul.biom.scaled$leg.id),
          sd)[3])
  dimnames(yal.biom.norm) <- list(rownames(yal.biom.norm),c("year","leg",
       "biom.m","biom.sd"))
  yal.lens.scaled <-
  aggregate(haul.len.scaled[,3:ncol(haul.len.scaled)],
          by = list(substr(rownames(haul.len.scaled),5,8),
                 haul.len.scaled$leg.id), sum)
                 dimnames(yal.lens.scaled) <-
                          list(rownames(yal.lens.scaled),
                          c("year","leg",colnames(yal.lens.scaled[as.character(13:60)])))
  yal.lens.meas <-
  aggregate(haul.len.meas[,3:ncol(haul.len.meas)],
          by = list(substr(rownames(haul.len.meas),5,8),
                 haul.len.meas$leg.id), sum)
                 dimnames(yal.lens.meas) <-
                          list(rownames(yal.lens.meas),
                          c("year","leg",colnames(yal.lens.meas[as.character(13:60)])))
  yal.vols <- tapply(dat.l$Volume/dat.l$depth_fished,list(dat.l$Year,dat.l$leg),
                  function(x){sum(unique(x))}) # yal.vols are depth-integrated volumes (m^2)
                  # by year, area, and leg (yal.vols[,,1])
  yal.lens.dens <- yal.lens.scaled
  yal.lens.dens[,as.character(13:60)] <-  
             yal.lens.scaled[,as.character(13:60)]/
             as.vector(yal.vols[!is.na(yal.vols)])
  yal.dens <- cbind(yal.lens.scaled[,1:2],dens.m2=apply(
                  yal.lens.scaled[,as.character(13:60)],1,sum) /
                  as.vector(yal.vols[!is.na(yal.vols)]))
  oc.srv <- cbind(yal.lens.scaled[,1:2],
                prop.table(as.matrix(yal.lens.scaled[,as.character(13:60)]),1))
  oc.srv.meas <- cbind(yal.lens.meas[,1:2],
                prop.table(as.matrix(yal.lens.meas[,as.character(13:60)]),1))
  net.dens.SD <- aggregate(haul.biom[,3],  # SD of densities, not of weights
                list(substr(rownames(haul.biom),5,8),
                    haul.biom$leg.id),sd,na.rm=T) # gm/m2 = metric tons/km2
  for(iyal in 1:nrow(net.dens.SD))
     if (is.na(net.dens.SD[iyal,"x"])) # if only one yal net sample, use average SD
         net.dens.SD[iyal,"x"] <-
	 mean(net.dens.SD$x[net.dens.SD$Group.2==net.dens.SD[iyal,"Group.2"]],na.rm=T)
  area.net.biom.SD <- aggregate(haul.biom.area[,3],
                    list(substr(rownames(haul.biom.area),5,8),
                    haul.biom.area$leg.id),sd,na.rm=T)
  colnames(area.net.biom.SD) <-c("year","leg","area.SD")
  # NET BIOMASS
  yal.biom.nets <- cbind(yal.lens.scaled[,1:2],
                       biom.tons.km2=apply(t(t(yal.lens.dens[,as.character(13:60)])*wt_len),1,sum))
  yal.biom.nets <- cbind(yal.biom.nets,
                       biom = yal.biom.nets$biom.tons.km2*
                       sqkm.all,
                       biom_sd = area.net.biom.SD$area.SD)
  }
##############
if(nareas==1 & nlegs == 1){
  yal.biom.norm <- 
      cbind(aggregate(haul.biom.scaled$biom.area,
          by = list(substr(rownames(haul.biom.scaled),5,8)),
          mean),
       aggregate(haul.biom.scaled$biom.area,
          by = list(substr(rownames(haul.biom.scaled),5,8)),
          sd)[2])
  dimnames(yal.biom.norm) <- list(rownames(yal.biom.norm),c("year",
       "biom.m","biom.sd"))
  yal.lens.meas <-
  aggregate(haul.len.meas[,3:ncol(haul.len.meas)],
          by = list(substr(rownames(haul.len.meas),5,8)), sum)
                 dimnames(yal.lens.meas) <-
                          list(rownames(yal.lens.meas),
                          c("year",colnames(yal.lens.meas[as.character(13:60)])))
  yal.lens.scaled <-
  aggregate(haul.len.scaled[,3:ncol(haul.len.scaled)],
          by = list(substr(rownames(haul.len.scaled),5,8)), sum)
                 dimnames(yal.lens.scaled) <-
                          list(rownames(yal.lens.scaled),
                          c("year",colnames(yal.lens.scaled[as.character(13:60)])))
  yal.vols <- tapply(dat.l$Volume/dat.l$depth_fished,list(dat.l$Year),
                  function(x){sum(unique(x))}) # yal.vols are depth-integrated volumes (m^2)
                  # by year, area, and leg (yal.vols[,,1])
  yal.lens.dens <- yal.lens.scaled
  yal.lens.dens[,as.character(13:60)] <-  
             yal.lens.scaled[,as.character(13:60)]/
             as.vector(yal.vols[!is.na(yal.vols)])
  yal.dens <- cbind(as.numeric(yal.lens.scaled[,1]),dens.m2=apply(yal.lens.scaled[,as.character(13:60)],1,sum) /
                  as.vector(yal.vols[!is.na(yal.vols)]))
  oc.srv <- cbind(year=as.numeric(yal.lens.scaled[,1]),
                prop.table(as.matrix(yal.lens.scaled[,as.character(13:60)]),1))
  oc.srv.meas <- cbind(year=as.numeric(yal.lens.meas[,1]),
                prop.table(as.matrix(yal.lens.meas[,as.character(13:60)]),1))
  net.dens.SD <- aggregate(haul.biom[,3],  # SD of densities, not of weights
                list(substr(rownames(haul.biom),5,8)),sd,na.rm=T) # gm/m2 = metric tons/km2
  for(iyal in 1:nrow(net.dens.SD))
     if (is.na(net.dens.SD[iyal,"x"])) # if only one yal net sample, use average SD
         net.dens.SD[iyal,"x"] <-
	 mean(net.dens.SD$x[net.dens.SD$Group.2==net.dens.SD[iyal,"Group.2"]],na.rm=T)
  area.net.biom.SD <- aggregate(haul.biom.area[,3],
                    list(substr(rownames(haul.biom.area),5,8)),sd,na.rm=T)
  colnames(area.net.biom.SD) <-c("year","area.SD")
  # NET BIOMASS
  yal.biom.nets <- cbind(as.numeric(yal.lens.scaled[,1]),
                       biom.tons.km2=apply(t(t(yal.lens.dens[,as.character(13:60)])*wt_len),1,sum))
  yal.biom.nets <- cbind(yal.biom.nets,
                       biom = yal.biom.nets[,"biom.tons.km2"]*
                       sqkm.all,
                       biom_sd = area.net.biom.SD$area.SD)
  }
################
# MATURITY
mature <- dat.l$maturity != "JUV"
f.ly.logistic <- glm(mature~as.numeric(length)*as.factor(Year),data=dat.l,
           family=binomial)
f.alegly.logistic <- glm(mature~as.numeric(length)+as.factor(amlr_area)*as.factor(Year),data=dat.l,
           family=binomial) # changed (removed) +as.factor(leg)
mdat.l <- data.frame(length=seq(12,60,1))
mat.yrs <- length(f.ly.logistic$xlevels[[1]])
mature.ly <- array(dim=c(49,mat.yrs))
dimnames(mature.ly) <- list(12:60,f.ly.logistic$xlevels[[1]])
for(iyr in as.numeric(f.ly.logistic$xlevels[[1]])){
  mature.ly[,as.character(iyr)] <- predict(f.ly.logistic,type="response",
                          newdata=data.frame(mdat.l,Year=rep(iyr,nrow(mdat.l))),
                          interval="prediction")
  }  
min(dat.l$length[dat.l$maturity != "JUV"],na.rm=T) # smallest mature is 25 mm
mature.ly[1:13,] <- 0
mature50 <- mature99 <- vector(length=19)
for(iyr in 1:length(f.ly.logistic$xlevels[[1]])){
  mature50[iyr] <- rownames(mature.ly)[match(mature.ly[
                   min(which(mature.ly[,iyr] >=0.49)),iyr], mature.ly[,iyr])]
  mature99[iyr] <- rownames(mature.ly)[match(mature.ly[
                  min(which(mature.ly[,iyr] > 0.99)),iyr], mature.ly[,iyr])]
  }
wt_age_m <- c(0,mean(wt_len[1:9]),mean(wt_len[10:29]),
            mean(wt_len[24:34]),rep(mean(wt_len[30:49]),2))
if(nages >6) wt_age_m <- c(wt_age_m,rep(wt_age_m[6],nages-6))
wt_age_se <- c(0,sd(wt_len[1:9]),sd(wt_len[10:29]),
            sd(wt_len[24:34]),rep(sd(wt_len[30:49]),2))
mat_age <- c(0,mean(mature.ly[1:9,]),mean(mature.ly[10:29,]),
            mean(mature.ly[24:34,]),rep(mean(mature.ly[30:49,]),2))
if(nages > 6) mat_age <- c(mat_age,rep(1,nages-6))
###########
# al_key
# age-length relationship from Siegel, 87
al_key <- array(dim=c(nages,49))
al_key[is.na(al_key)] <- 0
len.age.m <- c(12.7,30.5,40,46,50.3,54.2)
len.age.se <- c(1.9,2.8,4.6,4.3,1.9,2.2)
len.age.bin.means <- seq(12,60,1)
len.age.bin.upper <- seq(12.5,60.5,1)
dimnames(al_key) <- list(paste("age",1:nages,sep=""),len.age.bin.means)
for(iage in 1:nages){
  la.prob <-pnorm(len.age.bin.upper,mean=len.age.m[iage],sd=len.age.se[iage])
  al_key[iage,] <- round(c(la.prob[1],la.prob[2:49]-la.prob[1:48]),digits=6)
  }
older.lengths <- al_key[6,]
if(nages > 6){
  for(iage in 7:nages)
    al_key[iage,] <- older.lengths
  }
# make age 1 lengths visually fit the AMLR data
al_key[1,] <- round(dnorm(12:60, mean=27,sd=4),digits=4)


