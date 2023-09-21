# run "R2_amlr_srv.txt" to produce "haul.len.dens"

if(nareas == 1){
  l.frq <- aggregate(haul.len.dens[,as.character(13:60)],by=list(
        haul.len.dens$leg.id,substr(rownames(haul.len.dens),5,14)),sum)

  propRec.h <- apply(l.frq[,as.character(13:juv.l)],1,sum,na.rm=T)/apply(l.frq[,as.character(13:60)],1,sum,na.rm=T)
  propRec.h2 <- cbind(l.frq[,1:2],propRec.h)
  propRec.h2$yr <- substr(propRec.h2$Group.2,1,4)

  propRec.amlr <- aggregate(propRec.h2$propRec.h,by=list(propRec.h2$Group.1,
                      as.factor(propRec.h2$yr)),mean,na.rm=TRUE)
  propRec.amlr.sd <- aggregate(propRec.h2$propRec.h,by=list(propRec.h2$Group.1,
                      as.factor(propRec.h2$yr)),sd,na.rm=TRUE)

  propRec.amlr <-cbind(propRec.amlr,propRec.amlr.sd$x)
  colnames(propRec.amlr) <- c('leg','yr','mean','sd')
  # 'propRec.amlr' (dim 37 x 4) contains means and SDs of
  # proportional recruitment in AMLR surveys

  write.csv(propRec.amlr,paste('propRec_csvs/AMLR_srv_',juv.l,'mm_4strata.csv'))
  } #end nareas == 1

if(nareas == 4){
  l.frq <- aggregate(haul.len.dens[,as.character(13:60)],by=list(haul.len.dens$region.id,
        haul.len.dens$leg.id,substr(rownames(haul.len.dens),5,14)),sum)
  #alyr <- aggregate(l.frq[,as.character(13:60)],by=list(l.frq$Group.1,l.frq$Group.2,substr(l.frq$Group.3,1,4)),mean)

  propRec.h <- apply(l.frq[,as.character(13:juv.l)],1,sum,na.rm=T)/apply(l.frq[,as.character(13:60)],1,sum,na.rm=T)
  propRec.h2 <- cbind(l.frq[,1:3],propRec.h)
  propRec.h2$yr <- substr(propRec.h2$Group.3,1,4)

  propRec.amlr <- aggregate(propRec.h2$propRec.h,by=list(propRec.h2$Group.1,propRec.h2$Group.2,
                      as.factor(propRec.h2$yr)),mean,na.rm=TRUE)
  propRec.amlr.sd <- aggregate(propRec.h2$propRec.h,by=list(propRec.h2$Group.1,propRec.h2$Group.2,
                      as.factor(propRec.h2$yr)),sd,na.rm=TRUE)

  propRec.amlr <-cbind(propRec.amlr,propRec.amlr.sd$x)
  colnames(propRec.amlr) <- c('area','leg','yr','mean','sd')
  # 'propRec.amlr' (dim = 118 x 5) contains means and SDs of
  # proportional recruitment in AMLR surveys by area, leg, and year

  write.csv(propRec.amlr,paste('propRec_csvs/AMLR_srv_',juv.l,'mm_48.1.csv'))
  } #end nareas == 4

