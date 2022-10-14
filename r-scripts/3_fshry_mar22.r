# CCAMLR code snippet below is from "K_ObsAndCatch_Fxv3LFDs_LFDplot.R"
# except here 'juv.l' replaces '40' in the original code

library(dplyr)
#Read the data
Kmv=read.csv(paste(path.dat,'KobsLFDs.csv',sep=''))
#Highlight chosen months per area
Kmv$hi=NA
indx=which((Kmv$ASD=='481N' & Kmv$M==3)|
              (Kmv$ASD=='481S' & Kmv$M==3)|
              (Kmv$ASD=='48.2' & Kmv$M==2)|
              (Kmv$ASD=='48.3' & Kmv$M==7))
Kmv$hi[indx]=Kmv$fr[indx]

# modified code dhk
#if(nareas == 1 & nlegs == 1){ # combine 481S and 481N, Jan only
if( nlegs == 1){ # combine 481S and 481N, Jan only
  Fm=summarise(group_by(Kmv,ASD,t),f=sum(fr[L<=juv.l]),M=unique(M),Y=unique(Y),hi=unique(is.na(hi))==F)
  rec.prop.fshry <- as.data.frame(Fm)
  fshry <- as.data.frame(Fm[c(Fm$ASD == '481S' | Fm$ASD == '481N') & Fm$M==1,])
  fshry <- fshry[order(fshry$Y,fshry$M),]
  fshry <- aggregate(fshry[,2:5],list(fshry$Y),mean)
  write.csv(fshry,paste('propRec_csvs/fshry_',juv.l,'mm_48.1_Jan.csv',sep=''))
  }

#if(nareas == 1 & nlegs == 2){ # combine 481S and 481N, Jan and Feb
if( nlegs == 2){ # combine 481S and 481N, Jan and Feb
  Fm=summarise(group_by(Kmv,ASD,t),f=sum(fr[L<=juv.l]),M=unique(M),Y=unique(Y),hi=unique(is.na(hi))==F)
  rec.prop.fshry <- as.data.frame(Fm)
  fshry <- as.data.frame(Fm[(Fm$ASD == '481S') | 
                          (Fm$ASD == '481N'  ),]) 
  fshry <- fshry[order(fshry$ASD,fshry$Y),]
  fshry <- aggregate(fshry[,2:5],list(fshry$Y),mean)
  write.csv(fshry,paste('propRec_csvs/fshry_',juv.l,'mm_48.1_JanFeb.csv',sep=''))
  # fshry$M: 1=Jan, 2=Feb, 1.5 = both, or can be a fraction of the total months sampled
  }


