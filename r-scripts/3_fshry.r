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
if(nareas == 4){
  Fm=summarise(group_by(Kmv,ASD,t),f=sum(fr[L<=juv.l]),M=unique(M),Y=unique(Y),hi=unique(is.na(hi))==F)
  rec.prop.fshry <- as.data.frame(Fm)
  fshry <- as.data.frame(Fm[(Fm$ASD == '481S' & c(Fm$M==1 | Fm$M==2 | Fm$M %in% 7:9)) | 
                          (Fm$ASD == '481N'  & c(Fm$M==1 | Fm$M==2 | Fm$M %in% 7:9)),]) 
  fshry <- fshry[order(fshry$ASD,fshry$Y,fshry$M),]
  write.csv(fshry[fshry$M==1 & fshry$ASD=='481N' |
                fshry$M==2 & fshry$ASD=='481N' |
                fshry$M==1 & fshry$ASD=='481S' |
                fshry$M==2 & fshry$ASD=='481S',],
                paste('propRec_csvs/fshry_',juv.l,'mm.csv',sep=''))
  }

if(nareas == 1){
  Fm=summarise(group_by(Kmv,ASD,t),f=sum(fr[L<=juv.l]),M=unique(M),Y=unique(Y),hi=unique(is.na(hi))==F)
  rec.prop.fshry <- as.data.frame(Fm)
  fshry <- as.data.frame(Fm[ Fm$M==1  & Fm$ASD=='481N' |
                           Fm$M==1  &  Fm$ASD=='481S',]) 
  fshry <- fshry[order(fshry$Y,fshry$M),]
  write.csv(fshry,paste('propRec_csvs/fshry_',juv.l,'mm_agg.csv',sep=''))
  }

