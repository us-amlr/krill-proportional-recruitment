d.97.19 <- read.csv(paste(path.dat,'AERD_KRILL_LF_DATA.csv',sep=''))
n.y <- tapply(d.97.19$SAMPLE_ID,list(d.97.19$SAMPLE_YEAR),
                  function(x){length(unique(x))})
n.y.mo <- tapply(d.97.19$SAMPLE_ID,list(d.97.19$SAMPLE_YEAR,d.97.19$SAMPLE_MONTH),
                  function(x){length(unique(x))})
n.y.area.mo <- tapply(d.97.19$SAMPLE_ID,list(d.97.19$SAMPLE_YEAR,d.97.19$SITE,d.97.19$SAMPLE_MONTH),
                  function(x){length(unique(x))})
sqkm.areas <- c(43865,18151,24479,38524)
names.yrs <- 1992:2012
sqkm.all <- sum(sqkm.areas)
################
# abundance scaled up from individuals measured to captured
peng.nsmpl <- as.numeric(tapply(d.97.19$KRILL_LENGTH,list(d.97.19$SAMPLE_ID),
                  length)) # total number measured in SAMPLE_ID
peng.nsmpl.2D <- as.data.frame(cbind(peng.nsmpl,unique(d.97.19$SAMPLE_ID)))

d.97.19$amount <- NA # set up totals per sample_id column

amount <- aggregate(d.97.19$KRILL_LENGTH,list(d.97.19$SAMPLE_ID),length)
for (id in 1:length(unique(d.97.19$SAMPLE_ID))){
  d.97.19$amount[d.97.19$SAMPLE_ID == amount$Group.1[id]] <- amount$x[id]
  }
d.97.19 <- d.97.19[!d.97.19$SAMPLE_ID == "2007045A",] # remove the sample with a character ID

diet.len.meas <- tapply(d.97.19$amount,                 # Numbers measured in each length-class
                    list(d.97.19$SAMPLE_ID,d.97.19$KRILL_LENGTH),  # by sample.
                    length)
diet.len.meas[is.na(diet.len.meas)] <- 0

diet.meas <- tapply(d.97.19$amount,                     # Total numbers measured for length
                   list(d.97.19$SAMPLE_ID),
                   length) 
diet.meas <- diet.meas[ order(as.numeric(row.names(diet.meas))) ]
################
  # Length-weight relationship
# length-wt from Farber-Lorda, 1994
# wt_len <- 0.00000503*(12:60)^3.283
# length-wt from Hewitt et al 2004
wt_len = 0.000002236*c(5,6,10:65,67,69)^3.314; names(wt_len) <- 12:60
################
region.id <- tapply(as.character(d.97.19$SITE),d.97.19$SAMPLE_ID,paste)
spp.id <- tapply(as.character(d.97.19$SPP),d.97.19$SAMPLE_ID,paste)
mo.id <- tapply(as.character(d.97.19$SAMPLE_MONTH),d.97.19$SAMPLE_ID,paste)
yr.id <- tapply(as.character(d.97.19$SAMPLE_YEAR),d.97.19$SAMPLE_ID,paste)
  for(i.id in 1:length(region.id)){
    region.id[i.id] <- region.id[[i.id]][1]
    spp.id[i.id] <- spp.id[[i.id]][1]
    mo.id[i.id] <- mo.id[[i.id]][1]
    yr.id[i.id] <- yr.id[[i.id]][1]
    }
  region.id <- as.character(region.id) # change from array to character
  ssp.id <- as.character(spp.id)
  mo.id <- as.character(mo.id)
  yr.id <- as.character(yr.id)
  diet.len.meas <- 
               cbind(region.id,mo.id,yr.id,spp.id,as.data.frame(diet.len.meas),
               stringsAsFactors=T)                      # (dim = c(3130,62))

diet.biom.meas <- cbind(diet.len.meas[,1:4],        # tons/km^2
                          biom.tons.km2 = apply(t(t(diet.len.meas[,
                          5:ncol(diet.len.meas)])*wt_len),1,sum))
diet.biom.meas <- cbind(diet.biom.meas,
                            biom.area =diet.biom.meas$biom.tons.km2*
                            sqkm.all)
peng.lens <- colnames(diet.len.meas)[5:length(colnames(diet.len.meas))]
peng.min <- as.character(min(as.numeric(peng.lens)))			  
peng.30 <- 27 # 30 mm krill column
peng.35 <- 32 # 35 mm krill column
peng.40 <- 37 # 40 mm krill column

diet.len.meas$p32 <- apply(diet.len.meas[,peng.min:peng.30],1,sum)/
           apply(diet.len.meas[,peng.lens],1,sum)
diet.len.meas$p36 <- apply(diet.len.meas[,peng.min:peng.35],1,sum)/
           apply(diet.len.meas[,peng.lens],1,sum)
diet.len.meas$p41 <- apply(diet.len.meas[,peng.min:peng.40],1,sum)/
           apply(diet.len.meas[,peng.lens],1,sum)

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


