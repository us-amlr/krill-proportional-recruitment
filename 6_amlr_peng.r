p.diets <- read.csv(paste(path.dat,'AERD_KRILL_LF_DATA.csv',sep=''))
n.y <- tapply(p.diets$SAMPLE_ID,list(p.diets$SAMPLE_YEAR),
                  function(x){length(unique(x))})
n.y.mo <- tapply(p.diets$SAMPLE_ID,list(p.diets$SAMPLE_YEAR,p.diets$SAMPLE_MONTH),
                  function(x){length(unique(x))})
if(nareas == 1)
    n.y.area.mo <- tapply(p.diets$SAMPLE_ID,list(p.diets$SAMPLE_YEAR,p.diets$SAMPLE_MONTH),
                  function(x){length(unique(x))})
if(nareas == 4)
    n.y.area.mo <- tapply(p.diets$SAMPLE_ID,list(p.diets$SAMPLE_YEAR,p.diets$SITE,p.diets$SAMPLE_MONTH),
                  function(x){length(unique(x))})
sqkm.areas <- c(43865,18151,24479,38524)
names.yrs <- 1992:2012
sqkm.all <- sum(sqkm.areas)
################
# abundance scaled up from individuals measured to captured
peng.nsmpl <- as.numeric(tapply(p.diets$KRILL_LENGTH,list(p.diets$SAMPLE_ID),
                  length)) # total number measured in SAMPLE_ID
peng.nsmpl.2D <- as.data.frame(cbind(peng.nsmpl,unique(p.diets$SAMPLE_ID)))

p.diets$amount <- NA # set up totals per sample_id column

amount <- aggregate(p.diets$KRILL_LENGTH,list(p.diets$SAMPLE_ID),length)
for (id in 1:length(unique(p.diets$SAMPLE_ID))){
  p.diets$amount[p.diets$SAMPLE_ID == amount$Group.1[id]] <- amount$x[id]
  }
p.diets <- p.diets[!p.diets$SAMPLE_ID == "2007045A",] # remove the sample with a character ID

diet.len.meas <- tapply(p.diets$amount,                 # Numbers measured in each length-class
                    list(p.diets$SAMPLE_ID,p.diets$KRILL_LENGTH),  # by sample.
                    length)
diet.len.meas[is.na(diet.len.meas)] <- 0

diet.meas <- tapply(p.diets$amount,                     # Total numbers measured for length
                   list(p.diets$SAMPLE_ID),
                   length) 
diet.meas <- diet.meas[ order(as.numeric(row.names(diet.meas))) ]
################
# recalculate length-weight relationship
# for krill lengths in penguin diet dataset
# length-wt from Hewitt et al 2004
wt_len = 0.000002236*c(5,6,10:65,67,69)^3.314;
################
if(nareas == 1){ # combine COPA and CAPE samples
  spp.id <- tapply(as.character(p.diets$SPP),p.diets$SAMPLE_ID,paste)
  mo.id <- tapply(as.character(p.diets$SAMPLE_MONTH),p.diets$SAMPLE_ID,paste)
  yr.id <- tapply(as.character(p.diets$SAMPLE_YEAR),p.diets$SAMPLE_ID,paste)
  for(i.id in 1:length(spp.id)){
    #region.id[i.id] <- region.id[[i.id]][1]
    spp.id[i.id] <- spp.id[[i.id]][1]
    mo.id[i.id] <- mo.id[[i.id]][1]
    yr.id[i.id] <- yr.id[[i.id]][1]
    }
  ssp.id <- as.character(spp.id)
  mo.id <- as.character(mo.id)
  yr.id <- as.character(yr.id)
  diet.len.meas <- 
               cbind(mo.id,yr.id,spp.id,as.data.frame(diet.len.meas),
               stringsAsFactors=T)                      # (dim = c(3130,63))

  diet.biom.meas <- cbind(diet.len.meas[,1:4],        # tons/km^2
                          biom.tons.km2 = apply(t(t(diet.len.meas[,
                          4:ncol(diet.len.meas)])*wt_len),1,sum))
  diet.biom.meas <- cbind(diet.biom.meas,
                            biom.area =diet.biom.meas$biom.tons.km2*
                            sqkm.all)
  peng.lens <- colnames(diet.len.meas)[5:length(colnames(diet.len.meas))]
  peng.min <- as.character(min(as.numeric(peng.lens)))			  
 # peng.30 <- 27 # 30 mm krill column
 # peng.35 <- 32 # 35 mm krill column
 # peng.40 <- 37 # 40 mm krill column
  peng.juv <- juv.l-3
  diet.len.meas$juvl <- apply(diet.len.meas[,peng.min:peng.juv],1,sum)/
           apply(diet.len.meas[,peng.lens],1,sum)
  }

if(nareas == 4){ # aggregate COPA and CAPE separately
  region.id <- tapply(as.character(p.diets$SITE),p.diets$SAMPLE_ID,paste)
  spp.id <- tapply(as.character(p.diets$SPP),p.diets$SAMPLE_ID,paste)
  mo.id <- tapply(as.character(p.diets$SAMPLE_MONTH),p.diets$SAMPLE_ID,paste)
  yr.id <- tapply(as.character(p.diets$SAMPLE_YEAR),p.diets$SAMPLE_ID,paste)
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
               stringsAsFactors=T)                      # (dim = c(3130,64))

  diet.biom.meas <- cbind(diet.len.meas[,1:4],        # tons/km^2
                          biom.tons.km2 = apply(t(t(diet.len.meas[,
                          5:ncol(diet.len.meas)])*wt_len),1,sum))
  diet.biom.meas <- cbind(diet.biom.meas,
                            biom.area =diet.biom.meas$biom.tons.km2*
                            sqkm.all)
  peng.lens <- colnames(diet.len.meas)[5:length(colnames(diet.len.meas))]
  peng.min <- as.character(min(as.numeric(peng.lens)))			  
 # peng.30 <- 27 # 30 mm krill column
 # peng.35 <- 32 # 35 mm krill column
 # peng.40 <- 37 # 40 mm krill column
  peng.juv <- juv.l-3
  diet.len.meas$juvl <- apply(diet.len.meas[,peng.min:peng.juv],1,sum)/
           apply(diet.len.meas[,peng.lens],1,sum)

  }


