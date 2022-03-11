# set 'path.nm' to user's working directory
path.nm <- '/users/noaa/papers/dhk/2022_propRec/3mar/mar11/'
setwd(paste(path.nm,sep=''))
juv.l <- 30 # krill length in mm defined as <= juveniles
            # krill juv.l of 30, 35, 38, 40, 44 can be run without code modification
nareas <- 1 # no. sampling strata, must be 1 or 4
nlegs <- 1  # no. sampling legs, must be 1 or 2 
nages <- 7  # number of age classes
site.nm <- c('COPA','CS')
geo <- c('481S','481N')
if(!dir.exists(paste('plots_',juv.l,'mm',sep=''))){
    dir.create('propRec_csvs')
    dir.create(paste('plots_',juv.l,'mm',sep=''))
    }
path.dat <- paste(path.nm,'data/',sep='')
path.out <- paste(path.nm,'propRec_csvs/',sep='')

source(paste(path.nm,'2_amlr_srv.r',sep='')) # amlr survey lengths, densities 
                                          # 'haul.lens.scaled': scaled to numbers captured
                                          # 'haul.len.meas':unscaled measurements
                                          # 'haul.len.dens': scaled to numbers captured and haul volume
source(paste(path.nm,'2_amlr_srv2_dens.r',sep='')) # amlr 'propRec', 'alyr', 'haul.len.meas'
source(paste(path.nm,'3_fshry.r',sep='')) # fishery Kmv data access awaiting CCAMLR approval 
source(paste(path.nm,'4_lter_adelie.r',sep='')) # Palmer adelies ('ade')
source(paste(path.nm,'5_LTER_trwl.txt',sep='')) # LTER trawls

rm(list= ls()[!(ls() %in% c('path.dat','path.out','path.nm','site.nm','haul.len.meas','geo',
                        'rec.prop.lter','i.mo','propRec.amlr','alyr','ade.m1.m','ade.m2.m',
			'sam','ade','Kmv','fshry','yrs','yal.lens.meas','nages','nlegs',
			'yal.lens.scaled','oc.srv.meas','oc.srv','nareas','juv.l',
			'haul.len.scaled','lter.pRec'))]) # save files for plotting
                              
source(paste(path.nm,'6_amlr_peng.r',sep='')) # AERD penguins
source(paste(path.nm,'7_plot_propRec.r',sep='')) # plot proportional recruitments
source(paste(path.nm,'8_contour_amlrsrv.r',sep='')) # survey contours
source(paste(path.nm,'9_bivariate.r',sep='')) # propRec means X SDs all data sources
source(paste(path.nm,'10_contour_fsh_481.r',sep=''))
