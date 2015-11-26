setwd("E:/Downloads/Courses/CUNY/SPS/Git/IS 604 Simulation and Modeling techniques/Final Project/migration data")

#library(Quandl)
library(plyr)
library(dplyr)
library(reshape2)

mig <- read.csv("migrationflat.csv")

mig <- filter(mig,!(destination %in% c("Puerto Rico","U.S..Island.Area",
                                       "Foreign.Country","Total")))
mig <- filter(mig,source != "Puerto Rico")

states <- read.csv("states.csv")

mig <- merge(mig,states,by.x="source",by.y="State")
mig <- rename(mig,c("Region" = "src.region"))
mig <- merge(mig,states,by.x="destination",by.y="State")
mig <- rename(mig,c("Region" = "dest.region"))

migreg <- mig %>% group_by(src.region,dest.region,year) %>%
  summarize(migration = sum(migration))

e <- read.csv("employment.csv")

pmatrix <- function(yr){
  
  mignum <- migreg %>% filter(year == yr) %>% select(-year)
  mignum <- dcast(mignum, src.region~dest.region, value.var="migration")
  mignum <- as.matrix(mignum[,2:5])
  rownames(mignum) <- colnames(mignum)
  
  for(row in 1:4){
    mignum[row,] <- mignum[row,]/sum(mignum[row,])
  }
  
  #mignum <- apply(mignum,1,function(x) return(x/sum(x)))
  
  return(mignum)
}




