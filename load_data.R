library(dplyr)

# load migration data
mig <- read.csv("migrationflat.csv")

# remove non-states
mig <- filter(mig, !(destination %in% c("Puerto Rico", "U.S..Island.Area",
                                       "Foreign.Country", "Total")))
mig <- filter(mig,  source != "Puerto Rico")

# map states to source/destination regions
states <- read.csv("states.csv")

mig <- merge(mig,  states,  by.x="source",  by.y="State")
mig <- rename(mig, src.region = Region)
mig <- merge(mig, states, by.x="destination", by.y="State")
mig <- rename(mig, dest.region = Region)

# aggregate
migreg <- mig %>% group_by(src.region, dest.region, year) %>%
  summarize(migration = sum(migration))

# incorporating employment data into transition matrix
e <- read.csv("employment.csv")

# transition matrix w/probs based on empirical pop flow
pmatrix.pop <- function(yr){

  mignum <- migreg %>% filter(year == yr) %>% select(-year) %>%
    reshape2::dcast(src.region~dest.region, value.var="migration")
  mignum <- as.matrix(mignum[, 2:5])
  rownames(mignum) <- colnames(mignum)

  for(row in 1:4){
    mignum[row,] <- mignum[row,] / sum(mignum[row,])
  }

  #mignum <- apply(mignum, 1, function(x) return(x/sum(x)))

  return(mignum)
}
