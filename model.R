# set up simulation parameters
totalPop <- 10000

capacity <- c(5000, 5000)

slack <- c(4000, 4000)

capjobs <- 0.98 * capacity

UE <- 1 - (capjobs / capacity)

avgUE <- sum((capacity / totalPop) * UE)


marginaljobs <- function(p,  capacity,  slack) {
  return((capacity - p) / (capacity - slack))
}

intmarginaljobs <- function(p,  capacity,  slack) {
  imj <- (2 * capacity * p - p^2) / (2 * capacity - 2 * slack)
  return(imj)
}

numjobsraw <- function(p1, p2, capacity, slack) {
  nj <- intmarginaljobs(p2, capacity, slack) -
    intmarginaljobs(p1, capacity, slack)
  return(nj)
}

numjobs <- function(p1, p2, capacity, slack) {
  if (p1 < slack) {
    if (p2 < slack) {
      return((p2-p1))
    } else if (p2 >= slack && p2 < capacity) {
      nj <- (slack-p1) + numjobsraw(slack, p2, capacity, slack)
      return(nj)
    } else {
      nj <- (slack-p1) + numjobsraw(slack, capacity, capacity, slack)
      return(nj)
    }

  } else if (p1 >= slack && p1 < capacity) {
    if (p2 < slack) {
      nj <- (p2-slack) + numjobsraw(p1, slack, capacity, slack)
      return(nj)
    } else if (p2 >= slack && p2 < capacity) {
      nj <- numjobsraw(p1, p2, capacity, slack)
      return(nj)
    } else {
      nj <- numjobsraw(p1, capacity, capacity, slack)
      return(nj)
    }

  } else {
    if (p2 < slack) {
      nj <- numjobsraw(capacity, slack, capacity, slack) + (p2 - slack)
      return(nj)
    } else if (p2 >= slack && p2 < capacity) {
      nj <- numjobsraw(capacity, p2, capacity, slack)
      return(nj)
    } else {
      return(0)
    }
  }
}

# what happens to the average unemployment rate if we move 500 people from one region to another?
deltaPop <- c(-500, 500)
deltaJobs <- c(numjobs(5000, 4500, 5000, 4000), numjobs(5000, 5500, 5000, 4000))

population <- capacity + deltaPop
jobs <- capjobs + deltaJobs
UE <- 1-(jobs / population)
avgUE <- sum((capacity / totalPop) * UE)

avgUE

# this shows what happens when people move from a productive configuration to a non-productive configuration. Overall unemployment increases. Given this new configuration,  500 people would be better off if they moved back to region A.

# Now to try to apply MCMC to the pan model. First, just a repeat of how this would run (with larger populations).

# First, how it runs towards equilibrium as a markov chain:

u1 <- function(p) {
  return(-p[1]+8000)
}

u2 <- function(p) {
  return(-p[2]+14000)
}

c12 <- function(f) {
  return(2 * f[1])
}

c21 <- function(f) {
  return(2 * f[2])
}

p0 <- c(4000, 2000)

flowcalc <- function(p) {
  if (u2(p) > u1(p)) {
    f1 <- (p[1] - p[2] + 6000) / 4
    f <- c(f1, 0)
  } else if (u1(p) > u2(p)) {
    f2 <- (p[2] - p[1] - 6000) / 4
    f <- c(0, f2)
  } else {
    f <- c(0, 0)
  }

  return(f)
}

pdata <- data.frame(p1 = 4000,  p2 = 2000,  u1 = u1(p0),  u2 = u2(p0))


for(i in 1:10) {
  p <- c(tail(pdata$p1, 1), tail(pdata$p2, 1))
  f <- flowcalc(p)
  u <- c(u1(p), u2(p))
  totu <- sum(p * u)

  pdata <- rbind(pdata,  c(p[1]-f[1]+f[2],  p[2]+f[1]-f[2],  u))
}

# Now lets try to run this as MCMC. for this large a population,  the

p0 <- c(4000, 2000)
pdata <- data.frame(p1 = 4000,  p2 = 2000,  u1 = u1(p0),  u2 = u2(p0))

pdatastep <- function(pdata) {

  location <- sample(c(1, 2), 1)

  p <- c(tail(pdata$p1, 1), tail(pdata$p2, 1))

  if (location == 1) {
    if (p[1] >= 10 && p[2] <= 5990) {
      if (u1(p) < u2(p) - 200) {
        p[1] <- p[1]-10
        p[2] <- p[2]+10
        pdata <- rbind(pdata, c(p, u1(p), u2(p)))
      } else {
        probmove <- (u2(p) - 200) / u1(p)
        if (runif (1) < probmove) {
          p[1] <- p[1]-10
          p[2] <- p[2]+10
          pdata <- rbind(pdata, c(p, u1(p), u2(p)))
        } else {
          pdata <- rbind(pdata, c(p, u1(p), u2(p)))
        }
      }
    } else {
      pdata <- rbind(pdata, c(p, u1(p), u2(p)))
    }
  }

  if (location == 2) {
    if (p[2] >= 10 && p[1] <= 5990) {
      if (u2(p) < u1(p) - 200) {
        p[2] <- p[2]-10
        p[1] <- p[1]+10
        pdata <- rbind(pdata, c(p, u1(p), u2(p)))
      } else {
        probmove <- (u1(p) - 200) / u2(p)
        if (runif (1) < probmove) {
          p[2] <- p[2]-10
          p[1] <- p[1]+10
          pdata <- rbind(pdata, c(p, u1(p), u2(p)))
        } else {
          pdata <- rbind(pdata, c(p, u1(p), u2(p)))
        }
      }
    } else {
      pdata <- rbind(pdata, c(p, u1(p), u2(p)))
    }
  }

  return(pdata)

}

iter <- 1:10000
for(i in iter) {
  pdata <- pdatastep(pdata)
}

tail(pdata)

# do the population estimates converge?
plot(iter, pdata$p1[-1], type = 'l')
plot(iter, pdata$p2[-1], type = 'l')
