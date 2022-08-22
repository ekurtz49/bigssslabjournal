rm(list = ls())

fsave <- function(x, file, location = "./data/processed/", ...) {
  if (!dir.exists(location))
    dir.create(location)
  datename <- substr(gsub("[:-]", "", Sys.time()), 1, 8)
  totalname <- paste(location, datename, file, sep = "")
  print(paste("SAVED: ", totalname, sep = ""))
  save(x, file = totalname)
}

fpackage.check <- function(packages) {
  lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
}

colorize <- function(x, color) {
  sprintf("<span style='color: %s;'>%s</span>", color, x)
}


packages = c("tidyverse", "rgl", "spdep", "geosphere", "truncnorm", "progress", "foreach", "doParallel",
             "ape", "seg", "rgl", "OasisR", "compiler")

fpackage.check(packages)


# version 09-06-2007


# function define world
iniworld <- function(N = 2000, cn = 4, h = 1, tc = 0.9, pg = c(0.5, 0.5), distropTN = TRUE, plotworld = TRUE,
                     seed = NULL) {
  # N= number of agents (even number) cn= number of clusters (even number) h= cluster homogeneity
  # (0.5-1) tc= thinning constant. .9 means retain 90% pg= proportion of groups; length is number
  # of groups distropTN= use truncated normal to generate opinions, default = false
  
  # in paper opinions [0,1], here [-1,1] in paper tc is 1 - tc
  
  if (is.null(seed))
    seed <- sample(45667:159876, 1)
  
  set.seed(seed)
  
  N_ori <- N
  
  # functions
  spher_to_cart <- function(r, theta, phi) {
    x = r * cos(phi) * sin(theta)
    y = r * sin(theta) * sin(phi)
    z = r * cos(theta)
    coordinatesxyz <- matrix(c(x, y, z), ncol = 3)
    return(coordinatesxyz)
  }
  
  distl <- function(x) {
    distVincentySphere(x, matlonglat, r = 1)
  }
  
  # if tc<1 we need to increase initial N, make sure to keep even number
  if (tc < 1) {
    N <- trunc(N/(tc * 10)) * 10
  }
  
  # define (random) position of agents on sphere:
  # http://mathworld.wolfram.com/SpherePointPicking.html
  r <- 1
  phi <- 2 * pi * runif(N)
  theta <- acos(2 * runif(N) - 1)
  coordinatesxyz <- spher_to_cart(r, theta, phi)
  
  phi_r <- (360 * phi)/(2 * pi)
  theta_r <- (180 * theta)/pi
  lat <- 90 - theta_r
  long <- ifelse(phi_r >= 0 & phi_r < 180, -phi_r, abs(phi_r - 360))
  
  matlonglat <- matrix(c(long, lat), ncol = 2)
  
  # improve: we only need to calculate half
  matlonglatlist <- lapply(seq_len(nrow(matlonglat)), function(i) matlonglat[i, ])
  
  distl <- function(x) {
    distVincentySphere(x, matlonglat, r = 1)
  }
  
  matdist <- sapply(matlonglatlist, distl)
  
  # model segregation: could be improved. check existing packages.
  parents <- sample(1:N, cn)
  groups <- rep(NA, N)
  # fix if cn==1
  groups[parents] <- sample(c(rep(1, round(cn * pg[1])), rep(-1, cn - round(cn * pg[1]))), cn, replace = FALSE)
  
  # to whom do children belong
  clusterchildren <- rep(NA, N)
  
  for (i in c(1:N)) {
    if (!(i %in% parents)) {
      # which parents is closest
      clusterchildren[i] <- parents[which(matdist[i, parents] == min(matdist[i, parents]))]
      # give child same initial value as closest parent
      group <- groups[clusterchildren[i]]
      # change value child depending of cluster homogeneity
      groups[i] <- ifelse(group == -1, sample(c(-1, 1), 1, prob = c(h, 1 - h)), sample(c(-1, 1),
                                                                                       1, prob = c(1 - h, h)))
    }
  }
  
  # define opinions of agents
  if (distropTN == TRUE) {
    opinions <- rtruncnorm(N, a = -1, b = 1, mean = 0, sd = 0.45)
  }
  # if(distropTN==FALSE) {opinions <- runif(N, min = -1, max = 1)}
  
  # for (future) plotting
  color <- ifelse(groups == 1, "blue", "red")
  
  # thin clusters, make cluster boundaries sharper
  if (tc < 1) {
    childIDi <- sampletc <- NA
    # put in big function
    for (i in 1:cn) {
      childIDi <- which(clusterchildren == parents[i])
      distchildparenti <- matdist[parents[i], childIDi]
      # samplei <- sample(childIDi, trunc(tc*length(childIDi)),
      # prob=exp(-distchildparenti)^2)
      cutoffdistance <- quantile(distchildparenti, tc)
      samplei <- childIDi[distchildparenti < cutoffdistance]
      sampletc <- c(sampletc, samplei)
    }
    clusterchildren <- sampletc <- sampletc[-1]
    sampletc <- c(sampletc, parents)
    N_obs <- length(sampletc)
  }
  
  N <- N_ori  #setting back to original input
  
  if (tc == 1) {
    sampletc <- NA
    N_obs <- N_ori
  }
  
  if (plotworld & tc == 1) {
    .check3d()
    rgl.close()
    plot3d(coordinatesxyz, col = color, box = FALSE, axes = FALSE, xlab = "", ylab = "", zlab = "",
           size = 8, xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1, 1))
    rgl.spheres(0, 0, 0, radius = 0.995, color = "grey")
  }
  
  if (tc == 1) {
    worldlist <- list(seed, coordinatesxyz, color, groups, opinions, matdist, N, cn, h, tc, pg, N_obs,
                      parents, clusterchildren, matlonglat)
    names(worldlist) <- c("seed", "coordinatesxyz", "color", "groups", "opinions", "matdist", "N",
                          "cn", "h", "tc", "pg", "N_obs", "parents", "clusterchildren", "matlonglat")
    return(worldlist)
  }
  
  if (plotworld & tc < 1) {
    .check3d()
    rgl.close()
    plot3d(coordinatesxyz[sampletc, ], col = color[sampletc], box = FALSE, axes = FALSE, xlab = "",
           ylab = "", zlab = "", size = 8, xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1, 1))
    rgl.spheres(0, 0, 0, radius = 0.995, color = "grey")
  }
  
  if (tc < 1) {
    worldlist <- list(seed, coordinatesxyz[sampletc, ], color[sampletc], groups[sampletc], opinions[sampletc],
                      matdist[sampletc, sampletc], N, cn, h, tc, pg, N_obs, parents, clusterchildren, matlonglat[sampletc,
                      ])
    names(worldlist) <- c("seed", "coordinatesxyz", "color", "groups", "opinions", "matdist", "N",
                          "cn", "h", "tc", "pg", "N_obs", "parents", "clusterchildren", "matlonglat")
    return(worldlist)
  }
  
  
}



iniworld()







# define parameters
#N <- c(100, 200, 400, 800, 1600)
#cn <- c(4, 8, 16, 32, 64)
#h <- c(0.6, 0.7, 0.8, 0.9, 1)
#tc <- c(0.6, 0.7, 0.8, 0.9, 1)
#pg <- c(0.5, 0.6, 0.7, 0.8, 0.9)


N <- c(100, 200)
cn <- c(4, 8)
h <- c(0.6, 0.95)
tc <- c(0.6, 0.7)
pg <- c(0.5, 0.6)

# run the loop in parallel
n.cores <- parallel::detectCores() - 1  #save one core for other work
# create the cluster
my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
# register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)


# to get the same results
set.seed(5893743)

# make sure to define the correct folder beforehand
dataworlds <- foreach(Nsim = N, i = icount()) %:% 
  foreach(cnsim = cn, j = icount()) %:% 
    foreach(hsim = h, k = icount()) %:% 
      foreach(tcsim = tc, l = icount()) %:% 
        foreach(pgsim = pg, m = icount(), .packages = packages, .inorder = TRUE) %dopar% 
          {world <- iniworld(N = Nsim, cn = cnsim, h = hsim, tc = tcsim, pg = pgsim, plotworld = FALSE, seed = NULL)
          save(world, file = paste("./data/processed/worlds/worldN", Nsim, "cn", cnsim, "h", hsim, "tc", tcsim, "pg", pgsim, ".rda", sep = ""), compress="bzip2")
          }  




test <- 23
rm(test)
test
getwd()
save(world1, file="./data/processed/worlds/world1.rda")
load("./data/processed/worlds/world1.rda")

world1 <- iniworld() # isolation, clustering
iniworld(cn=128) #exposure, clustering
iniworld(h=0.5) # exposure, evenness
iniworld(N=10,h=0.5,pg=0.5)




load("~/Documents/GitHub/bigssslabjournal/data/processed/worlds/worldN100cn4h0.6tc0.6pg0.5.rda")
# clustering, exposure






test <- world
# first define data.
mydf <- as.data.frame(cbind(as.numeric(test$groups == 1), as.numeric(test$groups == -1)))
# define the coordinates. (note: this are from a sphere)
mycoordinates <- test$matlonglat
mydf$Longitude <- test$matlonglat[, 1]
mydf$Latitude <- test$matlonglat[, 2]
points = st_as_sf(mydf, coords = c("Longitude", "Latitude"), crs = 4326)
graticule = st_graticule(lat = seq(-80, 80, 10), lon = seq(-180, 180, 10))
robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
projected = st_transform(points, robinson)
graticule = st_transform(graticule, robinson)
{
  plot(projected$geometry, pch = 16, col = test$color, reset = FALSE)
  plot(graticule$geometry, col = "#00000040", add = T)
}





load("~/Documents/GitHub/bigssslabjournal/data/processed/worlds/worldN100cn8h0.95tc0.7pg0.5.rda")
# clustering, isolation






test <- world
# first define data.
mydf <- as.data.frame(cbind(as.numeric(test$groups == 1), as.numeric(test$groups == -1)))
# define the coordinates. (note: this are from a sphere)
mycoordinates <- test$matlonglat
mydf$Longitude <- test$matlonglat[, 1]
mydf$Latitude <- test$matlonglat[, 2]
points = st_as_sf(mydf, coords = c("Longitude", "Latitude"), crs = 4326)
graticule = st_graticule(lat = seq(-80, 80, 10), lon = seq(-180, 180, 10))
robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
projected = st_transform(points, robinson)
graticule = st_transform(graticule, robinson)
{
  plot(projected$geometry, pch = 16, col = test$color, reset = FALSE)
  plot(graticule$geometry, col = "#00000040", add = T)
}






world<-iniworld(N=100,h=0.5) # exposure, evenness
world<-iniworld(N=100,h=0.5,pg=c(0.1,0.9))









