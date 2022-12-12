# EXAMPLE CODE FOR KMEANS CLUSTERING
# AND UNSUPERVISED ANOMOLY DETECTION
#
# Gio Circo, Data Scientist

# you'll need the following libraries:

# install.packages("dbscan")
# install.packages("stray")
# install.packages("ISLR2")

library(dbscan)
library(stray)
library(ISLR2)

set.seed(31245)

# HELPER FUNCTIONS
#========================#

# run kmeans on c centers
calc_kmeans <- function(c){
  kmeans(X,centers = c, nstart = 20)
}

# minmax normalization
minmax <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

# SETUP DATA
#========================#

# load some sample data on cars from ISLR2
X <- ISLR2::Auto

# minmax scale
X <- data.frame(apply(X[,1:7], FUN = minmax,2))


# KMEANS CLUSTERING
#
# First, we need to figure out how many 
# cluster centers we should set
#========================#

# Let's try the "elbow" method

# run kmeans x times
klist <- lapply(1:10, calc_kmeans)

ssc <- unlist(lapply(klist, function(x) x$betweenss/x$totss))

# within sum of squares
par(mfrow = c(1,2))
plot(1:10, ssc, type = 'b', col = "blue", main = "Within SSC")
plot(2:10, diff(ssc), type = 'b', col = "blue", main = "Diff SSC")

# ~ 5 looks like optimal
kmn <- klist[[5]]$cluster

plot(X, col = kmn)
  
# OUTLIER DETECTION
#
# How might we identify "anomalous" points?
#========================#

# what if we want to identify possible outliers (noise points) without worrying
# about an intermediate step of specifying kmeans clusters? We can try two 
# approaches: DBSCAN and STRAY
# DBSCAN: https://cran.r-project.org/web/packages/dbscan/index.html
# STRAY: https://robjhyndman.com/publications/stray/

# (1) DBSCAN

# set minpts = 4,
# eps = 0.25, which identifies the distance ranges we
# will consider for clustering. Because we're on the 0-1 range, .25
# corresponds to clustering on roughly 1/4 of the max range of the data

# examine knndist
par(mfrow = c(1,2))
hist(kNNdist(X, k = 4))
kNNdistplot(X, k = 4)


dbs <- dbscan(X, minPts = 5, eps = 0.25)
print(dbs)

# flag noise points
noise <- ifelse(dbs$cluster != 0, 1,2)

# all variables
plot(X, col = noise, pch = noise)

# just 2
par(mfrow = c(1,1))
with(X, plot(mpg~weight, col = noise, pch = noise))

# check noise points
Xtest <- ISLR2::Auto
Xtest$dbs <- dbs$cluster

# "anomalous" observations
head(Xtest[Xtest$dbs == 2,])

# (2) STRAY

# OK, let's try STRAY anomoly detection
# set k = 10
sty <- (find_HDoutliers(X, k = 10, knnsearchtype = "brute"))$out_scores

# flag the top 5%?
sty_flag <- quantile(sty, probs = .95)

Xtest$sty <- ifelse(sty < sty_flag, 1,2)

# all variables
plot(X, col = Xtest$sty, pch = Xtest$sty)

# compare both, looking at just two variables
par(mfrow = c (1,2))
with(X, plot(mpg~weight, col = noise, pch = noise, main = "DBSCAN"))
with(X, plot(mpg~weight, col = Xtest$sty, pch = Xtest$sty, main = "STRAY"))

