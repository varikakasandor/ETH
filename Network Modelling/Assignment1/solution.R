library("sna")
n_managers = 21

n_ties = 190
n_mutual = 45
n_asymm = 100
n_null = 65

p1= n_ties / (n_managers*(n_managers-1))
p2_m = n_mutual/choose(n_managers, 2)
p2_a = n_asymm/choose(n_managers, 2)
p2_n = n_null/choose(n_managers, 2)

p3_mutual = p1*p1
p3_asymm = p1*(1-p1)*2
p3_null = (1-p1)*(1-p1)

#4
# yes probabilities are similar
d = 9
p5 = d / (n_managers - 1)
p5_m = p5*p5

#5


#6
obsMat <- read.csv("matrix.csv", header = FALSE)
cugRec <- cug.test(obsMat, grecip, cmode = "edges", reps=5000)
cugInd <- cug.test(obsMat, centralization, cmode="edges", FUN.arg=list(FUN=degree,cmode="indegree"), reps=5000)
cugTrans <- cug.test(obsMat, gtrans, cmode = "dyad.census", reps=5000)

##TASK 2
trade1 <- read.csv("trade1.csv", header = FALSE)
trade2 <- read.csv("trade2.csv", header = FALSE)
rownames(trade1) <- 1:nrow(trade1)
colnames(trade1) <- 1:nrow(trade1)
rownames(trade2) <- 1:nrow(trade2)
colnames(trade2) <- 1:nrow(trade2)

attr <- read.csv("attr.csv", header = TRUE)
str(attr)

set.seed(1908)
permutations = 1000

#1
nl0 <- netlm(trade2, trade1, reps=permutations, nullhyp="qapy")
nl0$names <- c("intercept", "association")
summary(nl0)

#2
sector <- attr$sector
sameSector <- outer(sector, sector, "==")*1 #hp1
publicMore <- (!matrix(sector, 18,18, byrow=FALSE))*1 #hp2

zm <- list(trade1, sameSector, publicMore)

#3

nl1 <- netlm(trade2, zm, reps = permutations, nullhyp = "qapspp")
nl1$names <- c("intercept", "association", "sameSector", "publicMore")
summary(nl1)

# The same sector hypothesis is confirmed, while the public more hypothesis cannot be supported given the data available.

#4
# Hp 3: The larger the (absolute) size difference is between the two companies, the less trades happen.
# Hp 4: Companies in the same region trade more.

copmanySize <- attr$size
sizeDifference <- abs(outer(copmanySize, copmanySize, "-"))

region <- attr$region
sameRegion <- outer(region, region, "==")*1

zm_extended <- list(trade1, sameSector, publicMore, sizeDifference, sameRegion)

#5
nl2 <- netlm(trade2, zm_extended, reps = permutations, nullhyp = "qapspp")
nl2$names <- c("intercept", "association", "sameSector", "publicMore", "sizeDifference", "sameRegion")
summary(nl2)

# The size difference hypothesis cannot be supported, but the same region hypothesis is confirmed.
