#*******************************************************************************
# Network Modeling - HS 2022
# C. Stadtfeld, A. Espinosa-Rada, A. Uzaheta  
# Social Network Labs
# Department of Humanities, Social and Political Sciences
# ETH Zurich
# 10 October 2022

# This script provides the code to run the 
# multiple regression quadratic assignment procedures (MR-QAP) in R.
# For more details please take a look at the lecture notes 
#*******************************************************************************

# ----------------------------- Data description ----------------------------- #
# Network visualization and QAP regression

# The folder "Practical1QAP" contains data from a network study of 
# corporate law partnership carried out in a Northeastern US corporate law firm 
# during the period 1988-1991 in New England. The data were collected over the 
# 71 attorneys (partners and associates) of this firm. 
# Here, we consider only the subset of the 36 partners. 
# The folders includes the following files:
# - Eladv36.dat: adjacency matrix of the advice network
# - Elfriend36.dat: adjacency matrix of the friendship network.
#
# Various partners' attributes are also part of the data set. 
# The file Elattr.dat contains information on
# - id: identifier
# - office: location of the office in which the lawyer work 
#   (1=Boston; 2=Hartford; 3=Providence)
# - seniority: the years spent working for the firm
# - school: law school in which the lawyer studied 
#   (1: Harvard, Yale; 2: ucon; 3: other) 

# We use this data to illustrate the code for visualizing networks and computing
# the QAP regression in R

# ------------------------------- Loading data ------------------------------- #
# Setting the WORKING DIRECTORY
# This is done using the function setwd and the path of the folder
# in which data are and results will be saved

getwd()
setwd("...")

list.files() # Listing the files in the working directory

# Installing and loading the necessary R packages 
# (installation to be done only the first time you use a library):
install.packages("sna")
install.packages("network")
library(sna)
library(network)


# Loading data: 
# - adjacency matrices 
advice <- as.matrix(read.table("ELadv36.dat",header=FALSE))
friendship <- as.matrix(read.table("ELfriend36.dat",header=FALSE))
rownames(advice) <- 1:nrow(advice)
colnames(advice) <- 1:nrow(advice)
rownames(friendship) <- 1:nrow(advice)
colnames(friendship) <- 1:nrow(advice)

# - demographic characteristics of the lawyers
attr <- read.table("ELattr36.dat",header=TRUE)
str(attr)


# ------------------------------ QAP regression ------------------------------ #
# We use the QAP regression to test if lawyers seek out their personal friends 
# for work-related advice

set.seed(1908) #To reproduce the results 
permutations <- 1000 # Number of permutations
nl0 <- netlogit(advice, friendship, rep=permutations, nullhyp="qapy")

# netlm for the linear regression model 
nl0$names <- c("intercept", "friendship")
summary(nl0)

# Predicted probability of an advice tie:
pftie <- exp(nl0$coefficients[1] + nl0$coefficients[2]*1)/
         (1+exp(nl0$coefficients[1] + nl0$coefficients[2]*1))
pftie
pnoftie <- exp(nl0$coefficients[1] + nl0$coefficients[2]*0)/
           (1+exp(nl0$coefficients[1] + nl0$coefficients[2]*0))
pnoftie


# ---------------------------- MR-QAP regression ----------------------------- #
# We use MR-QAP regression to test whether the following hypotheses are supported
# by the data:
# - Hp. 1: Senior lawyers are less likely to ask for advice
# - Hp. 2: Senior lawyers are more likely to be asked for advice
# - Hp. 3: Lawyers are more likely to ask their office mate for advice
# - Hp. 4: Lawyers are more likely to ask lawyers who studied in the same school 
#          for advice
# - Hp. 5: Advice and ``friendship'' relations correlate

# Step 1. Model specification:
# Creating the matrices Z with the information on the explanatory variables.
# Combining these matrices in a list or an array

# Hp 1 and Hp 2: seniority
seniority <- attr[,3]
senioritySender <- matrix(seniority,36,36,byrow=FALSE) # Hp. 1
seniorityReceiver <- matrix(seniority,36,36,byrow=TRUE) # Hp. 2

# Hp 3: office
office <- attr[,2]
sameOffice <- outer(office,office,"==")*1

# Hp 4: school
school <- attr[,4]
sameSchool <- outer(school,school,"==")*1

# Hp. 5: being friend 
friend <- friendship

# The explanatory variables must be combined in a list
zm <- list(senioritySender, seniorityReceiver, sameOffice, sameSchool, friendship)

# Step 2: running the MR-QAP regression
set.seed(1908) #To reproduce the results 
permutations <- 1000 # Number of permutations
nl <- netlogit(advice, zm, rep=permutations, nullhyp="qapspp")
nl$names <- c("intercept","senioritySender", "seniorityReceiver", 
              "sameOffice", "sameSchool", "friendship")
summary(nl)

# Step 3: Model check and interpretation
# Understanding the empirical p-values
z.values <- rbind(nl$dist,nl$tstat)
p.values <- function(x,permutations){
  sum(abs(x[1:permutations]) > abs(x[permutations+1]))/permutations}
empirical.p.values <- apply(z.values,2,p.values,permutations)
empirical.p.values

# Visualizing the empirical p-values
par(mfrow=c(2,3))
for (i in 1:6)
{
  hist(nl$dist[,i],breaks=30,xlim=c(min(c(nl$tstat[i],nl$dist[,i]))-1,
                                    max(c(nl$tstat[i],nl$dist[,i]))+1),
       main=nl$names[i],xlab="z-values")
  abline(v=nl$tstat[i],col="red",lwd=3,lty=2)
}

# Interpreting the parameters (see the commented output in the lecture notes)

# Step 4: Format and export the results
res <- summary(nl)
expRes <- cbind(res$coefficients, exp(res$coefficients), res$se, res$pgreqabs)
colnames(expRes) <- c("ESt.", "exp(Est.)", "s.e.", "p-value")
rownames(expRes) <- res$names
expRes
write.csv(expRes,"resQAP.csv")

# Exporting results in tex
library(xtable)
xtable(expRes,digits=3)



# ---------------------- MR-QAP and logistic regression ---------------------- #
# If we had used a standard logistic regression model, we would have obtained 
# a different result. 

# Create a data frame with the ids of the sender and receiver 
# and the value of the advice relation
dataLogit <- data.frame(sender = c(row(advice)), 
           receiver = c(col(advice)), 
           adviceTie = c(advice))
# Adding the explanatory variables as columns
dataLogit <- cbind(dataLogit, as.vector(senioritySender), 
                   as.vector(seniorityReceiver), 
                   as.vector(sameOffice),as.vector(sameSchool), 
                   as.vector(friend))
colnames(dataLogit) <- c("sender","receiver","adviceTie","senioritySender",
                         "seniorityReceiver","sameOffice", "sameSchool",
                         "friend") 
head(dataLogit)

# Discard the diagonal values
dataLogit <- dataLogit[-which(dataLogit$sender == dataLogit$receiver),]

# Estimate the logistic regression model
mod0 <- glm(adviceTie ~ senioritySender + seniorityReceiver + sameOffice + 
            sameSchool + friend, family = "binomial", data = dataLogit)
summary(mod0)
