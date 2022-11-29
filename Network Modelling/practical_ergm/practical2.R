#******************************************************************************************************
# Network Modeling - HS 2022
# Christoph Stadtfeld, Alejandro Espinosa-Rada, Alvaro Uzaheta
# Social Network Lab
# Department of Humanities, Social and Political Sciences
# ETH Zurich
# 25 October 2022

#******************************************************************************************************
# Working directory and packages
#******************************************************************************************************
# TODO: change this path to your working folder, with the folder knecht.zip unzipped:
setwd("")
list.files()

# Installing packages
# install.packages(c("sna","network","igraph", "ggraph"))
# library(devtools)
# install_version("ergm", version = "3.11.0", repos = "http://cran.us.r-project.org")
# We use the old version of ergm, as the new version seems to have different default
# estimation methods which do not allow our models to fit how we would expect

# Loading libraries
library(sna)
library(network)
library(ergm) # take a wild guess...
library(igraph) # for data objects and plotting
library(ggraph) # for plotting
# library(patchwork) # for plotting networks side-by-side

#******************************************************************************************************
# Loading data
#******************************************************************************************************
# The folder knecht.zip contains data collected by Andrea Knecht. This data is about a friendship network
# in a Dutch school class. The data were collected between September 2003 and June 2004 by Andrea Knecht,
# supervised by Chris Baerveldt, at the Department of Sociology of the University of Utrecht (NL).
#
# The 25 students were followed over their first year at secondary school during which friendship networks
# as well as other data were assessed at four time points at intervals of three months. There were 16 girls
# and 9 boys in the class, aged 11-13 at the beginning of the school year. Network data were assessed by
# asking students to indicate up to twelve classmates which they considered good friends.
# Delinquency is defined as a rounded average over four types of minor delinquency
# (stealing, vandalism, graffiti, and fighting), measured in each of the four waves of data collection.
# The five-point scale ranged from `never' to `more than 10 times', and the distribution is highly skewed
# Data description and data from http://www.stats.ox.ac.uk/~snijders/siena/siena.html.
#
# The folder contains the following files:
# - net.csv: contains the adjacency matrix of the network observed at the third time point
# - demographics.csv: contains the following five variables:
#       - gender (1 = girl, 2 = boy)
#       - age (years)
#       - ethnicity (1 = Dutch, 2 = other, 0 = missing)
#       - religion (1 = Christian, 2 = non-religious, 3 = non-Christian religion, 0 = missing)
#       - delinquency: rounded average of four items concerning stealing, vandalizing, fighting, graffiti
#         (categories: frequency over last three months, 1 = never, 2 = once, 3 = 2-4 times, 4 = 5-10
#         times, 5 = more than 10 times; 0 = missing.)
# - primary.csv: contains a variable indicating whether the pupils attend the same primary school
# (0 = no, 1 = yes)


# Reading the adjacency matrices of the friendship network
friendship <- as.matrix(read.csv("Knecht/net.csv", header = FALSE))
colnames(friendship) <- 1:nrow(friendship)
rownames(friendship) <- 1:nrow(friendship)

# Reading the demographic characteristics of the students
attributes <- read.csv("Knecht/demographics.csv", header = TRUE)

# Reading the information on the primary school of the students
primary <- as.matrix(read.csv("Knecht/primary.csv", header = FALSE))


#******************************************************************************************************
# Descriptive statistics and visualization
#******************************************************************************************************

# Attributes
View(attributes)
table(attributes$gender)
table(attributes$age)
table(attributes$delinquency)

# Network
View(friendship)

nvertices <- nrow(friendship) # Number of vertexes
nvertices
nedges <- sum(friendship) # Number of ties
nedges
density <- nedges / (nvertices * (nvertices - 1)) # Density
density
outdegree <- rowSums(friendship) # Outdegree distribution
mean(outdegree)
sd(outdegree)
indegree <- colSums(friendship) # Indegree distribution
mean(indegree)
sd(indegree)
par(mfrow = c(2, 1), mar = c(4, 3, 1, 3))
hist(outdegree, xlab = "Outdegree", col = "grey", main = "")
hist(indegree, xlab = "Indegree", col = "grey", main = "")

# Creating a network object
netw <- network(friendship, directed = TRUE)

# Adding attributes
netw %v% "gender" <- attributes$gender
netw %v% "age" <- attributes$age
netw %v% "ethnicity" <- attributes$ethnicity
netw %v% "religion" <- attributes$religion
netw %v% "delinquency" <- attributes$delinquency
netw %e% "primary" <- primary
netw

# Visualize the network with the sna package
par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
plot(netw)


# now prettier with ggraph (also possible in sna! However, I prefer ggraph for its grammar and consistency
# with e.g. ggplot2)

ggraph(netw) +
  geom_edge_link0(aes(colour = as.factor(primary)),
    arrow = arrow(
      angle = 10,
      length = unit(4, "mm"),
      type = "closed"
    )
  ) +
  scale_edge_colour_manual(values = c("1" = "black", "0" = "darkgrey")) +
  geom_node_point(
    size = 5,
    aes(
      shape = as.factor(gender),
      colour = as.factor(religion)
    )
  ) +
  theme_graph()


# better-customized version
ggraph(netw) +
  geom_edge_link0(aes(colour = as.factor(primary)),
    arrow = arrow(
      angle = 10,
      length = unit(4, "mm"),
      type = "closed"
    )
  ) +
  scale_edge_colour_manual(
    name = "Same primary",
    values = c("1" = "black", "0" = "darkgrey"),
    labels = c("0" = "false", "1" = "true")
  ) +
  geom_node_point(
    size = 5,
    aes(
      shape = as.factor(gender),
      fill = as.factor(religion)
    ),
    colour = "black"
  ) +
  scale_fill_discrete(
    name = "Religion",
    labels = c(
      "0" = "missing",
      "1" = "Christian",
      "2" = "nonreligious",
      "3" = "other religion"
    )
  ) +
  guides(fill = guide_legend(
    override.aes = list(shape = 21),
    labels = c(
      "0" = "missing",
      "1" = "Christian",
      "2" = "nonreligious",
      "3" = "other religion"
    )
  )) +
  scale_shape_manual(
    name = "Gender",
    values = c("1" = 21, "2" = 22),
    labels = c("1" = "woman", "2" = "man")
  ) +
  theme_graph()


#******************************************************************************************************
# ERGM estimation
#******************************************************************************************************

# ------------------------------------ Tie independence Model ------------------------------------
# ERGM
model0 <- ergm(netw ~ edges)
summary(model0)

# Logistic regression model
diag(friendship) <- NA # Exclude the diagonal
model.log <- glm(c(friendship) ~ 1, family = binomial) # 1 indicates an intercept
# only logistic regression
summary(model.log)

# TODO: we want to compute p, the probability of observing a tie
theta1 <- coef(model0)
p <- 0 # TODO: replace with the right value
p

# Remember the density of the network?
density

# ------------------------------------ Dyadic dependence Model ------------------------------------
model1 <- ergm(netw ~ edges + mutual)
summary(model1)

# Computing the probability of a tie not reciprocating an existing tie is
theta1 <- model1$coef[1]
oNorecip <- exp(theta1)
pNorecip <- oNorecip / (1 + oNorecip)

# Computing the probability of a tie reciprocating an existing tie is
theta2 <- model1$coef[2]
pRecip <- 0 # TODO: replace this with the right value


# ------------------------------------ Markov random graph model ------------------------------------
model2 <- ergm(netw ~ edges + mutual + ttriple)


# ------------------------------------ Partial conditional dependence model ------------------------------------
# Solving near-degeneracy
set.seed(1986)
model2.2 <- ergm(netw ~ edges + mutual + gwesp(decay = 0.3, fixed = TRUE))
summary(model2.2)

# For a model including covariates, which statistics should we consider?
vignette("ergm-term-crossRef")

# Selecting the admissible statistics given the data
search.ergmTerms(keywords = c("binary", "directed"))


# Some hypotheses we want to test:
# H1: there is a tendency towards reciprocity
# H2: there is a tendency towards transitivity
# H3: there is a tendency towards gender homophily
# H4: pupils who attended the same school are more likely to be friends
# H5: pupils with higher delinquent behavior tend to have a higher number of friends than pupils with a low behavior
# H6: pupils with higher delinquent behavior tend to be more popular than pupils with a low behavior



# TODO: add the effects related to delinquency hypotheses
# which two are the correct effects for this of the four below?
# nodeifactor, nodeicov, nodeocov, nodeofactor
set.seed(1986)
model3 <- ergm(netw ~ edges + mutual + gwesp(decay = 0.3, fixed = TRUE)
  + nodematch("gender") + edgecov(primary)
  + ...)


#******************************************************************************************************
# ERGM diagnostics and fit
#******************************************************************************************************

# ------------------------------------ Model convergence ------------------------------------
mcmc.diagnostics(model3)


# ------------------------------------ Goodness of fit ------------------------------------
model3gof <- gof(model3)
model3gof

par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
plot(model3gof)


#******************************************************************************************************
# ERGM interpretation
#******************************************************************************************************
summary(model3)


#******************************************************************************************************
# ERGM simulation (time permitting)
#******************************************************************************************************

# Simulating networks with higher parameter values for the nodeifactor statistics
newcoef <- model3$coeff
newcoef[6:7] <- c(0.7, 0.9)
simNets <- simulate(netw ~ edges + mutual + gwesp(decay = 0.3, fixed = TRUE) +
  nodematch("gender") + edgecov(primary)
  + ...,
nsim = 1, coef = unlist(newcoef)
)

# Representing the network and compare it with the observed one


p1 <- ggraph(netw) +
  geom_edge_link0( # aes(colour=as.factor(primary))
  ) + # 1= attend same primary school
  geom_node_point(
    size = 5,
    aes(
      shape = as.factor(gender), # 1 = girl, 2= boy
      colour = delinquency
    ) # delinquency level
  ) +
  theme_graph() +
  ggtitle("Observed")


p2 <- ggraph(simNets) +
  geom_edge_link0( # aes(colour=primary)
  ) + # 1= attend same primary school
  geom_node_point(
    size = 5,
    aes(
      shape = as.factor(gender), # 1 = girl, 2= boy
      colour = delinquency
    ) # delinquency level
  ) +
  theme_graph() +
  ggtitle("Simulated")

p1 / p2 + plot_layout(guides = "collect")


# Comparing the expected values of the statistics under the new parameter values with the observed one
observed <- summary(netw ~ edges + mutual + gwesp(decay = 0.3, fixed = TRUE) +
  nodematch("gender") + edgecov(primary))

simNetsStat <- simulate(netw ~ edges + mutual + gwesp(decay = 0.3, fixed = TRUE) +
  nodematch("gender") + edgecov(primary),
nsim = 1000,
coef = newcoef, output = "stats"
)
expected <- apply(simNetsStat, 2, mean)
observed
expected
