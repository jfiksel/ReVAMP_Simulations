library(openVA)
library(ReVAMP)
library(dplyr)
library(Rsolnp)
set.seed(123)
### Read in child data
child.raw <- read.csv(getPHMRC_url("child"))
### Get data frame matching va34 code to actual COD
cause.df <- unique(child.raw[,c("gs_text34", "va34")])
### Reduce these to the top 3 COD + other (but not including Other defined causes in top 3)
### VA34 code for other defined COD is 14
top3cod <- names(sort(table(child.raw$gs_text34[child.raw$va34 != 14]), decreasing = TRUE)[1:3])
top3cause.df <- cause.df[cause.df$gs_text34 %in% top3cod,]
### Add row for other
top3cause.df$gs_text34 <- as.character(top3cause.df$gs_text34)
top3cause.df <- rbind(top3cause.df, c("Other", 99))
### Clean data into output usable by Tariff & InsilicoVA
child.clean <- ConvertData.phmrc(child.raw, phmrc.type = "child")$output

### Read in the CSMFs that we will be using for test & train
csmf.split <- readRDS(file.path("..", "data", "csmf.rds"))

### Create setting data frame with number of runs (1-100) and each split type on it
setting.df <- expand.grid(run = 1:100, split = 1:5, calib.size = c(100, 200, 400, 600))

### Create list of seeds
all.seeds <- sample(1e6, size = nrow(setting.df), replace = F)

### i is row of setting.df
#i <- as.numeric(commandArgs(trailingOnly = TRUE))
i <- as.numeric(commandArgs(trailingOnly = TRUE))
setting <- setting.df[i,]
set.seed(all.seeds[i])

### Get CSMF splits & CSMF accuracy between these splits
ptrain <- csmf.split$ptrain[setting$split,]
ptest <- csmf.split$ptest[setting$split,]
csmf.acc.split <- 1 - sum(abs(ptest - ptrain)) / 2 * (1 - min(ptest))

### Add these probabilities into our top3cause.df object
top3cause.df$ptrain <- ptrain
top3cause.df$ptest <- ptest

### Randomly split data into train and test
smp_size <- floor(.5 * nrow(child.clean))
train_ind <- sample(seq_len(nrow(child.clean)), size = smp_size)
train.init <- child.clean[train_ind,]
test.init <- child.clean[-train_ind,]

### Split each of these data frames by cause
train.cause.split <- ifelse(train.init$Cause %in% top3cause.df$va34[1:3], train.init$Cause, 99)
train.split <- split(train.init, train.cause.split)

test.cause.split <- ifelse(test.init$Cause %in% top3cause.df$va34[1:3], test.init$Cause, 99)
test.split <- split(test.init, test.cause.split)

### Number of each type of cause in test & train
#Train
top3cause.df$train.n <- floor(top3cause.df$ptrain * 2500)
top3cause.df$train.n[4] <- 2500 - sum(top3cause.df$train.n[1:3])
# Test
top3cause.df$test.n <- floor(top3cause.df$ptest * (2500 + setting$calib.size))
top3cause.df$test.n[4] <- 2500 + setting$calib.size - sum(top3cause.df$test.n[1:3])

### First resample for the causes for the training set
train.list <- lapply(seq_along(train.split), function(i) {
    train.cause <- train.split[[i]]
    cause <- names(train.split)[i]
    ncause <- top3cause.df[top3cause.df$va34 == cause, "train.n"]
    return(train.cause[sample(nrow(train.cause), ncause, replace = TRUE),]) 
})
train.final <- do.call(rbind, train.list)

### Now for test + calibration set
test.list <- lapply(seq_along(test.split), function(i) {
    test.cause <- test.split[[i]]
    cause <- names(test.split)[i]
    ncause <- top3cause.df[top3cause.df$va34 == cause, "test.n"]
    return(test.cause[sample(nrow(test.cause), ncause, replace = TRUE),]) 
})
test.final <- do.call(rbind, test.list)

### Now break up test into test + calibration
calib.index <- sample(1:nrow(test.final), setting$calib.size, replace = F)
calib.final <- test.final[calib.index,]
test.final <- test.final[-calib.index,]
################################
### Make data directory for this run
sim.dir <- file.path("..", "data", "simulation_runs",
                     paste0("split_", setting$split, "_size_", setting$calib.size),
                     paste0("run_", setting$run))
dir.create(sim.dir, recursive = TRUE)

### Quit if results file already exists
if(file.exists(file.path(sim.dir, "results.rds"))){
    quit('no')
}
################################
### Now the 6 methods w/o calibration
### Tariff with just training set (will also predict on calibration data)

### Function to change all guesses not equal to top 3 causes to other
changeTopCOD <- function(topcod, topcause.df = top3cause.df) {
    topcod <- as.character(topcod)
    ncauses <- sum(topcause.df$va34 != "99")
    topcod <- ifelse(topcod %in% topcause.df$va34[1:ncauses], topcod, "99")
    return(topcod)
}

if(!file.exists(file.path(sim.dir, "tariff_train.rds"))){
    set.seed(123)
    tariff.train <- codeVA(data = rbind(calib.final, test.final),
                           data.type = "customize", model = "Tariff",
                           data.train = train.final, causes.train = "Cause")
    tariff.train.cod <- changeTopCOD(getTopCOD(tariff.train)[,2])
    saveRDS(tariff.train.cod, file.path(sim.dir, "tariff_train.rds"))
} else {
    tariff.train.cod <- readRDS(file.path(sim.dir, "tariff_train.rds"))
}

### Tariff with training + calibration
if(!file.exists(file.path(sim.dir, "tariff_train_calib.rds"))){
    set.seed(123)
    tariff.train.calib <- codeVA(data = test.final,
                                 data.type = "customize", model = "Tariff",
                                 data.train = rbind(train.final, calib.final), causes.train = "Cause")
    tariff.train.calib.cod <- changeTopCOD(getTopCOD(tariff.train.calib)[,2])
    saveRDS(tariff.train.calib.cod, file.path(sim.dir, "tariff_train_calib.rds"))
} else {
    tariff.train.calib.cod <- readRDS(file.path(sim.dir, "tariff_train_calib.rds"))
}
### Tariff with just calibration
if(!file.exists(file.path(sim.dir, "tariff_calib.rds"))){
    set.seed(123)
    tariff.calib <- codeVA(data = test.final,
                           data.type = "customize", model = "Tariff",
                           data.train = calib.final, causes.train = "Cause")
    tariff.calib.cod <- changeTopCOD(getTopCOD(tariff.calib)[,2])
    saveRDS(tariff.calib.cod, file.path(sim.dir, "tariff_calib.rds"))
} else {
    tariff.calib.cod <- readRDS(file.path(sim.dir, "tariff_calib.rds"))
}

### Set number of simulations for insilico
insilico.nsim <- 5000
### Commented out line for interactive testing
#insilico.nsim <- 500

### Insilico with just training set
if(!file.exists(file.path(sim.dir, "insilico_train.rds"))){
    set.seed(123)
    insilico.train <- codeVA(data = rbind(calib.final, test.final),
                             data.type = "customize", model = "InSilicoVA",
                             data.train = train.final, causes.train = "Cause",
                             jump.scale = 0.05, Nsim=insilico.nsim, auto.length = FALSE)
    insilico.train.cod <- changeTopCOD(getTopCOD(insilico.train)[,2])
    saveRDS(insilico.train.cod, file.path(sim.dir, "insilico_train.rds"))
} else {
    insilico.train.cod <- readRDS(file.path(sim.dir, "insilico_train.rds"))
}

### Insilico with training + calibration set
if(!file.exists(file.path(sim.dir, "insilico_train_calib.rds"))) {
    set.seed(123)
    insilico.train.calib <- codeVA(data = test.final,
                                   data.type = "customize", model = "InSilicoVA",
                                   data.train = rbind(train.final, calib.final), causes.train = "Cause",
                                   jump.scale = 0.05, Nsim=insilico.nsim, auto.length = FALSE)
    insilico.train.calib.cod <- changeTopCOD(getTopCOD(insilico.train.calib)[,2])
    saveRDS(insilico.train.calib.cod, file.path(sim.dir, "insilico_train_calib.rds"))
} else {
    insilico.train.calib.cod <- readRDS(file.path(sim.dir, "insilico_train_calib.rds"))
}

### Insilico with just calibration set
if(!file.exists(file.path(sim.dir, "insilico_calib.rds"))){
    set.seed(123)
    insilico.calib <- codeVA(data = test.final,
                             data.type = "customize", model = "InSilicoVA",
                             data.train = calib.final, causes.train = "Cause",
                             jump.scale = 0.05, Nsim=insilico.nsim, auto.length = FALSE) 
    insilico.calib.cod <- changeTopCOD(getTopCOD(insilico.calib)[,2])
    saveRDS(insilico.calib.cod, file.path(sim.dir, "insilico_calib.rds"))
} else {
    insilico.calib.cod <- readRDS(file.path(sim.dir, "insilico_calib.rds"))
}


### Get the COD estimates for test & calibration set from tariff & insilicova
tariff.train.cod.test <- tariff.train.cod[-(1:setting$calib.size)]
tariff.train.cod.calib <- tariff.train.cod[1:setting$calib.size]

insilico.train.cod.test <- insilico.train.cod[-(1:setting$calib.size)]
insilico.train.cod.calib <- insilico.train.cod[1:setting$calib.size]


#Calibration truth
calib.truth <- changeTopCOD(calib.final$Cause)

### Get causes (these will be the ordering we will use)
causes <- top3cause.df$va34

### MLE Methods
set.seed(123)
tariff.mle <- mle.calibration(tariff.train.cod.test, tariff.train.cod.calib, calib.truth, causes)

set.seed(123)
insilico.mle <- mle.calibration(insilico.train.cod.test, insilico.train.cod.calib, calib.truth, causes)

### ReVAMP
epsilon <- .001
alpha <- .001
beta <- .001
tau <- .1
tau.vec <- rep(tau, length(causes))
delta <- 1
gamma.init <- 1
ndraws <- 50E3
nchains <- 3

### ReVAMP with Tariff
set.seed(123)
revamp.seeds <- sample(1e6, nchains, replace = F)
tariff.revamp <- lapply(1:nchains, function(i) {
    set.seed(revamp.seeds[i])
    revamp.sampler(test.cod = tariff.train.cod.test, calib.cod = tariff.train.cod.calib,
                   calib.truth = calib.truth, causes = causes,
                   epsilon = epsilon, alpha=alpha, beta=beta,
                   tau.vec=tau.vec, delta=delta,
                   gamma.init=gamma.init, ndraws = ndraws)
})

### ReVAMP with InSilicoVA
set.seed(123)
revamp.seeds <- sample(1e6, nchains, replace = F)
insilico.revamp <- lapply(1:nchains, function(i) {
    set.seed(revamp.seeds[i])
    revamp.sampler(test.cod = insilico.train.cod.test, calib.cod = insilico.train.cod.calib,
                   calib.truth = calib.truth, causes = causes,
                   epsilon = epsilon, alpha=alpha, beta=beta,
                   tau.vec=tau.vec, delta=delta,
                   gamma.init=gamma.init, ndraws = ndraws)
})


### Get posterior draws for CSMF parameters
burnin <- 10E3
thin <- 10

tariff.revamp.csmf.list <- lapply(1:nchains, function(i) {
    revampCSMF(tariff.revamp[[i]], burnin = burnin, thin = thin)
})
tariff.revamp.csmf.df <- do.call(rbind, tariff.revamp.csmf.list)

insilico.revamp.csmf.list <- lapply(1:nchains, function(i) {
    revampCSMF(insilico.revamp[[i]], burnin = burnin, thin = thin)
})
insilico.revamp.csmf.df <- do.call(rbind, insilico.revamp.csmf.list)


### Function to get CSMF estimate for ReVAMP
revampCSMFDensityEstimate <- function(revamp.csmf.df, causes) {
    csmf.init <- sapply(causes, function(c) {
        revamp.cause <- revamp.csmf.df[revamp.csmf.df$cause == c,]
        dens <- density(revamp.cause$p)
        return(dens$x[which.max(dens$y)])
    })
    return(csmf.init / sum(csmf.init))
}

revampCSMFMeanEstimate <- function(revamp.csmf.df, causes) {
    csmf.init <- sapply(causes, function(c) {
        revamp.cause <- revamp.csmf.df[revamp.csmf.df$cause == c,]
        return(mean(revamp.cause$p))
    })
    return(csmf.init)
}

### Function to get CSMF estimate from openVA
openVACSMF <- function(topcod, causes) {
    csmf <- sapply(causes, function(c) mean(topcod == c))
    return(csmf)
}

### Get CSMF estimates 
tariff.revamp.csmf <- revampCSMFMeanEstimate(tariff.revamp.csmf.df, causes)
insilico.revamp.csmf <- revampCSMFMeanEstimate(insilico.revamp.csmf.df, causes)
tariff.mle.csmf <- tariff.mle$p
insilico.mle.csmf <- insilico.mle$p
tariff.train.csmf <- openVACSMF(tariff.train.cod.test, causes)
tariff.train.calib.csmf <- openVACSMF(tariff.train.calib.cod, causes)
tariff.calib.csmf <- openVACSMF(tariff.calib.cod, causes)
insilico.train.csmf <- openVACSMF(insilico.train.cod.test, causes)
insilico.train.calib.csmf <- openVACSMF(insilico.train.calib.cod, causes)
insilico.calib.csmf <- openVACSMF(insilico.calib.cod, causes)

### CSMF  data frame
methods <- c("tariff_revamp",
             "insilico_revamp",
             "tariff_mle",
             "insilico_mle",
             "tariff_train",
             "tariff_train_and_calib",
             "tariff_calib",
             "insilico_train",
             "insilico_train_and_calib",
             "insilico_calib")
csmf.df <- data.frame(csmf.est = c(tariff.revamp.csmf,
                                   insilico.revamp.csmf,
                                   tariff.mle.csmf,
                                   insilico.mle.csmf,
                                   tariff.train.csmf,
                                   tariff.train.calib.csmf,
                                   tariff.calib.csmf,
                                   insilico.train.csmf,
                                   insilico.train.calib.csmf,
                                   insilico.calib.csmf),
                      cause = rep(causes, length(methods)),
                      cause.text = rep(top3cause.df$gs_text34, length(methods)),
                      true.csmf = rep(top3cause.df$ptest, length(methods)),
                      method = rep(methods, each = nrow(top3cause.df)))

### Now CSMF Accuracy
ptrue <- top3cause.df$ptest
csmf.acc.df <- data.frame(csmf.acc = c(getCSMF_accuracy(tariff.revamp.csmf, ptrue),
                                       getCSMF_accuracy(insilico.revamp.csmf, ptrue),
                                       getCSMF_accuracy(tariff.mle.csmf, ptrue),
                                       getCSMF_accuracy(insilico.mle.csmf, ptrue),
                                       getCSMF_accuracy(tariff.train.csmf, ptrue),
                                       getCSMF_accuracy(tariff.train.calib.csmf, ptrue),
                                       getCSMF_accuracy(tariff.calib.csmf, ptrue),
                                       getCSMF_accuracy(insilico.train.csmf, ptrue),
                                       getCSMF_accuracy(insilico.train.calib.csmf, ptrue),
                                       getCSMF_accuracy(insilico.calib.csmf, ptrue)),
                          method = methods)

### Now CCC 
### Need custom function
ccc <- function(cod.est, cod.truth, causes) {
    C <- length(causes)
    ccc <- sapply(seq_along(causes), function(j) {
        cause.j <- causes[j]
        correct.assign <- sum(cod.est == cause.j & cod.truth == cause.j)
        total <- sum(cod.truth == cause.j)
        if(total == 0) {
            total <- 1
        }
        numerator <- (correct.assign / total) - (1 / C)
        denominator <- 1 - (1 / C)
        return(numerator / denominator)
    })
}

cod.truth <- changeTopCOD(test.final$Cause)

### Get CCC estimates for each cause for each method
### Only use first chain for this
tariff.revamp.cod <- revampIndPredictions(tariff.revamp[[1]],
                                          test.cod = tariff.train.cod.test,
                                          causes = causes, burnin = burnin,
                                          thin = thin)$topCOD
tariff.revamp.ccc <- ccc(tariff.revamp.cod, cod.truth, causes)
insilico.revamp.cod <- revampIndPredictions(insilico.revamp[[1]],
                                            test.cod = insilico.train.cod.test,
                                            causes = causes, burnin = burnin,
                                            thin = thin)$topCOD
insilico.revamp.ccc <- ccc(insilico.revamp.cod, cod.truth, causes)

tariff.train.ccc <- ccc(tariff.train.cod.test, cod.truth, causes)
tariff.train.calib.ccc <- ccc(tariff.train.calib.cod, cod.truth, causes)
tariff.calib.ccc <- ccc(tariff.calib.cod, cod.truth, causes)
insilico.train.ccc <- ccc(insilico.train.cod.test, cod.truth, causes)
insilico.train.calib.ccc <- ccc(insilico.train.calib.cod, cod.truth, causes)
insilico.calib.ccc <- ccc(insilico.calib.cod, cod.truth, causes)

methods.ccc <- methods[!grepl("mle", methods)]
ccc.df <- data.frame(ccc.cause = c(tariff.revamp.ccc,
                                 insilico.revamp.ccc,
                                 tariff.train.ccc,
                                 tariff.train.calib.ccc,
                                 tariff.calib.ccc,
                                 insilico.train.ccc,
                                 insilico.train.calib.ccc,
                                 insilico.calib.ccc),
                      cause = rep(causes, length(methods.ccc)),
                      cause.text = rep(top3cause.df$gs_text34, length(methods.ccc)),
                      true.ccc = rep(top3cause.df$ptest, length(methods.ccc)),
                      method = rep(methods.ccc, each = nrow(top3cause.df)))

avg.ccc.df <- data.frame(ccc.mean = c(mean(tariff.revamp.ccc),
                                      mean(insilico.revamp.ccc),
                                      mean(tariff.train.ccc),
                                      mean(tariff.train.calib.ccc),
                                      mean(tariff.calib.ccc),
                                      mean( insilico.train.ccc),
                                      mean( insilico.train.calib.ccc),
                                      mean(insilico.calib.ccc)),
                         method = methods.ccc)
results <- list(topcause.df = top3cause.df,
               csmf = csmf.df,
               csmf.acc = csmf.acc.df,
               ccc.cause = ccc.df,
               avg.ccc = avg.ccc.df,
               csmf.acc.split = csmf.acc.split) 
results.file <- file.path(sim.dir, "results.rds")
saveRDS(results, results.file)
quit('no')

