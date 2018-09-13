library(openVA)
library(ReVAMP)
library(dplyr)
library(Rsolnp)
set.seed(123)
### Read in child data
child.raw <- read.csv(getPHMRC_url("child"))
### Get data frame matching va34 code to actual COD
cause.df <- unique(child.raw[,c("gs_text34", "va34")])
cause.df$va34 <- as.character(as.numeric(cause.df$va34))
### Clean data into output usable by Tariff & InsilicoVA
child.clean <- ConvertData.phmrc(child.raw, phmrc.type = "child")$output
### Assign countries
countries <- rep(NA, nrow(child.raw))
countries[child.raw$site %in% c("AP", "UP")] <- "India"
countries[child.raw$site %in% c("Mexico")] <- "Mexico"
countries[child.raw$site %in% c("Dar", "Pemba")] <- "Tanzania"
countries[child.raw$site %in% c("Bohol")] <- "Philippines"
### Create setting data frame with number of runs (1-500) and each split type on it
### ncod will be the number of total causes: top (ncod - 1) causes + other
setting.df <- expand.grid(run = 1:500,
                          country = c("India", "Tanzania"),
                          calib.size = c(50, 100, 200, 400),
                          ncod = c(4, 8))
### Create list of seeds
all.seeds <- sample(1e6, size = nrow(setting.df), replace = F)

### i is row of setting.df
#i <- as.numeric(commandArgs(trailingOnly = TRUE))
#i <- 2485 for setting where insilico_calib does well
i <- as.numeric(commandArgs(trailingOnly = TRUE))
setting <- setting.df[i,]
ncod <- setting$ncod
set.seed(all.seeds[i])
### split data into country we are using and other
country.data <- child.clean[countries == setting$country,]
train.init <- child.clean[countries != setting$country,]
### Reduce these to the top 7 COD + other (but not including Other defined causes in top 7) for the country data
### VA34 code for other defined COD is 14
top.cod <- names(sort(table(country.data$Cause[country.data$Cause != 14]), decreasing = TRUE)[1:(ncod - 1)])
top.cause.df <- cause.df[cause.df$va34 %in% top.cod,]
### Add row for other
top.cause.df$gs_text34 <- as.character(top.cause.df$gs_text34)
top.cause.df <- rbind(top.cause.df, c("Other", 99))

### Get distribution of these causes in the train set
### first, function to change all guesses not equal to top 3 causes to other
changeTopCOD <- function(topcod, topcause.df = top.cause.df) {
    topcod <- as.character(topcod)
    ncauses <- sum(topcause.df$va34 != "99")
    topcod <- ifelse(topcod %in% topcause.df$va34[1:ncauses], topcod, "99")
    return(topcod)
}

top.cause.df$ptrain <- sapply(top.cause.df$va34, function(c) mean(changeTopCOD(train.init$Cause) == c))
top.cause.df$ptest_orig <- sapply(top.cause.df$va34, function(c) mean(changeTopCOD(country.data$Cause) == c))

### Read in the CSMFs that we will be using for ptest
### this should correspond to NCOD
data.dir <-file.path("..", "..", "data") 
csmf.test <- readRDS(file.path(data.dir, paste0("csmf_country_sims_", ncod, ".rds")))
ptest <- csmf.test[i,]

### Add these probabilities into our top.cause.df object
top.cause.df$ptest <- ptest

### Randomly split country data into training and calibration
test_smp_size <- floor(.66 * nrow(country.data))
test_ind <- sample(seq_len(nrow(country.data)), size = test_smp_size)
test.init <- country.data[test_ind,]
calib.init <- country.data[-test_ind,]

### Split each of these data frames by cause
train.split <- split(train.init, changeTopCOD(train.init$Cause))
calib.split <- split(calib.init, changeTopCOD(calib.init$Cause))
test.split <- split(test.init, changeTopCOD(test.init$Cause))

#### Function to resample data set
resample.data <- function(data.split, nresamp, causes, csmf) {
    ### csmf should be in same order as causes
    C <- length(causes)
    ncause <- floor(nresamp * csmf)
    ncause[C] <- nresamp - sum(ncause[1:(C-1)])
    
    data.list <- lapply(seq_along(causes), function(i) {
        cause <- causes[i]
        if(length(which(names(data.split) == cause)) == 0) {
            return(NULL)
        } else {
            data.cause <- data.split[[which(names(data.split) == cause)]]
            return(data.cause[sample(nrow(data.cause), ncause[i], replace = TRUE),]) 
        }
        
    })
    return(do.call(rbind, data.list))
}
### Get causes (these will be the ordering we will use)
causes <- top.cause.df$va34
### set sample sizes
ntrain <- 800
ntest <- 800
ncalib <- setting$calib.size
train.final <- resample.data(train.split, ntrain, causes, top.cause.df$ptrain)
calib.final <- resample.data(calib.split, ncalib, causes, top.cause.df$ptest_orig)
test.final <- resample.data(test.split, ntest, causes, top.cause.df$ptest)

################################
### Make data directory for this run
sim.dir <- file.path("..", "..", "data", "country_runs",
                     paste0(setting$country, "_size_", setting$calib.size, "_ncod_", ncod),
                     paste0("run_", setting$run))
dir.create(sim.dir, recursive = TRUE)

### Quit if results file already exists
if(file.exists(file.path(sim.dir, "results.rds"))){
    quit('no')
}
################################
### Now the 6 methods w/o calibration
### Tariff with just training set (will also predict on calibration data)

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
                      cause.text = rep(top.cause.df$gs_text34, length(methods)),
                      true.csmf = rep(top.cause.df$ptest, length(methods)),
                      method = rep(methods, each = nrow(top.cause.df)))

### Now CSMF Accuracy
ptrue <- top.cause.df$ptest
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
# ccc <- function(cod.est, cod.truth, causes) {
#     C <- length(causes)
#     ccc <- sapply(seq_along(causes), function(j) {
#         cause.j <- causes[j]
#         correct.assign <- sum(cod.est == cause.j & cod.truth == cause.j)
#         total <- sum(cod.truth == cause.j)
#         if(total == 0) {
#             total <- 1
#         }
#         numerator <- (correct.assign / total) - (1 / C)
#         denominator <- 1 - (1 / C)
#         return(numerator / denominator)
#     })
# }
# 
# cod.truth <- changeTopCOD(test.final$Cause)
# 
# ### Get CCC estimates for each cause for each method
# ### Only use first chain for this
# tariff.revamp.cod <- revampIndPredictions(tariff.revamp[[1]],
#                                           test.cod = tariff.train.cod.test,
#                                           causes = causes, burnin = burnin,
#                                           thin = thin)$topCOD
# tariff.revamp.ccc <- ccc(tariff.revamp.cod, cod.truth, causes)
# insilico.revamp.cod <- revampIndPredictions(insilico.revamp[[1]],
#                                             test.cod = insilico.train.cod.test,
#                                             causes = causes, burnin = burnin,
#                                             thin = thin)$topCOD
# insilico.revamp.ccc <- ccc(insilico.revamp.cod, cod.truth, causes)
# 
# tariff.train.ccc <- ccc(tariff.train.cod.test, cod.truth, causes)
# tariff.train.calib.ccc <- ccc(tariff.train.calib.cod, cod.truth, causes)
# tariff.calib.ccc <- ccc(tariff.calib.cod, cod.truth, causes)
# insilico.train.ccc <- ccc(insilico.train.cod.test, cod.truth, causes)
# insilico.train.calib.ccc <- ccc(insilico.train.calib.cod, cod.truth, causes)
# insilico.calib.ccc <- ccc(insilico.calib.cod, cod.truth, causes)
# 
# methods.ccc <- methods[!grepl("mle", methods)]
# ccc.df <- data.frame(ccc.cause = c(tariff.revamp.ccc,
#                                  insilico.revamp.ccc,
#                                  tariff.train.ccc,
#                                  tariff.train.calib.ccc,
#                                  tariff.calib.ccc,
#                                  insilico.train.ccc,
#                                  insilico.train.calib.ccc,
#                                  insilico.calib.ccc),
#                       cause = rep(causes, length(methods.ccc)),
#                       cause.text = rep(top.cause.df$gs_text34, length(methods.ccc)),
#                       true.ccc = rep(top.cause.df$ptest, length(methods.ccc)),
#                       method = rep(methods.ccc, each = nrow(top.cause.df)))
# 
# avg.ccc.df <- data.frame(ccc.mean = c(mean(tariff.revamp.ccc),
#                                       mean(insilico.revamp.ccc),
#                                       mean(tariff.train.ccc),
#                                       mean(tariff.train.calib.ccc),
#                                       mean(tariff.calib.ccc),
#                                       mean( insilico.train.ccc),
#                                       mean( insilico.train.calib.ccc),
#                                       mean(insilico.calib.ccc)),
#                          method = methods.ccc)
results <- list(topcause.df = top.cause.df,
               csmf = csmf.df,
               csmf.acc = csmf.acc.df) 
results.file <- file.path(sim.dir, "results.rds")
saveRDS(results, results.file)
quit('no')

