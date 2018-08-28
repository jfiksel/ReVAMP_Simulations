library(openVA)
library(ReVAMP)
library(dplyr)
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
i <- as.numeric(commandArgs(trailingOnly = TRUE))
setting <- setting.df[i,]
set.seed(all.seeds[setting$run])

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

calib.cause.split <- ifelse(test.init$Cause %in% top3cause.df$va34[1:3], test.init$Cause, 99)
calib.split <- split(test.init, calib.cause.split)

### Number of each type of cause in test & train
#Train
top3cause.df$train.n <- floor(top3cause.df$ptrain * 2500)
top3cause.df$train.n[4] <- 2500 - sum(top3cause.df$train.n[1:3])
# Test
top3cause.df$test.n <- floor(top3cause.df$ptest * 2500)
top3cause.df$test.n[4] <- 2500 - sum(top3cause.df$test.n[1:3])
# Calibration
### Should we have at least one sample for each cause?
top3cause.df$calib.n <- floor(top3cause.df$ptest * setting$calib.size)
top3cause.df$calib.n[4] <- setting$calib.size - sum(top3cause.df$calib.n[1:3])

### First resample for the causes for the training set
train.list <- lapply(seq_along(train.split), function(i) {
    train.cause <- train.split[[i]]
    cause <- names(train.split)[i]
    ncause <- top3cause.df[top3cause.df$va34 == cause, "train.n"]
    return(train.cause[sample(nrow(train.cause), ncause, replace = TRUE),]) 
})
train.final <- do.call(rbind, train.list)

### Now for calibration set
calib.list <- lapply(seq_along(calib.split), function(i) {
    calib.cause <- calib.split[[i]]
    cause <- names(calib.split)[i]
    ncause <- top3cause.df[top3cause.df$va34 == cause, "calib.n"]
    return(calib.cause[sample(nrow(calib.cause), ncause, replace = TRUE),]) 
})
calib.final <- do.call(rbind, calib.list)

### And test set
test.init <- test.init[!(test.init$ID %in% calib.final$ID),]
test.cause.split <- ifelse(test.init$Cause %in% top3cause.df$va34[1:3], test.init$Cause, 99)
test.split <- split(test.init, test.cause.split)
test.list <- lapply(seq_along(test.split), function(i) {
    test.cause <- test.split[[i]]
    cause <- names(test.split)[i]
    ncause <- top3cause.df[top3cause.df$va34 == cause, "test.n"]
    return(test.cause[sample(nrow(test.cause), ncause, replace = TRUE),]) 
})
test.final <- do.call(rbind, test.list)

################################
### Now all of our different runs
### Tariff with just training set (will also predict on calibration data)
set.seed(123)
tariff.train <- codeVA(data = rbind(calib.final, test.final),
                       data.type = "customize", model = "Tariff",
                       data.train = train.final, causes.train = "Cause")
### Tariff with training + calibration
set.seed(123)
tariff.train.calib <- codeVA(data = test.final,
                             data.type = "customize", model = "Tariff",
                             data.train = rbind(train.final, calib.final), causes.train = "Cause")
### Tariff with just calibration
set.seed(123)
tariff.calib <- codeVA(data = test.final,
                       data.type = "customize", model = "Tariff",
                       data.train = calib.final, causes.train = "Cause")
### Insilico with just training set
insilico.train <- codeVA(data = rbind(calib.final, test.final),
                         data.type = "customize", model = "InSilicoVA",
                         data.train = train.final, causes.train = "Cause",
                         jump.scale = 0.05, Nsim=10000, auto.length = FALSE)
### Insilico with training + calibration set
insilico.train.calib <- codeVA(data = test.final,
                         data.type = "customize", model = "InSilicoVA",
                         data.train = rbind(train.final, calib.final), causes.train = "Cause",
                         jump.scale = 0.05, Nsim=10000, auto.length = FALSE)
### Insilico with just calibration set
insilico.calib <- codeVA(data = test.final,
                         data.type = "customize", model = "InSilicoVA",
                         data.train = calib.final, causes.train = "Cause",
                         jump.scale = 0.05, Nsim=10000, auto.length = FALSE)
### Tariff with MLE method for calibration

