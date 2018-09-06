library(MCMCpack)
### This script will generate CSMFs for train & test set and then
### choose 5 of these based on a range of CSMF accuracies between the two CSMFs
### Causes will be top 3 COD (from the whole data set) and other


k <- 4
ndraws <- 100000
set.seed(1234)


ptrain.mat <- rdirichlet(n = ndraws, alpha = rep(1, k))
ptest.mat <- rdirichlet(n = ndraws, alpha = rep(1, k))

csmf.accuracy <- sapply(1:ndraws, function(i) {
    ptrain <- ptrain.mat[i,]
    ptest <- ptest.mat[i,]
    ### Check if all CSMFS are between .05 and 5
    if(all(c(ptrain, ptest) > .05) & all(c(ptrain, ptest) < .50)) {
        ### Treat ptest at CSMF_true and ptrain as CSMF_pred
        return(1 - sum(abs(ptest - ptrain)) / 2 * (1 - min(ptest)))
    } else {
        return(NA)
    }
})

index.to.keep <- !is.na(csmf.accuracy)
csmf.accuracy <- csmf.accuracy[index.to.keep]
ptrain.mat <- ptrain.mat[index.to.keep,]
ptest.mat <- ptest.mat[index.to.keep,]

o <- order(csmf.accuracy, decreasing = FALSE)
index.want <- o[seq(1, nrow(ptrain.mat), length.out = 5)]
ptrain.out <- ptrain.mat[index.want,]
ptest.out <- ptest.mat[index.want,]
data.dir <-file.path("..", "data") 
if(!(dir.exists(data.dir))) {
    dir.create(data.dir)
}
saveRDS(list(ptrain = ptrain.out, ptest = ptest.out), file.path(data.dir, "csmf.rds"))
#quit('no')