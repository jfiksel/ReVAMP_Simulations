library(MCMCpack)
### This script will generate CSMFs for the test set
### Causes will be top K COD (from the whole data set) and other
### Do this for K up to 10
### Restrict CSMFs to be > .03
### Will draw more than enough CSMF distributions 

ndraws <- 500000
set.seed(1234)
for(k in 4:10){
    ptest.mat <- rdirichlet(n = ndraws, alpha = rep(1, k))
    index.to.keep <- sapply(1:nrow(ptest.mat), function(i) {
        ptest <- ptest.mat[i,]
        return(all(ptest > .03))
    })
    ptest.mat <- ptest.mat[index.to.keep,]
    #print(nrow(ptest.mat))
    data.dir <-file.path("..", "..", "data") 
    if(!(dir.exists(data.dir))) {
        dir.create(data.dir)
    }
    saveRDS(ptest.mat, file.path(data.dir, paste0("csmf_country_sims_", k, ".rds")))
}

#quit('no')