setwd("C:\\Users\\Scott\\Dropbox\\COLLAB\\THOR\\CODE\\lib")
dir()

file = "ABACUS_MANN.tsv"

dset = read.THOR(file)

r = gq.THOR(dset,STEP=2,PLOT=T)

e = est.THOR(r)

plot(e$ecd[[1]]$loess,type="s",xlim=c(0,12),ylim=c(0,1))
for(i in 1:length(e$ecd)){
   lines(e$ecd[[i]]$loess,col=i,type="s");
}
