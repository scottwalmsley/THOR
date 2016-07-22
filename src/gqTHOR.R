require("lmtest")
gq.THOR = function (THOR.data, point = 0.5, fraction = 0.375,STEP = 1,alpha=0.05,PLOT=T){
   nam = array();
   RVAL = list();
   if(PLOT==T){
         pdf(file="M_THOR.pdf",onefile=TRUE,width=8,height=3)  
   }

   for(N in 1:length(THOR.data$r.M)){
     cat(paste("N=",N,"\n"));
      pvals =  array(); 
      GQ = array();
      SpC = array();

      r.M = THOR.data$r.M[[N]];
      M = THOR.data$M[[N]];
	M

      n = dim(r.M)[1];
      nr = n - (n %% STEP); 
      nr = nr - 100;
      id = seq(1,nr,by=STEP);
 
      j=1;i=1;
      for(i in id) {

         fit = lm(Ref~y-1,data=r.M[i:n,]);
         gq = gqtest(fit,alternative="greater");
         GQ[j] = gq$statistic;
         pvals[j] = gq$p.value;
         SpC[j] = exp(M$Ref[i]);
         j=j+1;

      }

      csum = cumsum(pvals)/sum(pvals);
      cor = cor(M$Ref,M$y,method="pearson");

      CO.id = min(id[csum >= alpha]);
      rM.CO = r.M$Ref[CO.id];
      M.CO =  exp(M$Ref[CO.id]);


      if(PLOT==T){
          	
         ############ estimate regression, 
         ############ and get 95% prediction interval. 

         w = r.M$Ref

         lm.fit = lm(y ~ Ref, data=r.M, weights=w)
         lm.pred = predict(lm.fit, data=r.M, interval="predict", level=0.95, weights=w)
         lm.pred = as.data.frame(lm.pred)
     
         #dev.new(width=8,height=3.5);
         par(mar=c(4,4,1,1))
	
         plot(y ~ Ref,xlim=c(0,8),ylim=c(-3,3), data=r.M,cex=0.5,xlab="Ref",ylab=paste("R",N,sep=""))
         abline(v=rM.CO, col=2)
         lines(lm.pred$fit ~ r.M$Ref, col=3, lwd=1)
         lines(lm.pred$lwr ~ r.M$Ref, col=2, lty=2)
         lines(lm.pred$upr ~ r.M$Ref, col=2, lty=2)

         ### identify the points 
         is.outlier = rep(NA, n)

         for(i in 1:n) {
            if(r.M$y[i] >= lm.pred$lwr[i]  &  r.M$y[i] <= lm.pred$upr[i]) {
               is.outlier[i] = FALSE
            }
            else{is.outlier[i] = TRUE}
         }
         points(y ~ Ref, cex=0.5,data=r.M[is.outlier, ], col=4);
      }

      RVAL[[N]] <- list(Ref=exp(M$Ref),SpC = exp(M$y),p.value = pvals,csum = csum,GQ = GQ,count = SpC,rm.co=rM.CO,m.co=M.CO,cor=cor);
      #names(RVAL[[N]]) = paste("R",N,sep="");
   }   
  
   if(PLOT==T){
         dev.off();
   }

   #RVAL = list(Ref=M$Ref,Result=RVAL);
   class(RVAL) = "THOR.result";
   #names(RVAL) = nam;
   return(RVAL)

}





