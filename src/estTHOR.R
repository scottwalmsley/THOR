est.THOR = function(THOR.result){
 
   pv = seq(0.0001,1,0.005);
  
   est.CO = function(M,pv,id){
      cutoffs = array();	
      id = seq(1,length(M$csum),1)
	for(i in 1:(length(pv))){
         min.id = min(id[M$csum >= pv[i]]);
         cutoffs[i] = M$Ref[min.id];
        
      }
      l = loess.smooth(cutoffs,pv, span=0.5,evaluation=100);
      return(list(CO=cutoffs,pv=pv,loess = l));
   }
   
   ecd = list();
   for(N in 1:length(THOR.result)){
      ecd[[N]] = est.CO(THOR.result[[N]],pv,id); 
       
   }
   return(list(ecd=ecd));
}



