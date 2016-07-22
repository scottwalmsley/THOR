source("lib\\prepTHOR.R");
read.THOR = function(file,raw){
   
   #######################################################
   ## vars   
   ########

   ## A function to prepare the data matrices
   prep.M  <- function(M,raw=FALSE){
      if(raw==FALSE){
         M <-subset(M,M$Ref > 0 & m$y > 0);
      }
      M <- M[order(M[,1]),];
      M = as.data.frame(M);
     
      colnames(M) = c("Ref", "y")
      return(M);
   }
 
   ## A function to rotate data vs Ref
   rotate.M = function (M){
	
     	M = as.data.frame(M);
	M = M[order(M$Ref, M$y),]

	### ROTATION matrix
	theta = 2 * pi / 8;   ## 45degree
	cc = cos(theta)
	ss = sin(theta)
	rot.matrix = matrix(c(cc, ss, -ss, cc), 2, 2)
	
	rM = as.matrix(M) %*% rot.matrix
	rM = as.data.frame(rM)
	colnames(rM) = c("Ref", "y")
	return (rM);
   }



   dset = NULL;   # the dataset
   names = NULL;  # 
   n_vars = NULL; # n variables per sample / replicate
   M = list();    # data matrices
   rM = list();   # rotated data matrices
   #######################################################

   ## Read the user assigned Abacus file
   data <- read.delim(file,header=TRUE,row.names=NULL);
   ids = data[,1];
   
   ## Number of samples
   N_samples = length(data[1,])-2;

  
   ## Loop to grab the sample lengths 
   i=3;j=1;
   while (i < N_samples + 3 ){
      
      ld = data[,i];
      if(raw==FALSE){
         dset = cbind(dset,log(ld));
      }
      else{ dset = cbind(dset,ld);}
      
      n_vars[j] = length(which(data[,i]>0));

      names[j] = paste("R",j,sep="");
      
      i=i+1;j=j+1;

   }
   
   ##  Group data, calculate mean and order data
   meanCol = length(dset[1,])+1;
   dset = cbind(dset,(apply(dset,1,mean)));
   dset = dset[order(-dset[,meanCol]),];
  
   rownames(dset) = ids;	
   dset = subset(dset,dset[,meanCol]>0);
   dset = dset[,-meanCol];	
  
   colnames(dset) = names;
   dset= as.data.frame(dset)

   ## prepare the data matrices for THOR input
   max_N = which.max(n_vars);
   Ref = data.frame(dset[,max_N]);	
   colnames(Ref) = "Ref";
   m_data = dset[-max_N];
   
   for (i in 1:(N_samples-1)){
      m = as.data.frame(cbind(Ref=Ref,y= m_data[,i]));
      rownames(m) = rownames(dset);
      M[[i]] = prep.M(m);
      rM[[i]] = rotate.M(M[[i]]);
   }

   ## Return list of data plus Matrices = THOR data object
   RVAL= (list(data=dset,M = M,r.M=rM));
   class(RVAL) = "THOR.data";
   return(RVAL);
   
}