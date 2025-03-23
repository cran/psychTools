#Developed December 2024 

#do hare ranking (aka rank choice voting)    
hare <- function(x, n=1) {
#loop over candidates
#find the person(s) with the fewest 1 place votes

while(NCOL(x) > n) {
#for(iter in (1:(NCOL(x)-n))) {  #continue until n +1 are left
count.first <- colSums(apply(x,2,function(x) x==1), na.rm=TRUE)
min.count <- min(count.first)
which.min <- which(count.first == min.count)
which.min <- which.min[1] #just do one candidate at a time
#reallocate their votes
who.min <- which(x[,which.min]==1, arr.ind=TRUE)
#the row is the person we want 
if(NROW(who.min )> 0) {
if(!is.null(dim(who.min))) who.min <- who.min[,1]

for(i in (1:length(who.min)))  {
 x[who.min[i],] <- x[who.min[i],] -1} 
 }  #reallocate their votes
x <- x[,-which.min,drop=FALSE]   #drop the lowest vote getter
   #do it again until the NCOL = the number we want

 }
 if(NCOL(x) > 1){
 count.first <- colSums(apply(x,2,function(x) x==1), na.rm=TRUE) 
  
 result <- sort(count.first,decreasing=TRUE)[1:n]} else {result <- colSums(x==1,na.rm=TRUE)
}
 
return(result)}   #show the winners

