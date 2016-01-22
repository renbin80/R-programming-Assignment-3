best<-function(state, outcome){
     if(!(outcome=="heart attack" | outcome=="heart failure" | outcome=="pneumonia"))     stop("invalid outcome")
     my_file<-read.csv("outcome-of-care-measures.csv")
#     my_file<-read.csv("short.csv")
     
     if(!any(state==my_file$State)) stop("invalid state")
     
     if (outcome=="heart attack") colnum<-11
     if (outcome=="heart failure") colnum<-17
     if (outcome=="pneumonia") colnum<-23

     my_subset<-subset(my_file,my_file$State==state,select=(c(State,2,colnum)))
     suppressWarnings(my_subset[,3]<-as.numeric(as.character(my_subset[,3])))

     my_sorted_subset<-as.matrix(my_subset[order(my_subset$Hospital.Name),])
     print(my_sorted_subset)
     position<-which.min(my_sorted_subset[,3])
     #print(position)
     #print(as.matrix(my_sorted_subset[position,2]))
     print(my_sorted_subset[position,2])
}