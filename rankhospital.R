rankhospital<-function(state, outcome, num="best"){
     if(!(outcome=="heart attack" | outcome=="heart failure" | outcome=="pneumonia"))     stop("invalid outcome")
     my_file<-read.csv("outcome-of-care-measures.csv")
     #     my_file<-read.csv("short.csv")
     
     if(!any(state==my_file$State)) stop("invalid state")
     
     if (outcome=="heart attack") colnum<-11
     if (outcome=="heart failure") colnum<-17
     if (outcome=="pneumonia") colnum<-23
     
     my_subset<-subset(my_file,my_file$State==state,select=(c(State,2,colnum)))
     suppressWarnings(my_subset[,3]<-as.numeric(as.character(my_subset[,3])))
     
     my_sorted_subset<-as.matrix(my_subset[order(my_subset[3],my_subset$Hospital.Name,na.last=NA),])
     
     #print(my_sorted_subset)
     max_position<-which.max(my_sorted_subset[,3])
     #print(max_position)
     #print(nrow(my_sorted_subset))
     #print(as.matrix(my_sorted_subset[position,2]))
     position <-num
     if(num=="best"){ position <-1} else
     if(num=="worst"){ position <-max_position} else
     if(num>nrow(my_sorted_subset)) return (NA)
     print(my_sorted_subset[position,2])
}