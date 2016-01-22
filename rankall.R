rankall<-function(outcome, num="best"){
     print(num)
     if(!(outcome=="heart attack" | outcome=="heart failure" | outcome=="pneumonia"))     stop("invalid outcome")
     my_file<-read.csv("outcome-of-care-measures.csv")
     
     if (outcome=="heart attack") colnum<-11
     if (outcome=="heart failure") colnum<-17
     if (outcome=="pneumonia") colnum<-23
     
     my_subset<-subset(my_file, select=(c(State,2,colnum)))
     
     suppressWarnings(my_subset[,3]<-as.numeric(as.character(my_subset[,3])))
     
     my_split_subset<-split(my_subset,my_subset$State)
     ranked_df <- matrix(character(0), nrow=length(my_split_subset), ncol=2)
     
     #     print(dim(ranked_df))
     #     print(names(ranked_df))
     for(i in 1:length(my_split_subset)){
          
          temp_df<-my_split_subset[[i]]
          temp_df<-temp_df[order(temp_df[3],temp_df[2],na.last=NA),]
          
          #         print(temp_df)
          #print(nrow(temp_df))
          #          if(num=="worst") 
          pos<-num
          if(num == "best") {
               pos<-1
               print("BEST")
          }
          
          if(num =="worst"){
               
               pos<-nrow(temp_df)
               print("WORST")
          }
          
          if(pos<=nrow(temp_df)){
               print("NORMAL")
               
               ranked_df[i,1]<-as.character(temp_df[[pos,2]])
               ranked_df[i,2]<-as.character(temp_df[[pos,1]])
          } else ranked_df[i,2]<-as.character(temp_df[[1,1]])
          
          
     }
     colnames(ranked_df)<-c("hospital","state")
     as.data.frame(ranked_df)
     #          print(as.data.frame(ranked_df))
}
