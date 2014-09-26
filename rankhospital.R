
##source("rankhospital.R")
##rankhospital.R

rankhospital<-function(state, outcome, num){

	data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	outcomeArray<-c("heart attack", "heart failure", "pneumonia")
	# "heart attack", "heart failure", or "pneumonia"

	if(!state %in% data$"State"){stop("invalid state")}
		
	else if(!outcome %in% outcomeArray){stop("invalid outcome")}

	else{
            # "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" =column 11
            # "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" =column 17
            # "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"     =column 23
			outcomeColumn<-switch(outcome,"heart attack"=11,"heart failure"=17,"pneumonia"=23)
	  
			## [2] "Hospital.Name"  
			## [7] "State"
        	dataSplit<-data[data$"State"==state, c(2,7,outcomeColumn)]
        
        	## remove "Not Available"  
			dataTemp<-dataSplit[ ,3]
	    	dataTemp[ dataTemp=="Not Available" ]<-65536  ## use 65536 to represent NA
       		dataSplit[ ,3]  <- as.numeric(dataTemp)
        
        	# negelect NA Part
       		dataSplit<-dataSplit[dataSplit[,3]<65536,]  ## for those not NA
        
        	dataRanked<-dataSplit[ order(dataSplit[,3], dataSplit[,1]), ]
        	dataRanked$Rank<- order(dataRanked[,3], dataRanked[,1])
            

        	
        	## min(dataSplit[ ,outcomeColumn],na.rm=TRUE)
            
            if(num=="best"){
                            return(dataRanked[1,1])
            }
            
            else if(num=="worst"){
            	return( dataRanked[ length(dataRanked$Rank), 1] )
            }
            
            else{
            	if(num %in% dataRanked$Rank){
                    return(dataRanked[num,1])
            	}
            	else{return(NA)} 
            }

    }

        
	
	



}