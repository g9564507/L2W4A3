##L2W4A3.R

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

head(outcome)

ncol(outcome)

names(outcome) 

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])



best<-function(state, outcome){

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
	    dataTemp[dataTemp=="Not Available"]<-NA
        dataSplit[ ,3]  <- dataTemp
        
        ## min(dataSplit[ ,outcomeColumn],na.rm=TRUE)
        bestHospital<- dataSplit[ grep( min(dataSplit[ ,3],na.rm=TRUE),dataSplit[ ,3]), 1]

        
	return( bestHospital) 
	}



}