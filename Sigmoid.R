

sigmoid<-function(x,constant=1){
        
        ## create predictions object
        value<-1/(1+exp(1)^(-1*constant*x))
        
        value
        
}
