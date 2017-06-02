getwd()
directorio <- paste(getwd(), "/", "Calidad de Hospitales - data", sep="")
getwd(directorio)

#Encontrar el mejor hospital en un estado 
mejor <- function(estado, resultado){
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        e <- outcome[ ,7]
        es <- e == estado
        
    if(resultado == "ataque"){
        separar <- subset(outcome, es)
        num <-  separar[grep("[[:digit:]]", separar[,11]), ]
        colres <- num[,11]
        y <- sort(as.numeric(as.vector(colres)))
        res <- as.numeric(y[1])
        if(res == round(res,0)){
            f <-  paste(res, ".0", sep = "")
        }else{
            f <-  y[1]
        }
        o <-  subset(num, as.vector(num[,11]) == f)
        p <- o[order(o[,2]),]
        w <-  p[1,2]
        as.character(w)
        
        print(w)
    } else if(resultado == "falla"){
        separar <- subset(outcome, es)
        num <-  separar[grep("[[:digit:]]", separar[,17]), ]
        colres <- num[,17]
        y <- sort(as.numeric(as.vector(colres)))
        res <- as.numeric(y[1])
        if(res == round(res,0)){
            f <-  paste(res, ".0", sep = "")
        }else{
            f <-  y[1]
        }
        o <-  subset(num, as.vector(num[,17]) == f)
        p <- o[order(o[,2]),]
        w <-  p[1,2]
        as.character(w)
        
        print(w)
    } else if(resultado == "neumonia"){
        separar <- subset(outcome, es)
        num <-  separar[grep("[[:digit:]]", separar[,23]), ]
        colres <- num[,23]
        y <- sort(as.numeric(as.vector(colres)))
        res <- as.numeric(y[1])
        if(res == round(res,0)){
            f <-  paste(res, ".0", sep = "")
        }else{
            f <-  y[1]
        }
        o <-  subset(num, as.vector(num[,23]) == f)
        p <- o[order(o[,2]),]
        w <- p[1,2]
        as.character(w)
        
        print(w)
    }
    }
     
    
   
    
    
    
    
    
    

   
    
    
    