getwd()
directorio <- paste(getwd(), "/", "Calidad de Hospitales - data", sep="")
getwd(directorio)

#jerarquia de hospitales en un estado
rankhospital <- function(estado, resultado, num){
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
e <- outcome[ ,7]
es <- e == estado
n <- num

if(resultado == "ataque"){
    separar <- subset(outcome, es)
    numero <-  separar[grep("[[:digit:]]", separar[,11]), ]
    colres <- numero[,11]
    y <- sort(as.numeric(as.vector(colres)))
    res <- as.numeric(y[n])
    o <-  subset(num, as.vector(numero[,11]) == res)
    p <- o[order(o[,2]), ]
    w <-  p[1,2]
    or <- w[order(w)]
    as.character(or)
    
    print(or)
} else if(resultado == "falla"){
    separar <- subset(outcome, es)
    numero <-  separar[grep("[[:digit:]]", separar[,17]), ]
    colres <- numero[,17]
    y <- sort(as.numeric(as.vector(colres)))
    res <- as.numeric(y[n])
    print(res)
    o <-  subset(numero, as.vector(numero[,17]) == res)
    p <- o[order(o[,2]),]
    w <-  p[1,2]
    or <- w[order(w)]
    as.character(or)
    
    print(or)
} else if(resultado == "neumonia"){
    separar <- subset(outcome, es)
    numero <-  separar[grep("[[:digit:]]", separar[,23]), ]
    colres <- numero[,23]
    y <- sort(as.numeric(as.vector(colres)))
    res <- as.numeric(y[n])
    o <-  subset(numero, as.vector(numero[,23]) == res)
    p <- o[order(o[,2]),]
    w <- p[1,2]
    or <- w[order(w)]
    as.character(or)
    
    print(or)
}
}
