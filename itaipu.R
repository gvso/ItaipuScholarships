## Remember to put data file in the work directory
## To set your work directory: http://rfunction.com/archives/1001

## The test-takers took 2 tests: one in Spanish and another in Math
## The total amount of points is 20 for each exam
## To approve the test, the test-takers should get a minimun
## 60% of 20 points (12/20 points) in both exams

## Read the dataset
data <- read.csv('data.csv')

## Get the total amount of rows in the dataset
total <- nrow(data)

## Get the approved test-takers

## params: spanish, math, both
approved <- function(test = 'both') {
    
    possibleArg = c('spanish', 'math', 'both')
    
    approved <- c()
    
    ## Check the argument passed to the function
    if(test %in% possibleArg) {
        ## Get the test-takers who approved Spanish
        if(test == 'spanish') {
            approved <- subset(data, CALIFICACION.CASTELLANO >= 12)
        }
        ## Get the test-takers who approved Math
        else if(test == 'math') {
            approved <- subset(data, CALIFICACION.MATEMATICA >= 12)
        }
        ## Get the test-takers who approved both exams
        else {
            approved <- subset(data, CALIFICACION.MATEMATICA >= 12 & CALIFICACION.CASTELLANO >= 12)
        }
        
        ## Get the number of test-takers who approved
        approved <- nrow(approved)
        
        ## Get the number of test-takers who failed in the test
        failed <- total - approved
        
        ## Create the vectors that will be passed to graphicPie
        
        ## Vector of data
        x <- c(failed, approved)
        
        ##Vector of labels
        lbls <- lbls <- c(paste(failed,'no aprobaron'), paste(approved, 'aprobaron'))
        
        graphPie(x, lbls, 'Becas de Itaipu 2016 - Exámenes computados: 3818')
    }
    else {
        stop("The argument should be: 'spanish', 'math', or 'both'")
    }
    
    
}

graphPie <- function(x, lbls, main) {
    ## Get the percentiles
    pct <- round(x/sum(x)*100, 2)
    
    ## Paste them in the labels
    lbls <- paste(lbls, pct)
    lbls <- paste(lbls, "%", sep="")
    
    ## Graph
    pie(x, labels = lbls, col=rainbow(length(lbls)), main= main)
}


## Get the people who get a perfect score

## TODO: add functionality to make it graph in a pie

## TODO: create another function for perfecto scores but 
## in relationship to the total amount of test-takers
perfectScore <- function(test = 'both') {
    possibleArg = c('spanish', 'math', 'both')
    
    perfectScorer <- c()
    
    ## Check the argument passed to the function
    if(test %in% possibleArg) {
        ## Get the test-takers who get perfect score in Spanish
        if(test == 'spanish') {
            perfectScorer <- subset(data, CALIFICACION.CASTELLANO == 20)
        } 
        ## Get the test-takers who get perfect score in Math
        else if (test == 'math') {
            perfectScorer <- subset(data, CALIFICACION.MATEMATICA == 20)
        }
        ## Get the test-takers who get perfect score in both exams
        else {
            perfectScorer <- subset(data, CALIFICACION.MATEMATICA == 20 & CALIFICACION.CASTELLANO == 20)
        }
    }
    else {
        stop("The argument should be: 'spanish', 'math', or 'both'")
    }
    
    cat('Hicieron puntaje perfecto:', nrow(perfectScorer), 'postulantes')
}



## TODO: Refactor all the below code and add documentation
perfectScore2 <- function() {
    approvedTest <- subset(data, CALIFICACION.MATEMATICA >= 12 & CALIFICACION.CASTELLANO >= 12)
    perfectScore <- subset(data, CALIFICACION.MATEMATICA == 20 & CALIFICACION.CASTELLANO == 20)
    
    print(perfectScore)
    
    perfectScore <- nrow(perfectScore)
  
    approvedTest <- nrow(approvedTest) - perfectScore
    
    x <- c(perfectScore, approvedTest)
    lbls <- c(paste(perfectScore,'lograron puntaje perfecto'), '')
    pct <- round(x/sum(x)*100, 2)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(x,labels = lbls, col=rainbow(length(lbls)),
        main="Becas de Itaipu 2016 - Puntajes perfectos")
}

perfectScore2 <- function() {
    perfectScore <- subset(data, CALIFICACION.MATEMATICA == 20 & CALIFICACION.CASTELLANO == 20)
    
    perfectScore <- nrow(perfectScore)
    
    applicants <- nrow(data) - perfectScore
    
    x <- c(perfectScore, applicants)
    lbls <- c(paste(perfectScore,'lograron puntaje perfecto'), '')
    pct <- round(x/sum(x)*100, 2)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(x,labels = lbls, col=rainbow(length(lbls)),
        main="Becas de Itaipu 2016 - Puntajes perfectos")
}

perfectScoreCast <- function() {
    approvedCast <- subset(data, CALIFICACION.CASTELLANO >= 12)
    perfectScore <- nrow(subset(data, CALIFICACION.CASTELLANO == 20))
    
    approvedCast <- nrow(approvedCast) - perfectScore
    
    x <- c(perfectScore, approvedCast)
    lbls <- c(paste(perfectScore,'lograron puntaje perfecto'), '')
    pct <- round(x/sum(x)*100, 2)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(x,labels = lbls, col=rainbow(length(lbls)),
        main="Becas de Itaipu 2016 - Puntajes perfectos en Castellano")
}

perfectScoreMath <- function() {
    approvedMath <- subset(data, CALIFICACION.MATEMATICA >= 12)
    perfectScore <- nrow(subset(data, CALIFICACION.MATEMATICA == 20))
    
    approvedMath <- nrow(approvedMath) - perfectScore
    
    x <- c(perfectScore, approvedMath)
    lbls <- c(paste(perfectScore,'lograron puntaje perfecto'), '')
    pct <- round(x/sum(x)*100, 2)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(x,labels = lbls, col=rainbow(length(lbls)),
        main="Becas de Itaipu 2016 - Puntajes perfectos en Matemática")
}

approvedExam60 <- function() {
    approved <- subset(data, CALIFICACION.MATEMATICA >= 12 & CALIFICACION.CASTELLANO >= 12)
    approvedTest <- subset(approved, CALIFICACION.MATEMATICA == 12 | CALIFICACION.CASTELLANO == 12)

    approvedTest <- nrow(approvedTest)
    approved <- nrow(approved) - approvedTest
    x <- c(approved, approvedTest)
    lbls <- c(paste(approved,'Aprobados con mas del 60%: '), paste(approvedTest, 'Aprobados con exactamente el 60%: '))
    pct <- round(x/sum(x)*100, 2)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(x,labels = lbls, col=rainbow(length(lbls)),
        main="Becas de Itaipu 2016 - Aprobaron con el 60%")
}

approvedExam50 <- function() {
    approvedTest <- subset(data, CALIFICACION.MATEMATICA >= 10 & CALIFICACION.CASTELLANO >= 10)
    
    approved <- nrow(approvedTest)
    participants <- total - approved
    v <- c(participants, approved)
    lbls <- c(paste(participants,'No aprobaron'), paste(approved, 'Aprobaron'))
    pct <- round(v/sum(v)*100, 2)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(v,labels = lbls, col=rainbow(length(lbls)),
        main="Becas de Itaipu 2016 - Exámenes computados: 3818")
}

approvedExam24 <- function() {
    approvedTest <- subset(data, CALIFICACION.MATEMATICA + CALIFICACION.CASTELLANO >= 24)
    
    approved <- nrow(approvedTest)
    participants <- total - approved
    v <- c(participants, approved)
    lbls <- c(paste(participants,'No aprobaron'), paste(approved, 'Aprobaron'))
    pct <- round(v/sum(v)*100, 2)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(v,labels = lbls, col=rainbow(length(lbls)),
        main="Becas de Itaipu 2016 - Exámenes computados: 3818")
}
