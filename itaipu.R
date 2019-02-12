## Data gotten from http://www.itaipu.gov.py/becas/index.php/sitio/seguimiento

## Remember to put the data file in the work directory
## To set your work directory: http://rfunction.com/archives/1001

## The test-takers took 2 tests: one in Spanish and another in Math
## The total amount of points is 20 for each exam
## To approve the test, the test-takers should get a minimun of
## 60% out of 20 points (12/20 points) in both exams

ROOT_DIR <- ("~/Data/Itaipu/ItaipuScholarships/")
MIN_SCORE <- 24
YEAR <- 2019

## Read the dataset
data <- read.csv(paste0(ROOT_DIR, 'scores_2019.csv'))

## Get the total amount of rows in the dataset
total <- nrow(data)

## Get the approved test-takers

## params: spanish, math, both
approved <- function(test = 'both') {
    
  possibleArg = c('spanish', 'math', 'both', 'both_old')
  
  approved <- c()
  
  ## Check the argument passed to the function
  if (test %in% possibleArg) {
    ## Get the test-takers who approved Spanish
    if (test == 'spanish') {
      approved <- subset(data, CALIFICACION.CASTELLANO >= 12)
    }
    ## Get the test-takers who approved Math
    else if (test == 'math') {
      approved <- subset(data, CALIFICACION.MATEMATICA >= 12)
    }
    ## Get the test-takers who approved both exams
    else if (test == 'both') {
      approved <- subset(data, TOTAL >= MIN_SCORE)
    }
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
    lbls <- lbls <- c(paste(failed, 'no aprobaron'), paste(approved, 'aprobaron'))
    
    graphPie(x, lbls, paste0('Becas de Itaipu ', YEAR, ' - Exámenes computados: ', total))
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
