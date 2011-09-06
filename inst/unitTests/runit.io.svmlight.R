## ------------------------------------------------------------------------- ##
## Tests for setConfiguration                                                ##
##                                                                           ##
## ------------------------------------------------------------------------- ##

cat("\n\nRUnit test cases for 'RSofia:::parse_formula' function\n\n")

# This function is executed at the end of the unit test
.tearDown <- function() {
  if (exists("track", envir=.GlobalEnv)) {
    rm(track, envir=.GlobalEnv)
  }
}


TOLERANCE <- .0001

### needs to cover factors
training_data <- data.frame(
  Y  = c(-1,  1,  -1, 1   ),
  X1 = c(.1, .2,  .3, .4  ),
  X2 = c(1 , 10, 100, 1000),
  X3 = c(4 ,  3,   2, 1   )
) 

tmp <- tempfile()

### no bias term
training_data.nobias1 <- parse_formula(Y ~ -1 + ., training_data)

### bias term
training_data.nobias0 <- parse_formula(Y ~ ., training_data)

test.svmlight.io <- function() {

  ### not saving r metadata
  write.svmlight(training_data.nobias1$labels, training_data.nobias1$data, training_data.nobias1$no_bias_term, file = tmp)
  training_data.nobias1.read <- read.svmlight(tmp)
 
  ### same names
  checkEquals(names(training_data.nobias1), names(training_data.nobias1.read))
  ### same values
  checkEqualsNumeric(training_data.nobias1$data, training_data.nobias1.read$data, tolerance = TOLERANCE)
  checkEqualsNumeric(training_data.nobias1$labels, training_data.nobias1.read$labels, tolerance = TOLERANCE)
  checkTrue(is.null(training_data.nobias1.read$no_bias_term))


} 
