#-----------------------------------------------------------------------+
# Defnitive Screening Design for Screening in the Presence of 
# Second Order Effects
# Based on the Paper "A Class of Three-Level Designs for Definitive
# Screening in the Presence of Second-Order Effects" of Bradley Jones
# and Christopher J. Nachtsheim (2011)
#
# Author: Sebastian Hoffmeister (Statcon)
# mailto: sebastian.hoffmeister@statcon.de
#-----------------------------------------------------------------------+


# Definitive Screening Designs based on: Xiao, Lin, Bai - "Constructing Definitive Screening Desigsn Using Conference Matrices"
defScreening = function(factors=3, response=1, randomize=TRUE) {
  if(class(factors) == "character") p <- length(factors) else p <- factors # Factors
  if(class(response) == "character") r <- length(response) else r <- response # Responses
  
  # Only design for up to 10 factors
  if(p > 10) stop("Only designs up to 10 factors are implemented. Use 'definitiveScreening' for the calculation of larger designs.")
  
  design = as.data.frame(matrix(NA, ncol=p+r, nrow=2*p+1)) # Setup the design matrix
  if(p %% 2 != 0) design = as.data.frame(matrix(NA, ncol=p+r, nrow=2*p+3))
  if(p == 2) {
    design[, 1:2] <- matrix(c(0,1,0,-1,0,   
                              1,0,-1,0,0), ncol=2)
  } else if(p == 3) {
    design[, 1:3] <- matrix(c(0,0,1,-1,1,-1,-1,1,0,   
                              1,-1,0,0,1,-1,1,-1,0,   
                              -1,1,-1,1,0,0,-1,1,0), ncol=3)
  } else if(p == 4) {
    design[, 1:4] <- matrix(c(0,-1,-1,-1,0,1,1,1,0,
                              1,0,1,-1,-1,0,-1,1,0,
                              1,-1,0,1,-1,1,0,1,0,
                              1,1,-1,0,-1,-1,1,0,0), ncol=4)
  } else if(p == 5) {
    design[, 1:5] <-matrix(c(0, 0, -1, 1, -1, 1, -1, 1, 1, -1,-1,1, 0,  
                             -1, 1, 0, 0, -1, 1, -1, 1, -1, 1,1,-1, 0,   
                             -1, 1, -1, 1, 0, 0, 1, -1, -1, 1,-1,1, 0,   
                             -1, 1, -1, 1, 1, -1, 0, 0, 1, -1,1,-1, 0,   
                             -1, 1, 1, -1, 1, -1, -1, 1, 0, 0,-1,1, 0), ncol=5) 
  } else if(p == 6) {
    design[, 1:6] <- matrix(c(0,1,1,1,1,1,0,-1,-1,-1,-1,-1,0,
                              1,0,1,1,-1,-1,-1,0,-1,-1,1,1,0,
                              1,1,0,-1,-1,1,-1,-1,0,1,1,-1,0,
                              1,1,-1,0,1,-1,-1,-1,1,0,-1,1,0,
                              1,-1,-1,1,0,1,-1,1,1,-1,0,-1,0,
                              1,-1,1,-1,1,0,-1,1,-1,1,-1,0,0), ncol=6)
      
  } else if(p == 7) {
    design[, 1:7] <-matrix(c(0,  0, 1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1,0,   
                             1, -1, 0,  0,  1, -1,  1, -1, -1,  1, -1,  1,  1, -1, -1,  1,0,   
                             -1,  1, 1, -1,  0,  0,  1, -1, -1,  1,  1, -1,  1, -1,  1, -1,0,   
                             1, -1,-1,  1,  1, -1,  0,  0, -1,  1,  1, -1, -1,  1,  1, -1,0,   
                             1, -1, 1, -1, -1,  1,  1, -1,  0,  0, -1,  1, -1,  1,  1, -1,0,   
                             -1,  1,-1,  1, -1,  1,  1, -1, -1,  1,  0,  0, -1,  1, -1,  1,0,   
                             1, -1, 1, -1, -1,  1, -1,  1, -1,  1,  1, -1,  0,  0, -1,  1,0), ncol=7)
  } else if(p == 8) {
    design[, 1:8] <- matrix(c(0,-1,-1,-1,-1,-1,-1,-1,0,1,1,1,1,1,1,1,0,
                              1,0,1,1,1,-1,-1,-1,-1,0,-1,-1,-1,1,1,1,0,
                              1,-1,0,-1,1,1,1,-1,-1,1,0,1,-1,-1,-1,1,0,
                              1,-1,1,0,-1,-1,1,1,-1,1,-1,0,1,1,-1,-1,0,
                              1,-1,-1,1,0,1,-1,1,-1,1,1,-1,0,-1,1,-1,0,
                              1,1,-1,1,-1,0,1,-1,-1,-1,1,-1,1,0,-1,1,0,
                              1,1,-1,-1,1,-1,0,1,-1,-1,1,1,-1,1,0,-1,0,
                              1,1,1,-1,-1,1,-1,0,-1,-1,-1,1,1,-1,1,0,0), ncol=8)
  } else if(p == 9) {
    design[, 1:9] <-matrix(c(0,  0, -1,  1, -1,  1, -1,  1,  1, -1, -1,  1, -1,  1,  1,-1, -1,  1, -1, 1, 0,
                             -1,  1,  0,  0,  1, -1,  1, -1,  1, -1, -1,  1,  1, -1,  1,-1, -1,  1,  1,-1, 0,
                             1, -1, -1,  1,  0,  0,  1, -1,  1, -1, -1,  1,  1, -1, -1, 1,  1, -1, -1, 1, 0,
                             1, -1, -1,  1,  1, -1,  0,  0, -1,  1, -1,  1, -1,  1,  1,-1,  1, -1,  1,-1, 0,
                             -1,  1, -1,  1,  1, -1, -1,  1,  0,  0,  1, -1,  1, -1,  1,-1,  1, -1, -1, 1, 0,
                             -1,  1, -1,  1,  1, -1,  1, -1, -1,  1,  0, 0,  -1,  1, -1, 1, -1,  1, -1, 1, 0,
                             1, -1, -1,  1,  1, -1, -1,  1,  1, -1,  1, -1,  0,  0, -1, 1, -1,  1,  1,-1, 0,
                             -1,  1, -1,  1, -1,  1,  1, -1,  1, -1,  1, -1, -1,  1,  0, 0,  1, -1,  1,-1, 0,
                             1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1,  1,-1,  0,  0, -1, 1, 0), ncol=9)
  } else if(p == 10) {
    design[, 1:10] <- matrix(c(0,1,1,1,1,1,1,1,1,1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,
                               1,0,-1,-1,-1,-1,1,1,1,1,-1,0,1,1,1,1,-1,-1,-1,-1,0,
                               1,-1,0,-1,1,1,-1,-1,1,1,-1,1,0,1,-1,-1,1,1,-1,-1,0,
                               1,-1,-1,0,1,1,1,1,-1,-1,-1,1,1,0,-1,-1,-1,-1,1,1,0,
                               1,-1,1,1,0,-1,-1,1,-1,1,-1,1,-1,-1,0,1,1,-1,1,-1,0,
                               1,-1,1,1,-1,0,1,-1,1,-1,-1,1,-1,-1,1,0,-1,1,-1,1,0,
                               1,1,-1,1,-1,1,0,-1,-1,1,-1,-1,1,-1,1,-1,0,1,1,-1,0,
                               1,1,-1,1,1,-1,-1,0,1,-1,-1,-1,1,-1,-1,1,1,0,-1,1,0,
                               1,1,1,-1,-1,1,-1,-1,0,-1,-1,-1,-1,1,1,-1,1,-1,0,1,0,
                               1,1,1,-1,1,-1,1,-1,-1,0,-1,-1,-1,1,-1,1,-1,1,1,0,0), ncol=10)
  }
  # Set colnames
  if(class(factors) == "character" & class(response) == "character") colnames(design) = c(factors, response)
  if(class(factors) == "character" & class(response) != "character") colnames(design) = c(factors, paste("Y", 1:r, sep=""))
  if(class(factors) != "character" & class(response) == "character") colnames(design) = c(paste("X", 1:p, sep=""), response)
  if(class(factors) != "character" & class(response) != "character") colnames(design) = c(paste("X", 1:p, sep=""), paste("Y", 1:r, sep=""))
  if(randomize) design <- design[sample(1:nrow(design)),] # Randomiziation
  design
}

# Function to return a definitve screening plan.Plans have been calculated with definitiveScreening(p, 1000, 50000).
# factors - enter either a list of factor names, e.g. c("Temp", "Pressure") or the number of factors.
# response - enter either a list of response names, e.g. c("Yield", "Cost") or the number of responses.
# randomize - TRUE: randomize the result / FALSE: resulting design in standard order.

defScreening.old = function(factors=3, response=1, randomize=TRUE) {
  if(class(factors) == "character") p <- length(factors) else p <- factors # Factors
  if(class(response) == "character") r <- length(response) else r <- response # Responses
  
  # Only design for up to 10 factors
  if(p > 10) stop("Only designs up to 10 factors are implemented. Use 'definitiveScreening' for the calculation of larger designs.")
  
  design = as.data.frame(matrix(NA, ncol=p+r, nrow=2*p+1)) # Setup the design matrix
  if(p %% 2 != 0) design = as.data.frame(matrix(NA, ncol=p+r, nrow=2*p+3))
  if(p == 2) {
    design[, 1:2] <- matrix(c(0,0,-1,1,0,   
                              -1,1,0,0,0), ncol=2)
  } else if(p == 3) {
    design[, 1:3] <- matrix(c(0,0,1,-1,1,-1,-1,1,0,   
                              1,-1,0,0,1,-1,1,-1,0,   
                              -1,1,-1,1,0,0,-1,1,0), ncol=3)
  } else if(p == 4) {
    design[, 1:4] <- matrix(c(0,0,1,-1,-1,1,-1,1,0,   
                              -1,1,0,0,1,-1,-1,1,0,
                              1,-1,-1,1,0,0,-1,1,0,   
                              -1,1,-1,1,-1,1,0,0,0), ncol=4)
  } else if(p == 5) {
    design[, 1:5] <-matrix(c(0, 0, -1, 1, -1, 1, -1, 1, 1, -1,-1,1, 0,  
                             -1, 1, 0, 0, -1, 1, -1, 1, -1, 1,1,-1, 0,   
                             -1, 1, -1, 1, 0, 0, 1, -1, -1, 1,-1,1, 0,   
                             -1, 1, -1, 1, 1, -1, 0, 0, 1, -1,1,-1, 0,   
                             -1, 1, 1, -1, 1, -1, -1, 1, 0, 0,-1,1, 0), ncol=5) 
  } else if(p == 6) {
    design[, 1:6] <- matrix(c(0, 0, -1, 1, 1, -1, -1, 1, -1, 1, 1, -1, 0,   
                              -1, 1, 0, 0, 1, -1, -1, 1, 1, -1, -1, 1, 0,   
                              1, -1, 1, -1, 0, 0, -1, 1, 1, -1, 1, -1, 0,  
                              1, -1, 1, -1, 1, -1, 0, 0, -1, 1, -1, 1, 0,   
                              1, -1, -1, 1, -1, 1, -1, 1, 0, 0, -1, 1, 0,   
                              1, -1, -1, 1, 1, -1, 1, -1, 1, -1, 0, 0, 0), ncol=6)
  } else if(p == 7) {
    design[, 1:7] <-matrix(c(0,  0, 1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1,0,   
                             1, -1, 0,  0,  1, -1,  1, -1, -1,  1, -1,  1,  1, -1, -1,  1,0,   
                            -1,  1, 1, -1,  0,  0,  1, -1, -1,  1,  1, -1,  1, -1,  1, -1,0,   
                             1, -1,-1,  1,  1, -1,  0,  0, -1,  1,  1, -1, -1,  1,  1, -1,0,   
                             1, -1, 1, -1, -1,  1,  1, -1,  0,  0, -1,  1, -1,  1,  1, -1,0,   
                            -1,  1,-1,  1, -1,  1,  1, -1, -1,  1,  0,  0, -1,  1, -1,  1,0,   
                             1, -1, 1, -1, -1,  1, -1,  1, -1,  1,  1, -1,  0,  0, -1,  1,0), ncol=7)
  } else if(p == 8) {
    design[, 1:8] <-matrix(c(0, 0, -1, 1, 1, -1, 1, -1, -1, 1, -1, 1, 1, -1, 1, -1, 0,
                             1, -1, 0, 0, -1, 1, -1, 1, -1, 1, -1, 1, 1, -1, -1, 1, 0,
                             1, -1, -1, 1, 0, 0, 1, -1, -1, 1, 1, -1, -1, 1, -1, 1, 0,
                             -1, 1, 1, -1, 1, -1, 0, 0, -1, 1, 1, -1, 1, -1, -1, 1, 0,
                             1, -1, 1, -1, -1, 1, 1, -1, 0, 0, 1, -1, 1, -1, 1, -1, 0,
                             1, -1, 1, -1, 1, -1, -1, 1, -1, 1, 0, 0, -1, 1, 1, -1, 0,
                             1, -1, 1, -1, 1, -1, 1, -1, 1, -1, -1, 1, 0, 0, -1, 1, 0,
                             1, -1, -1, 1, 1, -1, -1, 1, 1, -1, 1, -1, 1, -1, 0, 0, 0), ncol=8)
  } else if(p == 9) {
    design[, 1:9] <-matrix(c(0,  0, -1,  1, -1,  1, -1,  1,  1, -1, -1,  1, -1,  1,  1,-1, -1,  1, -1, 1, 0,
                            -1,  1,  0,  0,  1, -1,  1, -1,  1, -1, -1,  1,  1, -1,  1,-1, -1,  1,  1,-1, 0,
                             1, -1, -1,  1,  0,  0,  1, -1,  1, -1, -1,  1,  1, -1, -1, 1,  1, -1, -1, 1, 0,
                             1, -1, -1,  1,  1, -1,  0,  0, -1,  1, -1,  1, -1,  1,  1,-1,  1, -1,  1,-1, 0,
                            -1,  1, -1,  1,  1, -1, -1,  1,  0,  0,  1, -1,  1, -1,  1,-1,  1, -1, -1, 1, 0,
                            -1,  1, -1,  1,  1, -1,  1, -1, -1,  1,  0, 0,  -1,  1, -1, 1, -1,  1, -1, 1, 0,
                             1, -1, -1,  1,  1, -1, -1,  1,  1, -1,  1, -1,  0,  0, -1, 1, -1,  1,  1,-1, 0,
                            -1,  1, -1,  1, -1,  1,  1, -1,  1, -1,  1, -1, -1,  1,  0, 0,  1, -1,  1,-1, 0,
                             1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1,  1,-1,  0,  0, -1, 1, 0), ncol=9)
  } else if(p == 10) {
    design[, 1:10] <-matrix(c(0, 0, 1, -1, -1, 1, 1, -1, -1, 1, -1, 1, -1, 1, 1, -1, 1, -1, 1, -1, 0,
                              -1, 1, 0, 0, 1, -1, -1, 1, 1, -1, -1, 1, -1, 1, -1, 1, 1, -1, 1, -1, 0,
                              -1, 1, -1, 1, 0, 0, 1, -1, 1, -1, -1, 1, 1, -1, 1, -1, -1, 1, 1, -1, 0,
                              1, -1, 1, -1, 1, -1, 0, 0, 1, -1, -1, 1, 1, -1, 1, -1, 1, -1, -1, 1, 0,
                              -1, 1, -1, 1, 1, -1, 1, -1, 0, 0, 1, -1, -1, 1, 1, -1, 1, -1, -1, 1, 0,
                              -1, 1, 1, -1, -1, 1, -1, 1, 1, -1, 0, 0, -1, 1, 1, -1, -1, 1, -1, 1, 0,
                              -1, 1, 1, -1, 1, -1, 1, -1, -1, 1, -1, 1, 0, 0, -1, 1, -1, 1, -1, 1, 0,
                              -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, 1, -1, 0, 0, 1, -1, -1, 1, 0,
                              -1, 1, 1, -1, 1, -1, -1, 1, -1, 1, 1, -1, 1, -1, 1, -1, 0, 0, 1, -1, 0,
                              -1, 1, 1, -1, -1, 1, 1, -1, 1, -1, 1, -1, 1, -1, -1, 1, 1, -1, 0, 0, 0), ncol=10)
  }
  # Set colnames
  if(class(factors) == "character" & class(response) == "character") colnames(design) = c(factors, response)
  if(class(factors) == "character" & class(response) != "character") colnames(design) = c(factors, paste("Y", 1:r, sep=""))
  if(class(factors) != "character" & class(response) == "character") colnames(design) = c(paste("X", 1:p, sep=""), response)
  if(class(factors) != "character" & class(response) != "character") colnames(design) = c(paste("X", 1:p, sep=""), paste("Y", 1:r, sep=""))
  if(randomize) design <- design[sample(1:nrow(design)),] # Randomiziation
  design
}


# Calculate a definitive screening design for:
#       p       - factors
#       maxIter - maximum number of iterations // probably not such important
#       nStart  - number of random initial settings // important and very relevant for the calculation time
definitiveScreening = function(p,maxIter=50, nStart=1000) {
  n = 2*p+1 # Size of the design
  
  if(n %% 2 != 0) { # The definitive screening designs are not orthogonal for uneven numbers of factors! So we add 2 additional obs.
    n = n+2
  }
  s = nStart

  result = matrix()
  bestDet = 0
  
  steps <- nStart/20
  count <- steps
  for(i in 1:s) {
    # Estimate time for calculation
    if(i == 1) start.first.run = Sys.time()
    if(i == 11) {
      end.tenth.run = Sys.time()
      et = (end.tenth.run - start.first.run)*nStart/10
      if(et > 120) cat("Go get a coffee. Estimated time is:", as.character(et), "\n");
      cat("Expected finishing time:", as.character(Sys.time() + et), "\n")
    }
    # End of time thing
    
    # This just produces a process "bar"
    # if(count==0) {
    #    cat(".")
    #    count = steps
    #  } else {
    #    count = count - 1
    # } # End of Process "bar"
  
  # Create Starting Design
  f = matrix(0, nrow=n, ncol=p)
  iters = 0
  for(row in seq(1, (n-1), by=2)) { # Every second row because of foldover-part
    for(col in 1:p) { # iterate each column
      if(col != (row - iters)) { # leave out the zeros on the diagonal 
        f[row, col] = 2*runif(1) -1
        f[row+1, col] = 2*runif(1) - 1
      }
    }
    iters = iters + 1
  }

  
  # Improve the starting matrix
  X = cbind(rep(1, n), f) # Column of 1s prepended to F
  dCurrent = det(t(X)%*%X)
  iteration = 1
  madeswitch = TRUE
  while(madeswitch & iteration < maxIter) {
    madeswitch = FALSE
    row.iter = 2
    for(row in seq(1,n-1, by=2)) { # Iterate each second row -> foldover
      for(col in 1:p) {
        Z = X
        if((col+1) != row.iter) {
          if(Z[row, col+1] == 1) { # Change value
            Z[row, col+1] = -1 
            Z[row+1, col+1] = 1 # Respect Foldover
            dTemporary = det(t(Z)%*%Z)
            if(dTemporary > dCurrent) { # Change improved the matrix
              dCurrent = dTemporary
              X[row, col+1] = -1
              X[row+1, col+1] = 1
              madeswitch = TRUE
            } # Made Change to the matrix
          } else if(Z[row, col+1] == -1) {
            Z[row, col+1] = 1 # Change Entry
            Z[row+1, col+1] = -1 # Foldover
            dTemporary = det(t(Z)%*%Z)
            if(dTemporary > dCurrent) {
              dCurrent = dTemporary
              X[row, col+1] = 1
              X[row+1, col+1] = -1
              madeswitch = TRUE
            } # Made Change
          } else { # If matrix entry is not 1 or -1
            Z[row, col+1] = 1
            Z[row+1, col+1] = -1
            dTemporary1 = det(t(Z)%*%Z)
            Z[row,col+1] = -1
            Z[row+1, col+1] = 1
            dTemporary2 = det(t(Z)%*%Z)
            if(dTemporary1 > dTemporary2) {
              dCurrent = dTemporary1
              X[row, col+1] = 1
              X[row+1, col+1] = -1
              madeswitch=TRUE
            } else {
              dCurrent = dTemporary2
              X[row, col+1] = -1
              X[row+1, col+1] = 1
              madeswitch=TRUE
            }
          }
        } # If not on diagonal
      } # Iterate cols
      row.iter = row.iter + 1
    } # Iterate rows 
    iteration = iteration + 1
  } # While: Improving startmatrix
  if(det(t(X)%*%X) > bestDet) {
    result = X
    bestDet = det(t(X)%*%X)
  } # Update result if the new matrix is better in d-optimality
  } # randomStarts
  as.data.frame(result[,-1])
}