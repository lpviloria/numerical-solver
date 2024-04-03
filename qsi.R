source("polyreg.R") #Import the Gauss-Jordan function found in the polyreg.R file

QSI <- function(mat, x){ #QSI function initialization
  
  old_xVals = mat[,1] #Gets the x values from the matrix
  old_yVals = mat[,2] #Gets the y values from the matrix
  
  valuez = data.frame(old_xVals, old_yVals) #Store the x and y values in a data frame
  
  valuez = valuez[order(valuez$old_xVals, decreasing = FALSE), ] #Sort the x values in ascending order whilst maintaining their corresponding y values
  
  xVals = valuez$old_xVals #Get the sorted x values 
  yVals = valuez$old_yVals #Get the sorted y values
  
  interior_knots = c() #Initialize a list to store the interior knots of the graph
  
  for(j in 2:(nrow(mat) - 1)){#For loop to iterate through all of the x values
    interior_knots[j - 1] = xVals[j] #Stores the interior knots
  }
  
  n = length(xVals) - 1 #Initialize n 
  
  data_pts = length(xVals) #Initialize amount of data points present in the graph
  
  unknowns = 3*n #Initialize the amount of unknown equations
  
  newMat <- matrix(0:0, nrow = (3*n) - 1, ncol = (3*n) + 1) #Initialize a new matrix
  
  #Condition 1
  colSetter = 1
  rowSetter = 1
  for(i in 2:n){
    newMat[rowSetter, colSetter] = (xVals[i])^2
    newMat[rowSetter, colSetter + 1] = xVals[i]
    newMat[rowSetter, colSetter + 2] = 1
    newMat[rowSetter,(ncol(newMat))] = yVals[i]
    
    colSetter = colSetter + 3
    rowSetter = rowSetter + 1
    
    newMat[rowSetter, colSetter] = (xVals[i])^2
    newMat[rowSetter, colSetter + 1] = xVals[i]
    newMat[rowSetter, colSetter + 2] = 1
    newMat[rowSetter,(ncol(newMat))] = yVals[i]
    
    rowSetter = rowSetter + 1
    
  }
  
  #Condition 2
  rowSetter = ((2*n) - 2) + 1
  colSetter = 1
  newMat[rowSetter, colSetter] = (xVals[1])^2
  newMat[rowSetter, colSetter + 1] = xVals[1]
  newMat[rowSetter, colSetter + 2] = 1
  newMat[rowSetter,(ncol(newMat))] = yVals[1]
  
  rowSetter = rowSetter + 1
  colSetter = ncol(newMat) - 3
  
  newMat[rowSetter, colSetter] = (xVals[length(xVals)])^2
  newMat[rowSetter, colSetter + 1] = xVals[length(xVals)]
  newMat[rowSetter, colSetter + 2] = 1
  newMat[rowSetter,(ncol(newMat))] = yVals[length(yVals)]
  
  #Condition 3
  rowSetter = rowSetter + 1
  colSetter = 1
  for(k in 2:n){
    newMat[rowSetter, colSetter] = (xVals[k])*2
    newMat[rowSetter, colSetter + 1] = 1
    newMat[rowSetter, colSetter + 3] = -(xVals[k])*2
    newMat[rowSetter, colSetter + 4] = -1
    colSetter = colSetter + 3
    rowSetter = rowSetter + 1
  }
  
  newMat = subset(newMat, select = -c(1))
  
  coeffs = GaussJordanMethod(newMat)$solution #Get the solutions after performing the Gauss-Jordan method to the matrix
  
  qsi_fxns <- c() #Initialize vector to store the qsi functions per interval
  
  coeffs = c(0, coeffs) #Add zero to the list of coefficients since a1 is equal to zero
  
  coeffCounter = 4
  
  for(i in 1:n) { #For loop to get the string version of the functions per interval
    string = "function(x) "
    if(i == 1) {
      string = paste(string, coeffs[3], "*x + ", coeffs[4], sep = "", collapse = NULL)
    } else {
      for(j in 1:3) {
        if(j == 1) {
          string = paste(string, coeffs[coeffCounter], "*x^2 + ", sep = "", collapse = NULL)
          coeffCounter = coeffCounter + 1
        } else if(j == 2) {
          string = paste(string, coeffs[coeffCounter], "*x + ", sep = "", collapse = NULL)
          coeffCounter = coeffCounter + 1
        } else if(j == 3) {
          string = paste(string, coeffs[coeffCounter], sep = "", collapse = NULL)
          coeffCounter = coeffCounter + 1
        } 
      }
    }
    qsi_fxns[i] = string
  }
  
  
  
  for(m in 2:n){ #For loop to determine which function will be used with respect to the value being estimated
    if(x >= xVals[1] && x <= xVals[2]){
      finalList = list(intervals = qsi_fxns, fxn = eval(parse(text = qsi_fxns[1])), estimate = eval(eval(parse(text = qsi_fxns[1]))(x)), fxn_string = qsi_fxns[1])
    }
    else if(x >= xVals[n] && x <= xVals[n + 1]){
      finalList = list(intervals = qsi_fxns, fxn = eval(parse(text = qsi_fxns[n])), estimate = eval(eval(parse(text = qsi_fxns[n]))(x)), fxn_string = qsi_fxns[n])
    }
    else if(x >= xVals[m-1] && x <= xVals[m+1]){
      finalList = list(intervals = qsi_fxns, fxn = eval(parse(text = qsi_fxns[m-1])), estimate = eval(eval(parse(text = qsi_fxns[m-1]))(x)), fxn_string = qsi_fxns[m-1])
    }
  }
  
  return (finalList)
  
}


#mat = matrix(c(3.0,4.5,7.0,9.0,11.0,13.0,2.5,1.0,2.5,0.5,4.0,4.6), nrow = 6, ncol = 2, byrow = FALSE)
#mat = matrix(c(3.0,4.5,7.0,9.0,2.5,1.0,2.5,0.5), nrow = 4, ncol = 2, byrow = FALSE)
#mat = matrix(c(9,15,16.5,20,4,6.1,2,2.3), nrow = 4, ncol = 2, byrow = FALSE)
#mat = matrix(c(3.0,4.5,7.0,9.0,11.0,13.0,2.5,1.0,2.5,0.5,4.0,4.6), nrow = 6, ncol = 2, byrow = FALSE)
#mat = matrix(c(0, 8, 16, 24, 32, 40, 14.6, 11.84, 9.87, 8.42, 7.30, 6.41), nrow = 6, ncol = 2, byrow = FALSE)

mat = matrix(c(0, 32, 8, 16, 24, 40, 14.621, 7.305, 11.843, 9.87, 8.418, 6.413), nrow = 6, ncol = 2, byrow = FALSE)


QSI(mat, 12)