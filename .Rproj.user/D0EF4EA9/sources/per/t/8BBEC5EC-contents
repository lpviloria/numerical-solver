#
#
#
# VILORIA, Luis Angelo P.
# CMSC 150 - AB5L
# 2021-68380
#
#
#


GetVariables <- function(augcoeffmatrix){ #Function initialization for GetVariables, which takes in the augmented coefficient matrix as a parameter and returns a vector object with the variables of the linear equations in it
  varStr = colnames(augcoeffmatrix) #Stores the column names (aka where the variable names are) in the vector varStr
  vec = c() #Initializes vec as an empty vector
  for(i in 1:(ncol(augcoeffmatrix) - 1)){ #Iterates through all indexes of varStr except for the last index because it only contains the RHS variables
    vec[i] = varStr[i] #Stores the variable in the vector vec in index i
  }
  return (vec) #After iterating through all of the possible indexes, return vec
}

GetSolutions <- function(augcoeffmatrix){
  solVec = c()
  for(i in 1:nrow(augcoeffmatrix)){
    solVec[i] = augcoeffmatrix[i,ncol(augcoeffmatrix)]
  }
  
  return (solVec)
  
}


GaussJordanMethod <- function(augcoeffmatrix) {
  
  m <- nrow(augcoeffmatrix)
  n <- ncol(augcoeffmatrix)
  currCol <- 1
  nonZeroRowCount <- 0
  
  while ( (currCol < n+1) & (nonZeroRowCount+1 <= m) )  {
    
    if (sum(augcoeffmatrix[(nonZeroRowCount+1):m, currCol]) == 0) {
      
      currCol <- currCol + 1
      
    } else {
      
      rowIndex <- 0
      
      for (i in nonZeroRowCount+1:m) {
        
        if (augcoeffmatrix[i,currCol] != 0) {
          
          rowIndex <- i
          break
          
        }
        
      }
      
      nonZeroRowCount <- nonZeroRowCount + 1
      
      # switch rows
      
      row1 <- augcoeffmatrix[rowIndex,]
      row2 <- augcoeffmatrix[nonZeroRowCount,]
      augcoeffmatrix[rowIndex,] <- row2
      augcoeffmatrix[nonZeroRowCount,] <- row1
      
      # Use the second row operation (mult. by scalar) 
      # to convert the entry in row 
      # nonZeroRowCount and column currCol to a 1.
      
      augcoeffmatrix[nonZeroRowCount,] <- (1/augcoeffmatrix[nonZeroRowCount,currCol]) * augcoeffmatrix[nonZeroRowCount,]
      
      # Use the third row operation with row nonZeroRowCount
      # to convert every other entry of column j to zero.
      
      for (k in 1:m) {
        
        if ( (augcoeffmatrix[k, currCol] != 0) & (k != nonZeroRowCount) ) {
          
          # use row r (nonZeroRowCount) and row op 3 to make A[k, currCol] = 0
          
          scalar <- augcoeffmatrix[k, currCol] / augcoeffmatrix[nonZeroRowCount, currCol] 
          
          augcoeffmatrix[k, ] <- -1 * scalar * augcoeffmatrix[nonZeroRowCount, ] + augcoeffmatrix[k, ]
          
        }
        
      }
      
      # increment and repeat
      
      currCol <- currCol + 1
      
    }
    
  }
  
  finalList = list(augcoeffmatrix = augcoeffmatrix, solution = GetSolutions(augcoeffmatrix))
  
  return(finalList)
  
}

getSumxi <- function(lst, integer){ #Function declaration for getSumxi which tries to get the sum of all the independent variables with respect to the degree passed through
  sum = 0 #Initializes sum to 0
  for(i in 1:length(lst)){ #For loop to iterate through all of the Xi values
    sum = sum + (lst[i])^integer #Adds the Xi value with respect to the inputted degree to sum
  }
  
  return (sum) #Returns the value of sum
}

getSumxiyi <- function(lst, integer){ #Function declaration for getSumxiyi which tries to get the sum of all of the products of the independent and dependent variables, with respect to the degree passed through
  sum1 = 0
  for(i in 1:length(lst[[1]])){ #Same concept as in getSumxi
    sum1 = sum1 + ((lst[[1]][i])^integer * lst[[2]][i])
  }
  
  return (sum1)
}


PolynomialRegression <- function(integer, lst, x){ #Function declaration for PolynomialRegression which takes in an integer and a list as formal parameters
  if(integer < 1){ #If the inputted order of the polynomial is less than one, return NA immediately
    return (NA)
  }
  
  if(length(lst[[1]]) != length(lst[[2]])){ #If the number of independent and dependent variables do not match up, return NA immediately
    return (NA)
  }
  
  augcoeffmatrix <- matrix(0:0, nrow=integer+1, ncol=integer+2) #Initializes an empty matrix with dimensions dependent on the passed through integer
  
  
  for(i in 1:nrow(augcoeffmatrix)){ #For loop to iterate through all of the rows of the matrix
    degreeGetter = i-1 #Initializes a variable called degreeGetter which simply gets the value of i - 1
    for(j in 1:ncol(augcoeffmatrix)){ #For loop to iterate through all of the columns of the matrix
      if(j == ncol(augcoeffmatrix)){ #If j is equal to the number of columns of the matrix, do the code below instead to find the RHS of the augmented coefficient matrix
        augcoeffmatrix[i,j] = getSumxiyi(lst, i-1) #Calls the function getSumxiyi with lst and i-1 as its actual parameters, and puts the returned value to the respective ith row and jth column of the matrix
        break #Break from the for loop
      }
      augcoeffmatrix[i,j] = getSumxi(lst[[1]], degreeGetter) #Otherwise, if j is not equal to the number of columns in the matrix, call getSumxi instead with the list of independent variables and degreeGetter as its actual parameters then put the return value to the approrpriate ith row and jth column in the matrix
      degreeGetter = degreeGetter + 1 #Increment degreeGetter by one
    }
  }
  
  getCoeff = GetVariables(augcoeffmatrix) #Gets the variables present in the augment coefficient matrix
  
  getVars = GaussJordanMethod(augcoeffmatrix)$solution #Gets the solutions for the system of linear equations created from the Gauss-Jordan Method
  
  string = "function (x)" #Initializes string which contains "function (x)"
  for(i in 0:(length(getVars)-1)){ #For loop to iterate through all of the solutions
    if(i == 0){ #If i is equal to zero, do the code below
      string = paste(string,getVars[i+1], "+", collapse = "") #No x variable to attach to the solution because it is the first value in the equation
    }else if(i == length(getVars)-1){ #Else if i is equal to the number of solutions minus 1, do the code below
      string = paste(string,getVars[i+1], "* x ^",i, collapse = "") #Attach x variable raised to the appropriate degree to the solution but do not include a plus because it is the last value in the equation
    }else{ #Otherwise, do the code below
      string = paste(string,getVars[i+1], "* x ^",i,"+", collapse = "") #Same concept as in the second else if statement but this time, attach a plus at the end because the value is not the last in the equation yet
    } 
  }
  
  polynomial_function = eval(parse(text = string)) #Gets the function version of string
  
  estimate = eval(polynomial_function(x))
  
  finalList = list(augcoeffmatrix = augcoeffmatrix, coefficients = getVars, estimate = estimate, polynomial_string = string, polynomial_functions = polynomial_function) #Inserts augcoeffmatrix, solutions, string, and polynomial function to a list called finalList
  
  return (finalList) #Return finalList
  
}

#a <- c(20,20,25,27,30,30,33,35,35,40)
#b <- c(8.75,9.43,12.87,14.24,16.89,18.94,25.48,30.11,36.07,51.27)

#a <- c(1,3,6,7)
#b <- c(10,20,19,33)

#a <- c(50, 50, 50, 70, 70, 70, 80, 80, 80, 90, 90, 90, 100, 100, 100)
#b <- c(3.3, 2.8, 2.9, 2.3, 2.6, 2.1, 2.5, 2.9, 2.4, 3.0, 3.1, 2.8, 3.3, 3.5, 3.0)


#a <- c(3, 4, 5, 6, 7)
#b <- c(2.5, 3.2, 3.8, 6.5, 11.5)

a <- c(100, 150, 200, 250, 300, 400, 500, 600, 650, 700, 750, 800, 850, 900, 950, 1000)
b <- c(36, 33.8, 33, 32.4, 31.8, 30.8, 29.3, 27.6, 26.7, 25.8, 24.9, 24.1, 23.4, 22.8, 21.1, 21.4)

PolynomialRegression(4, list(a,b), 150)
