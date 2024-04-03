checkNegative <-function(tableau){ # Function to check if there are still negative values in the bottom row of the tableau
  ctr = 0 # Counter variable
  
  for(i in 1:ncol(tableau)){ #For loop to iterate through all of the values in the bottom row
    if(tableau[(nrow(tableau)),i] < 0){ #If the current value being accessed is negative, increment the counter variable
      ctr = ctr + 1
    }
  }
  
  if(ctr != 0){ #If the counter variable is not zero anymore, that means there are still negative values so return true
    return (TRUE)
  }
  else{ #Otherwise, return true
    return (FALSE)
  }
}

simplex <- function(tableau){ #Simplex function initialization
  
  tableauList = list() #Initializes a list which will store the tableaus per iteration
  
  while(checkNegative(tableau) == TRUE){ #Whilst there are still negative values in the bottom row, run the code block below
    
    ctr = 1 # Counter variable initialization
    
    highestNegMag = 0 #Initialize variable to hold the highest negative magnitude for finding the pivot column
    columnHolder = 0 #Initialize variable to hold the index of the column that contains the highest negative magnitude
    
    #Find Pivot Column
    for(i in 1:(ncol(tableau) - 1)){
      if(tableau[nrow(tableau),i] < highestNegMag){
        highestNegMag = tableau[nrow(tableau), i]
        columnHolder = i
      }
    }
    
    #Find Pivot Element
    minRatio = Inf
    rowIndex = 1
    pivotElement = 0
    for(i in 1:(nrow(tableau) - 1)){
      if((tableau[i,ncol(tableau)] / tableau[i,columnHolder]) < minRatio && tableau[i,ncol(tableau)] / tableau[i,columnHolder] > 0){
        minRatio = (tableau[i,ncol(tableau)] / tableau[i,columnHolder])
        rowIndex = i
      }
    }
    
    pivotElement = tableau[rowIndex, columnHolder]
  
    
    if(minRatio == Inf){
      finalList = list(Solution = -1, Final_Tableau = -1)
      return (finalList)
    }

    
    pivotRow = tableau[rowIndex, ]
  
    
    #Normalization
    npr = tableau[rowIndex, ] / pivotElement
    tableau[rowIndex, ] = tableau[rowIndex, ] / pivotElement
    
    
    #Elimination
    for(i in 1:nrow(tableau)){
      if(i == rowIndex){
        next
      }
      subtractor = npr * tableau[i, columnHolder]
      tableau[i, ] = tableau[i, ] - subtractor
    }

    
    #Get Basic Solution
    basicSolution = c()
    for(i in 1:(ncol(tableau) - 1)){
      basicSolutionIndexGetter = 1
      if(colSums(tableau)[i] == 1){
        
        for(j in 1:nrow(tableau)){
          if(tableau[j,i] == 1){
            basicSolutionIndexGetter = j
          }
        }
        
        basicSolution[i] = tableau[basicSolutionIndexGetter, ncol(tableau)]
        
      }
      else{
        basicSolution[i] = 0
      }
    }
    
    
    #If no more negative numbers on the bottom row of the tableau, acquire the final solution through the dual problem
    if(checkNegative(tableau) == FALSE){
      basicSolution = c()
      #Get slack variables and x variables
      for(i in 1:(ncol(tableau) - 2)){
        basicSolution[i] = tableau[nrow(tableau), i]
      }
      
      
      #Get Z
      rowGetter = 1
      for(j in 1:nrow(tableau)){
        if(tableau[j,(ncol(tableau) - 1)] == 1){
          rowGetter = j
        }
      }
      
      basicSolution[length(basicSolution) + 1] = tableau[rowGetter, ncol(tableau)]
      
    }
    
    tableauList[[ctr]] = tableau
    
    finalList = list(Basic_Solution = basicSolution, matrix = tableauList)
    
    ctr = ctr + 1
    
    #print(finalList)
    
  }
  
  finalList = list(Solution = basicSolution, Final_Tableau = tableauList)
  
  return(finalList)
  
}

initialTableau <- function(mat, objFunc, l1){ #Setting up initial tableau for minimization
    numSlack = length(l1) - 1
    numX = length(objFunc)
    
    tableau = matrix(0:0, nrow = nrow(mat), ncol = (numSlack + numX + 1))
    
    
    
    #Set up portion of matrix for slack vars
    for(i in 1:numSlack){
      tableau[,i] = mat[,i]
    }
  

    #Set up portion of matrix for x vars
    indexCtr = 1
    for(j in (numSlack + 1):(numSlack + numX)){
      for(k in (numSlack + 1):(numSlack + numX)){
        if(j == k){
          tableau[indexCtr,k] = 1
        }
      }
      indexCtr = indexCtr + 1
    }
    
    #Set up solution column
    tableau[,ncol(tableau)] = mat[,(ncol(mat))]
    
    #Last element of solution column must be zero
    tableau[nrow(tableau), ncol(tableau)] = 0
    
    simplex(tableau)
    
}

setMatrix <- function(objFunc, l1){ #Function to initialize the initial matrix for minimization
  
  numrow = length(l1)
  numcol = length(l1[[1]])
  
  
  mat = matrix(0:0, nrow = numrow, ncol = numcol)
  
  for(i in 1:numrow){
      for(j in 1:numcol){
        mat[i,j] = l1[[i]][j]
      }
  }
  
  
  mat = t(mat)
  
  mat[nrow(mat),(1:(ncol(mat) - 1))] = mat[nrow(mat),(1:(ncol(mat) - 1))] * -1
  
  return (mat)
  
}
# 
# objFunc <- c(14, 20, 1)
# cons1 <- c(1, 2, 4)
# cons2 <- c(7, 6, 20)
# 
# l1 = list(cons1, cons2, objFunc)

# objFunc <- c(2, 10, 8, 1)
# cons1 <- c(1, 1, 1, 6)
# cons2 <- c(0, 1, 2, 8)
# cons3 <- c(-1, 2, 2, 4)

# l1 = list(cons1, cons2, cons3, objFunc)

# objFunc <- c(3, 2, 1)
# cons1 <- c(2, 1, 6)
# cons2 <- c(1, 1, 4)
# 
# l1 = list(cons1, cons2, objFunc)
# 
# objFunc <- c(20000, 25000, 1)
# cons1 <- c(400, 300, 25000)
# cons2 <- c(300, 400, 27000)
# cons3 <- c(200, 500, 30000)
# 
# l1 = list(cons1, cons2, cons3, objFunc)
# 
# objFunc <- c(0.16, 0.07, 0.04, 1)
# 
# cons1 <- c(73.8, 23.7, 6.4, 2000)
# cons2 <- c(-73.8, -23.7, -6.4, -2250)
# 
# cons3 <- c(0, 0, 0, 0)
# cons4 <- c(0, 0, 0, -300)
# 
# cons5 <- c(0.8, 0.1, 0.1, 0)
# cons6 <- c(-0.8, -0.1, -0.1, -65)
# 
# cons7 <- c(-1, 0, 0, -10)
# cons8 <- c(0, -1, 0, -10)
# cons9 <- c(0, 0, -1, -10)
# 
# l1 = list(cons1, cons2, cons3, cons4, cons5, cons6, cons7, cons8, cons9, objFunc)

# objFunc <- c(0.16, 0.07, 0.04, 1)
# cons1 <- c(73.8, 23.7, 6.4, 2000)
# cons2 <- c(-73.8, -23.7, -6.4, -2250)
# cons3 <- c(0, 0, 0, 0)
# cons4 <- c(0, 0, 0, -300)
# cons5 <- c(0.8, 0.1, 0.1, 0)
# cons6 <- c(-0.8, -0.1, -0.1, -65)
# cons7 <- c(1, 0, 0, 0)
# cons8 <- c(-1, 0, 0, -10)
# cons9 <- c(0, 1, 0, 0)
# cons10 <- c(0, -1, 0, -10)
# cons11 <- c(0, 0, 1, 0)
# cons12 <- c(0, 0, -1, -10)
# 
# l1 = list(cons1, cons2, cons3, cons4, cons5, cons6, cons7, cons8, cons9, cons10, cons11, cons12, objFunc)

# objFunc <- c(0.16, 0.07, 0.04, 0.18, 0.02, 0.53, 0.06, 0.31, 0.84, 0.78, 0.27, 0.24, 0.15, 0.32, 0.49, 0.15, 0.16, 0.05, 0.06, 0.09, 1)
# 
# cons1 <- c(73.8, 23.7, 6.4, 72.2, 2.6, 20, 171.5, 88.2, 277.4, 358.2, 25.8, 81.4, 104.9, 15.1, 46.4, 61.6, 78, 65, 65, 81, 2000)
# cons2 <- c(-73.8, -23.7, -6.4, -72.2, -2.6, -20, -171.5, -88.2, -277.4, -358.2, -25.8, -81.4, -104.9, -15.1, -46.4, -61.6, -78, -65, -65, -81, -2250)
# 
# cons3 <- c(0, 0, 0, 0, 0, 0, 0, 0, 129.9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# cons4 <- c(0, 0, 0, 0, 0, 0, 0, 0, -129.9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -300)
# 
# cons5 <- c(0.8, 0.1, 0.1, 0.6, 0, 0.1, 0.2, 5.5, 10.8, 12.4, 0.4, 0.5, 0.5, 0.1, 0.3, 0.2, 0.5, 1, 1, 3.3, 0)
# cons6 <- c(-0.8, -0.1, -0.1, -0.6, 0, -0.1, -0.2, -5.5, -10.8, -12.4, -0.4, -0.5, -0.5, -0.1, -0.3, -0.2, -0.5, -1, -1, -3.3, -65)
# 
# cons7 <- c(68.2, 19.2, 34.8, 2.5, 1.8, 1.5, 15.2, 8.1, 125.6, 1237.1, 11.1, 0, 1.1, 0.5, 3.8, 0, 151.4, 134.5, 132.5, 68.9, 0)
# cons8 <- c(-68.2, -19.2, -34.8, -2.5, -1.8, -1.5, -15.2, -8.1, -125.6, -1237.1, -11.1, 0, -1.1, -0.5, -3.8, 0, -151.4, -134.5, -132.5, -68.9, -2400)
# 
# cons9 <- c(13.6, 5.6, 1.5, 17.1, 0.4, 4.8, 39.9, 2.2, 0, 58.3, 5.7, 21, 26.7, 4.1, 11.3, 15.4, 15.1, 12.4, 11.8, 12.4,0)
# cons10 <- c(-13.6, -5.6, -1.5, -17.1, -0.4, -4.8, -39.9, -2.2, 0, -58.3, -5.7, -21, -26.7, -4.1, -11.3, -15.4, -15.1, -12.4, -11.8, -12.4, -300)
# 
# cons11 <- c(8.5, 1.6, 0.7, 2, 0.3, 1.3, 3.2, 1.4, 0, 11.6, 1.4, 3.7, 2.7, 0.2, 2.6, 3.1, 0.6, 1.3, 1.1, 0.6, 25)
# cons12 <- c(-8.5, -1.6, -0.7, -2, -0.3, -1.3, -3.2, -1.4, 0, -11.6, -1.4, -3.7, -2.7, -0.2, -2.6, -3.1, -0.6, -1.3, -1.1, -0.6, -100)
# 
# cons13 <- c(8, 0.6, 0.3, 2.5, 0.2, 0.7, 3.7, 9.4, 42.2, 8.2, 1, 0.3, 1.2, 0.2, 0.8, 1.2, 3, 2.2, 2.3, 1.1, 50)
# cons14 <- c(-8, -0.6, -0.3, -2.5, -0.2, -0.7, -3.7, -9.4, -42.2, -8.2, -1, -0.3, -1.2, -0.2, -0.8, -1.2, -3, -2.2, -2.3, -1.1, -100)
# 
# cons15 <- c(5867.4, 15471, 53.6, 106.6, 66, 467.7, 0, 98.6, 77.4, 3055.2, 766.3, 73.1, 92.3, 24, 133, 268.6, 0, 0, 0, 2.9, 5000)
# cons16 <- c(-5867.4, -15471, -53.6, -106.6, -66, -467.7, 0, -98.6, -77.4, -3055.2, -766.3, -73.1, -92.3, -24, -133, -268.6, 0, 0, 0, -2.9, -50000)
# 
# cons17 <- c(160.2, 5.1, 2.8, 5.2, 0.8, 66.1, 15.6, 0.1, 0, 27.9, 23.5, 7.9, 10.4, 1, 74.5, 69.7, 0, 0, 0, 0.1, 50)
# cons18 <- c(-160.2, -5.1, -2.8, -5.2, -0.8, -66.1, -15.6, -0.1, 0, -27.9, -23.5, -7.9, -10.4, -1, -74.5, -69.7, 0, 0, 0, -0.1, -20000)
# 
# cons19 <- c(159, 14.9, 16, 3.3, 3.8, 6.7, 22.7, 121.8, 21.9, 80.2, 6.2, 9.7, 6.8, 3.4, 19.8, 52.4, 21, 10.8, 26.2, 6.7, 800)
# cons20 <- c(-159, -14.9, -16, -3.3, -3.8, -6.7, -22.7, -121.8, -21.9, -80.2, -6.2, -9.7, -6.8, -3.4, -19.8, -52.4, -21, -10.8, -26.2, -6.7, -1600)
# 
# cons21 <- c(2.3, 0.3, 0.2, 0.3, 0.1, 0.3, 4.3, 6.2, 1.8, 2.3, 0.6, 0.2, 0.4, 0.1, 0.3, 0.1, 1, 0.7, 0.8, 0.5, 10)
# cons22 <- c(-2.3, -0.3, -0.2, -0.3, -0.1, -0.3, -4.3, -6.2, -1.8, -2.3, -0.6, -0.2, -0.4, -0.1, -0.3, -0.1, -1, -0.7, -0.8, -0.5, -30)
# 
# cons23 <- c(-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -10)
# cons24 <- c(0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -10)
# cons25 <- c(0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -10)
# cons26 <- c(0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -10)
# cons27 <- c(0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -10)
# cons28 <- c(0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -10)
# cons29 <- c(0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -10)
# cons30 <- c(0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -10)
# cons31 <- c(0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -10)
# cons32 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -10)
# cons33 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -10)
# cons34 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, -10)
# cons35 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, -10)
# cons36 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, -10)
# cons37 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, -10)
# cons38 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, -10)
# cons39 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, -10)
# cons40 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, -10)
# cons41 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, -10)
# cons42 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -10)
# 
# l1 = list(cons1, cons2, cons3, cons4, cons5, cons6, cons7, cons8, cons9, cons10, cons11, cons12, cons13, cons14, cons15, cons16, cons17, cons18, cons19, cons20, cons21, cons22, cons23, cons24, cons25, cons26, cons27, cons28, cons29, cons30, cons31, cons32, cons33, cons34, cons35, cons36, cons37, cons38, cons39, cons40, cons41, cons42, objFunc)

#INFEASIBLE SAMPLE
# objFunc <- c(0.16, 0.07, 0.04, 0.18, 0.02, 0.53, 1)
# 
# cons1 <- c(73.8, 23.7, 6.4, 72.2, 2.6, 20, 2000)
# cons2 <- c(-73.8, -23.7, -6.4, -72.2, -2.6, -20, -2250)
# 
# cons3 <- c(0, 0, 0, 0, 0, 0, 0)
# cons4 <- c(0, 0, 0, 0, 0, 0, -300)
# 
# cons5 <- c(0.8, 0.1, 0.1, 0.6, 0, 0.1, 0)
# cons6 <- c(-0.8, -0.1, -0.1, -0.6, 0, -0.1, -65)
# 
# cons7 <- c(68.2, 19.2, 34.8, 2.5, 1.8, 1.5, 0)
# cons8 <- c(-68.2, -19.2, -34.8, -2.5, -1.8, -1.5, -2400)
# 
# cons9 <- c(13.6, 5.6, 1.5, 17.1, 0.4, 4.8, 0)
# cons10 <- c(-13.6, -5.6, -1.5, -17.1, -0.4, -4.8, -300)
# 
# cons11 <- c(8.5, 1.6, 0.7, 2, 0.3, 1.3, 25)
# cons12 <- c(-8.5, -1.6, -0.7, -2, -0.3, -1.3, -100)
# 
# cons13 <- c(8, 0.6, 0.3, 2.5, 0.2, 0.7, 50)
# cons14 <- c(-8, -0.6, -0.3, -2.5, -0.2, -0.7, -100)
# 
# cons15 <- c(5867.4, 15471, 53.6, 106.6, 66, 467.7, 5000)
# cons16 <- c(-5867.4, -15471, -53.6, -106.6, -66, -467.7, -50000)
# 
# cons17 <- c(160.2, 5.1, 2.8, 5.2, 0.8, 66.1, 50)
# cons18 <- c(-160.2, -5.1, -2.8, -5.2, -0.8, -66.1, -20000)
# 
# cons19 <- c(159, 14.9, 16, 3.3, 3.8, 6.7, 800)
# cons20 <- c(-159, -14.9, -16, -3.3, -3.8, -6.7, -1600)
# 
# cons21 <- c(2.3, 0.3, 0.2, 0.3, 0.1, 0.3, 10)
# cons22 <- c(-2.3, -0.3, -0.2, -0.3, -0.1, -0.3, -30)
# 
# cons23 <- c(-1, 0, 0, 0, 0, 0, -10)
# cons24 <- c(0, -1, 0, 0, 0, 0, -10)
# cons25 <- c(0, 0, -1, 0, 0, 0, -10)
# cons26 <- c(0, 0, 0, -1, 0, 0, -10)
# cons27 <- c(0, 0, 0, 0, -1, 0, -10)
# cons28 <- c(0, 0, 0, 0, 0, -1, -10)
# 
# l1 = list(cons1, cons2, cons3, cons4, cons5, cons6, cons7, cons8, cons9, cons10, cons11, cons12, cons13, cons14, cons15, cons16, cons17, cons18, cons19, cons20, cons21, cons22, cons23, cons24, cons25, cons26, cons27, cons28, objFunc)

# objFunc <- c(200, 300, 1)
# cons1 <- c(2, 3, 1200)
# cons2 <- c(-1, -1, -400)
# cons3 <- c(2, 1.5, 900)
# 
# l1 = list(cons1, cons2, cons3, objFunc)

#transposedMat = setMatrix(objFunc, l1)

#initialTableau(transposedMat, objFunc, l1)