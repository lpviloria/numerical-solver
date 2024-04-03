library(shiny)
library(shinythemes)
source("polyreg.R")
source("qsi.R")
source("simplex.R")

countFoodItems <- function(fList){
  count = 0
  
  for(i in 1:length(fList)){
    if(fList[i] == TRUE){
      count = count + 1
    }
  }
  
  return (count)
}

initializeServingVars <- function(numFood){
  mat = matrix(0:0, nrow = numFood, ncol = numFood + 1)
  for(i in 1:numFood){
     mat[i, i] = -1
     mat[i, ncol(mat)] = -10
  }
  
  serveList <- list()
  
  for(j in 1:numFood){
    serveList[[j]] = mat[j, ]
  }
  
  return (serveList)
  
}

initializeObjFunc <- function(fList, price){
  objFunc <- c()
  
  ctr = 1
  for(i in 1:62){
    if(fList[i] == TRUE){
      objFunc[ctr] = price[i]
      ctr = ctr + 1
    }
  }
  
  objFunc[ctr] = 1
  
  return (objFunc)
}

initializeNutFuncs <- function(fList, calories, cholesterol, totalFat, sodium, carbs, fiber, protein, vitA, vitC, calcium, iron){
  funcList <- list()
  
  cons1 <- c()
  cons2 <- c()
  cons3 <- c()
  cons4 <- c()
  cons5 <- c()
  cons6 <- c()
  cons7 <- c()
  cons8 <- c()
  cons9 <- c()
  cons10 <- c()
  cons11 <- c()
  cons12 <- c()
  cons13 <- c()
  cons14 <- c()
  cons15 <- c()
  cons16 <- c()
  cons17 <- c()
  cons18 <- c()
  cons19 <- c()
  cons20 <- c()
  cons21 <- c()
  cons22 <- c()
  
  ctr = 1
  
  for(i in 1:62){
    if(fList[i] == TRUE){
      cons1[ctr] = calories[i]
      cons2[ctr] = calories[i] * -1
      cons3[ctr] = cholesterol[i]
      cons4[ctr] = cholesterol[i] * -1
      cons5[ctr] = totalFat[i]
      cons6[ctr] = totalFat[i] * -1
      cons7[ctr] = sodium[i]
      cons8[ctr] = sodium[i] * -1
      cons9[ctr] = carbs[i]
      cons10[ctr] = carbs[i] * -1
      cons11[ctr] = fiber[i]
      cons12[ctr] = fiber[i] * -1
      cons13[ctr] = protein[i]
      cons14[ctr] = protein[i] * -1
      cons15[ctr] = vitA[i] 
      cons16[ctr] = vitA[i] * -1
      cons17[ctr] = vitC[i] 
      cons18[ctr] = vitC[i] * -1
      cons19[ctr] = calcium[i]
      cons20[ctr] = calcium[i] * -1
      cons21[ctr] = iron[i]
      cons22[ctr] = iron[i] * -1
      ctr = ctr + 1
    }
  }
  
  cons1[ctr] = 2000
  cons2[ctr] = -2250
  cons3[ctr] = 0
  cons4[ctr] = -300
  cons5[ctr] = 0
  cons6[ctr] = -65
  cons7[ctr] = 0
  cons8[ctr] = -2400
  cons9[ctr] = 0
  cons10[ctr] = -300
  cons11[ctr] = 25
  cons12[ctr] = -100
  cons13[ctr] = 50
  cons14[ctr] = -100
  cons15[ctr] = 5000
  cons16[ctr] = -50000
  cons17[ctr] = 50
  cons18[ctr] = -20000
  cons19[ctr] = 800
  cons20[ctr] = -1600
  cons21[ctr] = 10
  cons22[ctr] = -30
  
  funcList = list(cons1, cons2, cons3, cons4, cons5,cons6, cons7, cons8, cons9, cons10, cons11, cons12, cons13, cons14, cons15, cons16, cons17, cons18, cons19, cons20, cons21, cons22)
  
  return (funcList)
  
}


ui <- fluidPage(theme = shinytheme("sandstone"),
  
  # App title ----
  titlePanel(tags$h1("Welcome to the Numerical Solver v1.0!")),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("QSI", 
                numericInput(inputId = "valEst",
                            label = "Enter value to be estimated through Quadratic Spline Interpolation:",
                            1,
                            min = NA,
                            max = NA
                            ),
        
                fileInput(inputId = "csvInput",
                          label = "Upload .csv file which contains the initial data",
                          multiple = FALSE,
                          accept = ".csv",
                          buttonLabel = "Upload"
                          ),
                
                tableOutput("contents"), 
                tableOutput("intervals"), 
                tableOutput("fxn"), 
                tableOutput("estimate")
        
                ),
        
        tabPanel("Polynomial Regression",
                 numericInput(inputId = "regDegs",
                              label = "Enter degree level:",
                              1,
                              min = NA,
                              max = NA
                              ),
                 
                 numericInput(inputId = "regVal",
                              label = "Enter value to be estimated through Polynomial Regression:",
                              1,
                              min = NA,
                              max = NA
                 ),
                 
                 fileInput(inputId = "regCSV",
                           label = "Upload .csv file which contains the initial data",
                           multiple = FALSE,
                           accept = ".csv",
                           buttonLabel = "Upload"
                           ),
                 
                 tableOutput("regContents"),
                 tableOutput("regFxn"),
                 tableOutput("regEst")
                 
                 ),
        
        tabPanel("Diet Problem Solver",
             
                 fileInput(inputId = "nutcsv",
                           label = "Upload .csv file which contains the nutritional data",
                           multiple = FALSE,
                           accept = ".csv",
                           buttonLabel = "Upload"
                 ),
                 
                 column(
                 checkboxInput(inputId = "f1",
                               label = "Frozen Broccoli"
                               ),
                 checkboxInput(inputId = "f2",
                               label = "Raw Carrots"
                 ),
                 checkboxInput(inputId = "f3",
                               label = "Raw Celery"
                 ),
                 checkboxInput(inputId = "f4",
                               label = "Frozen Corn"
                 ),
                 checkboxInput(inputId = "f5",
                               label = "Raw Iceberg Lettuce"
                 ),
                 checkboxInput(inputId = "f6",
                               label = "Raw Sweet Peppers"
                 ),
                 checkboxInput(inputId = "f7",
                               label = "Baked Potatoes"
                 ),
                 checkboxInput(inputId = "f8",
                               label = "Tofu"
                 ),
                 checkboxInput(inputId = "f9",
                               label = "Roasted Chicken"
                 ),
                 checkboxInput(inputId = "f10",
                               label = "Spaghetti with sauce"
                 ),
                 checkboxInput(inputId = "f11",
                               label = "Raw Ripe Red Tomato"
                 ),
                 checkboxInput(inputId = "f12",
                               label = "Raw Apple with Skin"
                 ),
                 checkboxInput(inputId = "f13",
                               label = "Banana"
                 ),
                 checkboxInput(inputId = "f14",
                               label = "Grapes"
                 ),
                 checkboxInput(inputId = "f15",
                               label = "Raw Fresh Kiwi"
                 ),
                 checkboxInput(inputId = "f16",
                               label = "Oranges"
                 ),
                 checkboxInput(inputId = "f17",
                               label = "Bagels"
                 ),
                 checkboxInput(inputId = "f18",
                               label = "Wheat Bread"
                 ),
                 checkboxInput(inputId = "f19",
                               label = "White Bread"
                 ),
                 checkboxInput(inputId = "f20",
                               label = "Oatmeal Cookies"
                 ), width = 3
                 
                 ),
                 column(
                 checkboxInput(inputId = "f21",
                               label = "Apple Pie"
                 ), 
                 checkboxInput(inputId = "f22",
                               label = "Chocolate Chip Cookies"
                               ),
                 checkboxInput(inputId = "f23",
                               label = "Regular Butter"
                 ),
                 checkboxInput(inputId = "f24",
                               label = "Cheddar Cheese"
                 ),
                 checkboxInput(inputId = "f25",
                               label = "3.3% Whole Fat Milk"
                 ),
                 checkboxInput(inputId = "f26",
                               label = "2% Low Fat Milk"
                 ),
                 checkboxInput(inputId = "f27",
                               label = "Skimmed Milk"
                 ),
                 checkboxInput(inputId = "f28",
                               label = "Poached Eggs"
                 ),
                 checkboxInput(inputId = "f29",
                               label = "Scrambled Eggs"
                 ),
                 checkboxInput(inputId = "f30",
                               label = "Turkey Bologna"
                 ),
                 checkboxInput(inputId = "f31",
                               label = "Beef Frankfurter"
                 ),
                 checkboxInput(inputId = "f32",
                               label = "Extra Lean Sliced Ham"
                 ),
                 checkboxInput(inputId = "f33",
                               label = "Pork Kielbasa"
                 ), 
                 checkboxInput(inputId = "f34",
                               label = "Cap'N Crunch"
                 ),
                 checkboxInput(inputId = "f35",
                               label = "Cheerios"
                 ),
                 checkboxInput(inputId = "f36",
                               label = "Kellogg's Corn Flakes"
                 ),
                 checkboxInput(inputId = "f37",
                               label = "Kellogg's Raisin Bran"
                 ),
                 checkboxInput(inputId = "f38",
                               label = "Rice Krispies"
                 ),
                 checkboxInput(inputId = "f39",
                               label = "Special K"
                 ),
                 checkboxInput(inputId = "f40",
                               label = "Oatmeal"
                 ), width = 3
                 
                ),
                column(
                 checkboxInput(inputId = "f41",
                               label = "Chocolate Malt-o-meal"
                 ),
                 checkboxInput(inputId = "f42",
                               label = "Pepperoni Pizza"
                 ),
                 checkboxInput(inputId = "f43",
                               label = "Taco"
                 ),
                 checkboxInput(inputId = "f44",
                               label = "Hamburger with toppings"
                 ),
                 checkboxInput(inputId = "f45",
                               label = "Plain Hotdog"
                 ),
                 checkboxInput(inputId = "f46",
                               label = "Couscous"
                 ),
                 checkboxInput(inputId = "f47",
                               label = "White Rice"
                 ),
                 checkboxInput(inputId = "f48",
                               label = "Cooked Macaroni"
                 ),
                 checkboxInput(inputId = "f49",
                               label = "Peanut Butter"
                 ),
                 checkboxInput(inputId = "f50",
                               label = "Pork"
                 ),
                 checkboxInput(inputId = "f51",
                               label = "Sardines in Oil"
                 ),
                 checkboxInput(inputId = "f52",
                               label = "White Tuna in Water"
                 ),
                 checkboxInput(inputId = "f53",
                               label = "Air-popped Popcorn"
                 ),
                 checkboxInput(inputId = "f54",
                               label = "Barbecue-flavored Potato Chips"
                 ),
                 checkboxInput(inputId = "f55",
                               label = "Pretzels"
                 ),
                 checkboxInput(inputId = "f56",
                               label = "Tortilla Chips"
                 ),
                 checkboxInput(inputId = "f57",
                               label = "Chicken Noodle Soup"
                 ),
                 checkboxInput(inputId = "f58",
                               label = "Split Pea & Ham Soup"
                 ),
                 checkboxInput(inputId = "f59",
                               label = "Vegetable Beef Soup"
                 ),
                 checkboxInput(inputId = "f60",
                               label = "New England Clam Chowder"
                 ),
                 checkboxInput(inputId = "f61",
                               label = "Tomato Soup"
                 ),
                 checkboxInput(inputId = "f62",
                               label = "New England Clam Chowder with Milk"
                 ), width = 3
                ),
                column(
                 checkboxInput(inputId = "f63",
                               label = "Cream of Mushroom Soup with Milk"
                 ),
                 checkboxInput(inputId = "f64",
                               label = "Bean and Bacon Soup with Water"
                 ), width = 3),
                column(
                  actionButton("start", "Start Calculations"), width = 3
                 ),
                column(
                  tableOutput("estimated"),
                  tableOutput("cost_serving"), width = 3),
                column(
                  tableOutput("basicsols"),
                  tableOutput("tableau"), width = 10
                )
        )
      )
    )
  )


server <- function(input, output) {
  
  listofFood <- eventReactive(input$start, {
    foodList = c(input$f1, input$f2, input$f3, input$f4, input$f5, input$f6, input$f7, input$f8, input$f9, input$f10, input$f11, input$f12, input$f13, input$f14, input$f15, input$f16, input$f17, input$f18, input$f19, input$f20, input$f21, input$f22, input$f23, input$f24, input$f25, input$f26, input$f27, input$f28, input$f29, input$f30, input$f31, input$f32, input$f33, input$f34, input$f35, input$f36, input$f37, input$f38, input$f39, input$f40, input$f41, input$f42, input$f43, input$f44, input$f45, input$f46, input$f47, input$f48, input$f49, input$f50, input$f51, input$f52, input$f53, input$f54, input$f55, input$f56, input$f57, input$f58, input$f59, input$f60, input$f61, input$f62, input$f63, input$f64)
  })
  
  output$cost_serving <- renderTable({
    
    foodNames = c("Frozen Broccoli", "Carrots", "Celery", "Corn", "Lettuce", "Sweet Peppers", "Baked Potatoes", "Tofu", "Roasted Chicken", "Spaghetti", "Tomato", "Apple", "Banana", "Grapes", "Kiwifruit", "Oranges", "Bagels", "Wheat Bread", "White Bread", "Oatmeal Cookies", "Apple Pie", "Chocolate Chip Cookies", "Butter", "Cheddar Cheese", "Fat Whole Milk", "Lowfat Milk", "Skimmed Milk", "Poached Eggs", "Scrambled Eggs", "Turkey Bologna", "Frankfurter", "Sliced Extralean Ham", "Kielbasa", "Cap'N Crunch", "Cheerios", "Corn Flakes", "Raisin Bran", "Rice Krispies", "Special K", "Oatmeal", "Malt-o-meal", "Pepperoni Pizza", "Taco", "Hamburger", "Hotdog", "Couscous", "White Rice", "Macaroni", "Peanut Butter", "Pork", "Sardines in Oil", "White Tuna", "Popcorn", "Potato Chips", "Pretzels", "Tortilla Chip", "Chicken Noodle Soup", "Split Pea Soup", "Beef Vegetable Soup", "Clam Chowder", "Tomato Soup", "Clam Chowder with Milk", "Cream of Mushroom Soup", "Bean and Bacon Soup")
    
    foodList = listofFood()
    
    numFood = countFoodItems(foodList)
    
    file <- input$nutcsv
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a .csv file!"))
    
    price = read.csv(file$datapath, header = TRUE)$Price.Serving
    calories = read.csv(file$datapath, header = TRUE)$Calories
    cholesterol = read.csv(file$datapath, header = TRUE)$Cholesterol.mg
    totalFat = read.csv(file$datapath, header = TRUE)$Total_Fat.g
    sodium = read.csv(file$datapath, header = TRUE)$Sodium.mg
    carbs = read.csv(file$datapath, header = TRUE)$Carbohydrates.g
    fiber = read.csv(file$datapath, header = TRUE)$Dietary_Fiber.g
    protein = read.csv(file$datapath, header = TRUE)$Protein.g
    vitA = read.csv(file$datapath, header = TRUE)$Vit_A.IU
    vitC = read.csv(file$datapath, header = TRUE)$Vit_C.IU
    calcium = read.csv(file$datapath, header = TRUE)$Calcium.mg
    iron = read.csv(file$datapath, header = TRUE)$Iron.mg
    
    
    #Setup objFunc
    objFunc = initializeObjFunc(foodList, price)
    
    #Setup constraints
    cons = initializeNutFuncs(foodList, calories, cholesterol, totalFat, sodium, carbs, fiber, protein, vitA, vitC, calcium, iron)
    
    #Setup xvars
    serveVars = initializeServingVars(numFood)
    
    l1 <- list()
    
    for(l in 1:length(cons)){
      l1[[l]] = cons[[l]]
    }
    
    for(p in 1:length(serveVars)){
      l1[[1 + length(l1)]] = serveVars[[p]]
    }
    
    l1[[length(l1) + 1]] = objFunc
    
    
    transposedMat = setMatrix(objFunc, l1)
    
    checker = initialTableau(transposedMat, objFunc, l1)
    
    req(checker)
    validate(need((checker[[1]] != -1), "This problem is infeasible. Cannot calculate cost and serving per item."))
    
    solutions = initialTableau(transposedMat, objFunc, l1)$Solution
    
    cost = c()
    
    ctr = 1
    
    for(h in (length(solutions) - numFood):(length(solutions) - 1)){
      cost[ctr] = objFunc[ctr] * solutions[h]
      ctr = ctr + 1
    }
    
    servings = c()
    
    ctr = 1
    
    for(h in (length(solutions) - numFood):(length(solutions) - 1)){
      servings[ctr] = solutions[h]
      ctr = ctr + 1
    }
    
    nameCtr = 1
    rNames = c()
    
    #Get Names
    for(q in 1:length(foodList)){
      if(foodList[q] == TRUE){
        rNames[nameCtr] = foodNames[q]
        nameCtr = nameCtr + 1
      }
    }
    
    data.frame(Serving.Per.Item = servings, Cost.Per.Item = cost, row.names = rNames)
    
  }, rownames = TRUE)
  
  output$basicsols <- renderTable({
    
    foodList = listofFood()
    
    numFood = countFoodItems(foodList)
    
    file <- input$nutcsv
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a .csv file!"))
    
    price = read.csv(file$datapath, header = TRUE)$Price.Serving
    calories = read.csv(file$datapath, header = TRUE)$Calories
    cholesterol = read.csv(file$datapath, header = TRUE)$Cholesterol.mg
    totalFat = read.csv(file$datapath, header = TRUE)$Total_Fat.g
    sodium = read.csv(file$datapath, header = TRUE)$Sodium.mg
    carbs = read.csv(file$datapath, header = TRUE)$Carbohydrates.g
    fiber = read.csv(file$datapath, header = TRUE)$Dietary_Fiber.g
    protein = read.csv(file$datapath, header = TRUE)$Protein.g
    vitA = read.csv(file$datapath, header = TRUE)$Vit_A.IU
    vitC = read.csv(file$datapath, header = TRUE)$Vit_C.IU
    calcium = read.csv(file$datapath, header = TRUE)$Calcium.mg
    iron = read.csv(file$datapath, header = TRUE)$Iron.mg
    
    
    #Setup objFunc
    objFunc = initializeObjFunc(foodList, price)
    
    #Setup constraints
    cons = initializeNutFuncs(foodList, calories, cholesterol, totalFat, sodium, carbs, fiber, protein, vitA, vitC, calcium, iron)
    
    #Setup xvars
    serveVars = initializeServingVars(numFood)
    
    l1 <- list()
    
    for(l in 1:length(cons)){
      l1[[l]] = cons[[l]]
    }
    
    for(p in 1:length(serveVars)){
      l1[[1 + length(l1)]] = serveVars[[p]]
    }
    
    l1[[length(l1) + 1]] = objFunc
    
    
    transposedMat = setMatrix(objFunc, l1)
    
    checker = initialTableau(transposedMat, objFunc, l1)
    
    req(checker)
    validate(need((checker[[1]] != -1), "This problem is infeasible. Cannot calculate basic solutions"))
    
    solutions = initialTableau(transposedMat, objFunc, l1)$Solution
    
    data.frame(Basic.Solutions = solutions)
    
  })
  
  
  output$tableau <- renderTable({
    
    foodList = listofFood()
    
    numFood = countFoodItems(foodList)
    
    file <- input$nutcsv
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a .csv file!"))
    
    price = read.csv(file$datapath, header = TRUE)$Price.Serving
    calories = read.csv(file$datapath, header = TRUE)$Calories
    cholesterol = read.csv(file$datapath, header = TRUE)$Cholesterol.mg
    totalFat = read.csv(file$datapath, header = TRUE)$Total_Fat.g
    sodium = read.csv(file$datapath, header = TRUE)$Sodium.mg
    carbs = read.csv(file$datapath, header = TRUE)$Carbohydrates.g
    fiber = read.csv(file$datapath, header = TRUE)$Dietary_Fiber.g
    protein = read.csv(file$datapath, header = TRUE)$Protein.g
    vitA = read.csv(file$datapath, header = TRUE)$Vit_A.IU
    vitC = read.csv(file$datapath, header = TRUE)$Vit_C.IU
    calcium = read.csv(file$datapath, header = TRUE)$Calcium.mg
    iron = read.csv(file$datapath, header = TRUE)$Iron.mg
    
    
    #Setup objFunc
    objFunc = initializeObjFunc(foodList, price)
    
    #Setup constraints
    cons = initializeNutFuncs(foodList, calories, cholesterol, totalFat, sodium, carbs, fiber, protein, vitA, vitC, calcium, iron)
    
    #Setup xvars
    serveVars = initializeServingVars(numFood)
    
    l1 <- list()
    
    for(l in 1:length(cons)){
      l1[[l]] = cons[[l]]
    }
    
    for(p in 1:length(serveVars)){
      l1[[1 + length(l1)]] = serveVars[[p]]
    }
    
    l1[[length(l1) + 1]] = objFunc
    
    
    transposedMat = setMatrix(objFunc, l1)
    
    checker = initialTableau(transposedMat, objFunc, l1)
    
    req(checker)
    validate(need((checker[[1]] != -1), "This problem is infeasible. Cannot calculate tableau"))
    
    tableau = initialTableau(transposedMat, objFunc, l1)$Final_Tableau
    
    data.frame(Tableau = tableau)
    
  })
  

  output$estimated <- renderTable({
    
    foodList = listofFood()
    
    numFood = countFoodItems(foodList)
      
    file <- input$nutcsv
    ext <- tools::file_ext(file$datapath)
      
    req(file)
    validate(need(ext == "csv", "Please upload a .csv file!"))
      
    price = read.csv(file$datapath, header = TRUE)$Price.Serving
    calories = read.csv(file$datapath, header = TRUE)$Calories
    cholesterol = read.csv(file$datapath, header = TRUE)$Cholesterol.mg
    totalFat = read.csv(file$datapath, header = TRUE)$Total_Fat.g
    sodium = read.csv(file$datapath, header = TRUE)$Sodium.mg
    carbs = read.csv(file$datapath, header = TRUE)$Carbohydrates.g
    fiber = read.csv(file$datapath, header = TRUE)$Dietary_Fiber.g
    protein = read.csv(file$datapath, header = TRUE)$Protein.g
    vitA = read.csv(file$datapath, header = TRUE)$Vit_A.IU
    vitC = read.csv(file$datapath, header = TRUE)$Vit_C.IU
    calcium = read.csv(file$datapath, header = TRUE)$Calcium.mg
    iron = read.csv(file$datapath, header = TRUE)$Iron.mg
    
      
    #Setup objFunc
    objFunc = initializeObjFunc(foodList, price)
      
    #Setup constraints
    cons = initializeNutFuncs(foodList, calories, cholesterol, totalFat, sodium, carbs, fiber, protein, vitA, vitC, calcium, iron)
      
    #Setup xvars
    serveVars = initializeServingVars(numFood)
      
    l1 <- list()
      
    for(l in 1:length(cons)){
      l1[[l]] = cons[[l]]
    }
      
    for(p in 1:length(serveVars)){
      l1[[1 + length(l1)]] = serveVars[[p]]
    }
      
    l1[[length(l1) + 1]] = objFunc
      
      
    transposedMat = setMatrix(objFunc, l1)
    
    checker = initialTableau(transposedMat, objFunc, l1)
    
    req(checker)
    validate(need((checker[[1]] != -1), "This problem is infeasible. Cannot calculate optimal cost"))
      
    solutions = initialTableau(transposedMat, objFunc, l1)$Solution
      
    optimalCost = solutions[length(solutions)]
      
    data.frame(Optimal.Cost = optimalCost)
    
  })
  

  output$contents <- renderTable({
    file <- input$csvInput
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a .csv file!"))
    
    read.csv(file$datapath, header = FALSE, col.names = c("x", "y"))
    
  })
  
  
  output$intervals <- renderTable({
    file <- input$csvInput
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a .csv file!"))
    
    xValues = read.csv(file$datapath, header = FALSE)$V1
    yValues = read.csv(file$datapath, header = FALSE)$V2
    list_a = c(xValues, yValues)
    mat = matrix(c(list_a), nrow = length(xValues), ncol = 2, byrow = FALSE)
    req(input$valEst)
    validate(need((input$valEst >= xValues[1] && input$valEst <= xValues[length(xValues)]), "Invalid value! Cannot calculate functions per interval!"))
    data.frame(Function.Intervals = QSI(mat, input$valEst)$intervals)
  }, rownames = TRUE)
  
  output$fxn <- renderTable({
    file <- input$csvInput
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a .csv file!"))
    
    xValues = read.csv(file$datapath, header = FALSE)$V1
    yValues = read.csv(file$datapath, header = FALSE)$V2
    list_a = c(xValues, yValues)
    mat = matrix(c(list_a), nrow = length(xValues), ncol = 2, byrow = FALSE)
    req(input$valEst)
    validate(need((input$valEst >= xValues[1] && input$valEst <= xValues[length(xValues)]), "Invalid value! Cannot determine function to evaluate!"))
    data.frame(Function = QSI(mat, input$valEst)$fxn_string)
  }, rownames = TRUE)
  
  output$estimate <- renderTable({
    file <- input$csvInput
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a .csv file!"))
    
    xValues = read.csv(file$datapath, header = FALSE)$V1
    yValues = read.csv(file$datapath, header = FALSE)$V2
    list_a = c(xValues, yValues)
    mat = matrix(c(list_a), nrow = length(xValues), ncol = 2, byrow = FALSE)
    req(input$valEst)
    validate(need((input$valEst >= xValues[1] && input$valEst <= xValues[length(xValues)]), "Invalid value! Cannot calculate estimate!"))
    data.frame(Estimate = QSI(mat, input$valEst)$estimate)
  }, rownames = TRUE)
  
  
  output$regContents <- renderTable({
    file <- input$regCSV
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a .csv file!"))
    
    read.csv(file$datapath, header = FALSE, col.names = c("x", "y"))
    
  })
  
  output$regFxn <- renderTable({
    file <- input$regCSV
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a .csv file!"))
    
    a = read.csv(file$datapath, header = FALSE)$V1
    b = read.csv(file$datapath, header = FALSE)$V2
    
    data.frame(Function = PolynomialRegression(input$regDegs, list(a,b), input$regVal)$polynomial_string)
    
  }, rownames = TRUE)
  
  output$regEst <- renderTable({
    file <- input$regCSV
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a .csv file!"))
    
    a = read.csv(file$datapath, header = FALSE)$V1
    b = read.csv(file$datapath, header = FALSE)$V2
    
    data.frame(Estimate = PolynomialRegression(input$regDegs, list(a,b), input$regVal)$estimate)
    
  }, rownames = TRUE)
  
}


shinyApp(ui = ui, server = server)