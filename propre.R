library(shiny)
library(shinydashboard)
library(quantmod)
library(fPortfolio)
library(DT)
library(PerformanceAnalytics)
library(dplyr)
library(PortfolioAnalytics)
library(imputeTS) 
library(ROI.plugin.glpk)
library(ROI)
library(ROI.plugin.quadprog)
library(shinythemes)
library(ggplot2)

##############################################################################################################################
##############################################################################################################################

ui <- dashboardPage( 
  
  #Header
  dashboardHeader(title = "Quantmod OONW"), skin = "purple",
  
  #sideBar  
  dashboardSidebar(sidebarMenu(
    menuItem("Introduction", tabName = "Ratio", icon = icon("th")),
    menuItem("Visualisation", tabName = "summary", icon = icon("th")),
    menuItem("Monte-Carlo", tabName = "Visuel", icon = icon("th")),
    menuItem("Analyse CAPM", tabName = "Evaluation", icon = icon("th")),
    menuItem("Efficient Frontier",tabName = "efficientFrontier", icon = icon("th")),
    menuItem("BackTesting (WFO)",tabName = "backtesting", icon = icon("th")),
    menuItem("Generer un rapport", tabName = "Report", icon = icon("th"))
  )),
  
  #Body
  dashboardBody(
    tabItems(
      
      ##############################################################################################################################      
      #PAGE PROJECTION
      ################
      
      tabItem(tabName = "Visuel",
              
              fluidPage(
                
                box(
                  title = "Controls", 
                  textInput("action","Symbole de l'action / ex: Netflix = NFLX", "NFLX"),
                  tags$footer("ATTENTION: freq. must > 15 for p=0.05", align = "left"),
                  tags$footer("", align = "left"),
                  sliderInput("samplesize", "Fenetre d'apparition de l'evenement (t)", min = 15, max = 52, step = 1, value = 15),
                  sliderInput("numberPath", "Nombre de chemins possible:", min = 100, max = 5000, step = 10, value = 1500 ),
                  h6("Represente la taille de l'echantillon generer selon la distribution")
                ),
                
                box(column(12, 
                           numericInput("stoploss", "Indiquez votre StopLoss ($)", min = 0, max = 5000, step = 1, value = 0),
                           numericInput("takeprofit", "Indiquez votre TakeProfit ($)", min = 0, max = 5000, step = 1, value = 1000))),
                
                box("Probabilite de P(x) > TP",
                    verbatimTextOutput ("probaTP", placeholder = TRUE )
                ), 
                box( "Probabilite de P(x) < SL",
                     verbatimTextOutput ("probaSL", placeholder = TRUE )
                ), 
                
                box("Code couleur et visuel:", 
                    h6("Rouge = StopLoss", col = "red"),
                    h6("Bleu = Last Price", col = "blue"),
                    h6("Noir = balises 25%, 50%, 97,5%"),
                    h6("Vert = TakeProfit", col = "green"),
                    h6(" "),
                    h6("Chaque ligne fine represente une trajectoire possible de l'action dans le futur, en fonction de sa distribution dans le passe sur l'echantillon (t)")),
                
                box("Methode:", 
                    h6("1. Selectionnez le symbole de l'action"),    
                    h6("2. Votre SL = prix plancher de l'action a vos yeux"),
                    h6("3. Votre TP = prix vise pour cette action"),
                    h6("P(x) > TP est la probabilite que l'action depasse le prix TP sur la periode (t)"),
                    h6("P(x) < TP est la probabilite que l'action chute sous le prix SL sur la periode (t)")
                )
              ),
              
              fluidRow(
                
                column(12,plotOutput("Visuel", height = 600, width = 1200))),
              
              tags$footer("Designed By SweetpastaBox", align = "left")
      ),
      
      ##############################################################################################################################            
      #PAGE ROI TIMESERIES
      ####################
      
      tabItem(tabName = "summary",
              fluidRow(
                
                
                column(12,
                       textInput("dt", "Date de debut: yyyy-mm-dd", "2019-10-10"),
                       
                       
                       plotOutput("candlesticks", width = '100%', height = '600px')),
                
                
                
                box(title = "Returns (%,t)",
                    h5("Graphique des retours en fonction du temps"),
                    plotOutput("hist")
                ),
                
                box(title = "Coverage (H:L:O:C)",
                    h5("Visualisation du comportement de l'action a travers 4 balises"),
                    plotOutput("openClose")
                ),
                
                box(column(12,
                           title = "TableControl", 
                           textInput("actionT","Symbole de l'action", "AAPL"),
                           h6("Par exemple: Apple = 'AAPL', liste sur finance.yahoo.com"),
                           
                           
                           selectInput("echeance", "Choix de la periode: ", c("Retour Quotiden" = "dailyReturn", "Retour Hebdomadaire" = "weeklyReturn", "Retour Mensuel" = "monthlyReturn"), selected = "dailyReturn", multiple = FALSE,
                                       selectize = TRUE, width = NULL, size = NULL),
                           h6("Selectionnez le type de retour (j/m/a)"),
                           
                           
                           sliderInput("taille", "Taille de l'historique:", min = 1, max = 1000, step = 5, value = 300),
                           h6("Selectionnez le nombre de valeurs a afficher"),
                           tags$footer("Designed By SweetpastaBox", align = "left")
                )),
                
                box("Code couleur:", 
                    h5("Green = Highest Price, max(P) sur la periode (t)"),
                    h5("Red = Lowest Price, min(P) sur la periode (t)"),
                    h5("Blue = Opening Price, Opening(P) sur la periode (t)"),
                    h5("Black = Closing Price, Closing(P) sur la periode (t)")),
                
                
                box(column(12,
                           title = "histoRendControl",
                           sliderInput("multiCslider", "Taille de l'Historique:", min = 2, max = 1000, step = 1, value = 20),
                           h6("Selectionnez le nombre de valeurs a afficher"),
                           tags$footer("Designed By SweetpastaBox", align = "left"))
                ),
                
                column(width = 12,
                       DT::dataTableOutput("summary2"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                )
              )
      ),
      
      ##############################################################################################################################
      #PAGE POSITIV RETURN RATIO
      ##########################
      
      tabItem(tabName = "Ratio",  
              fluidRow( 
               
                column(12,
                       
                       box(title = "Ratios",
                           sliderInput("tailleS", "Taille de l'echantillon:", value = 1500, min = 1, max = 10000, step = 10),
                           textInput("ActionR", "Action choisie", value = "AAPL")),
                       
                       box(selectInput("echeance2", "Choix de la periode: ", c("Retour Quotiden" = "dailyReturn2", "Retour Hebdomadaire" = "weeklyReturn2", "Retour Mensuel" = "monthlyReturn2"), selected = "dailyReturn2", multiple = FALSE,
                                       selectize = TRUE, width = NULL, size = NULL))
                ),
                
                column(12, 
                       box(
                         h3("Bienvenue sur Quantmod OONW 1.0.0"),
                         h5("Quandmod OONW est un outils d'analyse et visualisation statistique financiere."),
                         h5("Cet outils est designe de maniere exploiter des informations et requetes formulees par l'utilisateur."),
                         h5("Disclaimer: Quantmod OONW est un modele analytique amateur, privilegiez les simulations et exercices."),
                         h5(" "),
                         h3("Tutorial des fonctionalites:"),
                         h5("1. Entrez le code d'une action dans 'Action choisie'"),
                         h5("2. Choisissez une periode"),
                         h5("3. Definissez une taille d'echantillon"),
                         h5(" "),
                         h5("Vous pouvez desormais observer la distribution des retours de votre action sur la periode demandee"),
                         h5("En l'occurence, celle ci est majoritairement > 0, la periode definie est haussiere pour cet Asset.")
                         
                       ),
                       
                       box( title = "distribution", 
                            plotOutput("distributionHist"),
                            tags$footer("Designed By SweetpastaBox", align = "left"))
                ))
      ),
      
      ##############################################################################################################################      
      
      tabItem(tabName = "Evaluation", 
              fluidRow(
                
                box(column(12, 
                           title = "Composition", 
                           textInput("asset1", "Symbole Asset1", "NFLX"),
                           textInput("asset2", "Symbole Asset2", "AMZN"),
                           textInput("asset3", "Symbole Asset3", "GOOGL"),
                           textInput("asset4", "Symbole Asset4", "AAPL"),
                           textInput("asset5", "Symbole Asset5", "TM")),
                    tags$footer("Designed by SweetPastaBox", align = "left")),
                
                box(
                  numericInput("weight1", "(%)CAP Asset1", value = 0.2, min = 0, max = 1, step = 0.000001, width = NULL),
                  numericInput("weight2", "(%)CAP Asset2", value = 0.2, min = 0, max = 1, step = 0.000001, width = NULL),
                  numericInput("weight3", "(%)CAP Asset3", value = 0.2, min = 0, max = 1, step = 0.000001, width = NULL),
                  numericInput("weight4", "(%)CAP Asset4", value = 0.2, min = 0, max = 1, step = 0.000001, width = NULL),
                  numericInput("weight5", "(%)CAP Asset5", value = 0.2, min = 0, max = 1, step = 0.000001, width = NULL),
                  tags$footer("ATTENTION: somme(CAP(1,2,..,n) must = 1", align = "left")),
                
                
                box(textInput("datePortefeuille", "Serie Temporelle: ", "2017-01-01")),
                
                box("Code couleur:", 
                    h5("Black: Asset 1 returns/price"),
                    h5("Red: Asset 2 returns/price"),
                    h5("Green: Asset 3 returns/price"),
                    h5("Dark Blue: Asset 4 returns/price"),
                    h5("Light Blue: Asset 5 returns/price")),
                
                column(12, height = 500, 
                       plotOutput("portfolioPrices")),
                
                column(12, height = 500,
                       plotOutput("portfolioReturns")),
                
                box("CAPM Analysis",
                    h5("CAPM Beta - Beta"),
                    verbatimTextOutput("beta", placeholder = TRUE),
                    h5("CAPM Beta - Bull"),
                    verbatimTextOutput("betaBull", placeholder = TRUE),
                    h5("CAPM Beta - Bear"),
                    verbatimTextOutput("betaBear", placeholder = TRUE)
                ),
                box(h5("Alpha Jensen"),
                    verbatimTextOutput("Alpha", placeholder = TRUE)), 
                
                box(h5("Sharp Ratio"),
                    verbatimTextOutput("sharp", placeholder = TRUE))
              )
      ),
      ##############################################################################################################################      
      tabItem(tabName = "efficientFrontier", 
              fluidRow(
                
                box(column(12, 
                           title = "Composition", 
                           textInput("EFasset1", "Symbole Asset1", "NFLX"),
                           textInput("EFasset2", "Symbole Asset2", "AMZN"),
                           textInput("EFasset3", "Symbole Asset3", "GOOGL"),
                           textInput("EFasset4", "Symbole Asset4", "AAPL"),
                           textInput("EFasset5", "Symbole Asset5", "TM")),
                    tags$footer("Designed by SweetPastaBox", align = "left")),
                
                box(column(12, 
                           title = "efficientFrontier", 
                           textInput("minBox", "Variation minimum des poids (%)", value = 0.10),
                           textInput("maxBox", "Variation maximum des poids (%)", value = 0.40)
                )),
                box(h4("Composition du portefeuille optimise"),
                    plotOutput("opt")
                ),
                
                column(12, 
                       plotOutput("efrg"))
              )),
      ##############################################################################################################################      
      
      tabItem(tabName = "backtesting", 
              fluidRow(
                
                box(column(12, 
                           title = "Composition", 
                           textInput("Basset1", "Symbole Asset1", "NFLX"),
                           textInput("Basset2", "Symbole Asset2", "AMZN"),
                           textInput("Basset3", "Symbole Asset3", "GOOGL"),
                           textInput("Basset4", "Symbole Asset4", "AAPL"),
                           textInput("Basset5", "Symbole Asset5", "TM")),
                    tags$footer("Designed by SweetPastaBox", align = "left")),
                
                box(column(12, 
                           title = "boxRebalancing", 
                           h5("Flottement Individuel"),
                           textInput("BminBox", "Variation minimum des poids (%)", value = 0.10),
                           textInput("BmaxBox", "Variation maximum des poids (%)", value = 0.40), 
                           h5("Flottement du portefeuille"),
                           textInput("BGminBox", "Reduction minimum du portefeuille (% du Pf1)", value = 0.99),
                           textInput("BGmaxBox", "Expension maximum du portefeuille (% du Pf1)", value = 1.01),
                           h5("Deviation Standat"),
                           textInput("stdevtarget", "Deviation Standart cible", value = 0.005)
                )),
                
                box(column(12,
                           textInput("TCS", "Couts de transactions TCS", value = 0.001, placeholder = TRUE))
                ),
                
                box(column(12, title = "WalkForward Optimization Setup",
                           textInput("rolling", "Fenetre de roulement (mois)", value = 10, placeholder = TRUE),
                           textInput("training", "Fenetre de test de l'optimisation (mois)", value = 1, placeholder = TRUE)
                )),
                
                column(12, height = 700, 
                       h3("Cette fonctionnalite n'est pas disponible. Elle le sera tres prochainement"),
                       h4("soon here: affichage de consolidation f(t) selon la methode WalkForward avec roulement ecoute/training"),
                       plotOutput("RebalancingWeights")),
                
                column(12, height = 700,
                       h3("Cette fonctionnalite n'est pas disponible. Elle le sera tres prochainement"),
                       h4("soon here: Performance chart (t) du portefeuille optimise selon WalkForward"),
                       plotOutput("performanceRB"))
                
              )),
      ##############################################################################################################################      
      
      tabItem(tabName = "Report", 
              h2("PDF Report tab content"),
              h3("Cette fonctionnalite n'est pas encore disponible. Elle le sera tres prochainement."),
              h4("soon here: synthese des resultats sur une session analytique, export pdf. et csv. envoi automatique"),
              tags$footer("Designed By SweetpastaBox", align = "left"))
      
      ##############################################################################################################################
    )))

##############################################################################################################################
##############################################################################################################################

server <- function(input, output){
  
  
  ##############################################################################################################################
  #DISTRIBUTION HISTOGRAMME // POSITIV RETURN RATION
  ##################################################
  
  output$distributionHist <- renderPlot({
    
    ACTIONR <- getSymbols(input$ActionR, auto.assign = FALSE)
    tailleAR <- input$tailleS
    AX <- tail(ACTIONR, tailleAR)
    AXX <- ACTIONR[,4]
    
    ifelse(input$echeance2 == "dailyReturn2", 
           ActionR.produit <- dailyReturn(AXX),
           
           ifelse(input$echeance2 == "weeklyReturn2", 
                  ActionR.produit <- weeklyReturn(AXX),
                  ActionR.produit <- monthlyReturn(AXX)))
    
    chartSeries(ActionR.produit, col = "lightgreen", border = "white",
                xlab = "Return",
                ylab = "Frequence",
                main = "Histogram of distribution freq.")
    
  })
  
  output$candlesticks <- renderPlot({
    
    dt <- input$dt
    
    actionh <- getSymbols.yahoo(input$ActionR, from=dt, auto.assign = F)
    actionhClose <- getSymbols.yahoo(input$ActionR, from=dt, auto.assign = F)[,6]
    
    actionhRets <- na.omit(dailyReturn(actionhClose, type="log"))
    
    chartSeries(actionh)
  })
  
  output$summary2 <- renderDataTable({
    
    ACTIONA <- as.data.frame(getSymbols(input$actionT, auto.assign = FALSE))
    taille2 <- as.numeric(input$taille)
    TABLEB <- tail(ACTIONA, taille2)
    datatable(TABLEB[0:taille2,0:6], options = list(paging = FALSE))
  })
  
##############################################################################################################################
  #PLOT DE VISUEL // PROJECTION
  #############################
  
  output$Visuel <- renderPlot({
    
    getSymbols(input$action)
    ACTIONA <- getSymbols(input$action, auto.assign = FALSE)
    A.Close <- ACTIONA [,4]
    ActionA.produitM <- monthlyReturn(A.Close)
    Value1 <- last(A.Close)
    ValueX <- as.numeric(Value1)
    valY <- ACTIONA[,2]
    maxvalY <- 2*as.numeric(max(valY))
    
    NPath = input$numberPath
    
    plot( NULL,
          xlim = c(2020,2021),
          ylim = c(-100, maxvalY),
          xlab = "Temps",
          ylab = "Trajectoires Possibles"
          
    )
    
    abline(h = Value1, col = "black",lty = 1, lwd = 5)
    abline(h = input$stoploss, col = "red",lty = 1, lwd = 2)
    abline(h = input$takeprofit, col = "green", lty = 1, lwd =2)
    abline(h = 0, col = "darkred", lty = 1, lwd = 2)
    
    
    out1 <- rep(0, NPath)
    Nbcol <- as.numeric(input$samplesize)
    out2 <- matrix(0, nrow = NPath, ncol = Nbcol)
    
    for ( i in 1:NPath) {
      produit1.samp <- sample(as.vector(ActionA.produitM), Nbcol, replace = FALSE)
      produit1.samp[1] <- 0
      
      produit2.samp <- 1 + produit1.samp
      produit3.samp <- cumprod(produit2.samp)
      value2 <- produit3.samp*as.numeric(Value1)
      out2[ i, ] <- value2
      takeprofit <- as.numeric(input$takeprofit)
      
      Flag1 <- ifelse( value2 > takeprofit, 0, 1 )
      Flag1[1] <- 0
      
      out1 [i] <- max(Flag1)
      value3 <- ts( value2, start = c(2020, 0), frequency =  12)
      lines(value3, col = i, lwd = 0.75)
      abline(h = Value1, col = "blue",lty =1, lwd = 2)
    }
    probSupOjd <- mean(out1)
    
    bounds1 <- apply(out2, 2, quantile, c(0.25, 0.5, 0.957))
    lb1 <- ts( bounds1[1,], start = c(2020, 0), frequency = 12)
    lines(lb1, col = "black", lty = 2, lwd = 3 )
    ub1 <- ts( bounds1[3,], start = c(2020, 0), frequency = 12)
    lines(ub1, col = "black", lty = 2, lwd = 3 )
    mb1 <- ts( bounds1[2,], start = c(2020, 0), frequency = 12)
    lines(mb1, col = "black", lty = 2, lwd = 3)
    
  })
  
  output$probaTP <- renderPrint ({
    
    getSymbols(input$action)
    ACTIONB <- getSymbols(input$action, auto.assign = FALSE)
    B.Close <- ACTIONB [,4]
    ActionA.produitMA <- monthlyReturn(B.Close)
    Value1a <- last(B.Close)
    ValueX <- as.numeric(Value1a)
    NPathN = as.numeric(input$numberPath)
    out1a <- rep(0, NPathN)
    NbcolA <- as.numeric(input$samplesize)
    out2a <- matrix(0, nrow = NPathN, ncol = NbcolA)
    
    
    for ( i in 1:NPathN){
      
      produit1.sampA <- sample(as.vector(ActionA.produitMA), NbcolA, replace = FALSE)
      produit1.sampA[1] <- 0
      produit2.sampA <- 1 + produit1.sampA
      produit3.sampA <- cumprod(produit2.sampA)
      value2A <- produit3.sampA*as.numeric(Value1a)
      out2a[ i, ] <- value2A
      Flag1A <- ifelse( value2A > input$takeprofit, 1, 0)
      Flag1A[1] <- 0
      out1a [i] <- max(Flag1A)
      
    }
    
    probSupOjd <- mean(out1a)
    print(probSupOjd)
  })
  
  output$probaSL <- renderPrint ({
    
    getSymbols(input$action)
    ACTIONB <- getSymbols(input$action, auto.assign = FALSE)
    B.Close <- ACTIONB [,4]
    ActionA.produitMA <- monthlyReturn(B.Close)
    Value1a <- last(B.Close)
    ValueX <- as.numeric(Value1a)
    NPathN = as.numeric(input$numberPath)
    out1a <- rep(0, NPathN)
    NbcolA <- as.numeric(input$samplesize)
    out2a <- matrix(0, nrow = NPathN, ncol = NbcolA)
    
    
    for ( i in 1:NPathN){
      
      produit1.sampA <- sample(as.vector(ActionA.produitMA), NbcolA, replace = FALSE)
      produit1.sampA[1] <- 0
      produit2.sampA <- 1 + produit1.sampA
      produit3.sampA <- cumprod(produit2.sampA)
      value2A <- produit3.sampA*as.numeric(Value1a)
      out2a[ i, ] <- value2A
      Flag1A <- ifelse( value2A < input$stoploss, 1, 0)
      Flag1A[1] <- 0
      out1a [i] <- max(Flag1A)
      
    }
    
    probSupOjd <- mean(out1a)
    print(probSupOjd)
    
  })
  
  ##############################################################################################################################
  #PLOT/HISTOGRAMME FREQUENCES // ROI TIMESERIES
  ##############################################  
  
  output$hist <- renderPlot({
    
    ACTIONT <- getSymbols(input$actionT, auto.assign = FALSE)
    taille2 <- as.numeric(input$taille)
    TABLEC <- tail(ACTIONT, taille2)
    A.Close <- TABLEC [,4]
    
    ifelse(input$echeance == "dailyReturn", 
           ActionT.produit <- dailyReturn(A.Close),
           
           ifelse(input$echeance == "weeklyReturn", 
                  ActionT.produit <- weeklyReturn(A.Close),
                  ActionT.produit <- monthlyReturn(A.Close)))
    
    plot(ActionT.produit)
    lines(ActionT.produit, col = "purple")
  })
  
  output$openClose <- renderPlot({
    ACTIONTX <- getSymbols(input$actionT, auto.assign = FALSE)
    taille2X <- as.numeric(input$multiCslider)
    TABLECX <- tail(ACTIONTX, taille2X)
    
    Action.CloseX <- TABLECX[,1]
    Action.OpenX <- TABLECX[,4]
    Action.HighX <- TABLECX[,2]
    Action.LowX<- TABLECX[,3]
    
    plot( Action.CloseX, col = "black", lwd = 1.5)
    lines(Action.OpenX, col = "blue", lwd = 1.5)
    lines(Action.HighX, col = "green", lwd = 3)
    lines(Action.LowX, col = "red", lwd = 3)
  })
  ##############################################################################################################################
  output$portfolioPrices <- renderPlot({
    
    ActionP1 <- input$asset1
    ActionP2 <- input$asset2
    ActionP3 <- input$asset3
    ActionP4 <- input$asset4
    ActionP5 <- input$asset5
    
    Weight1 <- as.numeric(input$weight1)
    Weight2 <- as.numeric(input$weight2)
    Weight3 <- as.numeric(input$weight3)
    Weight4 <- as.numeric(input$weight4)
    Weight5 <- as.numeric(input$weight5)
    
    tickers <- c(ActionP1, ActionP2, ActionP3, ActionP4, ActionP5)
    weights <- c(Weight1, Weight2, Weight3, Weight4, Weight5)
    dt <- input$datePortefeuille
    portfolioPrices <- NULL
    
    for (Ticker in tickers){
      
      #il faudra modifier la date de start
      #variable de la periodicite
      
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols.yahoo(Ticker, from=dt, periodicity = "daily", auto.assign=FALSE)[,4])
    }
    
    portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    
    benchmarkPrices <- getSymbols.yahoo("^GSPC", from=dt, periodicity = "daily", auto.assign=FALSE)[,4]
    
    benchmarkReturns <- na.omit(ROC(benchmarkPrices, type="discrete"))
    colnames(portfolioPrices) <- tickers
    colSums(is.na(portfolioPrices))
    
    dailyReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)
    
    plot(portfolioPrices, legend = tickers) 
    
  })
  
  output$portfolioReturns <- renderPlot({
    
    ActionP1 <- input$asset1
    ActionP2 <- input$asset2
    ActionP3 <- input$asset3
    ActionP4 <- input$asset4
    ActionP5 <- input$asset5
    
    Weight1 <- as.numeric(input$weight1)
    Weight2 <- as.numeric(input$weight2)
    Weight3 <- as.numeric(input$weight3)
    Weight4 <- as.numeric(input$weight4)
    Weight5 <- as.numeric(input$weight5)
    
    tickers <- c(ActionP1, ActionP2, ActionP3, ActionP4, ActionP5)
    weights <- c(Weight1, Weight2, Weight3, Weight4, Weight5)
    dt <- input$datePortefeuille
    portfolioPrices <- NULL
    
    for (Ticker in tickers){
      
      #il faudra modifier la date de start
      #variable de la periodicite
      
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols.yahoo(Ticker, from=dt, periodicity = "daily", auto.assign=FALSE)[,4])
    }
    
    portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    
    benchmarkPrices <- getSymbols.yahoo("^GSPC", from=dt, periodicity = "daily", auto.assign=FALSE)[,4]
    
    benchmarkReturns <- na.omit(ROC(benchmarkPrices, type="discrete"))
    colnames(portfolioPrices) <- tickers
    colSums(is.na(portfolioPrices))
    
    dailyReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)
    
    charts.PerformanceSummary(portfolioReturn)
  })
  
  output$beta <- renderText({
    
    ActionP1 <- input$asset1
    ActionP2 <- input$asset2
    ActionP3 <- input$asset3
    ActionP4 <- input$asset4
    ActionP5 <- input$asset5
    
    Weight1 <- as.numeric(input$weight1)
    Weight2 <- as.numeric(input$weight2)
    Weight3 <- as.numeric(input$weight3)
    Weight4 <- as.numeric(input$weight4)
    Weight5 <- as.numeric(input$weight5)
    
    tickers <- c(ActionP1, ActionP2, ActionP3, ActionP4, ActionP5)
    weights <- c(Weight1, Weight2, Weight3, Weight4, Weight5)
    dt <- input$datePortefeuille
    portfolioPrices <- NULL
    
    for (Ticker in tickers){
      
      #il faudra modifier la date de start
      #variable de la periodicite
      
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols.yahoo(Ticker, from=dt, periodicity = "daily", auto.assign=FALSE)[,4])
    }
    
    portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    
    benchmarkPrices <- getSymbols.yahoo("^GSPC", from=dt, periodicity = "daily", auto.assign=FALSE)[,4]
    
    benchmarkReturns <- na.omit(ROC(benchmarkPrices, type="discrete"))
    colnames(portfolioPrices) <- tickers
    colSums(is.na(portfolioPrices))
    
    dailyReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)
    
    print(CAPM.beta(portfolioReturn, benchmarkReturns, .035/252))
    
  })
  
  output$betaBull <- renderText({
    
    ActionP1 <- input$asset1
    ActionP2 <- input$asset2
    ActionP3 <- input$asset3
    ActionP4 <- input$asset4
    ActionP5 <- input$asset5
    
    Weight1 <- as.numeric(input$weight1)
    Weight2 <- as.numeric(input$weight2)
    Weight3 <- as.numeric(input$weight3)
    Weight4 <- as.numeric(input$weight4)
    Weight5 <- as.numeric(input$weight5)
    
    tickers <- c(ActionP1, ActionP2, ActionP3, ActionP4, ActionP5)
    weights <- c(Weight1, Weight2, Weight3, Weight4, Weight5)
    dt <- input$datePortefeuille
    portfolioPrices <- NULL
    
    for (Ticker in tickers){
      
      #il faudra modifier la date de start
      #variable de la periodicite
      
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols.yahoo(Ticker, from=dt, periodicity = "daily", auto.assign=FALSE)[,4])
    }
    
    portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    
    benchmarkPrices <- getSymbols.yahoo("^GSPC", from=dt, periodicity = "daily", auto.assign=FALSE)[,4]
    
    benchmarkReturns <- na.omit(ROC(benchmarkPrices, type="discrete"))
    colnames(portfolioPrices) <- tickers
    colSums(is.na(portfolioPrices))
    
    dailyReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)
    
    print(CAPM.beta.bull(portfolioReturn, benchmarkReturns, .035/252)
    )
    
  })
  
  output$betaBear <- renderText({
    
    ActionP1 <- input$asset1
    ActionP2 <- input$asset2
    ActionP3 <- input$asset3
    ActionP4 <- input$asset4
    ActionP5 <- input$asset5
    
    Weight1 <- as.numeric(input$weight1)
    Weight2 <- as.numeric(input$weight2)
    Weight3 <- as.numeric(input$weight3)
    Weight4 <- as.numeric(input$weight4)
    Weight5 <- as.numeric(input$weight5)
    
    tickers <- c(ActionP1, ActionP2, ActionP3, ActionP4, ActionP5)
    weights <- c(Weight1, Weight2, Weight3, Weight4, Weight5)
    dt <- input$datePortefeuille
    portfolioPrices <- NULL
    
    for (Ticker in tickers){
      
      #il faudra modifier la date de start
      #variable de la periodicite
      
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols.yahoo(Ticker, from=dt, periodicity = "daily", auto.assign=FALSE)[,4])
    }
    
    portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    
    benchmarkPrices <- getSymbols.yahoo("^GSPC", from=dt, periodicity = "daily", auto.assign=FALSE)[,4]
    
    benchmarkReturns <- na.omit(ROC(benchmarkPrices, type="discrete"))
    colnames(portfolioPrices) <- tickers
    colSums(is.na(portfolioPrices))
    
    dailyReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)
    
    print(CAPM.beta.bear(portfolioReturn, benchmarkReturns, .035/252)
    )
    
  })
  
  output$Alpha <- renderText({
    
    ActionP1 <- input$asset1
    ActionP2 <- input$asset2
    ActionP3 <- input$asset3
    ActionP4 <- input$asset4
    ActionP5 <- input$asset5
    
    Weight1 <- as.numeric(input$weight1)
    Weight2 <- as.numeric(input$weight2)
    Weight3 <- as.numeric(input$weight3)
    Weight4 <- as.numeric(input$weight4)
    Weight5 <- as.numeric(input$weight5)
    
    tickers <- c(ActionP1, ActionP2, ActionP3, ActionP4, ActionP5)
    weights <- c(Weight1, Weight2, Weight3, Weight4, Weight5)
    dt <- input$datePortefeuille
    portfolioPrices <- NULL
    
    for (Ticker in tickers){
      
      #il faudra modifier la date de start
      #variable de la periodicite
      
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols.yahoo(Ticker, from=dt, periodicity = "daily", auto.assign=FALSE)[,4])
    }
    
    portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    
    benchmarkPrices <- getSymbols.yahoo("^GSPC", from=dt, periodicity = "daily", auto.assign=FALSE)[,4]
    
    benchmarkReturns <- na.omit(ROC(benchmarkPrices, type="discrete"))
    colnames(portfolioPrices) <- tickers
    colSums(is.na(portfolioPrices))
    
    dailyReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)
    
    print(CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, .035/252)
    )
    
  })
  
  output$sharp <- renderText({
    
    ActionP1 <- input$asset1
    ActionP2 <- input$asset2
    ActionP3 <- input$asset3
    ActionP4 <- input$asset4
    ActionP5 <- input$asset5
    
    Weight1 <- as.numeric(input$weight1)
    Weight2 <- as.numeric(input$weight2)
    Weight3 <- as.numeric(input$weight3)
    Weight4 <- as.numeric(input$weight4)
    Weight5 <- as.numeric(input$weight5)
    
    tickers <- c(ActionP1, ActionP2, ActionP3, ActionP4, ActionP5)
    weights <- c(Weight1, Weight2, Weight3, Weight4, Weight5)
    dt <- input$datePortefeuille
    portfolioPrices <- NULL
    
    for (Ticker in tickers){
      
      #il faudra modifier la date de start
      #variable de la periodicite
      
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols.yahoo(Ticker, from=dt, periodicity = "daily", auto.assign=FALSE)[,4])
    }
    
    portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    
    benchmarkPrices <- getSymbols.yahoo("^GSPC", from=dt, periodicity = "daily", auto.assign=FALSE)[,4]
    
    benchmarkReturns <- na.omit(ROC(benchmarkPrices, type="discrete"))
    colnames(portfolioPrices) <- tickers
    colSums(is.na(portfolioPrices))
    
    dailyReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
    portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)
    
    print(SharpeRatio(portfolioReturn, Rf = .035/252, p = 0.95, FUN = "StdDev",
                      weights = NULL, annualize = FALSE)
    )
  })
  
  ##############################################################################################################################  
  output$efrg <- renderPlot({
    
    EFActionP1 <- input$EFasset1
    EFActionP2 <- input$EFasset2
    EFActionP3 <- input$EFasset3
    EFActionP4 <- input$EFasset4
    EFActionP5 <- input$EFasset5
    
    minBOX <- as.numeric(input$minBox)
    maxBOX <- as.numeric(input$maxBox)
    
    tickers <- c(EFActionP1, EFActionP2, EFActionP3, EFActionP4, EFActionP5)
    
    portfolioPrices <- NULL
    
    for(ticker in tickers){
      
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols.yahoo(ticker, from='2016-01-03', periodicity = 'daily', auto.assign=FALSE)[,4])
    }
    portfolioReturns <- na.omit(ROC(portfolioPrices))
    
    portf <- portfolio.spec(colnames(portfolioReturns))
    
    portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
    portf <- add.constraint(portf, type="box", min=minBOX, max=maxBOX)
    portf <- add.objective(portf, type="return", name="mean")
    portf <- add.objective(portf, type="risk", name="StdDev")
    
    optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI", trace=TRUE)
    chart.Weights(optPort)
    ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 25,
                                   risk_aversion = NULL)
    
    chart.EfficientFrontier(ef,
                            match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                            cex.axis = 0.8, element.color = "blue", main = "Efficient Frontier",
                            RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                            chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                            cex.assets = 0.8)
    
  })
  ##############################################################################################################################  
  output$opt <- renderPlot({
    
    EFActionP1 <- input$EFasset1
    EFActionP2 <- input$EFasset2
    EFActionP3 <- input$EFasset3
    EFActionP4 <- input$EFasset4
    EFActionP5 <- input$EFasset5
    
    minBOX <- as.numeric(input$minBox)
    maxBOX <- as.numeric(input$maxBox)
    
    tickers <- c(EFActionP1, EFActionP2, EFActionP3, EFActionP4, EFActionP5)
    
    portfolioPrices <- NULL
    
    for(ticker in tickers){
      
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols.yahoo(ticker, from='2016-01-03', periodicity = 'daily', auto.assign=FALSE)[,4])
    }
    portfolioReturns <- na.omit(ROC(portfolioPrices))
    
    portf <- portfolio.spec(colnames(portfolioReturns))
    
    portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
    portf <- add.constraint(portf, type="box", min=minBOX, max=maxBOX)
    portf <- add.objective(portf, type="return", name="mean")
    portf <- add.objective(portf, type="risk", name="StdDev")
    
    optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI", trace=TRUE)
    chart.Weights(optPort)
    
  })
  
  ##############################################################################################################################  
}
##############################################################################################################################
#COMPILOR
#########

shinyApp(ui, server)
