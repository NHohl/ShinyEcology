library("shiny")
library("ggplot2")
library("extrafont")
library("deSolve")

# Must be saved in UTC-8 to show accents correctly.

ui <- fluidPage(
  titlePanel("A Primer of Ecology With Shiny ;p", "Nathalia's shiny app"),
  br(),
  sidebarLayout(
    sidebarPanel(sliderInput("N0GeoIn", "N0",
                             min = 1, max = 100, value = 5),
                 sliderInput("RGeoIn", "R",
                             min = -1, max = 3, value = 0.1, step = 0.01),
                 sliderInput("tGeoIn", "t",
                             min = 0, max = 100, value = 50)
    ),
    mainPanel(plotOutput("geoGplot"))
  ),
  
  sidebarLayout(
    sidebarPanel(sliderInput("N0ExpIn", "N0",
                           min = 1, max = 100, value = 1),
               sliderInput("rExpIn", "r",
                           min = -3, max = 3, value = 0.1, step = 0.01),
               sliderInput("tExpIn", "t",
                           min = 0, max = 100, value = 50)),
    mainPanel(plotOutput("expGPlot")) 
  ),
  
  br(),
  sidebarLayout(
    sidebarPanel(sliderInput("N0LogIn", "N0",
                             min = 1, max = 100, value = 1),
                 sliderInput("rLogIn", "r",
                             min = -3, max = 3, value = 0.2, step = 0.01),
                 sliderInput("tLogIn", "t",
                             min = 0, max = 100, value = 80),
                 sliderInput("kLogIn", "K", min = 0, max = 100, value = 50)
                 ),
    mainPanel(plotOutput("logGPlot"))
  ),
  
  br(),
  sidebarLayout(
    sidebarPanel(sliderInput("N01Comp", "N01",
                             min = 1, max = 100, value = 50),
                 sliderInput("N02Comp", "N02",
                                          min = 1, max = 100, value = 50),
                 numericInput("r1Comp", "r1",
                             min = -5, max = 5, value = 0.1, step = 0.01),
                 numericInput("r2Comp", "r2",
                             min = -5, max = 5, value = 0.1, step = 0.01),
                 numericInput("K1Comp", "K1", min = 1, max = 500, value = 100, step = 1),
                 numericInput("K2Comp", "K2", min = 0, max = 3, value = 50, step = 1),
                 numericInput("alphaComp", "alpha", min = 0, max = 3, value = 0.5, step = 0.005),
                 numericInput("betaComp", "beta", min = 0, max = 0.1, value = 0.5, step = 0.005),
                 sliderInput("tComp", "t", min = 1, max = 300, value = 100)
                 
    ),
    mainPanel(plotOutput("compPlot"),
              br(),
              plotOutput("compIsoPlot")
              )

  ),
  
  br(),
  sidebarLayout(
    sidebarPanel(
      numericInput("P0Pred", "População inicial de Predadores - P0\n 
                   (milhares de indivíduos)",
                   min = 0, max = 100, value = 0.9, step = 0.1),
      numericInput("V0Pred", "População inicial de Vítimas - V0\n 
                   (milhares de indivíduos)",
                   min = 0, max = 100, value = 0.9, step = 0.1),
      numericInput("rPred", "Taxa de cresc. intríns. da vítima - r)",
                   min = 0, max = 3, value = 0.6, step = 0.1),
      numericInput("mPred", "Taxa de mortalidade do predador (m ou q)",
                   min = 0, max = 3, value = 1, step = 0.1),
      numericInput("cPred", "Eficiência de captura do predador (c ou alpha)",
                   min = 0, max = 3, value = 0.5, step = 0.1),
      numericInput("aPred", "Eficiência de conversão do predador (a ou beta)",
                   min = 0, max = 3, value = 0.8, step = 0.1),
      numericInput("tPred", "t (de 0 a 1000)", min = 0, max = 1000, value = 50)
    ),
    mainPanel(plotOutput("predPlot"),
              br(),
              h4("O modelo pode gerar populações em valores decimais, o que não reflete\n
                 a realidade.\n
                 Utilizar \"milhares de indivíduos\" como unidade reduz o conflito."),
              h4("Aviso: Utilizar valores que gerem P ou V muito grandes (sejam \n
                 positivos ou negativos) fará o gráfico parar de funcionar por exceder \n
                 a capacidade do computador."),
              br(),
              plotOutput("predIsoPlot")
    )
  )

)#end of UI


server <- function(input, output){
  #----- GEOMETRIC GROWTH
  
  # You can't use 0:input$x. A solution is to create a vector with the possible
  # input values and filter it inside of the reactive data frame creation.
  t <- c(0:100)
  n <- c(0:100)
  df <- data.frame(t,n)
  
  geoGdf <- reactive({
    df <- df[df$t %in% seq(from = 0, to = input$tGeoIn, by = 1),] # Filters for the
    #actual input.
    df$n <- input$N0GeoIn * (1 + input$RGeoIn) ^ df$t
    #print(df) #for debugging
    df
  })
  
  #renders the GEOMETRIC GROWTH plot
  
  output$geoGplot <- renderPlot({
    ggplot(geoGdf(), aes(x = t, y = n)) +
    geom_line(linetype = "dashed") +
    geom_point(colour='lightsteelblue4', size = 3) +
    theme_bw() +
    ggtitle("Crescimento Geométrico (Em Passos)") +
    xlab("t") +
    ylab("N(t)") +
    theme(plot.title = element_text(size = 24, hjust = 0.5,
                                      family = "Calibri", face = "bold"),
            axis.title = element_text(size = 20,
                                      family = "Calibri", face = "bold"),
            axis.text = element_text(size = 16,
                                     family = "Calibri", face = "bold")
    )
    
  })
  
  
  
  #-----CONTINUOUS EXPONENTIAL GROWTH
  t1 <- c(0:100)
  N1 <- c(0:100)
  df1 <- data.frame(t1,N1)
  
  expGdf <- reactive({
    df1 <- df1[df1$t1 %in% seq(from=0,to=input$tExpIn,by=1),]
    df1$N1 <-  input$N0ExpIn * exp((input$rExpIn * df1$t1)) #exp(r*t) = e^(rt)
    df1
  })

  # renders the EXPONENTIAL GROWTH PLOT
  output$expGPlot<-renderPlot({
    ggplot(expGdf(),aes(x=t1,y=N1)) +
    geom_line(color = "lightsteelblue4") +
    geom_point(colour='lightsteelblue4', size = 3) +
    theme_bw() +
    ggtitle("Crescimento Exponencial Contínuo") +
    xlab("t") +
    ylab("N(t)") +
    theme(plot.title = element_text(size = 24, hjust = 0.5,
                                    family = "Calibri", face = "bold"),
          axis.title = element_text(size = 20,
                                    family = "Calibri", face = "bold"),
          axis.text = element_text(size = 16,
                                   family = "Calibri", face = "bold")
    )
    })

  #-----LOGISTIC (DENSITY-DEPENDENT) GROWTH
  t2 <- c(0:100)
  n2 <- c(0:100)
  df2 <- data.frame(t2,n2)

  
  logGdf <- reactive({
    df2 <- df2[df2$t2 %in% seq(from=0,to=input$tLogIn, by=1),]
    df2$n2 <- input$kLogIn / (1 + ((input$kLogIn - input$N0LogIn) / input$N0LogIn) * exp(-input$rLogIn * df2$t2))
    #print(df2) #only for debugging
    df2
  })
  
  # formula: Nt = ( k / (1 + ((k - n0) / n0) * exp(-r * t)))

  #renders the LOGISTIC GROWTH PLOT
  output$logGPlot<-renderPlot({
    ggplot(logGdf(),aes(x=t2,y=n2)) +
      geom_point(colour='lightsteelblue4', size = 3) +
      geom_hline(yintercept = input$kLogIn, linetype = "dashed", color = "red1") +
      annotate("text", x = input$tLogIn, y = (input$kLogIn - input$kLogIn/20 ),
               label = "K", size = 6, color = "red1") + # o y desce o "K" em 20% do valor para arrumar
      theme_bw() +
      ggtitle("Crescimento Logístico") +
      xlab("t") +
      ylab("N(t)") +
      theme(plot.title = element_text(size = 24, hjust = 0.5,
                                      family = "Calibri", face = "bold"),
            axis.title = element_text(size = 20,
                                      family = "Calibri", face = "bold"),
            axis.text = element_text(size = 16,
                                     family = "Calibri", face = "bold")
      )
  })  

  
  #--------------- INTERESPECIFIC COMPETITION  (with continuous logistic growth)
  #using deSolve package
  #code modified from A Primer of Ecology With R
  
  t3 <- c(0:300) # The fixed maximum value of t. It creates a vector that could hold
                 #any possible number of points and is then edited according to input t.
                 
  df3 <- data.frame(t3)
  
  Compdf <- reactive({
    df3 <- df3[df3$t3 %in% seq(from=0,to=input$tComp, by=1),] #adjusts the df size to input t
    # (df3 isn't returned in Compdf, but it still serves as a way of filtering
    # for the requested t. Maybe it could be done in a vector instead of df for
    # performance reasons, but this is the best current solution.)
    
    steps <- as.vector(df3) #used later as the "times" for ode function and to create the color columns

    parms <- c(r1 = input$r1Comp, r2 = input$r2Comp, K1 = input$K1Comp,
               beta = input$betaComp, K2 = input$K2Comp, alpha = input$alphaComp) #named vector of doubles
    
    initialN <- c(input$N01Comp,input$N02Comp)
    
    #From the ode() help:
    #If func is an R-function, it must be defined as: 
    #func <- function(t, y, parms,...). t is the current time point in the 
    #integration, y is the current estimate of the variables in the ODE system.
    #
    #Function that'll be used in ode():
    lv.comp2 <- function(t, n, parms) {
      with(as.list(parms), {
        dn1dt <- r1 * n[1] * (1 - n[1]/K1 - (alpha * n[2]/K1) )
        dn2dt <- r2 * n[2] * (1 - n[2]/K2 - (beta * n[2]/K2) )
        list(c(dn1dt, dn2dt))
      })
      #n is used in place of y, but it's in the right position so it still works.
      #n is the vector that holds N1 and N2 values at a given time.
      #If parms wasn't used as a named list, you'd have to access its values
      #with parms[1] and so on instead of using their names (K1, K2, etc), so the
      #"with" is needed.
    }
    
    # General Solver of Ordinary Differential Equations
    # ode(y, times, func, parms, [...])
    
    out <- ode(y = initialN, times = steps, func = lv.comp2, parms = parms)
    
    print(out)
    
    N1 = rep("N1",length(steps))
    N2 = rep("N2",length(steps))
    
    result <- data.frame(out, N1, N2) #N1 and N2 are used by the aes function to colour the lines

    result
  })
  
  
  
  
  # renders the COMPETITION PLOT
  output$compPlot<-renderPlot({
    ggplot(Compdf(),aes(x=time,y=X1)) + #X1 and X2 come from ode() returned values
      geom_point(aes(colour=N1), size = 2) +
      geom_point(size = 2, aes(x = time, y =X2, colour = N2)) +
      geom_hline(yintercept = input$K1Comp, linetype = "dashed", color = "black") +
      annotate("text", x = input$tLogIn, y = (input$K1Comp - (input$K1Comp)/20 ),
               label = "K1", size = 6, color = "black") +
      geom_hline(yintercept = input$K2Comp, linetype = "dashed", color = "black") +
      annotate("text", x = input$tLogIn, y = (input$K2Comp - (input$K2Comp)/20 ),
               label = "K2", size = 6, color = "black") +
      theme_bw() +
      ggtitle("Competição Interespecífica") +
      xlab("t") +
      ylab("N(t)") +
      theme(plot.title = element_text(size = 24, hjust = 0.5,
                                      family = "Calibri", face = "bold"),
            axis.title = element_text(size = 20,
                                      family = "Calibri", face = "bold"),
            axis.text = element_text(size = 16,
                                     family = "Calibri", face = "bold"),
            legend.text = element_text(size = 16,
                                       family = "Calibri", face = "bold"),
            legend.title = element_blank()
      )
  })
  
 #--------- COMPETITION ISOCLINES
  # Uses the same input from the Competition Plot to generate the correspondent isoclines.

  xy <- reactiveValues() # This is used to create reactive values from anywhere that can be accessed
  # through reactive functions. Ex:
  # xy$a <- 1 (from anywhere inside the server)
  # print(xy$a) (from a reactive context)
 
  Isodf <- reactive({
    
    k1 <- input$K1Comp
    k2 <- input$K2Comp
    
    # 'xy$lim' gets the max number of points any isocline will need (as the greatest value on
    # either x or y axis) and is used as both the number of points to be calculed by the function
    # and the plot area limits.
    
    xy$lim <- max(c(input$K1Comp/input$alphaComp, input$K2Comp, input$K2Comp/input$betaComp,
                  input$K1Comp))

    npoints <- 0:xy$lim # Creates the vector to be used by the function

    df4 <- data.frame(Nmax = npoints) # Creates the appropriately sized dataframe

    N1Iso <- input$K1Comp - npoints * input$alphaComp  # npoints would be N2 in the formula
    N2Iso <- input$K2Comp - npoints * input$betaComp   # npoints would be N1 in the formula

    df4$Iso1 <- N1Iso
    df4$Iso2 <- N2Iso
    
    np <- xy$lim
    df4$color1 <- rep("N1", times = np + 1)
    df4$color2 <- rep("N2", times = np + 1)
    
    #print(head(df4[,1:3],3)) 
    #print(tail(df4[,1:3],3)) 

    df4
  })
  
  # renders the COMPETITION ISOCLINES plot
  output$compIsoPlot<-renderPlot({
    ggplot(Isodf(), aes(x = Iso1, y = Nmax,
                        color = color1)) + #N1 iso
      geom_line(size = 0.8) +
      geom_line(aes(x = Nmax, y = Iso2,
                    color = color2), #N2 iso
                size = 0.8) + 
      theme_bw() +
      ggtitle("Isoclinas") +
      xlab("N1") +
      theme_bw() +
      ylab("N2") +
      xlim(0,xy$lim) +
      ylim(0, xy$lim) +
      theme(plot.title = element_text(size = 24, hjust = 0.5,
                                      family = "Calibri", face = "bold"),
            axis.title = element_text(size = 20,
                                      family = "Calibri", face = "bold"),
            axis.text = element_text(size = 16,
                                     family = "Calibri", face = "bold"),
            legend.text = element_text(size = 16,
                                       family = "Calibri", face = "bold"),
            legend.title = element_blank()
            #axis.line.x = element_line(colour = 'black', size = 0.5, linetype='solid')
      ) #+
      #annotate("text", label = "K1", x = input$K1Comp + xy$lim/20, y = xy$lim/20, size = 6, color = "red") +
      #annotate("text", label = "K1/a", y = (input$K1Comp/input$alphaComp), x = xy$lim/20, size = 6, color = "red") +
      #annotate("text", label = "K2", y = input$K2Comp + xy$lim/20, x = xy$lim/20, size = 6, color = "blue") +
      #annotate("text", label = "K2/B", x = ( (input$K2Comp/input$betaComp) + xy$lim/20), y = xy$lim/20, size = 6, color = "blue")
    
      # MARCAR O PONTO DE EQUILIBRIO
    
  })
  
  
  # PREDATION x PREY MODEL ----------------------
  
  t5 <- c(0:1000) # The fixed maximum value of t. It creates a vector that could hold
  #any possible number of points and is then edited according to the input t.
  
  df5 <- data.frame(t5)
  
  Pred.df <- reactive({
    df5 <- df5[df5$t5 %in% seq(from=0,to=input$tPred, by=1),] #adjusts the df size to the input t
    
    steps <- as.vector(df5) #used later as the "times" for ode function and to create the color columns through repetition
    
    parms <- c(r = input$rPred, V = input$V0Pred, c = input$cPred,
               m = input$mPred, P = input$P0Pred, a = input$aPred)
    initialN <- c(input$V0Pred,input$P0Pred)
    
    # See comments on Competition for help.
    lv.pred <- function(t, n, parms) {
      with(as.list(parms), {
        dV.dt <- r * n[1] - c * n[1] * n[2]
        dP.dt <- a * c * n[1] * n[2] - m * n[2]
        list(c(dV.dt, dP.dt))
      })
    }
    
    # General Solver of Ordinary Differential Equations
    # ode(y, times, func, parms, [...]) See Competition for more.
    
    out <- ode(y = initialN, times = steps, func = lv.pred, parms = parms)
    
    V = rep("V",length(steps)) #used in plot legend and colors
    P = rep("P",length(steps))
    
    result <- data.frame(out, V, P) #V and P are used by the aes function to color 
    #the lines.
    
    print(head(result, 3))
    print(tail(result, 3))
    
    result
  })
  
  predIsodf <- reactive({
    isoP <- input$mPred / (input$aPred * input$cPred)
    isoV <- input$rPred / input$cPred
    df <- data.frame(isoV, isoP)
    print(df)
    df
  })
  
  # ------- Predation N(t) plot
  
  output$predPlot <- renderPlot({
    ggplot(data = Pred.df()) +
      geom_point(aes(x = time, y = X1, color = V), size = 2) +
      geom_line(aes(x = time, y = X1, color = V), size = 0.5) +
      geom_point(aes(x = time, y = X2, color = P), size = 2) +
      geom_line(aes(x = time, y = X2, color = P), size = 0.5) +
      theme_bw() +
      ggtitle("Predador x Presa") +
      xlab("t") +
      theme_bw() +
      ylab("Tamanho das Populações de P e V\n (milhares de indivíduos)") +
      theme(plot.title = element_text(size = 24, hjust = 0.5,
                                      family = "Calibri", face = "bold"),
            axis.title = element_text(size = 20,
                                      family = "Calibri", face = "bold"),
            axis.text = element_text(size = 16,
                                     family = "Calibri", face = "bold"),
            legend.text = element_text(size = 16,
                                       family = "Calibri", face = "bold"),
            legend.title = element_blank()
            )
  })
  
  # Phase Space Plot (Predation x Prey)
  output$predIsoPlot <- renderPlot({
    ggplot() +
      geom_hline(data = predIsodf(), aes(yintercept = isoV)) +
      geom_vline(data = predIsodf(), aes(xintercept = isoP)) +
      geom_point(data = Pred.df(), aes(x = X1, y = X2),
                 color = c("lightsteelblue4"), alpha = 0.4, size = 3) +
      geom_path(data = Pred.df(), aes(x = X1, y = X2),
                color = "lightsteelblue4", alpha = 0.6) +
      theme_bw() +
      ggtitle("Espaço de Fase") +
      xlab("Pop. de Presas (V)") +
      ylab("Pop. de Predadores (P)") +
      theme(plot.title = element_text(size = 24, hjust = 0.5,
                                      family = "Calibri", face = "bold"),
            axis.title = element_text(size = 20,
                                      family = "Calibri", face = "bold"),
            axis.text = element_text(size = 16,
                                     family = "Calibri", face = "bold"),            legend.text = element_text(size = 16,
                                       family = "Calibri", face = "bold"),
            legend.title = element_blank()
      )
    
    
  })


  
#----------------------------------------------------------------------
  
  #--------------------OBSERVE----------------
  #observe({ print(input$priceInput[1]) }) 
  
  #un-comment the last line for testing
  # by using observe you can access
  #a reactive variable (like an input) outside of reactive context (that could be
  #a render function)
  #this is not mapped to the ui and just prints the value in the console
  
  #-------------------REACTIVE----------------
  #
  #priceDiff <- reactive({
  #  diff(input$priceInput)
  #})
  #observe({print( priceDiff() ) })
  #}
  
  #un-comment the last section for testing
  #By using reactive you can create a reactive variable that does not come
  #directly from input.
  #When printing, you must use observe({}) and refer to the variable name as it
  #was a function with () 
  
  
  
  
  
  
  
  
} #end of server, don't exclude (again)

shinyApp(ui = ui, server = server)

# rtsudio Version 1.1.442
# R version 3.4.3 (2017-11-30) Kite-Eating Tree 