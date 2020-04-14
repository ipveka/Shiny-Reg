
##### Server

library("shinydashboard") # Shiny dashboard
library("shinycssloaders") # Animated CSS loader
library("shinyalert") # Shiny Alerts
library("shinyWidgets") # Shiny Widgets
library("shinytest") # For testing 
library("shinyjs") # JavaScript
library("markdown")
library("lmtest")
library("mctest")
library("lm.beta")
library("visreg")
library("caret")
library("DT")

server <- function(input, output) {
  
  ### Funcions i crides:
  
  # TaulaContinguts(x)
  
  TaulaContinguts <- function(x) {
    data.frame(row.names = 1:length(x),
               Name = substr(names(x), 1, 19),
               Class = sapply(x, class),
               N_unique = sapply(x, function(v) length(unique(v))))
  }
  
  # Colors()
  
  Colors <- function(nv=4){
    if(require(RColorBrewer)){
      cols=brewer.pal(nv,"Pastel1")
    }else{
      warning("This plot would like nicer if you installed RColorBrewer")
      cols=(1:nv)
    }
    return(cols)
  }
  
  # Sdbeta()
  
  Sdbeta <- function(lm){
    b <- summary(lm)$coef[-1, 1]
    beta <- c()
    for(i in 1:length(b)){
      sx <- lm$model[-1]
      sdxi <- sd(sx[,i])
      sy <- sd(lm$model[,1])
      beta <- c(beta,b[i] * sdxi/sy)
    }
    return(beta)
  }
  
  ### Data: ----------------------------------------------------------------
  
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(iris)
    data <- read.csv(inFile$datapath, header = input$header,
                     sep = input$sep,
                     quote = input$quote)
  })
  
  output$table <- DT::renderDataTable(
    myData(),
    options = list(scrollX = TRUE,pageLength = 15)
  )
  
  ### Summary of data: ----------------------------------------------------------------
  
  output$select <- renderUI({
    df <- myData()
    selectInput("variable","Variable:",names(df))
  })
  
  output$toc <- renderDataTable({
    a <- TaulaContinguts(myData())
    datatable(a, rownames=FALSE, 
              options = list(dom = 't',ordering=F, columnDefs = list(
                list(className = "dt-center", targets = "_all")
              )))
  })
  
  output$summary <- renderDataTable({
    options(digits = 3)
    df <- myData()
    df <- df[,input$variable]
    v <- c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
    a <- as.vector(summary(df))
    c <- data.frame(v,a)
    colnames(c) <- c("Statisic","Value")
    datatable(c, rownames=FALSE, 
              options = list(dom = 't',ordering=F, columnDefs = list(
                list(className = "dt-center", targets = "_all")
              )))
  })
  
  ### Summary plots: ----------------------------------------------------------------
  
  output$plot1 <- renderPlot({
    df <- myData()
    df <- df[,input$variable]
    if(is.numeric(df)){
      hist(df, col="gray16",type="p",ylab=input$variable,xlab="Index")
    }
    else{
      plot(df,col="gray16",type="p",ylab=input$variable,xlab="Index")
    }
  })
  
  output$plot2 <- renderPlot({
    df <- myData()
    df <- df[,input$variable]
    if(is.numeric(df)){
      plot(df,col="gray16",type="p",ylab=input$variable,xlab="Index")
    }
  })
  
  output$plot3 <- renderPlot({
    df <- myData()
    df <- df[,input$variable]
    if(is.numeric(df)){
      boxplot(df, col="gray16",ylab=input$variable,xlab="Index")
    } 
  })
  
  ### Regression: ----------------------------------------------------------------
  
  output$model_select<-renderUI({
    selectInput("modelselect","Select Algo",choices = c("Ordinary Least Square"="reg"))
  })
  
  output$var1_select<-renderUI({
    selectInput("ind_var_select","Select Independent Var", choices =as.list(names(myData())),multiple = FALSE)
  })
  
  output$rest_var_select<-renderUI({
    checkboxGroupInput("other_var_select","Select Dependent Var",choices =as.list(names(myData())))
  })
  
  output$other_val_show<-renderPrint({
    input$other_var_select
    input$ind_var_select
    f<-myData()
    library(caret)
    form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
    print(form)
    reg <- lm(as.formula(form),data=f)
    print(summary(reg))
  })
  
  myReg <- reactive({
    input$other_var_select
    input$ind_var_select
    f<-myData()
    library(caret)
    form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
    print(form)
    reg <-lm(as.formula(form),data=f)
    reg
  })
  
  ### Diagnostics: ----------------------------------------------------------------
  
  ### Anova: ----------------------------------------------------------------
  
  output$anova <- renderDataTable({
    a <- myReg()
    a <- anova(a)
    a$coefficients
    Variables <- as.vector(row.names(a))
    Df <- a[,1]
    SumSq <- a[,2]
    MeanSq <- a[,3]
    Fval <- a[,4]
    Pval <- a[,5]
    t <- data.frame(Variables,Df,SumSq,MeanSq,Fval,Pval)
    colnames(t) <- c("Variables","DF","Sum of Squares","Mean of Squares","F-value","P-value")
    datatable(t, rownames=FALSE, 
              options = list(dom = 't',ordering=F, columnDefs = list(
                list(className = "dt-center", targets = "_all")
              )))
  })
  
  ### Reg table: ----------------------------------------------------------------
  
  output$reg <- renderDataTable({
    a1 <- myReg()
    a <- summary(a1)
    a$coefficients
    Variables <- as.vector(row.names(a$coefficients))
    Estimate <- a$coefficients[,1]
    b <- lm.beta(a1)
    StdEstimate <- b$coefficients
    SdError <- a$coefficients[,2]
    Tval <- a$coefficients[,3]
    Pval <- a$coefficients[,4]
    t <- data.frame(Variables,Estimate,StdEstimate,SdError,Tval,Pval)
    colnames(t) <- c("Variables","Estimate","Standardized Est.","Std. Error","T-value","P-value")
    datatable(t, rownames=FALSE, 
              options = list(dom = 't',ordering=F, columnDefs = list(
                list(className = "dt-center", targets = "_all")
              )))
  })
  
  ### Conf intervals: ----------------------------------------------------------------
  
  output$confint1 <- renderDataTable({
    a1 <- myReg()
    t <-   as.data.frame(confint(lm.beta(a1)))
    datatable(t, rownames=TRUE, 
              options = list(dom = 't',ordering=F, columnDefs = list(
                list(className = "dt-center", targets = "_all")
              )))
  })
  
  output$confint2 <- renderDataTable({
    a1 <- myReg()
    t <- confint(a1)
    datatable(t, rownames=TRUE, 
              options = list(dom = 't',ordering=F, columnDefs = list(
                list(className = "dt-center", targets = "_all")
              )))
  })
  
  ### Infer table: ----------------------------------------------------------------
  
  output$model <- renderDataTable({
    a <- myReg()
    a <- summary(a)
    q <- a$fstatistic[1] #Value
    w <- a$fstatistic[2] #Groups
    e <- a$fstatistic[3] #DF
    pvalue <- df(q,w,e)
    v1 <- c("F-value","P-value")
    v2 <- c(q,pvalue)
    t <- data.frame(v1,v2)
    colnames(t) <- c("Statistic","P-Value")
    datatable(t, rownames=FALSE, 
              options = list(dom = 't',ordering=F, columnDefs = list(
                list(className = "dt-center", targets = "_all")
              )))
  })
  
  ### Deter table: ----------------------------------------------------------------
  
  output$deter <- renderDataTable({
    a <- myReg()
    a <- summary(a)
    q <- a$r.squared[1] 
    w <- a$adj.r.squared 
    v1 <- c("R-squared","Adj. R-squared")
    v2 <- c(q,w)
    t <- data.frame(v1,v2)
    colnames(t) <- c("Statistic","P-Value")
    datatable(t, rownames=FALSE, 
              options = list(dom = 't',ordering=F, columnDefs = list(
                list(className = "dt-center", targets = "_all")
              )))
  })
  
  ### Reset table: ----------------------------------------------------------------
  
  output$reset <- renderDataTable({
    a <- myReg()
    m <- a
    a <- resettest(m,power=2,type="regressor")
    b <- resettest(m,power=3,type="regressor")
    c <- resettest(m,power=4,type="regressor")
    a1 <- a$statistic
    a2 <- a$p.value
    b1 <- b$statistic
    b2 <- b$p.value
    c1 <- c$statistic
    c2 <- c$p.value
    stats <- c(a1,b1,c1)
    pval <- c(a2,b2,c2)
    form <- c("2","3","4")
    df <- data.frame(form,stats,pval)
    colnames(df) <- c("Term","Statistic","P-value")
    datatable(df, rownames=FALSE, 
              options = list(dom = 't',ordering=F, columnDefs = list(
                list(className = "dt-center", targets = "_all")
              )))
  })
  
  ### White table: ----------------------------------------------------------------
  
  output$white <- renderDataTable({
    a <- myReg()
    data <- myData()
    u2 <- a$residuals^2
    y <- fitted(a)
    Ru2<- summary(lm(u2 ~ y + I(y^2)))$r.squared
    LM <- nrow(data)*Ru2
    p.value <- 1-pchisq(LM, 2)
    t <- data.frame(LM,p.value)
    colnames(t) <- c("Statistic","P-value")
    datatable(t, rownames=FALSE, 
              options = list(dom = 't',ordering=F, columnDefs = list(
                list(className = "dt-center", targets = "_all")
              )))
  })
  
  ### Pagan table: ----------------------------------------------------------------
  
  output$pagan <- renderDataTable({
    a <- myReg()
    b <- bptest(a)
    a1 <- b$statistic
    a2 <- b$p.value
    t <- data.frame(a1,a2)
    colnames(t) <- c("Statistic","P-value")
    datatable(t, rownames=FALSE, 
              options = list(dom = 't',ordering=F, columnDefs = list(
                list(className = "dt-center", targets = "_all")
              )))
  })
  
  ### Durbin table: ----------------------------------------------------------------
  
  output$durbin <- renderDataTable({
    a <- myReg()
    b <- dwtest(a)
    a1 <- b$statistic
    a2 <- b$p.value
    t <- data.frame(a1,a2)
    colnames(t) <- c("Statistic","P-value")
    datatable(t, rownames=FALSE, 
              options = list(dom = 't',ordering=F, columnDefs = list(
                list(className = "dt-center", targets = "_all")
              )))
  })
  
  ### Graphics: ----------------------------------------------------------------
  
  output$var_plot<-renderUI({
    selectInput("var_plot","Select Dependent Var",choices =as.list(paste0(input$other_var_select)))
  })
  
  output$visreg <- renderPlot({
    a <- myReg()
    b <- input$var_plot
    visreg(a, b, gg=TRUE)
  })
  
  ### VIF graphics: ----------------------------------------------------------------
  
  output$vifs <- renderPlot({
    a <- myReg()
    ids <- unlist(lapply(a$model, is.numeric)) 
    z <- a$model[ , ids]
    x <- z[,-1]
    y <- z[,1]
    b <- mc.plot(x, y)
  })
  
  ### Plots: ----------------------------------------------------------------
  
  output$reg1 <- renderPlot({
    a <- myReg()
    plot(a,which=1,col="gray16")
  })
  output$reg2 <- renderPlot({
    a <- myReg()
    plot(a,which=2,col="gray16")
  })
  output$reg3 <- renderPlot({
    a <- myReg()
    plot(a,which=3,col="gray16")
  })
  output$reg4 <- renderPlot({
    a <- myReg()
    plot(a,which=4,col="gray16")
  })
  output$reg5 <- renderPlot({
    a <- myReg()
    plot(a,which=5,col="gray16")
  })
  output$reg6 <- renderPlot({
    a <- myReg()
    plot(a,which=6,col="gray16")
  })
  
} 
