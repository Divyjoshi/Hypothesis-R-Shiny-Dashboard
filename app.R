library(shiny)
library(ggplot2)
library(ggpubr)
library(reshape)
library(gridExtra)
library(ggforce)
library(ggsignif)
library(dplyr)
library(shinythemes)
library(patchwork)
library(Rmisc)
library(scales)


ui <- fluidPage(
  tags$style(type='text/css', ".selectize-dropdown { font-size: 12px; line-height:12px; }"),
  theme = shinytheme("cyborg"),
  # App Title
  titlePanel("Hypothesis Tunnel"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      helpText(strong("Upload your csv file here:")),
      fileInput(
        "file1",
        "Choose CSV File",
        multiple = TRUE,
        accept = c(".csv")
      ),
      tags$hr(),
      helpText(strong("Or manually enter your data below")),
      br(),
      br(),
      
      
      selectInput(
        "select",
        label = "Choose a test to perform",
        choices = list(
          "Correlation" = 1,
          "Paired Samples t-test" = 2,
          "Descriptive Statistics" = 3,
          "Test of Normal Distribution" = 4
        ),
        selected = FALSE
      ),
      conditionalPanel(
        condition = "input.select == 2",
        selectInput(
          "method",
          label = "Choose a testing method",
          choices = list(
            "Parametric" = 1,
            "Non-parametric" = 2
          ),
          selected = FALSE
        ),
        sliderInput(
          "slider1",
          h5("Alpha value"),
          min = 0,
          max = 1,
          value = 0.5,
          step = 0.01
        ),
        sliderInput(
          "slider2",
          h5("Confidence interval %"),
          min = 0,
          max = 100,
          step = 1,
          value = 95
        )
      ),
      conditionalPanel(
        condition = "input.select == 1",
        selectInput(
          "method2",
          label = "Choose a correlation test",
          choices = list(
            "Pearson correction formula" = 1,
            "Spearman correction formula" = 2,
            "Kendall correction formula" = 3),
          selected = 1
          )
        ),
      conditionalPanel(
        condition = "input.select == 1",
        sliderInput(
          "slider3",
          label = "Zoom out",
          min = 0,
          max = 50,
          step = 5,
          value = 20
        
        )
      )
      ),
      
    
    

  
  # Input: two lists of numbers
  
  mainPanel(plotOutput("contents"),
            textOutput("resultText"))
))

server <-  function(input, output) {
  dataFileCorrelation <- data.frame(age = c(sample(18:70,200, replace=T)),
                                    earnings = c(sample(10000:60000, 200, replace=T))) # random data to use for correlation
  
  dataFileTtest <- data.frame(Test_Scores_Day = c(sample(150:250,200, replace=T)),
                              Test_Scores_Night = c(sample(125:225,200, replace=T))) # random data to use for t test
  output$contents <- renderPlot({
    

    
    
    req(input$slider1) # also confirm that the slider is working as intended
    if (is.null(input$file1$datapath)){
      if (input$select == 1){
      x <- dataFileCorrelation
      } else {x <- dataFileTtest}
    } else{
    x <- read.csv(input$file1$datapath) # read the csv file
    }
    data <- melt(x)
    p <- as.character(unique(data$variable)) # find the unique string names of the columns
    x <- as_tibble(x) # make the data cleaner
    x1 <- ls(x)[[1]] # find the variable name of x1
    x2 <- ls(x)[[2]] # find the variable name of x2
    if (input$select == 1) {

      
      if(input$method2 == 1){
        methodR <- "pearson"
      } else if (input$method2 == 2){
        methodR <- "spearman"
      } else if (input$method2 == 3){
        methodR <- "kendall"
      } # control for which test we want to perform based on the input selection "method"
      
      fig1 <- ggscatter(data=x,x=x1,y=x2,color = factor(x1),
                add = "reg.line",
                conf.int = TRUE,
                palette = "jco",
                cor.coef = TRUE,
                cor.method = methodR,
                cor.coeff.args = list(label.y = max(x[2]) + max(x[2])/100*5),
                title = "Scatter plot of the two variables with regression line") +
        theme(legend.position = "none")#  scatter plot with regression line
      
      
      d <- dist(scale(x),method="euclidean",diag=TRUE,upper=TRUE) # creating a hierarchical clustering model
      hls <- hclust(d,method="complete") # compiling the clusters
      
      cluster <- cutree(hls,4) # distributing to 4 bins
      
      ggData <- cbind(x,cluster) # combining the data
      ggData$cluster <- as.factor(ggData$cluster)
      borderX <- max(x[1])/100*input$slider3
      borderY <- max(x[2])/100*input$slider3
      
      fig2 <- ggplot(ggData, aes_string(x=(p[1]),y=(p[2]),color=factor(cluster)))+
        geom_point(size=2.3) +
        ylim(min(x[2]) - borderY, max(x[2]) + borderY) +
        xlim(min(x[1]) - borderX, max(x[1]) + borderX) +
      geom_mark_circle(aes(color = factor(cluster),fill = cluster)) +
        ggtitle("Hierarchical clustering model") +
        theme_classic() +
        theme(legend.position = "none")

      figure <- grid.arrange(
        fig1,
        fig2,
        ncol = 2)
      
    } # contents here creates a scatter plot and a hierarchical clustering
    else if (input$select == 2) {
      fig1 <- ggplot(data = data,
               aes(variable, value, fill = variable, color = variable)) +
        geom_boxplot(
          alpha = 0.5,
          lwd = 1.1,
          outlier.colour = "black",
          outlier.size = 5,
          outlier.shape = 7
        ) +
        geom_jitter(stroke = 1.2)+
        scale_fill_brewer(palette="Blues") +
        ggtitle("Boxplot of the two variables")
        
      
      p1 <- subset(data, variable == p[1])
      p1 <- mean(p1$value) # find the mean values of each column
      p2 <- subset(data, variable == p[2])
      p2 <- mean(p2$value)
      pDistance <- max(p1, p2) + (max(p1, p2) / 10) # calculate the y_position for the significance test by taking the highest mean and adding 10% height.
      
      tdc <- summarySE(data,measurevar="value", groupvars = c("variable"))
      
      fig2 <- ggplot(data, aes(variable, value, fill = variable)) +
        geom_bar(position = "dodge",
                 stat = "summary",
                 fun = "mean") +
        geom_errorbar(data=tdc,aes(ymin=value-se,ymax=value+se),
                      width=.2,
                      position=position_dodge(.9))+
        geom_signif(
          data = data,
          comparisons = split(t(combn(levels(data$variable), 2)), seq(nrow(t(combn(levels(data$variable), 2))))), # allow for all comparisons
          map_signif_level = TRUE,
          test = "t.test",
          y_position = pDistance,
          size = 1,
          textsize = 5
        ) +
        scale_fill_brewer(palette="Blues") +
        ggtitle("Bar chart of variable means with standard error")
      
      
      figure <- grid.arrange(
        fig1,
        fig2,
        ncol = 2
      )
      
      return(figure)
    } # contents here box plot and bar chart and significance
    else if (input$select == 3) {
      exclude <- "N/A"
      
      resultTable1 <- subset(data, variable == p[1])
      resultTable1 <- data.frame(summary(resultTable1))
      resultTable1 <- data.frame(subset(resultTable1, !is.na(Freq)))
      resultTable1 <- data.frame(strsplit(resultTable1$Freq, split = ":"))
      colnames(resultTable1) <- c("Frequency","N/A","Lowest value","25% percentile","Median","Mean","75% percentile","Highest value")
      
      resultTable1 <- resultTable1[2,]
      
      resultTable2 <- subset(data, variable == p[2])
      resultTable2 <- data.frame(summary(resultTable2))
      resultTable2 <- data.frame(subset(resultTable2, !is.na(Freq)))
      resultTable2 <- data.frame(strsplit(resultTable2$Freq, split = ":"))
      colnames(resultTable2) <- c("N/A","Frequency","Lowest value","25% percentile","Median","Mean","75% percentile","Highest value")
      
      resultTable2 <- resultTable2[2,]
      
      resultTable1 <- resultTable1 %>% select(-matches("N/A"))
      resultTable2 <- resultTable2 %>% select(-matches("N/A"))
      
      resultTable <- rbind(resultTable1,resultTable2)
      rownames(resultTable) <- c(p[1],p[2])
      
      resultTable <- ggtexttable(resultTable)
      
      return(resultTable)
      
      
      
    } # contents table of the descriptive statistics transformed into a plot
    else if (input$select == 4) {
      fig1 <-
        ggplot(data = data, aes(
          x = value,
          fill = variable,
          colour = variable
        )) +
        geom_histogram(
          aes(y = ..density..),
          binwidth = 10,
          alpha = 0.5,
          position = "identity"
        )  +
        scale_fill_brewer(palette="Blues") +
        ggtitle("Histogram of data distribution")# create the first figure - a histogram of the datasets
      
      
      for (i in length(p)) {
        df <- data %>% filter(variable == p[i])
        fig1 <- fig1 + geom_density(alpha = 0.3) +
          stat_function(
            data = df,
            fun = dnorm,
            args = list(
              mean = mean(data$value),
              sd = sd(data$value)
            )
          ) + scale_fill_brewer(palette="Blues") +
          ggtitle("Quantile-Quantile probability plot")
        
      }# this creates individual density plots that overlap the data iteratively
      
      
      fig2 <- ggqqplot(data, x = "value",
                       color = "variable",
                       palette = "Blues")

      
      figure <- grid.arrange(
        fig1,
        fig2,
        ncol = 1,
        nrow = 2
      ) # this creates the output - we are 1 column and 2 rows to represent the data

      return(figure)
      
    } # contents here we show a histogram and a quantile-quantile function
  })
  
  output$resultText <- renderText({
    if (is.null(input$file1$datapath)){
      if (input$select == 1){
        x <- dataFileCorrelation
      } else {x <- dataFileTtest}
    } else{
      x <- read.csv(input$file1$datapath) # read the csv file
    }
    data <- melt(x) # melt into wide format
    p <- unique(data$variable) # find the unique variable names
    
    if (input$select == 1) {

      
      if(input$method2 == 1){
        
        res <- cor.test(as.numeric(unlist(x[1])),as.numeric(unlist(x[2])),
                        method = "pearson")# run the correlation, as.numeric(unlist) is how we are converting the data type to array
      } else if(input$method2 == 2){
        
        res <- cor.test(as.numeric(unlist(x[1])),as.numeric(unlist(x[2])),
                        method = "spearman")
      } else if(input$method2 == 3){
        
        res <- cor.test(as.numeric(unlist(x[1])),as.numeric(unlist(x[2])),
                        method = "kendall")
      }
      

      
      
      if(res$estimate >= 0){
        corDirection = "positive"
      } else{
        corDirection = "negative"
      }
      
      textR <- sprintf("A %s was computed to assess the linear relationship between %s and %s \n There was a %s correlation between the two varaiables,
                       r(%s) = %s, %s).",res$method,p[1],p[2],corDirection,nrow(x)-1,round(res$estimate,3),round(res$p.value,4))
      
      
    } # output text of the correlation
    else if (input$select == 2) {
      

      if(input$method == 1){
      
      ttestResult <- t.test(value ~ variable,data = data,
                            paired = TRUE,
                            alternative = "two.sided",
                            mu = input$slider1,
                            conf.level = (input$slider2/100)) #  run a t-test with values specified by the user
      test <- "t"
      } else {
        ttestResult <- wilcox.test(value ~ variable,data = data,
                                   paired = TRUE,
                                   alternative = "two.sided",
                                   mu = input$slider1,
                                   conf.level = input$slider2)
        test <- "Z"
      } # run a wilcoxon ranked test if they select non-parametric
      
      if(ttestResult$p.value <= 0.001){
      textR <-  sprintf("Results of the %s demonstrate that the measured value %s was significantly different from the value %s (%s(%s) = %s, p < 0.001)",
                        ttestResult$method,
                        p[1],p[2],
                        test,
                        nrow(x)-1,
                        round(ttestResult$statistic,4)) # if the result is significant
      
      } else if(ttestResult$p.value < 0.05){
      textR <-  sprintf("Results of the %s demonstrate that the measured value %s was significantly different from the value %s (%s(%s) = %s, p = %#.3f)",
                        ttestResult$method,
                        p[1],p[2],
                        test,
                        nrow(x)-1,
                        round(ttestResult$statistic,4),
                        ttestResult$p.value) # if the result is very significant
      
      } else if(ttestResult$p.value >= 0.05){
      textR <-  sprintf("Results of the %s demonstrate that the measured value %s was not significantly different from the value %s (%s(%s) = %s, p = %s)",
                        ttestResult$method,
                        p[1],p[2],
                        test,
                        nrow(x)-1,
                        round(ttestResult$statistic,4),
                        round(ttestResult$p.value,4)) # if the result is not significant
      }
      
    } # output text of the significance test
    else if (input$select == 3) {
      textR <- "A table of the descriptive statistics"
    } # output text for the descriptive statistics
    else if (input$select == 4) {
      

    normality <- shapiro.test(data$value) # the shapiro test for normal distribution
    
    if (normality$p.value <= 0.001) {
      normality$p.value = "< 0.001"
      normality$result = "the data are non-parametric and do not meet the standards of being normally distributed"
      
    } else if (normality$p.value >= 0.05) {
      normality$result = "the data are parametric and meet the standards of being normally distributed"
    }
    
    textR <- sprintf("The results of the %s found a W score = %s with a p-value of %s \n therefore %s",
                     normality$method,
                     as.character(round(normality$statistic[[1]], digits = 4)),
                     as.character(normality$p.value),
                     normality$result)
    
    } # output text of the normal distribution plot
    
    
    
    
  }) # text here is the result of shapiro.test of normal distribution
  
}


shinyApp(ui = ui, server = server)



