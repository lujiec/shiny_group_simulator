library(shiny)

source('global_functions.R')

import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE, stringsAsFactors=FALSE))
}

order <- 3
numT <- 30
num_group <- 4

#betas 
beta.df <- import.csv('beta.csv')
beta.matrix <- t(as.matrix(beta.df[,1:(order+1)]))

#mu matrix - mean functions
X <- 1:numT
T_mat <- matrix(nrow=length(X),ncol=order+1)
for (kk in 1:(order+1)){
  T_mat[,kk] <- X^(kk-1)
}
mu_matrix <- T_mat %*% beta.matrix  # compute only once for each model

gp_sigma0 <- 0.005
gp_L0 <- 5

gp_sigma <- 0.005
gp_L <- 5
grp.count <- c(100,100,100,100)
pos.pctg <- c(1,0,1,1)


shinyServer(function(input, output, session) {
  
  
  get_data <- reactive({
    
    gp_sigma <- gp_sigma0* (2^input$gp_sigma_factor)
    gp_L <- gp_L0* (2^ input$gp_L_factor)
    grp.count <- c(input$grp1, input$grp2, input$grp3, input$grp4)
    pos.pctg <- c(input$pctg1, input$pctg2, input$pctg3, input$pctg4)
    
    
    sigma <- calcSigma(X,X, gp_sigma, gp_L)
    sample.df <- sample_from_gp(X, mu_matrix, grp.count, sigma)
    sample.df.mean <- ddply(sample.df, .(grp, tvalue), summarize, mean=mean(ovalue))
    sample.df2 <- get_sample_with_label(sample.df, pos.pctg)
    auc.df <- get_auc(sample.df2)
    list(sample.df=sample.df, sample.df.mean=sample.df.mean, auc.df=auc.df, sample.df2=sample.df2)
   #list(gp_sigma=gp_sigma, gp_L=gp_L)

  })
  
  
  output$summary <- renderPrint({
    dataset <- get_data()
    print(nrow(dataset$sample.df))
  })
  
    
  output$traj_plot <- renderPlot({
   make_plot1(get_data()$sample.df, get_data()$sample.df.mean)  
  })
  
  output$pred_plot <- renderPlot({    
    make_plot3(get_data()$sample.df2)    
  })
  
  output$auc_plot <- renderPlot({    
   make_plot2(get_data()$auc.df)      
  })
  
  
  
})