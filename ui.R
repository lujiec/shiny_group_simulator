library(shiny)




# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title.
  titlePanel("Trajectory Group vs AUC simulation"),
  
  sidebarLayout(
    sidebarPanel(
      
      h5("GP parameters"),
      
      sliderInput("gp_sigma_factor", "GP Sigma", 
                  min = -10, max = 10, value = 0, step= 0.1),
      
      sliderInput("gp_L_factor", "GP L", 
                  min = -10, max = 10, value = 0, step= 0.1),
            
      
      h5("Group Distribution"),
      
      sliderInput("grp1", "Group1", 
                  min = 0, max = 100, value = 100, step= 1),
      sliderInput("grp2", "Group2", 
                  min = 0, max = 100, value = 100, step= 1),
      sliderInput("grp3", "Group3", 
                  min = 0, max = 100, value = 100, step= 1),
      sliderInput("grp4", "Group4", 
                  min = 0, max = 100, value = 100, step= 1),
      
      h5("Percentage of Positive"),
      sliderInput("pctg1", "pctg1", 
                  min = 0, max = 1, value = 1, step= 0.1),
      sliderInput("pctg2", "pctg2", 
                  min = 0, max = 1, value = 1, step= 0.1),
      sliderInput("pctg3", "pctg3", 
                  min = 0, max = 1, value = 1, step= 0.1),
      sliderInput("pctg4", "pctg4", 
                  min = 0, max = 1, value = 0, step= 0.1),
      
      h5("Show/hide plots"),
      checkboxInput('rawplot', 'Simulated traj groups', value=TRUE),
      checkboxInput('biplot', 'group by label plot', value=TRUE),
      checkboxInput('aucplot', 'AUC', value=TRUE)     
  
    ),
    

    mainPanel(
      
      #verbatimTextOutput("summary"),
      
      
      conditionalPanel(
        condition = "input.rawplot == true",
        h4("Simulated trajectory groups"),
        plotOutput("traj_plot")
      ),
     
      conditionalPanel(
        condition = "input.biplot == true",
        h4("group by labels"),
        plotOutput("pred_plot")
      ),
      
      conditionalPanel(
        condition = "input.aucplot == true",          
        h4("AUC"),
        plotOutput("auc_plot")
      )
    )
  )
))

