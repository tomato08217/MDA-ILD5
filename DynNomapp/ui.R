ui = bootstrapPage(fluidPage(
      titlePanel('Dynamic Nomogram'),
           sidebarLayout(sidebarPanel(
           textInput("txt1", "wavelet.LLL_glcm_MCC", 0),
           textInput("txt2", "wavelet.LLH_glcm_Idmn", 0),
           textInput("txt3", "wavelet.HLL_glszm_LargeAreaHighGrayLevelEmphasis", 0),
           textInput("txt4", "original_shape_Flatness", 0),
           textInput("txt5", "wavelet.LLL_firstorder_Skewness", 0),
           verbatimTextOutput("radscore"),
          
           textInput("rad_score", "rad_score", "0"),
           sliderInput("age","age",min = min(data$age),max = max(data$age),value = median(data$age),step = 1),
           selectInput("FVC_rate","FVC_rate",choices = c(0,1,2),selected = 0),
           checkboxInput("times", "Predicted Survival at this Follow Up:"),
           conditionalPanel(condition = "input.times == true",
                            sliderInput("Survival", "Survival", min = min(data$survival.time), 
                                        max = max(data$survival.time), value = median(data$survival.time))),
           checkboxInput('trans', 'Alpha blending (transparency)', value = TRUE),
           actionButton('add', 'Predict'),
           br(), br(),
           helpText('Press Quit to exit the application'),
           actionButton('quit', 'Quit')
           ),
           mainPanel(tabsetPanel(id = 'tabs',
           tabPanel('Survival plot', plotOutput('plot')),
           tabPanel('Predicted Survival', plotlyOutput('plot2')),
           tabPanel('Numerical Summary', verbatimTextOutput('data.pred')),
           tabPanel('Model Summary', verbatimTextOutput('summary'))
           )
           )
           )))
