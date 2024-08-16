library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(metafor)
library(DT)
library(knitr)
library(kableExtra)
library(randomNames)
library(rmarkdown)
library(showtext)


ui <- fluidPage(theme = shinytheme("sandstone"),
                tags$head(includeCSS("www/styles.css"),
                  tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
                  tags$style(HTML("
      .custom-box {
        border: 1px solid #ddd !important;
        box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2) !important;
        border-radius: 5px !important;
        padding: 20px !important;
        padding-bottom: 10px !important;
      }
      .spacer {
        margin-bottom: 20px;
      }
      a {color: #00ba6f}
  .delete-btn {
  background-color: transparent;
  border: none;
  color: black;
  font-size: 12px; 
  padding: 0; 
  width: 20px; 
  height: 20px; 
  cursor: pointer;
  vertical-align: middle; 
}
.delete-btn:hover {
  color: white;
}
.kable-table {
  font-family: Arial, sans-serif !important;
  font-size: 12px; 
} 
    #tab2_content {
      background-image: url('g6890-3.svg');
      background-position: bottom center;
      background-repeat: no-repeat;
      background-size: cover;
      background-attachment: fixed;
      height: 100vh;  
      padding: 20px;
    }
    .plot-border {
      border: 2px solid #7998a3; 
      padding: 10px; 
      margin-bottom: 20px; 
    }
input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    "))
                
                ),
                
                fluidRow(
                  column(
                    width = 8,
                    h2("MetaAnalyze"),
                    p("The teaching tool to easily perform a meta-analysis"),
                  ),
                  column(
                    width = 4,
                    a(href = "https://stride-lab.pages.uzh.ch/website/", target = "_blank",img(src = "stride_logo.png", height = "50px", width = "200px")),
                    a(href = "https://www.uzh.ch/cmsssl/en.html/", target = "_blank",img(src = "uzh_logo.png", height = "88px", width = "190px"))# https://www.uzh.ch/cmsssl/en.html
                  )),
                  
                
                tabsetPanel(
                  tabPanel(
                    "Welcome",
                    div(id = "tab2_content",
                    fluidRow(
                      column(1
                    ),
                    column(9,br(), h3(strong("Welcome to MetaAnalyze!")),
                           p("Welcome to MetaAnalyze, a didactive tool designed to simplify the process of conducting meta-analyses and visualizing results through forest plots. 
                             Whether you're new to meta-analysis or just looking for a straightforward way to analyze your data, this app has you covered!"),
                           p(strong("What Can You Do Here?")),
                           HTML("<ul>
             <li><strong>Enter Your Data:</strong> Easily input your study results directly into the app.</li>
             <li><strong>Perform Meta-Analysis:</strong> Automatically calculate summary statistics and conduct a meta-analysis with minimal effort.</li>
             <li><strong>Generate Forest Plots:</strong> Visualize your results through clear and informative forest plots.</li>
           </ul>"),
                           p("Our goal is to provide a user-friendly experience that enables you to focus on learning the basic concepts of a meta-analysis without needing to install additional software or write any code."),
                           p(strong("Acknowledgments")),
                           p("This tool was conceptualized and created by the",a("STRIDE-Lab",href="https://stride-lab.pages.uzh.ch/website/"),
                             " and designed by",a("Marianna Rosso.",href="https://stride-lab.pages.uzh.ch/website/people/rosso-marianna/"), 
                             br(), "We appreciate your use of MetaAnalyze and hope it enhances your learning process.

Feel free to explore and get started with your meta-analysis!"))
                    ))
                  ),
                  
                  tabPanel("Effect Size",
                           fluidRow(
                             column(2, box(title = "Study", status = "primary", solidHeader = TRUE, width = 12, class = "custom-box",style = "height: 180px; background-color: #dde4e7;",
                                           textInput("study_id", "Paper ID",value=
                                                       paste0(
                                                       sample(randomNames(1,which.names="last",ethnicity=c("African American", "Hispanic", "White")),1),
                                                       sample(1980:2024, 1))),
                                           tags$p(HTML("Unique ID for the study paper, this will be displayed in the forest plot"), 
                                                  style = "font-size: 10px; font-family: Arial, sans-serif;")
                             )),
                             column(5, box(title = "Control Group", status = "primary", solidHeader = TRUE, width = 12, class = "custom-box",style = "height: 180px; background-color: #bbcad0;",
                                           fluidRow(
                                             column(3, numericInput("mean_c", HTML("x̄"), value =  round(rnorm(1,50,15),1)),
                                                    tags$p(HTML("Unit-less mean value for the control group"), 
                                                           style = "font-size: 10px; font-family: Arial, sans-serif;")
                                                    ),
                                             column(3, numericInput("n_c", "n",value= sample(5:25, 1)),
                                             tags$p(HTML("Sample size for the control group"), 
                                                    style = "font-size: 10px; font-family: Arial, sans-serif;")
                                                    ),
                                             column(3, numericInput("sem_c", "SEM", value =  round(rnorm(1,5,2),1)),
                                             tags$p(HTML("Standard error of the mean (if available) for the control group"), 
                                                    style = "font-size: 10px; font-family: Arial, sans-serif;")
                                                    ),
                                             column(3, numericInput("sd_c", "SD", value =  0),
                                             tags$p(HTML("Standard deviation of the mean (if available) for the control group"), 
                                                    style = "font-size: 10px; font-family: Arial, sans-serif;"))
                                           )
                             )),
                             column(5, box(title = "Treatment Group", status = "primary", solidHeader = TRUE, width = 12, class = "custom-box",style = "height: 180px; background-color: #99b0b9;",
                                           fluidRow(
                                             column(3, numericInput("mean_t", HTML("x̄"), value =  round(rnorm(1,100,15),1)),
                                             tags$p(HTML("Unit-less mean value for the treatment group"), 
                                                    style = "font-size: 10px; font-family: Arial, sans-serif;")),
                                             column(3, numericInput("n_t", "n", value= sample(5:25, 1)),
                                             tags$p(HTML("Sample size for the treatment group"), 
                                                    style = "font-size: 10px; font-family: Arial, sans-serif;")
                                                    ),
                                             column(3, numericInput("sem_t", "SEM", value =  round(rnorm(1,9,2),1)),
                                             tags$p(HTML("Standard error of the mean (if available) for the treatment group"), 
                                                    style = "font-size: 10px; font-family: Arial, sans-serif;")),
                                             column(3, numericInput("sd_t", "SD", value = 0),
                                             tags$p(HTML("Standard deviation of the mean (if available) for the treatment group"), 
                                                    style = "font-size: 10px; font-family: Arial, sans-serif;"))
                                           )
                             ))
                           ),
                           fluidRow(
                             column(7,
                                    div(class = "spacer"),
                                    fluidRow(column(7, actionButton("add_row", "Add Row",style = "background-color: #7998a3; color: white; border: none;"), offset = 9.5)),
                                    div(class = "spacer"),
                                    fluidRow(box(status = "primary", solidHeader = TRUE, width = 12, class = "custom-box",
                                                 htmlOutput("data_table"),
                                                 actionButton("update_sd", "Update",style = "background-color: #587f8d; color: white; border: none; padding: 10px"),
                                                 downloadButton("download_table", "Download CSV",style = "background-color: #48423e; color: white; border: none; padding: 10px;"),
                                                 downloadButton("download_plot", "Download Plot",style = "background-color: #48423e; color: white; border: none; padding: 10px;")
                                    ))),
                             column(5, box(title = "Forest Plot", status = "primary", solidHeader = TRUE, width = 12, class = "custom-box",style = "height: 350px;",
                                           
                                           plotOutput("forest_plot", height = "300px")))
                           )
                  ),
                  tabPanel(
                    "Summary",
                    column(1),
                    column(5,br(),div(class = "plot-border", plotOutput("forest_plot2"))),
                    column(1),
                    column(5,br(),
                           htmlOutput("metaanalysis_summary"))
                    
                  ),
                  tabPanel("Sources",
                           htmlOutput("text_and_code")
                           ))
)

server <- function(input, output, session) {
  # Initial empty data frame
  data <- reactiveVal(data.frame(
    StudyID = character(),
    MeanControl = numeric(),
    SampleSizeControl = numeric(),
    SEMControl = numeric(),
    SDControl = numeric(),
    MeanTreatment = numeric(),
    SampleSizeTreatment = numeric(),
    SEMTreatment = numeric(),
    SDTreatment = numeric(),
    yi = numeric(),  # Effect size
    vi = numeric(),  # Variance of effect size
    stringsAsFactors = FALSE
  ))
  
  # Observe add_row button click event
  observeEvent(input$add_row, {
    # Adding the current row data to the reactive data object
    new_row <- data.frame(
      StudyID = input$study_id,
      MeanControl = input$mean_c,
      SampleSizeControl = input$n_c,
      SEMControl = input$sem_c,
      SDControl = input$sd_c,
      MeanTreatment = input$mean_t,
      SampleSizeTreatment = input$n_t,
      SEMTreatment = input$sem_t,
      SDTreatment = input$sd_t,
      yi = NA,
      vi = NA,
      stringsAsFactors = FALSE
    )
    data(rbind(data(), new_row))
    
    # Generate new example data
    new_examples <- list(
      mean_c = round(rnorm(1, 50, 15), 1),
      n_c = sample(5:25, 1),
      sem_c = round(rnorm(1, 5, 2), 1),
      sd_c = 0,
      mean_t = round(rnorm(1, 100, 15), 1),
      n_t = sample(5:25, 1),
      sem_t = round(rnorm(1,8, 2), 1),
      sd_t = 0
    )
    
    # Reset the input fields to new example data
    updateNumericInput(session, "mean_c", value = new_examples$mean_c)
    updateNumericInput(session, "n_c", value = new_examples$n_c)
    updateNumericInput(session, "sem_c", value = new_examples$sem_c)
    updateNumericInput(session, "sd_c", value = new_examples$sd_c)
    updateNumericInput(session, "mean_t", value = new_examples$mean_t)
    updateNumericInput(session, "n_t", value = new_examples$n_t)
    updateNumericInput(session, "sem_t", value = new_examples$sem_t)
    updateNumericInput(session, "sd_t", value = new_examples$sd_t)
    
    # Optionally, you can also reset the study ID input if needed
    updateTextInput(session, "study_id",value=
                      paste0(
                        sample(randomNames(1,which.names="last",ethnicity=c("African American", "Hispanic", "White")),1),
                        sample(1980:2024, 1)))
  })
  
  
  # Update SD and calculate effect size (yi) and variance (vi)
  observeEvent(input$update_sd, {
    updated_data <- data() %>%
      mutate(
        SDControl = ifelse(SDControl == 0 & SEMControl > 0 & SampleSizeControl > 0,
                           round(SEMControl * sqrt(SampleSizeControl), 2), SDControl),
        SDTreatment = ifelse(SDTreatment == 0 & SEMTreatment > 0 & SampleSizeTreatment > 0,
                             round(SEMTreatment * sqrt(SampleSizeTreatment), 2), SDTreatment)
      ) 
    
    # Calculate effect size (yi) and variance (vi) using escalc from metafor
    escalc_results <- escalc(
      measure = "SMD", 
      m1i = updated_data$MeanTreatment, 
      sd1i = updated_data$SDTreatment, 
      n1i = updated_data$SampleSizeTreatment, 
      m2i = updated_data$MeanControl, 
      sd2i = updated_data$SDControl, 
      n2i = updated_data$SampleSizeControl,
      slab = updated_data$StudyID,
      data = updated_data
    )
    
    updated_data$yi <- round(escalc_results$yi, 2)
    updated_data$vi <- round(escalc_results$vi, 2)
    
    data(updated_data)
  })
  
  # Render the data table
  output$data_table <- renderUI({
    df_t <- data()
    
    if (nrow(df_t) == 0) {
      return(NULL)
    }
    
    # Add a delete button for each row
    df_t$Delete <- sapply(seq_len(nrow(df_t)), function(i) {
      as.character(actionButton(inputId = paste0("delete_", i), label = icon("trash"), 
                                class = "delete-btn", 
                                onclick = sprintf('Shiny.onInputChange("%s", "%s")', "deletePressed", i)))
    })
    
    HTML(
      df_t %>%
        kbl(escape = FALSE, col.names = c("Study ID", "x̄", "n", "SEM", "SD",
                                          "x̄", "n", "SEM", "SD", "ES", "Var", ""),
            table.attr = 'class="kable-table"') %>% 
      row_spec(seq(1,nrow(df_t),2), background="#ebf0f1") %>%
        kable_styling("striped", full_width = TRUE) %>%
        add_header_above(c(" ", "Control" = 4, "Treatment" = 4, " ", " ","")) %>% 
        column_spec(column = 12, width = "0.5in")%>%
        scroll_box(height = "250px")
    )
  })
  
  
  output$download_table <- downloadHandler(
    filename = function() {
      paste("my_table", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  # Observe delete button clicks
  observeEvent(input$deletePressed, {
    deleteIndex <- as.numeric(input$deletePressed)
    currentData <- data()
    if (!is.na(deleteIndex) && deleteIndex > 0 && deleteIndex <= nrow(currentData)) {
      newData <- currentData[-deleteIndex, ]
      data(newData)
    }
  })
  
  output$forest_plot <- renderPlot({
    req(nrow(data()) > 0)  # Ensure there is data to plot
    
    df <- data()
    
    # Perform a check for sufficient data for the rma function to avoid errors
    if (nrow(df) > 0 && sum(!is.na(df$yi)) > 0) {
      res <- rma(yi, vi, data = df)
      forest(res,header = "Author(s), Year", xlab="Correlation coefficient")
      text(-16, -1, pos=4, cex=0.75, bquote(paste(
        "RE Model (Q = ", .(round(res$QE, digits=2)),
        ", df = ", .(res$k - res$p), ", ",
        .(round(res$QEp, digits=3)), "; ",
        I^2, " = ", .(round(res$I2, digits=1)), "%)")))
    } else {
      plot.new()  
    }
  })
  
  
  output$forest_plot2 <- renderPlot({
    req(nrow(data()) > 0)  # Ensure there is data to plot
    
    df <- data()
    
    # Perform a check for sufficient data for the rma function to avoid errors
    if (nrow(df) > 0 && sum(!is.na(df$yi)) > 0) {
      res <- rma(yi, vi, data = df)
      forest(res,header = "Author(s), Year", xlab="Correlation coefficient")
      text(-16, -1, pos=4, cex=0.75, bquote(paste(
        "RE Model (Q = ", .(round(res$QE, digits=2)),
        ", df = ", .(res$k - res$p), ", ",
        .(round(res$QEp, digits=3)), "; ",
        I^2, " = ", .(round(res$I2, digits=1)), "%)")))
    } else {
      plot.new()  
    }
  })
  
  
  output$metaanalysis_summary <- renderUI({
    
    df <- data()
    res_meta <- rma(yi, vi, data = df)
    
    metafor_summary_fun<-function(res){
    
    logLik <- res$fit.stats$REML[1]
    deviance <- res$fit.stats$REML[2]
    aic <- res$fit.stats$REML[3]
    bic <- res$fit.stats$REML[4]
    aicc <- res$fit.stats$REML[5]
    tau2 <- res$tau2
    tau <- sqrt(tau2)
    i2 <- res$I2
    h2 <- 1 / (1 - res$I2 / 100)
    qtest <- res$QE
    qdf <- res$k - 1
    q_pval <- res$QEp
    if(q_pval<.0001){
      q_pval<-"<.0001"
    } else {q_pval<-round(q_pval,4)}
    coef <- res$b
    se <- res$se
    zval <- coef / se
    pval <- 2 * pnorm(-abs(zval))
    if(pval<.0001){
      pval<-"<.0001"
    } else {pval<-round(pval,4)}
    ci.ub <- res$ci.ub
    ci.lb <- res$ci.lb
    
    HTML(
      
      paste(
        "Random-Effects Model (k =", res$k, "; tau^2 estimator: REML)","<br/>","<br/>",
       strong( "Model Fit Statistics:"),"<br/>","<br/>",
        
        "logLik:",round(logLik,4),"<br/>",
            "Deviance:",round(deviance,4),"<br/>",
            "AIC:",round(aic,4),"<br/>",
           "BIC:",round(bic,4),"<br/>",
           "AICC:",round(aicc,4),"<br/>","<br/>",
       "tau^2 (estimated amount of total heterogeneity):",round(tau2,4),"(SE =",round(res$se.tau,4),")","<br/>",
       "tau (square root of estimated tau^2 value):",round(tau,4),"<br/>",
       "I^2 (total heterogeneity / total variability):",round(i2,4),"<br/>",
       "H^2 (total variability / sampling variability):",round(h2,4),"<br/>","<br/>",
       
       strong("Test for Heterogeneity:"),"<br/>","<br/>",
       "Q (df =", qdf,") =",round(qtest,4),"<br/>","p-val =", q_pval,"<br/>","<br/>",
       strong("Model Results:"),"<br/>","<br/>",


   "estimate: ",round(coef,4),"<br/>",
    "se: ",round(se,4),"<br/>",
    "zval: ",round(zval,4),"<br/>",
    "pval: ",pval,"<br/>",
   "ci.lb: ",round(ci.lb,4),"<br/>",
    "ci.ub: ",round(ci.ub,4)
      ))
    }
    
    metafor_summary_fun(res_meta)
  })
  
  output$text_and_code <- renderUI({
    HTML(paste0("</br>",
                "<p>This app was generated entirely using R. The code used to generate this app is published ",
                "<a href='https://github.com/marianna-rosso-uzh/MetaAnalyser' target='_blank'>here</a>.",
                "</p>",
      "<p>The meta-analytic model calculates <i>hedges' g</i> as estimate of effect size through the use of the ",
      "<a href='https://www.metafor-project.org/doku.php/metafor' target='_blank'>metafor package</a>.",
      "</p>",
      "<p>Briefly, a dataframe is generated using the values of mean, samplze size, and standard deviation,
      for both the treatment and control group. Next, the effect size and variance are calculated as such:</p>",
      "<pre><code>", "metafor::escalc(
      measure = SMD, 
      m1i = MeanTreatment, 
      sd1i = SDTreatment, 
      n1i = SampleSizeTreatment, 
      m2i = MeanControl, 
      sd2i = SDControl, 
      n2i = SampleSizeControl,
      slab = StudyID,
      data = data
    )", "</code></pre>",
      "<p>The meta-analysis is then performed:</p>",
      "<pre><code>", "res_meta <- rma(yi, vi, data = data)", "</code></pre>",
      "<p>Where <italic>yi</italic> and <italic>vi</italic> are calculated with the metafor::escalc function.</p>",
      "<p>Finally, a forest plot is generated:</p>",
      "<pre><code>", "forest(res)", "</code></pre>"
    ))
  })

}

shinyApp(ui = ui, server = server)

#trying to push