#' Title Prints a clusterised plot of the data
#'
#' @param abiotics_df The dataframe with all abiotic data rasterized
#'
#'@importFrom Hmisc varclus
#'@import shiny
#' @return Returns nothing
#' @export
#'
#' @examples
clustering <- function(abiotics_df) {
  method_names <- c("single", "complete", "average")
  variable_names <- c("annual_meanT", "T_seasonality", "maxT_WM", "minT_CM", "annual_rangeT", "meanT_DQ", "annual_prec", "prec_WM", "prec_DM", "prec_seasonality", "ph_max", "elevation_avg", "slope_avg", "flow_df_av", "flow_df_mi", "flow_df_ma", "srad", "vapr", "dl_annual_min", "dl_annual_max", "dl_annual_range")
  variable_names_display<- c("annual mean Temperature", "Temperature seasonality", "maximum Temperature of the Warmest month","minimum Temperature of the Coldest month","Temperature_annual_range","mean Temperature of the Driest quarter", "annual precipitation", "precipitation of the Wettest month", "precipitation of the Driest month", "precipitations seasonality","maximum ph of the soil", "elevation average", "slope average", "average flow","minimum flow","maximum flow", "solar radiations", "water vapor pressure", "daylength annual min", "daylength annual max", "daylength annual range")
  abiotics_df_sub <- abiotics_df[,-(1:2)] #remove x and y columns

  shinyApp(
    ui <- fluidPage(
      titlePanel("Correlations"),
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "Method",
                      label = "Choose method:",
                      choices = method_names),
          numericInput("threshold", "Threshold value :", 0.7, min = 0, max = 1),
          verbatimTextOutput("value"),

          checkboxGroupInput(inputId = "Factor",
                             label = "Choose factors to keep:",
                             choiceNames = variable_names_display,
                             choiceValues = variable_names),

          actionButton(inputId= "submit", label= "submit")
        ),

        # Main panel for displaying outputs ----
        mainPanel(
          plotOutput("plot1", width = "100%", height = 600)
        )
      )
    ),

    server <- function(input, output) {

      output$plot1 <- renderPlot({
        BioClust <- varclus (as.matrix (abiotics_df_sub) , similarity = "spearman", method = input$Method)
        plot (BioClust, hang=-1)
        abline(h= 1-input$threshold,lwd=2,col="red")
      })

      observeEvent(input$submit, {
        selected_abiotics <<- input$Factor
      })
    }
  )
}
