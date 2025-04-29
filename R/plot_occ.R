#' Title Plot retained and removed occurence points per species
#'
#' @param data Dataframe; The whole species coordinates dataframe
#' @param data_cl Dataframe; The filtered species coordinates dataframe
#'
#' @return Returns nothing
#' @export
#' @import rnaturalearth
#' @examples

plot_occ <- function(data, data_cl){

  #load worldmap
  world <- ne_countries(scale = "medium", returnclass = "sf")

  #get the species list
  species_list <- unique(data_cl$species)

  shinyApp(ui = fluidPage(

    titlePanel("Species distribution"),

    sidebarLayout(

      sidebarPanel(

        selectInput(inputId = "Species",
                    label = "Choose a species:",
                    choices = species_list)
      ),

      # Main panel for displaying outputs ----
      mainPanel(

        plotOutput("map", width = "100%", height = 400)

      )
    )
  ),

  server = function(input, output) {

    observeEvent(input$Species,{
      #get the data points for the selected species
      data_filtered <- data[data$species==input$Species,]
      #get the cleaned data points for the selected species
      data_cl_filtered <- data_cl[data_cl$species==input$Species,]
      #plot the map
      output$map <- renderPlot({
        ggplot(data = world)+
          geom_sf()+
          geom_point(data = data_filtered , aes(x = decimalLongitude, y = decimalLatitude), col = "red")+
          geom_point(data = data_cl_filtered , aes(x = decimalLongitude, y = decimalLatitude), col = "blue")

      })
    })

  })
}
