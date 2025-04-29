#' Title Plot species density diagrams for individual species
#'
#' @param species_df Dataframe of species coordinates
#' @param abiotics_df Dataframe of abiotics data and coordinates
#' @param species_list List of species name
#' @param minlat Float; Minimum latitude (default=-56)
#' @param maxlat Float; Maximum latitude (default=60)
#' @param minlong Float; Minimum longitude (default=-145)
#' @param maxlong Float; Maximum longitude (default=180)
#'
#' @return Returns nothing
#' @export
#'
#' @examples
design_monoculture_app <- function (species_df, abiotics_df, species_list , minlat = -56, maxlat = 60 , minlong =-145, maxlong = 180){

  #get abiotics list from the dataframe
  selected_abiotics <- as.list(colnames(abiotics_df[,-(1:2)]))

  #set abiotics names to display
  names(selected_abiotics)[selected_abiotics=="annual_meanT"] <- "Annual mean temperature (°C)"
  names(selected_abiotics)[selected_abiotics=="maxT_WM"] <- "Maximum temperature of the warmest month (°C)"
  names(selected_abiotics)[selected_abiotics=="annual_rangeT"] <- "Temperature annual range (°C)"
  names(selected_abiotics)[selected_abiotics=="ph_max"] <- "Maximum pH of the soil"
  names(selected_abiotics)[selected_abiotics=="T_seasonality"] <- "Temperature seasonality"
  names(selected_abiotics)[selected_abiotics=="minT_CM"] <- "Minimum temperature of the coldest month (°C)"
  names(selected_abiotics)[selected_abiotics=="meanT_DQ"] <- "Mean temperature of the driest quarter (°C)"
  names(selected_abiotics)[selected_abiotics=="elevation_avg"] <- "Average elevation (meters)"
  names(selected_abiotics)[selected_abiotics=="slope_avg"] <- "Average slope([°]*100)"
  names(selected_abiotics)[selected_abiotics=="flow_df_av"] <- "Average flow (m3.s-1)"
  names(selected_abiotics)[selected_abiotics=="flow_df_mi"] <- "Minimum flow (m3.s-1)"
  names(selected_abiotics)[selected_abiotics=="flow_df_ma"] <- "Maximum flow (m3.s-1)"
  names(selected_abiotics)[selected_abiotics=="srad"] <- "Solar radiation (kJ.m-2.day-1)"
  names(selected_abiotics)[selected_abiotics=="vapr"] <- "Water vapor pressure (kPa)"
  names(selected_abiotics)[selected_abiotics=="annual_prec"] <- "Annual precipitations (mm)"
  names(selected_abiotics)[selected_abiotics=="prec_WM"] <- "Precipitation of the wettest month (mm)"
  names(selected_abiotics)[selected_abiotics=="prec_DM"] <- "Precipitation of the driest month (mm)"
  names(selected_abiotics)[selected_abiotics=="prec_seasonality"] <- "Precipitation seasonality"

  #round abiotics to have homogeneous coordinates
  is.num <- sapply(abiotics_df, is.numeric)
  abiotics_df[is.num] <- lapply(abiotics_df[is.num], round, 4)

  #round species to have homogeneous coordinates
  is.num <- sapply(species_df, is.numeric)
  species_df[is.num] <- lapply(species_df[is.num], round, 4)

  abiotics_df_sub <- abiotics_df %>%
    dplyr::filter(y<maxlat, y>(minlat), x>(minlong), x<maxlong)

  species_abiotics_df <- merge(species_df, abiotics_df_sub, by=c("x","y"))
  species_abiotics_df <- species_abiotics_df[-(1:2)]

  if (length(abiotics_df[,1]) == 0){
    stop("The are no occcurences in this area")
  }


  #divide temperatures and pH by 10
  species_abiotics_df$annual_meanT <- species_abiotics_df$annual_meanT/10
  species_abiotics_df$maxT_WM <-  species_abiotics_df$maxT_WM/10
  species_abiotics_df$annual_rangeT <-  species_abiotics_df$annual_rangeT/10
  species_abiotics_df$ph_max <-  species_abiotics_df$ph_max/10
  species_abiotics_df$minT_CM <-  species_abiotics_df$minT_CM/10
  species_abiotics_df$meanT_DQ <-  species_abiotics_df$meanT_DQ/10


  shinyApp(ui = fluidPage(titlePanel("Species density"),
                          sidebarLayout(

                            sidebarPanel(

                              #Choice of the parameter
                              selectInput(inputId = "Factor",
                                          label = "Choose a factor:",
                                          choices = selected_abiotics),

                              #choice of the species to show
                              checkboxGroupInput(inputId = "species_show",
                                                 label = "Chose species to show:",
                                                 choiceNames = species_list,
                                                 choiceValues = species_list)
                            ),

                            #plotting of the density diagram
                            mainPanel(plotOutput("plot", width = "100%", height = 400)


                            )
                          )
  ),

  server = function(input, output) {

    output$plot <- renderPlot({
      #subset the dataframe to the species to show
      species_abiotics_df_sub <- species_abiotics_df %>%
        dplyr::filter(species %in% input$species_show)
      #plot density diagram
      ggplot(species_abiotics_df_sub, aes(x = species_abiotics_df_sub[,input$Factor], fill = species))+
        geom_density(alpha = 0.4) +
        xlab(names(selected_abiotics[which(selected_abiotics %in%
                                             input$Factor)]))
    })
  }
  )
}
