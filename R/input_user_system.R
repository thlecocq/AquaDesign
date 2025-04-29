#' Title Register the parameters corresponding to user's system
#'
#' @param abiotics_df data frame with abiotics data
#'
#' @return Returns a dataframe with the edited user parameters
#' @export
#'
#' @examples
input_user_system <- function(abiotics_df){



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
  names(selected_abiotics)[selected_abiotics=="dl_annual_min"] <- "Daylength annual min (Hours)"
  names(selected_abiotics)[selected_abiotics=="dl_annual_max"] <- "Daylength annual max (Hours)"
  names(selected_abiotics)[selected_abiotics=="dl_annual_range"] <- "Daylength annual range (Hours)"


  #divide temperatures and pH by 10
  abiotics_df$annual_meanT <- abiotics_df$annual_meanT/10
  abiotics_df$maxT_WM <- abiotics_df$maxT_WM/10
  abiotics_df$annual_rangeT <- abiotics_df$annual_rangeT/10
  abiotics_df$ph_max <- abiotics_df$ph_max/10
  abiotics_df$minT_CM <- abiotics_df$minT_CM/10
  abiotics_df$meanT_DQ <- abiotics_df$meanT_DQ/10


  #ask for latitude and longitude
  user_lat <- as.numeric(dlgInput("Enter your site location latitude", "47.75")$res)
  user_long <- as.numeric(dlgInput("Enter your site location longitude", "6.2500")$res)

  #get the closest cell to coordinates
  rank <- which(abs(abiotics_df$y - user_lat) == min(abs(abiotics_df$y - user_lat))&abs(abiotics_df$x - user_long) == min(abs(abiotics_df$x - user_long)))
  user_lat <- abiotics_df$y[rank]
  user_long <- abiotics_df$x[rank]



  #get the line of the abiotics_df corresponding to these coordinates
  user_coords <- data.frame(y = user_lat, x =user_long)
  user_param <- merge(user_coords, abiotics_df, by=c("x","y"))
  user_param <- user_param[,-(1:2)] #remove x and y
  user_param_edited <- user_param

  #for each parameter, ask the user if they want to edit
  for (i in 1:length(user_param[1,])){
    user_val <- as.numeric(dlgInput(names(selected_abiotics[i]), as.character(user_param[i]))$res)
    user_param_edited[i] <- as.numeric(user_val)
  }

  #multiply temperatures and pH by 10 for the analysis
  user_param_edited[1] <-  user_param_edited[1]*10
  user_param_edited[2] <-  user_param_edited[2]*10
  user_param_edited[3] <-  user_param_edited[3]*10
  user_param_edited[4] <-  user_param_edited[4]*10
  user_param_edited[6] <-  user_param_edited[6]*10
  user_param_edited[7] <-  user_param_edited[7]*10


  return(user_param_edited)

}
