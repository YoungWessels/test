# HTML NLL Data Viz

nll_map <- function(show_franch, cons_yaxis, 
                    start_date, end_date,
                    team1, kit1, 
                    team2=NULL, kit2=NULL,
                    team3=NULL, kit3=NULL, 
                    team4=NULL, kit4=NULL, 
                    team5=NULL, kit5=NULL) {
  
  start.time <- Sys.time() #====================================================FOR TESTING - REMOVE ME
  
  #initialize variables
  franchise_history <- list(team1, team2, team3, team4, team5)
  kit_set <- list(kit1, kit2, kit3, kit4, kit5)
  data_list <- list()
  graphdata <- data.frame()
  list_index <- 1
  global_game_num <- 1
  highest_attended <- NULL 
  highest_attended_color <- NULL
  slider_min <- NULL
  slider_max <- NULL
  
  # Removing franchise history if box is unticked 
  if(!show_franch){
    franchise_history <- franchise_history[[max(which(!sapply(franchise_history, is.null)))]]
    kit_set <- kit_set[[max(which(!sapply(kit_set, is.null)))]]
    }

  # Team colors
  colors <- setNames(kit_set, franchise_history)

  for (team in franchise_history) {
    if(!is.null(team)) {
      html_file <- paste("Data/", team, ".htm", sep="") 
    
      # Read the HTML file
      # Extract tables from the HTML content
      tables <- html_table(read_html(html_file))
      
      # Initialize variable to store the index of the desired table
      desired_table_index <- NULL
      
      # Find the index of the table with 12 columns and the maximum number of rows
      desired_table_index <- which.max(sapply(tables, function(t) if (ncol(as.data.frame(t)) == 12) nrow(as.data.frame(t)) else -Inf))
      
      # Retrieve the desired table
      desired_table <- tables[[desired_table_index]]
      
      # Choose the desired table
      gamedata <- as.data.frame(tables[[desired_table_index]])
      
      #Check to see what the actual first season is, and correct it if the slider window is out of that range
      slider_min <- min(slider_min,gamedata$Season,graphdata$Season)
      slider_max <- max(slider_max,gamedata$Season,graphdata$Season)
      
      if(slider_min > start_date & slider_max > end_date){
        start_date <- slider_min
        end_date <- slider_max
      }

      
      gamedata <- gamedata %>%
        filter(
          !str_detect(Opponent, '@'), # Removes away games
          between(as.numeric(Season), as.numeric(start_date), as.numeric(end_date)), # Filters by season range 
          ) %>%
        mutate(
          year = ifelse(duplicated(Season) | Season == "2021", "", Season), # Creates Year column for later use as graph label 
          Opponent = gsub("\\[\\d+\\]$", "", Opponent), # Removes [1] from opponents names
          Attendance = as.numeric(Attendance),
          Game_Num = seq(global_game_num, by = 1, length.out = n()) # Adds ID number to games (for graphing)
        )%>%
         filter(!is.na(Attendance), is.finite(Attendance)) # Removes rows where Attendance is NA or infinite
       
      
      global_game_num <- global_game_num + nrow(gamedata)
      
      # Add the processed dataframe to the list and overall
      graphdata <- rbind(graphdata,gamedata)
      data_list[[list_index]] <- gamedata

      #Record attendance
      current_max_attendance <- max(data_list[[list_index]]$Attendance, na.rm = TRUE)
      
      if (is.null(highest_attended) || current_max_attendance > max(highest_attended$Attendance, na.rm = TRUE)) {
        highest_attended <- data_list[[list_index]][data_list[[list_index]]$Attendance == current_max_attendance, ]
        highest_attended_color <- colors[[list_index]]
      }
      
      list_index <- list_index + 1
    }
  }
  
  # Only proceed with plotting if we have valid data
  if(nrow(graphdata) == 0) {
    return(list(
      plot = ggplot() + 
        theme_void() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available for selected period"),
      slider_min = slider_min,
      slider_max = slider_max
    ))
  }

  
  # Remove blank data
  graphdata$Attendance <- na.approx(graphdata$Attendance)
  
  # Consistent Y-Axis? 
  if (cons_yaxis) {
    scale_yaxis <- list(0,20000)
  }
  
  # Define the ggplot object without setting y-axis limits
  plot <- ggplot(graphdata, aes(x = Game_Num)) +
    geom_line(aes(y = Attendance), color = "whitesmoke", linewidth = 0.7) +
    geom_label_repel(data = highest_attended[nrow(highest_attended), ], 
                     aes(x = Game_Num, y = Attendance * 1.18, 
                         label = if (nrow(highest_attended) > 1) {
                           paste('Record Attendance:', format(Attendance, big.mark = ","),
                                 '(Multiple Times)\nMost Recently:', Date, 'vs', Opponent)
                         } else {
                           paste('Record Attendance:', format(Attendance, big.mark = ","),
                                 '\n', Date, 'vs', Opponent)
                         }),
                     fill = "lightsteelblue2", color = "black", size = 5, label.padding = unit(0.75, "lines"),
                     force = 10, point.padding = 10) +
    geom_point(data = highest_attended, aes(x = Game_Num, y = Attendance), size = 3, color = highest_attended_color) +
    scale_color_manual(values = colors) + 
    labs(title = paste(franchise_history[[max(which(!sapply(franchise_history, is.null)))]], " home attendance", sep=""), 
         x = "Season",
         subtitle = paste(min(graphdata$Season)," - ",paste(max(graphdata$Season)), sep=""),
         colour = NULL) + 
    scale_x_continuous(breaks = graphdata$Game_Num[graphdata$year != ""],
                       labels = paste0("'", substr(graphdata$year[graphdata$year != ""], 3, 4)),
                       expand = c(0, 0)) +

    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(colour = "white",size = 15),
          axis.text = element_text(colour = "white", size = 15),
          axis.text.x = element_text(angle = 70, hjust = 1),
          plot.title = element_text(face = "bold", colour = "white", size = 25),
          panel.background = element_rect(fill = "lightsteelblue4", colour = NA),
          plot.background = element_rect(fill = "lightsteelblue4", colour = NA),
          strip.text = element_text(colour = NA),
          plot.subtitle = element_text(face = "bold.italic", colour = "white", size = 15),
          panel.grid.major = element_line(colour = "gray70"),
          legend.text = element_text(face = "bold",colour = "white", size = 15),
          legend.key = element_rect(fill = "lightsteelblue2"),
          legend.background = element_rect(fill = "lightsteelblue4"),
          legend.position = "bottom",
          legend.direction = "horizontal")

  # Conditionally adding geom_smooth layers
  # This is the only way I could get it to work
  # A for loop would set K for all datasets to be equal to the last given value
  
  if (length(data_list) >= 1) {
    plot <- plot + geom_smooth(data = data_list[[1]], method = "gam", 
                               formula = y ~ s(x, bs = "cs", fx = TRUE, k = max(3, length(unique(data_list[[1]]$Season)))), 
                               aes(y = Attendance, color = franchise_history[[1]]), linewidth = 2.25)
  }
  
  if (length(data_list) >= 2) {
    plot <- plot + geom_smooth(data = data_list[[2]], method = "gam", 
                               formula = y ~ s(x, bs = "cs", fx = TRUE, k = max(3, length(unique(data_list[[2]]$Season)))), 
                               aes(y = Attendance, color = franchise_history[[2]]), linewidth = 2.25)
  }
  
  if (length(data_list) >= 3) {
    plot <- plot + geom_smooth(data = data_list[[3]], method = "gam", 
                               formula = y ~ s(x, bs = "cs", fx = TRUE, k = max(3, length(unique(data_list[[3]]$Season)))), 
                               aes(y = Attendance, color = franchise_history[[3]]), linewidth = 2.25)
  }
  
  if (length(data_list) >= 4) {
    plot <- plot + geom_smooth(data = data_list[[4]], method = "gam", 
                               formula = y ~ s(x, bs = "cs", fx = TRUE, k = max(3, length(unique(data_list[[4]]$Season)))), 
                               aes(y = Attendance, color = franchise_history[[4]]), linewidth = 2.25)
  }
  
  if (length(data_list) >= 5) {
    plot <- plot + geom_smooth(data = data_list[[5]], method = "gam", 
                               formula = y ~ s(x, bs = "cs", fx = TRUE, k = max(3, length(unique(data_list[[5]]$Season)))), 
                               aes(y = Attendance, color = franchise_history[[5]]), linewidth = 2.25)
  }
  
  # Conditionally add y-axis limits if cons_yaxis is true
  if (cons_yaxis) {
    plot <- plot + scale_y_continuous(limits = c(0, 23000))
  }
  
  end.time <- Sys.time()#========================================================FOR TESTING - REMOVE ME
  time.taken <- end.time - start.time
  print(time.taken)
  
  return(list(
    plot = plot,
    slider_min = slider_min,
    slider_max = slider_max
  ))
}
