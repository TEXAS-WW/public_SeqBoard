server <- function(input, output, session) {
  
  
  #########Map of watewater sites
  output$map_image <- renderImage({
    list(
      src = "distribution.jpg",
      contentType = "image/jpeg",
      width = "100%",
      height = "auto",
      alt = "Distribution map"
    )
  }, deleteFile = FALSE)
  
  
  
  # Function to calculate date range
  get_date_range <- function(selection, max_date) {
    end_date <- max_date
    start_date <- switch(selection,
                         "6w" = end_date - weeks(6),
                         "3m" = end_date - months(3),
                         "6m" = end_date - months(6),
                         "all" = as.Date("1900-01-01"))
    return(c(start_date, end_date))
  }
  
  # Reactive function for virus data
  virus_df <- reactive({
    req(input$virus)
    req(input$dateRange)
    
    validate(
      need(nrow(sense_path_expand_dt) > 0, "No data available in sense_path_expand_dt")
    )
    
    max_date <- max(sense_path_expand_dt$Week, na.rm = TRUE)
    date_range <- get_date_range(input$dateRange, max_date)
    
    df <- sense_path_expand_dt %>%
      filter(!is.na(moving_average),
             species == input$virus,
             Week >= date_range[1],
             Week <= date_range[2])
    
    validate(
      need(nrow(df) > 0, "No data available for the selected criteria")
    )
    
    # Add some debugging output
    print(paste("Rows in df for", input$virus, ":", nrow(df)))
    print(summary(df$moving_average))
    
    return(df)
  })
  
  # Reactive value for selected city
  selected_city <- reactiveVal(NULL)
  
  #################### Render the main plot of cities and virus selections
  output$virusPlot <- renderPlotly({
    df <- virus_df()
    
    if(is.null(df) || nrow(df) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
    }
    
    df$hover_text <- sprintf(
      "%s<br>Date: %s<br>Value: %.3f",
      df$City,
      format(df$Week, "%Y-%m-%d"),
      df$moving_average
    )
    
    # Automatically calculate y-axis range and tick values
    y_max <- max(df$moving_average, na.rm = TRUE)
    y_min <- min(df$moving_average, na.rm = TRUE)
    y_range <- y_max - y_min
    
    # Generate 5 evenly spaced tick values
    y_values <- seq(y_min, y_max, length.out = 5)
    
    x_format <- switch(input$dateRange,
                       "6w" = "%m/%d/%y",
                       "3m" = "%m/%d/%y",
                       "6m" = "%b'%y",
                       "all" = "%b'%y",
                       "%m/%d/%y")
    
    if(input$dateRange %in% c("6m", "all")) {
      x_dtick <- "M1"
      x_nticks <- NULL
    } else {
      x_dtick <- NULL
      x_nticks <- 10
    }
    
    p <- plot_ly(df, x = ~Week, y = ~moving_average, color = ~City, colors = color_palette, 
                 type = 'scatter', mode = 'lines+markers', 
                 hoverinfo = "text", text = ~hover_text) %>%
      layout(
        xaxis = list(
          title = "",
          tickangle = 45,
          tickformat = x_format,
          tickmode = "auto",
          dtick = x_dtick,
          nticks = x_nticks
        ),
        yaxis = list(
          title = 'City-Wide Abundance (RPKMF)',
          type = 'linear',
          range = c(y_min, y_max),
          tickmode = "array",
          tickvals = y_values,
          ticktext = sprintf("%.2f", y_values),
          showgrid = TRUE,
          zeroline = TRUE
        ),
        legend = list(title = list(text = 'City')),
        title = list(
          text = paste("Wastewater Levels of", input$virus, "in Texas Cities"),
          font = list(size = 16)
        )
      )
    
    selected <- selected_city()
    if (!is.null(selected) && length(selected) > 0 && selected %in% df$City) {
      p <- p %>% style(
        opacity = ifelse(df$City == selected, 1, 0.1)
      )
    }
    
    p <- p %>% style(
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "black",
        font = list(
          color = "black",
          size = 14,
          family = "Arial",
          weight = 700
        )
      )
    )
    
    p <- p %>% config(
      displayModeBar = FALSE
    )
    
    p <- p %>% 
      event_register("plotly_click") %>%
      event_register("plotly_legendclick") %>%
      event_register("plotly_doubleclick")
    
    # Add accessibility description to the plot
    description <- paste("Line graph showing", input$virus, "levels in wastewater for different Texas cities over time.")
    p$x$attrs[[1]]$description <- description
    
    return(p)
  })
  
  # Observe plot interactions
  observeEvent(event_data("plotly_click"), {
    click_data <- event_data("plotly_click")
    df <- virus_df()
    
    if (!is.null(click_data) && !is.null(df) && !is.null(click_data$curveNumber)) {
      curve_number <- as.numeric(click_data$curveNumber) + 1
      if (!is.na(curve_number) && curve_number > 0 && curve_number <= nrow(df)) {
        clicked_city <- df$City[curve_number]
        current_selected <- selected_city()
        if (!is.null(current_selected) && length(current_selected) > 0 && clicked_city == current_selected) {
          selected_city(NULL)
        } else {
          selected_city(clicked_city)
        }
      }
    }
  })
  
  observeEvent(event_data("plotly_legendclick"), {
    legend_data <- event_data("plotly_legendclick")
    df <- virus_df()
    
    if (!is.null(legend_data) && !is.null(df) && !is.null(legend_data$curveNumber)) {
      curve_number <- as.numeric(legend_data$curveNumber) + 1
      if (!is.na(curve_number) && curve_number > 0 && curve_number <= nrow(df)) {
        clicked_city <- df$City[curve_number]
        current_selected <- selected_city()
        if (!is.null(current_selected) && length(current_selected) > 0 && clicked_city == current_selected) {
          selected_city(NULL)
        } else {
          selected_city(clicked_city)
        }
      }
    }
  })
  
  observeEvent(event_data("plotly_doubleclick"), {
    selected_city(NULL)
  })
  
  # Observe virus selection for notification about different y axis
  observeEvent(input$virus, {
    if(input$virus == "Mpox virus") {
      showNotification(
        "Note: The y-axis scale is lower than other plots",
        type = "warning",
        duration = 5,
        closeButton = FALSE
      )
    } else if(input$virus == "Choose 1") {
      showNotification(
        "Note: The y-axis scale for this plot goes up to 16 instead of 3.",
        type = "warning",
        duration = 5,
        closeButton = FALSE
      )
    } else if(input$virus == "Choose 2") {
      showNotification(
        "Note: The y-axis scale for this plot goes up to 9 instead of 3.",
        type = "warning",
        duration = 5,
        closeButton = FALSE
      )
    } else if(input$virus == "Choose 3") {
      showNotification(
        "Note: The y-axis scale for this plot goes up to 400 instead of 3.",
        type = "warning",
        duration = 5,
        closeButton = FALSE
      )
    } else if(input$virus == "Choose 4") {
      showNotification(
        "Note: The y-axis scale for this plot goes up to 4 instead of 3.",
        type = "warning",
        duration = 5,
        closeButton = FALSE
      )
    }
  })
  
  
  
  ########################## Observe video toggle button
  observeEvent(input$toggleVideoBtn, {
    session$sendCustomMessage(
      type = 'toggleVideo',
      message = input$toggleVideoBtn %% 2 == 1
    )
  })
  
  
  
  
  
  ############## Add the overview output
  # Helper function to generate species boxes
  generate_species_boxes <- function(qual_eval, preprocessed_data) {
    sorted_qual_eval <- qual_eval %>%
      mutate(
        level = factor(level, levels = preprocessed_data$level_order, ordered = TRUE)
      ) %>%
      arrange(level, Species)
    
    lapply(1:nrow(sorted_qual_eval), function(i) {
      species_data <- sorted_qual_eval[i, ]
      status <- list(
        text = species_data$level,
        class = tolower(species_data$level),
        color = preprocessed_data$color_map[species_data$level]
      )
      
      trend_arrow <- preprocessed_data$trend_arrows[species_data$Trend]
      
      description <- if(species_data$Trend == "None") {
        sprintf("%s concentration with no trend", status$text)
      } else {
        sprintf("%s concentration with %s trend", 
                status$text, 
                tolower(species_data$Trend))
      }
      
      species_title <- {
        if(species_data$Species %in% names(preprocessed_data$link_map)) {
          div(
            style = "display: flex; align-items: center; gap: 10px;",
            span(species_data$Species),  # Plain text for virus name
            tags$a(
              href = preprocessed_data$link_map[species_data$Species],
              target = "_blank",
              tags$span(
                "CDC Info",
                style = "color: #003366; font-size: 0.9em; text-decoration: underline;"
              )
            )
          )
        } else {
          species_data$Species
        }
      }
      
      div(class = paste("species-box", status$class),
          style = sprintf("background-color: %s;", status$color),
          div(class = "species-title", species_title),
          div(class = "species-level",
              span(status$text, style = "margin-right: 5px;"),
              HTML(paste0("<span style='font-size: 1.2em;'>", trend_arrow, "</span>"))
          ),
          div(class = "species-description", description)
      )
    })
  }
  
  # Desktop species summary
  output$species_summary <- renderUI({
    req(qual_eval)
    preprocessed_data <- readRDS("species_summary_data.rds")
    boxes <- generate_species_boxes(qual_eval, preprocessed_data)
    div(class = "species-boxes-container", boxes)
  })
  
  # Mobile species summary
  output$mobile_species_summary <- renderUI({
    req(qual_eval)
    preprocessed_data <- readRDS("species_summary_data.rds")
    boxes <- generate_species_boxes(qual_eval, preprocessed_data)
    div(class = "species-boxes-container mobile", boxes)
  })
  
  
  
  
  
##########  covid variant plot
  output$sars_linp <- renderPlotly({
    # Load the pre-computed ggplot object
    p <- readRDS("sars_lineage_plot.rds")
    
    # Convert to plotly, adjust layout, and configure menu options
    ggplotly(p, tooltip = c("Week", "n_sites")) %>%
      layout(showlegend = FALSE,
             autosize = TRUE,
             height = 600) %>%
      config(displayModeBar = FALSE, # This hides the mode bar completely
             scrollZoom = FALSE,
             doubleClick = FALSE,
             displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 
               'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 
               'hoverCompareCartesian', 'toggleSpikelines'
             ))
  })
  
  

  ########### Texas-wide Pathogen Levels Plot
  
  output$TX_wide_plot <- renderPlotly({
    readRDS("tx_wide_plotly.rds")
  })


  
  ############################ Add the reactive genome coverage plot using preproceesed table data

  output$nice_table <- renderReactable({
    table_data <- readRDS("preprocessed_table_data.rds")
    
    # Ensure Date column is in Date format
    table_data$Date <- as.Date(table_data$Date)
    
    # Common style for all sortable columns
    sortable_style <- list(
      cursor = "pointer",
      "& [aria-label='Sort']" = list(
        padding = "8px",
        transform = "scale(1.5)",
        display = "inline-flex",
        alignItems = "center",
        justifyContent = "center"
      ),
      "&:hover [aria-label='Sort']" = list(
        opacity = 0.8
      )
    )
    
    # Style for header cells to increase clickable area
    header_style <- list(
      padding = "1.5rem 8px",  # Increased vertical padding
      fontSize = "2.2rem",     # Slightly larger font
      fontWeight = "600",      # Make text more prominent
      cursor = "pointer",
      "&:hover" = list(
        backgroundColor = "rgba(0, 0, 0, 0.05)"
      )
    )
    
    reactable(
      table_data,
      columns = list(
        Date = colDef(
          style = sortable_style,
          headerStyle = header_style,
          cell = function(value) {
            formatted <- format(value, "%m/%d/%Y")
            cat("Original:", as.character(value), "Formatted:", formatted, "\n")
            formatted
          },
          filterMethod = JS("
          function(rows, columnId, filterValue) {
            console.log('Filter value:', filterValue);
            return rows.filter(function(row) {
              var cellValue = row.values[columnId];
              console.log('Cell value:', cellValue);
              
              var filterPattern = filterValue.replace(/[/-]/g, '[/-]');
              var regex = new RegExp(filterPattern);
              
              var result = regex.test(cellValue);
              console.log('Match result:', result);
              return result;
            });
          }
        ")
        ),
        City = colDef(
          style = sortable_style,
          headerStyle = header_style
        ),
        sequence_name = colDef(
          name = "Sequence Name",
          style = sortable_style,
          headerStyle = header_style
        ),
        accession = colDef(
          name = "Accession",
          style = sortable_style,
          headerStyle = header_style
        ),
        Percent_covered = colDef(
          name = "% Covered",
          style = sortable_style,
          headerStyle = header_style,
          cell = function(value) paste0(formatC(value * 100, format = "f", digits = 2), "%"),
          filterMethod = JS("
          function(rows, columnId, filterValue) {
            return rows.filter(function(row) {
              var percentValue = (row.values[columnId] * 100).toFixed(2);
              var filterNum = parseFloat(filterValue);
              if (isNaN(filterNum)) {
                return true;
              }
              var percentNum = parseFloat(percentValue);
              return percentNum >= filterNum && percentNum < (filterNum + 1);
            });
          }
        ")
        ),
        RPKMF = colDef(
          name = "RPKMF",
          style = sortable_style,
          headerStyle = header_style,
          format = colFormat(digits = 2),
          filterMethod = JS("
          function(rows, columnId, filterValue) {
            return rows.filter(function(row) {
              if (filterValue === '') return true;
              var cellValue = row.values[columnId].toFixed(2);
              
              if (filterValue.startsWith('.')) {
                filterValue = '0' + filterValue;
              }
              
              var parts = filterValue.split('.');
              if (parts.length === 1) {
                filterValue += '.00';
              } else if (parts[1].length === 1) {
                filterValue += '0';
              }
              
              return cellValue === filterValue;
            });
          }
        ")
        ),
        coverage = colDef(
          name = "Coverage",
          style = sortable_style,
          headerStyle = header_style,
          cell = function(value) {
            sparkline(value, type = "line", chartRangeMin = 0, width = 200, height = 40)
          },
          width = 250
        )
      ),
      filterable = TRUE,
      searchable = TRUE,
      pagination = TRUE,
      defaultPageSize = 20,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 20, 50, 100, 400),
      defaultSorted = list(Date = "desc")
    )
  })
  
  
  
  
  
  #Render methods image for desktop
  output$methodsImage <- renderImage({
    list(
      src = "www/methods.png",
      width = "1284px",
      height = "1240px",
      alt = "Methods Diagram"
    )
  }, deleteFile = FALSE)
  
  
  

  
  
}

