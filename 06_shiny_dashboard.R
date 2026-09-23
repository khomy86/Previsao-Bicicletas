library(shiny)
library(shinydashboard)
library(tidymodels)
library(readr)
library(plotly)
library(leaflet)
library(DT)

seasons <- c("Winter", "Spring", "Summer", "Autumn")
season_colours <- c(Winter = "#4575b4", Spring = "#66bd63",
                    Summer = "#f46d43", Autumn = "#c9a227")

variable_labels <- c(
  HOUR = "Hour of day", TEMPERATURE_C = "Temperature", HUMIDITY = "Humidity",
  SEASONS = "Season", DEW_POINT_TEMPERATURE_C = "Dew point",
  RAINFALL_MM = "Rainfall", VISIBILITY_10M = "Visibility",
  WIND_SPEED_M_S = "Wind speed", SNOWFALL_CM = "Snowfall", HOLIDAY = "Holiday"
)

fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE)

# Data ------------------------------------------------------------------------

bikes <- read_csv("data/clean/seoul_bike_sharing.csv", show_col_types = FALSE) |>
  mutate(SEASON = factor(SEASONS, levels = seasons))

model <- readRDS("models/best_model.rds")
model_results <- read_csv("models/model_comparison.csv", show_col_types = FALSE)
importance <- read_csv("models/feature_importance.csv", show_col_types = FALSE) |>
  mutate(variable = variable_labels[variable])

forecast <- NULL
if (file.exists("data/clean/weather_forecast.csv")) {
  forecast <- read_csv("data/clean/weather_forecast.csv", show_col_types = FALSE)
  forecast$PREDICTED <- round(predict(model, forecast)$.pred)
}

daily <- bikes |>
  group_by(DATE) |>
  summarise(rentals = sum(RENTED_BIKE_COUNT))

seasonal <- bikes |>
  group_by(SEASON) |>
  summarise(
    avg_rentals = round(mean(RENTED_BIKE_COUNT)),
    avg_temp = round(mean(TEMPERATURE_C), 1),
    rainfall = round(sum(RAINFALL_MM)),
    snowfall = round(sum(SNOWFALL_CM))
  )

hourly <- bikes |>
  filter(FUNCTIONING_DAY == "Yes") |>
  group_by(SEASON, HOUR) |>
  summarise(avg_rentals = round(mean(RENTED_BIKE_COUNT)), .groups = "drop")

best <- model_results[1, ]

# UI --------------------------------------------------------------------------

compact_table <- function(data, ...) {
  datatable(data, rownames = FALSE, options = list(dom = "t", pageLength = 50), ...)
}

forecast_tab <- if (is.null(forecast)) {
  fluidRow(box(
    width = 12, status = "warning",
    p("No weather forecast data. Set", code("OPENWEATHER_API_KEY"),
      "and run the pipeline again.")
  ))
} else {
  tagList(
    fluidRow(
      box(
        title = "Predicted peak demand over the next 5 days",
        status = "primary", solidHeader = TRUE, width = 8,
        leafletOutput("map", height = 480)
      ),
      box(
        title = "By city", status = "primary", solidHeader = TRUE, width = 4,
        DTOutput("peaks_table")
      )
    ),
    fluidRow(
      box(
        title = "Hourly forecast", status = "info", solidHeader = TRUE, width = 12,
        selectInput("city", NULL, choices = sort(unique(forecast$CITY)),
                    selected = "Seoul"),
        plotlyOutput("city_forecast", height = 300)
      )
    ),
    p(class = "text-muted",
      "Predictions from the best model trained on Seoul data, applied to the ",
      "OpenWeather forecast. They assume a fleet and riding habits similar ",
      "to Seoul's.")
  )
}

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Bike Demand", titleWidth = 260),
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      menuItem("Forecast", tabName = "forecast", icon = icon("map-location-dot")),
      menuItem("Seoul in 2018", tabName = "overview", icon = icon("chart-line")),
      menuItem("Weather", tabName = "weather", icon = icon("cloud-sun")),
      menuItem("Hourly patterns", tabName = "hourly", icon = icon("clock")),
      menuItem("Models", tabName = "models", icon = icon("diagram-project"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("forecast", forecast_tab),

      tabItem(
        "overview",
        fluidRow(
          valueBox(fmt(sum(bikes$RENTED_BIKE_COUNT)), "Rentals in the year",
                   icon = icon("bicycle"), color = "light-blue"),
          valueBox(fmt(mean(bikes$RENTED_BIKE_COUNT)), "Average per hour",
                   icon = icon("clock"), color = "green"),
          valueBox(fmt(max(bikes$RENTED_BIKE_COUNT)), "Busiest hour",
                   icon = icon("arrow-trend-up"), color = "orange")
        ),
        fluidRow(
          box(title = "Daily rentals", status = "primary", width = 8,
              plotlyOutput("daily_plot", height = 350)),
          box(title = "Hourly distribution", status = "primary", width = 4,
              plotlyOutput("distribution_plot", height = 350))
        ),
        fluidRow(
          box(title = "Seasonal summary", status = "primary", width = 12,
              DTOutput("seasonal_table"))
        )
      ),

      tabItem(
        "weather",
        fluidRow(
          box(title = "Temperature vs. rentals", status = "primary", width = 9,
              plotlyOutput("temp_plot", height = 420)),
          box(
            title = "Filters", status = "primary", width = 3,
            checkboxGroupInput("weather_seasons", "Seasons",
                               choices = seasons, selected = seasons),
            sliderInput("temp_range", "Temperature (°C)",
                        min = floor(min(bikes$TEMPERATURE_C)),
                        max = ceiling(max(bikes$TEMPERATURE_C)),
                        value = range(bikes$TEMPERATURE_C) |> round(),
                        step = 1)
          )
        ),
        fluidRow(
          box(title = "Total precipitation by season", status = "primary",
              width = 12, plotlyOutput("precipitation_plot", height = 300))
        )
      ),

      tabItem(
        "hourly",
        fluidRow(
          box(title = "Average rentals by hour of day", status = "primary",
              width = 9, plotlyOutput("hourly_plot", height = 420)),
          box(
            title = "Seasons", status = "primary", width = 3,
            checkboxGroupInput("hourly_seasons", NULL,
                               choices = seasons, selected = seasons)
          )
        ),
        fluidRow(
          box(title = "Average rentals by season", status = "primary",
              width = 12, plotlyOutput("seasonal_plot", height = 300))
        )
      ),

      tabItem(
        "models",
        fluidRow(
          valueBox(best$model, "Best model", icon = icon("trophy"),
                   color = "yellow"),
          valueBox(sprintf("%.3f", best$rsq), "Test set R²",
                   icon = icon("bullseye"), color = "green"),
          valueBox(fmt(best$rmse), "RMSE (bikes/hour)",
                   icon = icon("ruler"), color = "light-blue")
        ),
        fluidRow(
          box(title = "RMSE by model (lower is better)", status = "primary",
              width = 7, plotlyOutput("model_plot", height = 350)),
          box(title = "Variable importance (Random Forest)",
              status = "primary", width = 5,
              plotlyOutput("importance_plot", height = 350))
        ),
        fluidRow(
          box(title = "Test set metrics", status = "primary",
              width = 12, DTOutput("model_table"))
        )
      )
    )
  )
)

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {

  as_plotly <- function(p) {
    ggplotly(p) |> config(displayModeBar = FALSE)
  }

  if (!is.null(forecast)) {
    peaks <- forecast |>
      group_by(CITY, COUNTRY, LAT, LON) |>
      slice_max(PREDICTED, n = 1, with_ties = FALSE) |>
      ungroup() |>
      arrange(desc(PREDICTED))

    output$map <- renderLeaflet({
      pal <- colorNumeric("YlOrRd", peaks$PREDICTED)
      leaflet(peaks) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addCircleMarkers(
          lng = ~LON, lat = ~LAT, layerId = ~CITY,
          radius = ~sqrt(PREDICTED) / 2,
          color = ~pal(PREDICTED), fillOpacity = 0.8, stroke = FALSE,
          label = ~paste0(CITY, ": ", fmt(PREDICTED), " bikes/hour"),
          popup = ~paste0("<b>", CITY, "</b><br>",
                          "Peak: ", fmt(PREDICTED), " bikes/hour<br>",
                          format(LOCAL_TIME, "%b %d, %H:%M"), " (local time)")
        ) |>
        addLegend("bottomright", pal = pal, values = ~PREDICTED,
                  title = "Bikes/hour")
    })

    observeEvent(input$map_marker_click, {
      updateSelectInput(session, "city", selected = input$map_marker_click$id)
    })

    output$peaks_table <- renderDT({
      peaks |>
        transmute(City = CITY, Peak = PREDICTED,
                  When = format(LOCAL_TIME, "%b %d, %H:00")) |>
        compact_table()
    })

    output$city_forecast <- renderPlotly({
      filter(forecast, CITY == input$city) |>
        plot_ly(
          x = ~LOCAL_TIME, y = ~PREDICTED,
          type = "scatter", mode = "lines", line = list(color = "#3c8dbc"),
          text = ~paste0(PREDICTED, " bikes · ", TEMPERATURE_C, " °C"),
          hoverinfo = "x+text"
        ) |>
        layout(xaxis = list(title = "Local time"),
               yaxis = list(title = "Predicted rentals per hour")) |>
        config(displayModeBar = FALSE)
    })
  }

  output$daily_plot <- renderPlotly({
    p <- ggplot(daily, aes(DATE, rentals)) +
      geom_line(colour = "#3c8dbc", alpha = 0.6) +
      geom_smooth(method = "loess", formula = y ~ x, span = 0.3,
                  se = FALSE, colour = "#d7301f") +
      labs(x = NULL, y = "Rentals per day") +
      theme_minimal()
    as_plotly(p)
  })

  output$distribution_plot <- renderPlotly({
    p <- ggplot(bikes, aes(RENTED_BIKE_COUNT)) +
      geom_histogram(bins = 30, fill = "#3c8dbc", colour = "white") +
      labs(x = "Bikes rented in an hour", y = "Hours") +
      theme_minimal()
    as_plotly(p)
  })

  output$seasonal_table <- renderDT({
    compact_table(
      seasonal,
      colnames = c("Season", "Avg rentals/hour", "Avg temperature (°C)",
                   "Total rainfall (mm)", "Total snowfall (cm)")
    )
  })

  weather_data <- reactive({
    bikes |>
      filter(SEASON %in% input$weather_seasons,
             between(TEMPERATURE_C, input$temp_range[1], input$temp_range[2]))
  })

  output$temp_plot <- renderPlotly({
    p <- ggplot(weather_data(), aes(TEMPERATURE_C, RENTED_BIKE_COUNT, colour = SEASON)) +
      geom_point(alpha = 0.4, size = 0.8) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
      scale_colour_manual(values = season_colours, name = NULL) +
      labs(x = "Temperature (°C)", y = "Bikes rented per hour") +
      theme_minimal()
    as_plotly(p)
  })

  output$precipitation_plot <- renderPlotly({
    p <- seasonal |>
      pivot_longer(c(rainfall, snowfall)) |>
      mutate(name = if_else(name == "rainfall", "Rain (mm)", "Snow (cm)")) |>
      ggplot(aes(SEASON, value, fill = name)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("#2171b5", "#969696"), name = NULL) +
      labs(x = NULL, y = NULL) +
      theme_minimal()
    as_plotly(p)
  })

  output$hourly_plot <- renderPlotly({
    p <- hourly |>
      filter(SEASON %in% input$hourly_seasons) |>
      ggplot(aes(HOUR, avg_rentals, colour = SEASON)) +
      geom_line(linewidth = 1) +
      scale_x_continuous(breaks = seq(0, 23, 3)) +
      scale_colour_manual(values = season_colours, name = NULL) +
      labs(x = "Hour", y = "Average rentals") +
      theme_minimal()
    as_plotly(p)
  })

  output$seasonal_plot <- renderPlotly({
    p <- ggplot(seasonal, aes(SEASON, avg_rentals, fill = SEASON)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = season_colours) +
      labs(x = NULL, y = "Rentals per hour") +
      theme_minimal()
    as_plotly(p)
  })

  output$model_plot <- renderPlotly({
    p <- ggplot(model_results, aes(rmse, reorder(model, -rmse))) +
      geom_col(aes(fill = model == best$model), show.legend = FALSE) +
      scale_fill_manual(values = c("grey70", "#3c8dbc")) +
      labs(x = "RMSE", y = NULL) +
      theme_minimal()
    as_plotly(p)
  })

  output$importance_plot <- renderPlotly({
    p <- ggplot(importance, aes(importance, reorder(variable, importance))) +
      geom_col(fill = "#3c8dbc") +
      labs(x = "Importance (%)", y = NULL) +
      theme_minimal()
    as_plotly(p)
  })

  output$model_table <- renderDT({
    compact_table(model_results, colnames = c("Model", "RMSE", "R²", "MAE"))
  })
}

shinyApp(ui, server)
