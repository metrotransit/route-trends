source("helpers.R")

shinyServer(function(input, output, session) {
  # trends ----
  dataInputTrends <- reactiveValues(data = NULL)

  observeEvent(input$input_trends, {
    inFile <- input$input_trends
    if (is.null(inFile)) return(NULL)
    dataInput <- fread(inFile$datapath, stringsAsFactors = FALSE)
    updateSelectInput(session, 'input_route', choices = c(sort(unique(dataInput[[3]]))), selected = sort(unique(dataInput[[3]]))[1])
    len <- length(unique(lubridate::floor_date(as.Date(dataInput[[1]], "%m/%d/%y"))))
    if (len < 25) {
      updateSelectInput(session, 'fcMethod', choices = c("ETS" = "ets", "ARIMA (longer run time)" = "arima", "TBATS (longer run time)" = 'tbats'))
    } else if (len >= 25 & len < 49) {
      updateSelectInput(session, 'fcMethod', choices = c("ETS" = "ets", "ARIMA (longer run time)" = "arima", "STL using ETS" = "stl-ets",
                                                         "STL using ARIMA" = "stl-arima", "TBATS (longer run time)" = 'tbats',
                                                         "NNETAR (longer run time)" = 'nnet'))
    } else {
      updateSelectInput(session, 'fcMethod', choices = c("ETS" = "ets", "ARIMA (longer run time)" = "arima", "STL using ETS" = "stl-ets",
                                                         "STL using ARIMA" = "stl-arima", "TBATS (longer run time)" = 'tbats',
                                                         "NNETAR (longer run time)" = 'nnet', "Hybrid Model (longer run time)" = 'hybrid'))
    }
    dates <- as.Date(dataInput[[1]], "%m/%d/%y")
    updateDateInput(session, 'start.date', value = min(dates), min = min(dates), max = max(dates))
    updateDateInput(session, 'end.date', value = max(dates), min = min(dates), max = max(dates))
    dataInputTrends$data <- dataInput
  }, ignoreInit = TRUE)

  observeEvent(input$example, {
    example_dataset <- fread("test_dataset.csv", stringsAsFactors = FALSE)
    updateSelectInput(session, 'input_route', choices = c(sort(unique(example_dataset$route))), selected = sort(unique(example_dataset$route))[1])
    len <- length(unique(lubridate::floor_date(as.Date(example_dataset[[1]], "%m/%d/%y"))))
    if (len < 25) {
      updateSelectInput(session, 'fcMethod', choices = c("ETS" = "ets", "ARIMA (longer run time)" = "arima", "TBATS (longer run time)" = 'tbats',
                                                         "Neural Network Time Series (longer run time)" = 'nnet'))
    } else if (len >= 25 & len < 49) {
      updateSelectInput(session, 'fcMethod', choices = c("ETS" = "ets", "ARIMA (longer run time)" = "arima", "STL using ETS" = "stl-ets",
                                                         "STL using ARIMA" = "stl-arima", "TBATS (longer run time)" = 'tbats',
                                                         "NNETAR (longer run time)" = 'nnet'))
    } else {
      updateSelectInput(session, 'fcMethod', choices = c("ETS" = "ets", "ARIMA (longer run time)" = "arima", "STL using ETS" = "stl-ets",
                                                         "STL using ARIMA" = "stl-arima", "TBATS (longer run time)" = 'tbats',
                                                         "NNETAR (longer run time)" = 'nnet', "Hybrid Model (longer run time)" = 'hybrid'))
    }
    dates <- as.Date(example_dataset[[1]], "%m/%d/%y")
    updateDateInput(session, 'start.date', value = min(dates), min = min(dates), max = max(dates))
    updateDateInput(session, 'end.date', value = max(dates), min = min(dates), max = max(dates))
    dataInputTrends$data <- example_dataset
  }, ignoreInit = TRUE)

  dataClean <- reactive({
    req(!is.null(dataInputTrends$data))
    clean_data(dataInputTrends$data)
  })

  output$trendsPreview <- renderTable({
    return(dataInputTrends$data)
  })

  output$trendsPlot <- renderDygraph({
    plot_trends(dataClean(), input$zero_y, input$input_route)
  })

  # stl ----
  output$stl_actual <- renderDygraph({
    plot_stl(dataClean(), plot_series = "actual", input$input_route)
  })

  output$stl_seasonal <- renderDygraph({
    plot_stl(dataClean(), plot_series = "seasonal", input$input_route)
  })

  output$stl_trend <- renderDygraph({
    plot_stl(dataClean(), plot_series = "trend", input$input_route)
  })

  output$stl_remainder <- renderDygraph({
    plot_stl(dataClean(), plot_series = "remainder", input$input_route)
  })

  # forecasts ----
  forecast_item <- reactive({
    plot_forecasts(dataClean(), input$zero_y, input$input_route, input$fcMethod)
  })
  output$forecastsPlot <- renderDygraph({
    forecast_item()[[1]]
  })

  output$mape <- renderText({
    if (input$fcMethod == "hybrid") {
      paste0("Out-of-sample MAPE: ", scales::percent(forecast_item()[[2]]/100))
    } else {
      paste0("In-sample MAPE: ", scales::percent(forecast_item()[[2]]/100))
    }
  })

  output$dl <- downloadHandler(
    filename = 'plot.zip',
    content = function(fname) {
      setwd(tempdir())
      if (input$menu == 'trends') {
        fs <- paste0(paste0(input$input_route, collapse = ","), "_trends.pdf")
        ggsave(fs, plot_trends(dataClean(), input$zero_y, input$input_route, pdf_out = TRUE), width = 10, height = 7)
      } else if (input$menu == "change") {
        fs <- c(paste0(input$start.date, "_", input$end.date, "_change_in_trends.pdf"), paste0(input$start.date, "_", input$end.date, "_percent_change_in_trends.pdf"))
        print(class(plot_delta_trend(dataClean(), route_number = input$input_route, start_date = input$start.date, end_date = input$end.date, pct = F, sortPct = input$sortby, plot_actual = input$plot_actual)))
        ggsave(fs[1], plot_delta_trend(dataClean(), route_number = input$input_route, start_date = input$start.date, end_date = input$end.date, pct = F, sortPct = input$sortby, plot_actual = input$plot_actual),
               width = 10, height = 7)
        ggsave(fs[2], plot_delta_trend(dataClean(), route_number = input$input_route, start_date = input$start.date, end_date = input$end.date, pct = T, sortPct = input$sortby, plot_actual = input$plot_actual),
               width = 10, height = 7)
      } else if (input$menu == 'forecasts') {
        fs <- paste0(paste0(input$input_route, collapse = ","), "_forecasts.pdf")
        ggsave(fs, plot_forecasts(dataClean(), input$zero_y, input$input_route, input$fcMethod, pdf_out = TRUE), width = 10, height = 8)
      } else if (input$menu == 'stl') {
        fs <- paste0(paste0(input$input_route, collapse = ","), c("_actual.pdf", "_seasonal.pdf", "_trend.pdf", "_remainder.pdf"))
        ggsave(fs[1], plot_stl(dataClean(), plot_series = "actual", input$input_route, pdf_out = TRUE), width = 10, height = 5)
        ggsave(fs[2], plot_stl(dataClean(), plot_series = "seasonal", input$input_route, pdf_out = TRUE), width = 10, height = 5)
        ggsave(fs[3], plot_stl(dataClean(), plot_series = "trend", input$input_route, pdf_out = TRUE), width = 10, height = 5)
        ggsave(fs[4], plot_stl(dataClean(), plot_series = "remainder", input$input_route, pdf_out = TRUE), width = 10, height = 5)
      }
      zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )

  # trends across routes ----
  output$plot.current.trend <- renderPlot({
    plot_delta_trend(dataClean(), route_number = input$input_route, start_date = input$start.date, end_date = input$end.date, pct = F, sortPct = input$sortby, plot_actual = input$plot_actual)
  })

  output$plot.current.trend.pct <- renderPlot({
    plot_delta_trend(dataClean(), route_number = input$input_route, start_date = input$start.date, end_date = input$end.date, pct = T, sortPct = input$sortby, plot_actual = input$plot_actual)
  })

  output$data_table <- DT::renderDataTable(
    datatable(plot_delta_trend(dataClean(), start_date = input$start.date, end_date = input$end.date, route_number = input$input_route, tab_out = T)[order(`Percent Change in Trend`)],
              rownames = FALSE, options = list(ordering = TRUE)) %>% formatPercentage(c("Percent Change in Trend", "Percent Change in Ridership")) %>%
      formatCurrency(c(paste(format(floor_date(as.Date(input$start.date), 'month'), "%b %Y"), "Trend"),
                       paste(format(floor_date(as.Date(input$end.date), 'month'), "%b %Y"), "Trend"), "Change in Trend",
                       "Change in Ridership", paste(format(floor_date(as.Date(input$start.date), 'month'), "%b %Y"), "Actual Rides"),
                       paste(format(floor_date(as.Date(input$end.date), 'month'), "%b %Y"), "Actual Rides")), currency = '', mark = ','))
})