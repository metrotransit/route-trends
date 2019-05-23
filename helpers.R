library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(dygraphs)
library(forecast)
library(forecastHybrid)
library(lubridate)
library(scales)
library(ggplot2)

# functions ----
clean_data <- function(dataInput) {
  if (is.null(dataInput)) stop("Please go to Load Data tab and upload your dataset in the specified format or click on 'See example!' button first.")
  if (sum(is.na(dataInput)) > 0) return(stop("Missing values are not allowed. Please re-upload data with no missing values."))
  setDT(dataInput)
  setnames(dataInput, c("ym", "rides", 'route'))
  dataInput[, ym := lubridate::floor_date(as.Date(ym, format = "%m/%d/%y"), 'month')]
  dataInput[, rides := as.numeric(gsub(",| ", "", rides))]
  dataInput <- dataInput[, .(rides = sum(rides)), keyby = .(route, ym)][order(route, ym)]
  return(dataInput)
}
plot_trends <- function(dataInput, zero_y = T, input_route, pdf_out = FALSE) {
  dataInput <- dataInput[route %in% input_route, .(rides = sum(rides)), by = .(ym)]
  if (nrow(dataInput) < 25) stop("At least 25 monthly observations is needed.")
  MonthYear <- as.Date(dataInput$ym)
  rides <- as.numeric(dataInput$rides)
  rides_ts <- ts(rides, start = c(year(min(MonthYear)), month(min(MonthYear))), freq = 12)
  stl_rides <- stl(rides_ts, "per")
  trend_rides <- as.data.frame(stl_rides$time.series[1:length(rides_ts), 2])
  ### Merging both together
  route_decomp <- data.table(MonthYear, round(trend_rides, 0), round(rides, 0))
  setnames(route_decomp, c("MonthYear", "Ridership Trend", "Actual Ridership"))
  route_decomp[, MonthYear := as.Date(MonthYear)]

  ## Set the Y axis limits for the Graphs
  g1_min <- ifelse(zero_y, 0, 0.9*min(route_decomp$`Actual Ridership`))
  g1_max <- 1.1*max(route_decomp$`Actual Ridership`)

  if (pdf_out) {
    ggplot(data = route_decomp) +
      geom_line(aes(x = MonthYear, y = `Actual Ridership`), color = "#666666") +
      geom_line(aes(x = MonthYear, y = `Ridership Trend`), color = "#0053A0", linetype = 2, size = 1) +
      theme_bw() +
      scale_y_continuous(limits = c(g1_min, g1_max)) +
      labs(x = "", y = "Rides", title = "Ridership Trend")
  } else {
    dygraph(route_decomp, ylab = "Rides", main = "Ridership Trend", group = "Trends") %>%
      dySeries("Ridership Trend", strokeWidth = 3, strokePattern = "dashed", color = "#0053A0", label = "Ridership Trend") %>%
      dySeries("Actual Ridership", strokeWidth = 2, color = "#666666", label = "Actual Ridership") %>%
      dyAxis("y", axisLabelFormatter=JS("function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}"),
             valueFormatter=JS("function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}"),
             valueRange = c(g1_min, g1_max)) %>%
      dyOptions(gridLineColor = "lightgray", titleHeight = 30) %>%
      dyRangeSelector()
  }
}

plot_stl <- function(dataInput, plot_series = c("actual", "seasonal", "trend", "remainder"), input_route, pdf_out = FALSE) {
  dataInput <- dataInput[route %in% input_route, .(rides = sum(rides)), by = .(ym)]
  if (nrow(dataInput) < 25) stop("At least 25 monthly observations is needed.")
  MonthYear <- as.Date(dataInput$ym)
  rides <- as.numeric(dataInput$rides)
  rides_ts <- ts(rides, start = c(year(min(MonthYear)), month(min(MonthYear))), freq = 12)
  stl_rides <- stl(rides_ts, "per")

  #extract stl decomposition components
  seasonal <- data.table(month = MonthYear, seasonal = stl_rides$time.series[, 1])
  trend <- data.table(month = MonthYear, trend = stl_rides$time.series[, 2])
  remainder <- data.table(month = MonthYear, remainder = stl_rides$time.series[, 3])
  actual.data <- data.table(MonthYear, rides)

  if (plot_series == "actual") {
    if (pdf_out) {
      ggplot(data = actual.data) +
        geom_line(aes(x = MonthYear, y = rides), color = "#0053A0", size = 1) +
        geom_vline(xintercept = as.numeric(as.Date(paste0(seq(year(min(MonthYear)), year(max(MonthYear)), 1), '-01-01'))), linetype = 2, size = .3) +
        theme_bw() +
        labs(x = "", y = "Actual Data")
    } else {
      dygraph(actual.data, ylab = "Actual Data", group = "stl") %>%
        dyOptions(colors = "#0053A0", gridLineColor = "lightgray", strokeWidth = 3) %>%
        dyEvent(paste0(seq(year(min(MonthYear)), year(max(MonthYear)), 1), '-01-01'), color = "black") %>%
        dyRangeSelector()
    }
  } else if (plot_series == "seasonal") {
    if (pdf_out) {
      ggplot(data = seasonal) +
        geom_bar(aes(x = month, y = seasonal), stat = 'identity', fill = "#0053A0") +
        geom_vline(xintercept = as.numeric(as.Date(paste0(seq(year(min(MonthYear)), year(max(MonthYear)), 1), '-01-01'))), linetype = 2, size = .3) +
        theme_bw() +
        labs(x = "", y = "Seasonality")
    } else {
      dygraph(seasonal, ylab = "Seasonality", group = "stl") %>%
        dyOptions(useDataTimezone = TRUE, plotter =
                    "function barChartPlotter(e) {
                  var ctx = e.drawingContext;
                  var points = e.points;
                  var y_bottom = e.dygraph.toDomYCoord(0);  // see     http://dygraphs.com/jsdoc/symbols/Dygraph.html#toDomYCoord

                  // This should really be based on the minimum gap
                  var bar_width = 2/3 * (points[1].canvasx - points[0].canvasx);
                  ctx.fillStyle = e.color;

                  // Do the actual plotting.
                  for (var i = 0; i < points.length; i++) {
                  var p = points[i];
                  var center_x = p.canvasx;  // center of the bar

                  ctx.fillRect(center_x - bar_width / 2, p.canvasy,
                  bar_width, y_bottom - p.canvasy);
                  ctx.strokeRect(center_x - bar_width / 2, p.canvasy,
                  bar_width, y_bottom - p.canvasy);
                  }
    }", gridLineColor = "lightgray", colors = "#0053A0") %>%
        dyEvent(paste0(seq(year(min(MonthYear)), year(max(MonthYear)), 1), '-01-01'), color = "black")
    }
} else if (plot_series == "trend") {
  if (pdf_out) {
    ggplot(data = trend) +
      geom_line(aes(x = month, y = trend), color = "#0053A0", size = 1) +
      geom_vline(xintercept = as.numeric(as.Date(paste0(seq(year(min(MonthYear)), year(max(MonthYear)), 1), '-01-01'))), linetype = 2, size = .3) +
      theme_bw() +
      labs(x = "", y = "Trend")
  } else {
    dygraph(trend, ylab = "Trend", group = "stl") %>%
      dyOptions(gridLineColor = "lightgray", colors = "#0053A0", strokeWidth = 3) %>%
      dyEvent(paste0(seq(year(min(MonthYear)), year(max(MonthYear)), 1), '-01-01'), color = "black")
  }
} else if (plot_series == "remainder") {
  if (pdf_out) {
    ggplot(data = remainder) +
      geom_bar(aes(x = month, y = remainder), stat = 'identity', fill = "#A9A9A9") +
      geom_vline(xintercept = as.numeric(as.Date(paste0(seq(year(min(MonthYear)), year(max(MonthYear)), 1), '-01-01'))), linetype = 2, size = .3) +
      theme_bw() +
      labs(x = "", y = "Remainder")
  } else {
    dygraph(remainder, ylab = "Remainder", group = "stl") %>%
      dyOptions(useDataTimezone = TRUE, plotter =
                  "function barChartPlotter(e) {
                var ctx = e.drawingContext;
                var points = e.points;
                var y_bottom = e.dygraph.toDomYCoord(0);  // see     http://dygraphs.com/jsdoc/symbols/Dygraph.html#toDomYCoord

                // This should really be based on the minimum gap
                var bar_width = 2/3 * (points[1].canvasx - points[0].canvasx);
                ctx.fillStyle = e.color;

                // Do the actual plotting.
                for (var i = 0; i < points.length; i++) {
                var p = points[i];
                var center_x = p.canvasx;  // center of the bar

                ctx.fillRect(center_x - bar_width / 2, p.canvasy,
                bar_width, y_bottom - p.canvasy);
                ctx.strokeRect(center_x - bar_width / 2, p.canvasy,
                bar_width, y_bottom - p.canvasy);
                }
  }", gridLineColor = "lightgray", colors = "#A9A9A9") %>%
      dyEvent(paste0(seq(year(min(MonthYear)), year(max(MonthYear)), 1), '-01-01'), color = "black")
  }
  }
}

plot_forecasts <- function(dataInput, zero_y, input_route, fcMethod, pdf_out = FALSE) {
  dataInput <- dataInput[route %in% input_route, .(rides = sum(rides)), by = .(ym)]
  MonthYear <- as.Date(dataInput$ym)
  rides <- as.numeric(dataInput$rides)
  rides_ts <- ts(rides, start = c(year(min(MonthYear)), month(min(MonthYear))), freq = 12)
  if (fcMethod == "ets") {
    mod <- ets(rides_ts)
    model_temp <- as.data.table(forecast(mod, h = 24))[, c('Point Forecast', 'Lo 95', 'Hi 95')]
    setnames(model_temp, c("rides", 'low', 'high'))
  } else if (fcMethod == "arima") {
    mod <- auto.arima(rides_ts, stepwise = F)
    model_temp <- as.data.table(forecast(mod, h = 24))[, c('Point Forecast', 'Lo 95', 'Hi 95')]
    setnames(model_temp, c("rides", 'low', 'high'))
  } else if (fcMethod == "stl-ets") {
    mod <- stlm(rides_ts, method = 'ets')
    model_temp <- as.data.table(forecast(mod, h = 24))[, c('Point Forecast', 'Lo 95', 'Hi 95')]
    setnames(model_temp, c("rides", 'low', 'high'))
  } else if (fcMethod == "stl-arima") {
    mod <- stlm(rides_ts, method = 'arima')
    model_temp <- as.data.table(forecast(mod, h = 24))[, c('Point Forecast', 'Lo 95', 'Hi 95')]
    setnames(model_temp, c("rides", 'low', 'high'))
  } else if (fcMethod == "tbats") {
    mod <- tbats(rides_ts, use.parallel = TRUE)
    model_temp <- as.data.table(forecast(mod, h = 24))[, c('Point Forecast', 'Lo 95', 'Hi 95')]
    setnames(model_temp, c("rides", 'low', 'high'))
  } else if (fcMethod == "nnet") {
    mod <- nnetar(rides_ts)
    model_temp <- as.data.table(forecast(mod, h = 24, level = 95, PI = TRUE))[, c('Point Forecast', 'Lo 95', 'Hi 95')]
    setnames(model_temp, c("rides", "low", "high"))
  } else if (fcMethod == "hybrid") {
    mod <- hybridModel(rides_ts, models = "enst",
                              s.args = list(method = 'arima'),
                              errorMethod = "RMSE",
                              weights = "cv.errors",
                              cvHorizon = 12,
                              windowSize = length(rides_ts) - 24,
                              parallel = TRUE)
    model_temp <- as.data.table(forecast(mod, h = 24))[, c('Point Forecast', 'Lo 95', 'Hi 95')]
    setnames(model_temp, c("rides", 'low', 'high'))
  }
  model_temp[, ym := seq.Date(max(MonthYear), length.out = 25, by = 'month')[2:25]]
  for_plot <- rbind(dataInput, model_temp, fill = TRUE)
  cutoff <- max(MonthYear)

  post_trend <- stl(ts(for_plot$rides, start = c(year(min(MonthYear)), month(min(MonthYear))), freq = 12), 'per')$time.series[, "trend"]
  for_plot <- for_plot[, trend := post_trend][, .(ym, rides = round(rides, 0), low = round(low, 0), high = round(high, 0), trend = round(trend, 0))]

  y_min <- 0.9*ifelse(zero_y, 0, min(for_plot[, .(rides, low, high)], na.rm = TRUE))
  y_max <- 1.1*max(for_plot[, .(rides, low, high)], na.rm = TRUE)

  if (pdf_out) {
    ggplot(data = for_plot) +
      geom_ribbon(aes(x = ym, ymin = low, ymax = high), color = "#D3D3D3", fill = "#D3D3D3") +
      geom_line(aes(x = ym, y = rides), color = "#666666", size = .6) +
      geom_line(aes(x = ym, y = trend), color = "#0053A0", size = 1, linetype = 2) +
      geom_vline(xintercept = as.numeric(cutoff), linetype = 2) +
      theme_bw() +
      scale_y_continuous(limits = c(y_min, y_max)) +
      labs(x = "", y = "Rides", title = "Ridership Forecast")
  } else {
    p <- dygraph(for_plot, ylab = "Rides", main = "Ridership Forecast") %>%
      dySeries(c('low', 'rides', 'high'), label = "Rides", strokeWidth = 2, color = "#666666") %>%
      dyEvent(cutoff) %>%
      dyAxis("y", axisLabelFormatter = "function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}",
             valueFormatter = "function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}",
             valueRange = c(y_min, y_max)) %>%
      dySeries("trend", label = "Trend", strokeWidth = 3, strokePattern = "dashed", color = "#0053A0") %>%
      dyRangeSelector()
    return(list(p = p, mape = accuracy(mod)[5]))
  }
}

get_trend <- function(route_number, routes_avg_wk) {
  route_data <- routes_avg_wk[route == route_number]
  if (nrow(route_data) < 25) stop("At least 25 monthly observations is needed.")
  ts_rides <- ts(route_data$rides, start = c(year(route_data[, min(ym)]), month(route_data[, min(ym)])), frequency = 12)
  stl_rides <- stl(ts_rides, "per")
  trend_rides <- as.data.table(stl_rides$time.series)[, trend]
  return(data.table(route_data, trend = trend_rides))
}

plot_delta_trend <- function(routes_avg_wk, route_number, start_date, end_date, pct = F, sortPct = F, tab_out = F, plot_actual = F) {
  start_date <- floor_date(as.Date(start_date), 'month')
  end_date <- floor_date(as.Date(end_date), 'month')
  if (end_date > max(routes_avg_wk$ym)) {
    stop(paste0("Dates out of bound."))
  } else if (end_date <= start_date) {
    stop("End date has to be larger than the start date.")
  }
  trend_table <- rbindlist(lapply(as.list(route_number), get_trend, routes_avg_wk))
  start_table <- trend_table[route %in% route_number & ym == start_date]
  end_table <- trend_table[route %in% route_number & ym == end_date]
  merged_table <- start_table[end_table, on = .(route)][, `:=` (yoyTrend = round(i.trend - trend, 0),
                                                                yoyTrendPct = i.trend / trend - 1,
                                                                yoyRides = round(i.rides - rides, 0),
                                                                yoyRidesPct = i.rides / rides -1)]
  if (sortPct) {
    if (plot_actual) {
      setkey(merged_table, yoyRidesPct)
    } else {
      setkey(merged_table, yoyTrendPct)
    }
  } else {
    if (plot_actual) {
      setkey(merged_table, yoyRides)
    } else {
      setkey(merged_table, yoyTrend)
    }
  }
  merged_table <- within(merged_table, route <- factor(route, levels = route))
  if (pct) {
    if (plot_actual) {
      (ggplot(merged_table, aes(x = as.factor(route), y = yoyRidesPct, colour = yoyRidesPct > 0, fill = yoyRidesPct > 0)) +
              geom_bar(stat = "identity") +
              xlab("Route Number") +
              ylab(paste0("Percentage Change in Ridership from ", format(start_date, "%B %Y"), " to ", format(end_date, "%B %Y"))) +
              ggtitle(paste0("Percentage Change in Ridership from ", format(start_date, "%B %Y"), " to ", format(end_date, "%B %Y"))) +
              scale_y_continuous(label = percent, limits = c(min(merged_table$yoyRidesPct, 0), max(merged_table$yoyRidesPct, 0))) +
              scale_colour_manual(values = setNames(c("#0053A0", "#ED1B2E"), c(T, F))) +
              scale_fill_manual(values = setNames(c("#0053A0", "#ED1B2E"), c(T, F))) +
              theme(legend.position = 'none', axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 14),
                    plot.title = element_text(size = 22)) +
              geom_text(aes(label = percent(yoyRidesPct))) +
              geom_label(aes(label = percent(yoyRidesPct)), color = "white", fontface = 'bold'))
    } else {
      (ggplot(merged_table, aes(x = as.factor(route), y = yoyTrendPct, colour = yoyTrendPct > 0, fill = yoyTrendPct > 0)) +
              geom_bar(stat = "identity") +
              xlab("Route Number") +
              ylab(paste0("Percentage Change in Trend from ", format(start_date, "%B %Y"), " to ", format(end_date, "%B %Y"))) +
              ggtitle(paste0("Percentage Change in Trend from ", format(start_date, "%B %Y"), " to ", format(end_date, "%B %Y"))) +
              scale_y_continuous(label = percent, limits = c(min(merged_table$yoyTrendPct, 0), max(merged_table$yoyTrendPct, 0))) +
              scale_colour_manual(values = setNames(c("#0053A0", "#ED1B2E"), c(T, F))) +
              scale_fill_manual(values = setNames(c("#0053A0", "#ED1B2E"), c(T, F))) +
              theme(legend.position = 'none', axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 14),
                    plot.title = element_text(size = 22)) +
              geom_text(aes(label = percent(yoyTrendPct))) +
              geom_label(aes(label = percent(yoyTrendPct)), color = "white", fontface = 'bold'))
    }

  } else if (tab_out) {
    merged_table[, `:=` (rides = round(rides, 0), i.rides = round(i.rides, 0),
                         yoyRides = round(yoyRides, 0),
                         trend = round(trend, 0), i.trend = round(i.trend, 0),
                         yoyTrend = round(yoyTrend, 0))][, c("ym", "i.ym") := NULL]
    out_table <- merged_table[, .(route, rides, i.rides, trend, i.trend, yoyRides, yoyRidesPct, yoyTrend, yoyTrendPct)]
    setnames(out_table, c("Route Number", paste(format(start_date, "%b %Y"), "Actual Rides"), paste(format(end_date, "%b %Y"), "Actual Rides"),
                             paste(format(start_date, "%b %Y"), "Trend"), paste(format(end_date, "%b %Y"), "Trend"),
                          "Change in Ridership", "Percent Change in Ridership", "Change in Trend", "Percent Change in Trend"))
    return(out_table)
  } else {
    if (plot_actual) {
      (ggplot(merged_table, aes(x = as.factor(route), y = yoyRides, colour = yoyRides > 0, fill = yoyRides > 0)) +
              geom_bar(stat = "identity") +
              xlab("Route Number") +
              ylab(paste0("Change in Ridership from ", format(start_date, "%B %Y"), " to ", format(end_date, "%B %Y"))) +
              ggtitle(paste0("Change in Ridership from ", format(start_date, "%B %Y"), " to ", format(end_date, "%B %Y"))) +
              scale_y_continuous(label = comma, limits = c(min(merged_table$yoyRides, -10), max(merged_table$yoyRides, 10))) +
              scale_colour_manual(values = setNames(c("#0053A0", "#ED1B2E"), c(T, F))) +
              scale_fill_manual(values = setNames(c("#0053A0", "#ED1B2E"), c(T, F))) +
              theme(legend.position = 'none', axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 14),
                    plot.title = element_text(size = 22)) +
              geom_text(aes(label = yoyRides)) +
              geom_label(aes(label = yoyRides), color = "white", fontface = 'bold'))
    } else {
      (ggplot(merged_table, aes(x = as.factor(route), y = yoyTrend, colour = yoyTrend > 0, fill = yoyTrend > 0)) +
              geom_bar(stat = "identity") +
              xlab("Route Number") +
              ylab(paste0("Change in Trend from ", format(start_date, "%B %Y"), " to ", format(end_date, "%B %Y"))) +
              ggtitle(paste0("Change in Trend from ", format(start_date, "%B %Y"), " to ", format(end_date, "%B %Y"))) +
              scale_y_continuous(label = comma, limits = c(min(merged_table$yoyTrend, -10), max(merged_table$yoyTrend, 10))) +
              scale_colour_manual(values = setNames(c("#0053A0", "#ED1B2E"), c(T, F))) +
              scale_fill_manual(values = setNames(c("#0053A0", "#ED1B2E"), c(T, F))) +
              theme(legend.position = 'none', axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 14),
                    plot.title = element_text(size = 22)) +
              geom_text(aes(label = yoyTrend)) +
              geom_label(aes(label = yoyTrend), color = "white", fontface = 'bold'))
    }
  }
}

