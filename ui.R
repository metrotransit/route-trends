source("helpers.R")

shinyUI(
  fluidPage(
    tags$head(
        tags$link(rel = 'icon', type = 'image/png', href = 't_logo.png'),

        tags$style(HTML(".sidebar {
                      height: 97vh; overflow-y: auto;
                      }"
      ) # close HTML
      ) # close tags$style
      ),
    dashboardPage(skin = 'black',
                  dashboardHeader(title = span(tagList(img(src = "t_logo.png", height = '35px'), "Route Trends and Forecasts - User Input Data")), titleWidth = 500),
                  title = 'Route Trends',
                  ## Sidebar ####
                  dashboardSidebar(width = 250,
                                   sidebarMenu(id = 'menu',
                                     menuItem(text = "About", icon = icon("list-alt"), tabName = 'abt'),
                                     menuItem(text = "Load Data", icon = icon("upload"), tabName = 'load'),
                                     menuItem(text = "Trends & Forecasts", icon = icon("line-chart"),
                                              menuSubItem(text = "Trends", tabName = 'trends'),
                                              menuSubItem(text = "Change in Trends", tabName = 'change'),
                                              menuSubItem(text = "Forecasts", tabName = 'forecasts'),
                                              menuSubItem(text = "STL Decomposition", tabName = 'stl')),
                                     menuItem(text = "Feedback", icon = icon("envelope-open"), tabName = 'fb')
                                   ),
                                   tags$hr(),
                                   box(width = 12, background = 'light-blue',
## conditional panels only appear when certain tabs are selected ####
                                       conditionalPanel(
                                         condition = "input.menu == 'trends' | input.menu == 'forecasts'",
                                         checkboxInput('zero_y', "Start Y-Axis at Zero", value = TRUE)
                                       ),
                                       conditionalPanel(
                                         condition = "input.menu == 'trends' | input.menu == 'change' | input.menu == 'forecasts' | input.menu == 'stl'",
                                         selectInput('input_route', "Select Route(s)/Route Type(s)", choices = c(), multiple = TRUE, selectize = FALSE)
                                       ),
                                       conditionalPanel(
                                         condition = "input.menu == 'forecasts'",
                                         selectInput('fcMethod', "Choose Forecasting Method", choices = c()),
                                         tags$hr(),
                                         textOutput('mape')
                                       ),
                                       conditionalPanel(
                                         condition = "input.menu == 'change'",
                                         dateInput('start.date', "Reference Date:", value = Sys.Date()),
                                         dateInput('end.date', "Comparison Date:", value = Sys.Date()),
                                         checkboxInput('sortby', "Sort by Percentage Change"),
                                         checkboxInput('plot_actual', "Plot Actual Rides (Default to Trends)")
                                       ),
                                       conditionalPanel(
                                         condition = "input.menu == 'trends' | input.menu == 'change' | input.menu == 'forecasts' | input.menu == 'stl'",
                                         tags$hr(),
                                         downloadButton('dl', "Download Plot"),
                                         tags$hr(),
                                         p("See", strong("Methodology"), "section in", strong("About"), "tab for descriptions of and links to the different statistical methods.")
                                       )
                                       )
                  ),
                    ## body of tabs ####
                  dashboardBody(
                    tabItems(
                      ## "About" tab ####
                      tabItem(
                        'abt',
                        box(width = 6, status = 'primary', solidHeader = TRUE, title = "What this app does",
                            h5(strong("Load Data"), style = "color:#0053A0"),
                            tags$ul(
                              tags$li(p("This is where you upload your own dataset.")),
                              tags$li(p("If not sure how to format your dataset for upload, there is an example dataset available."))
                            ),
                            h5(strong("Trends & Forecasts"), style = "color:#0053A0"),
                            tags$ul(
                              tags$li(p("Once the data are uploaded, you can look at route-level or aggregate-level ridership trends, forecasts, change in trends between any two points in time, and
                                        look at how actual data are decomposed to get the trends"))
                            ),
                            h5(strong("Feedback"), style = "color:#0053A0"),
                            tags$ul(
                              tags$li(p("You can rate the application and send us any feedback, comment, or suggestion. We highly recommend that
        you leave us feedback so we can further improve this app."))
                              ),
                            tags$hr(),
                            em(paste0("This app was last updated on ", max(file.info('ui.R')$mtime, file.info('server.R')$mtime,
                                                                           file.info('helpers.R')$mtime),
                                      "."))
                            ),
                        box(width = 6, status = 'success', solidHeader = TRUE, title = "New",
                            tags$ul(
                              tags$li(p("You can now download plots as shown on the app."))
                            )
                            ),
                        box(width = 6, status = 'primary', solidHeader = TRUE, title = "Methodology",
                            p("Trends are calculated using ", tags$a(href = "https://www.otexts.org/fpp/6/5", "Seasonal-Trend Decomposition Procedure Based on Loess (STL Decomposition)"), ".
                        STL Decomposition is a filtering procedure for decomposing a seasonal time series into three compenents:
                              trend, seasonal, and remainder."),
                            p("In this app, we include six different forecasting methods:"),
                            tags$ul(
                              tags$li(p(tags$a(href = "https://www.otexts.org/fpp/7/7", "Exponential Smoothing State Space (ETS)"), ": needs at least 24 monthly observations.")),
                              tags$li(p(tags$a(href = "https://www.otexts.org/fpp/8/5", "Autoregressive Integrated Moving Average (ARIMA)"), ": needs at least 24 monthly observations.")),
                              tags$li(p(tags$a(href = "https://www.otexts.org/fpp/6/6", "STL using ETS"), ": needs at least 25 monthly observations.")),
                              tags$li(p(tags$a(href = "https://www.otexts.org/fpp/6/6", "STL using ARIMA"), ": needs at least 25 monthly observations.")),
                              tags$li(p(tags$a(href = "http://robjhyndman.com/papers/ComplexSeasonality.pdf", "Exponential Smoothing State Space model with Box-Cox Tranformation, ARMA errors, Trend and Seasonal Components
                                               (TBATS)"), ": needs at least 24 monthly observations.")),
                              tags$li(p(tags$a(href = "https://www.otexts.org/fpp/9/3", "Neural Network Time Series (NNETAR)"), ": need at least 25 monthly observations.")),
                              tags$li(p(tags$a(href = "https://cran.r-project.org/web/packages/forecastHybrid/forecastHybrid.pdf", "Hybrid Model of ETS, NNETAR, STL using ARIMA, and TBATS"), ": whose weights are determined by cross-validated
                                        root mean square error (RMSE), and needs at least 49 monthly observations.")

                              )
                              ),
                            p("To access the accuracy of the forecasting models, in-sample mean absolute percentage error (MAPE) is calculated. MAPE is the average (mean) percentage difference between
                              actual and predicted values. Small MAPE is preferable.")
                            )
                      ),
                      ## "Load Data" tab ####
                     tabItem(
                       'load',
                       box(width = 6,
                           fileInput('input_trends', "Choose CSV File:"),
                           tags$hr(),
                           p("Please upload a CSV file that consists of three columns:"),
                           tags$ol(
                             tags$li(p("first column is Date in '%m/%d/%y' format (e.g. 5/1/17)")),
                             tags$li(p("second column is Ridership")),
                             tags$li(p("third column is the corresponding Route Number or Route Type."))
                           ),
                           p("You can upload the ridership data for as many routes as you want. The input data can be in monthly or in daily frequency (which will aggregate to monthly levels).
                             Click button below to see a data format example as well as its corresponding trend and forecast. "),
                           actionButton('example', "See example!")
                           ),
                       box(width = 6,
                           h2("Data Preview"),
                           tableOutput('trendsPreview')
                           )
                     ),
                     ## "Trends & Forecasts" group ####
                     ## "Trends" tab ####
                     tabItem(
                       'trends',
                       box(width = 12, height = '800px',
                           dygraphOutput('trendsPlot', height = '750px')
                           )
                     ),
                     ## "Change in Trends" tab ####
                      tabItem(
                       'change',
                       plotOutput('plot.current.trend', height = '500px'),
                       plotOutput('plot.current.trend.pct', height = '500px'),
                       tags$hr(),
                       h2("Data Table"),
                       DT::dataTableOutput("data_table")
                     ),
                     ## "Forecasts" tab ####
                     tabItem(
                       'forecasts',
                       box(width = 12, height = '800px',
                           dygraphOutput('forecastsPlot', height = '750px')
                           )
                     ),
                     ## "STL Decomposition" tab ####
                     tabItem(
                       'stl',
                       dygraphOutput('stl_actual', height = '300px'),
                       dygraphOutput('stl_seasonal', height = '300px'),
                       dygraphOutput('stl_trend', height = '300px'),
                       dygraphOutput('stl_remainder', height = '300px')
                     ),
                     ## "Feedback" tab ####
                     tabItem(
                       'fb',
                       box(width = 6,
                           p('Would you like to see a new feature? Have you found a problem with the app?'),
                           tags$a(href = "mailto:Eric.Lind@metrotransit.org?
                                  body=''
                                  &subject='user upload STL App Feedback'", "Email Us!"),
                           br(),
                           p('If you are emailing about a problem with the app, please include a screenshot of the error message/the page you are on,
                             and/or explain how you got to the error.')
                           # )
                           ))
                    )
                  )
    )
  )
)