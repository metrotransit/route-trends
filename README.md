# Authors

Creator: Joel Huting\
Authors: Joey Reid, [Kim Eng Ky](https://github.com/kykimeng), [Eric Lind](https://github.com/elindie)\
Sponsoring Institution: [Metro Transit](https://www.metrotransit.org), Minneapolis-St. Paul, MN USA

# Purpose

R Shiny app to ingest ridership time series, and return:  

* seasonal, trend, and residual components according to STL methodology
* forecasts including uncertainty based on those components

The main purpose of this app is to facilitate use of the timeseries methodology
by transit agencies interested in better understanding their ridership data. The
descriptions and examples are thus focused on counts of riders, by route, on a monthly 
basis, matching required submissions to the [National Transit Database](https://www.transit.dot.gov/ntd)
where many such timeseries can be found.

# Methods
### R, R Studio, and Shiny 
This repository contains code to be run in [RStudio](https://www.rstudio.com/resources/training/), the IDE built for the statistical language R.
The code includes R packages developed by others (see list of packages at the end of this README),
including [Shiny](https://shiny.rstudio.com/tutorial/), a package to build interactive web apps
in html and java, from within the R environment. 

### Data format  
The app requires a datafile in CSV with the following columns, _in this order_:  

* Date in '%m/%d/%y' format (e.g. 5/1/17)  
* Ridership in numeric or integer format  
* Identifier (route, mode, route type) in string format

### Analysis methods
Trends are calculated using ["Seasonal-Trend Decomposition Procedure Based on Loess (STL)"](https://otexts.com/fpp2/stl.html)
STL Decomposition is a filtering procedure for decomposing a seasonal time series into three compenents:  

* **trend**: this is typically of most interest to transit agencies. what is the long-term
trend of a given route?

* **seasonal**: this is of interest but typically known by transit agencies. Ridership
may be typically higher in fall than mid-summer; routes serving university campuses 
may rise and fall with the academic calendar. By incorporating these regularities into
the time series analsyis, the trend can be better understood independent of the seasonality.
This represents one key advance over year-over-year same-month comparisons, which 
are industry standard practice for dealing with the seasonality of ridership.

* **remainder**: remainders which are small indicate random variation around the trend
given the season (month). Remainders which are high, and remainders which are strongly 
in one direction for multiple months in a row, may indicate an inflection in ridership
or other non-stationarity.

#### Approaches for extracting STL components:
In this app, we include six different forecasting methods. Each method has its strengths
and may fit better to a given timeseries. Note the timeseries length requirements 
can differ among approaches, but all generally require at least two years or 25 months
of data to be estimated.  

* [Autoregressive Integrated Moving Average (ARIMA)](https://otexts.com/fpp2/non-seasonal-arima.html): needs at least 24 monthly observations  
* [STL using ARIMA](https://otexts.com/fpp2/seasonal-arima.html): needs at least 25 monthly observations
* [Exponential Smoothing State Space (ETS)](https://www.otexts.org/fpp/7/7): needs at least 24 monthly observations
* [STL using ETS](https://otexts.com/fpp2/arima-ets.html): needs at least 25 monthly observations
* [Exponential Smoothing State Space model with Box-Cox Tranformation, ARMA errors, Trend and Seasonal Components 
                                               (TBATS)](http://robjhyndman.com/papers/ComplexSeasonality.pdf): needs at least 24 monthly observations
* [Neural Network Time Series (NNETAR)](https://otexts.com/fpp2/nnetar.html): need at least 25 monthly observations
* [Hybrid forecasts: model average of ETS, NNETAR, STL using ARIMA, and TBATS](https://cran.r-project.org/web/packages/forecastHybrid/): need at least 49 monthly observations, as model weights are
determined by cross-validated root mean square error (RMSE)

#### Deciding on best-fit models for your timeseries  
The simplest determinant of the accuracy of forecasting models is in-sample 
mean absolute percentage error (MAPE).MAPE is the average (mean) percentage difference between 
actual and predicted values. Small MAPE is preferable, and can be compared across
different models forecasting the same time series. 
                           
## R Packages Used  

* [data.table](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html) read, write, join and aggregate data
* [DT](https://cran.r-project.org/web/packages/DT) interactive data table 
displays
* [dygraphs](https://cran.r-project.org/web/packages/dygraphs/) flexible interactive timeseries plotting
* [forecast](https://cran.r-project.org/package=forecast) timeseries modeling and forecasting
* [forecastHybrid](https://cran.r-project.org/web/packages/forecastHybrid/) ensemble and model averaging
for timeseries models
* [ggplot2](https://ggplot2.tidyverse.org/) static graphics
* [lubridate](https://cran.r-project.org/web/packages/lubridate/) parsing of date strings
* [scales](https://cran.r-project.org/web/packages/scales/) effective data breaks and labels
* [shiny](https://shiny.rstudio.com/) interactive website
* [shinydashboard](https://rstudio.github.io/shinydashboard/) app theme
