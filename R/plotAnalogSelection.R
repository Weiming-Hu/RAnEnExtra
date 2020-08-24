# "`-''-/").___..--''"`-._
#  (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
#  (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#    _ ..`--'_..-_/  /--'_.' ,'
#  (il),-''  (li),'  ((!.-'
#
# Author: Guido Cervone <cervone@psu.edu>
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University
#

#' RAnEnExtra::plotAnalogSelection
#'
#' RAnEnExtra::plotAnalogSelection visualize the selection of analog ensemble members.
#' It shows how ensemble members are selected based on the similarity metrics.
#' It also shows the selected forecasts for each parameter used during the
#' analog generation, compared with the other unselected forecasts.
#'
#' @param forecasts The forecasts data array
#' @param fcst.times Forecast times
#' @param sims The similarity member from analogs.
#' @param sims.index The similarity index member from analogs.
#' @param i.station The station index from analogs.
#' @param i.test.day The test day index from analogs.
#' @param i.flt The FLT index from analogs.
#' @param parameter.names The parameter names that are associated with forecasts.
#' @param num.analogs Number of analogs
#' @param weights The weights used for AnEn
#' @param cex.lab The font size of labels.
#' @param pch.selected The point type for selected points.
#' @param pch.unselected The point type for unselected points.
#' @param pch.current The point type for current forecasts.
#' @param col.selected The color for selected points.
#' @param col.unselected The color for unselected points.
#' @param col.current The color for current forecasts.
#' @param col.reference The color for the reference line.
#' @param lty.reference The line type for the reference line.
#' @param lwd.reference The line width for the reference line.
#' @param as.POSIXct Whether the time information should be converted to R date/time.
#' @param origin The origin for times in forecasts. This will be passed to as.POSIXct to convert
#' times in forecasts to date/time objects.
#' @param tz The time zone in forecasts. This will be passed to as.POSIXct to convert
#' times in forecasts to date/time objects.
#' @param single.figure Whether to generate only a single figure.
#' @param mar Margins for subplots. This is effective only when single.figure is TURE.
#' It is passed to the function par.
#' @param omi Outer margin for the plot. This is effective only when single.figure is TURE.
#' It is passed to the function par.
#' @param use.plotly Whether to use plotly. The plotly referrence can be found
#' [here](https://plot.ly/r/reference/).
#' @param hovermode The hover mode for visualization. This is only effective when plotly is used.
#' @param spikemode The spike mode for visualization. This is only effective when plotly is used.
#' @param spikethickness The spike thickness for visualization. This is only effective when
#' plotly is used.
#' @param spikedash The spike dash style or the length in px for visualization. This is
#' only effective when plotly is used.
#' @param spikecolor The spike color for visualization. This is only effective when
#' plotly is used.
#' @param prevent.search.future The `prevent_search_future` member from `Config`. This controls
#' whether the reference line will be extended both way.
#' @param sim.ylim The limit for the similairty panel
#' @param add.sim Whether to add the similarity panel at the top
#'
#' @author Weiming Hu \email{cervone@@psu.edu}
#' @author Laura CLemente-Harding \email{lec170@@psu.edu}
#' @author Martina Calovi \email{mcalovi@@psu.edu}
#' @author Guido Cervone \email{cervone@@psu.edu}
#'
#' @md
#' @export
plotAnalogSelection <- function(
  forecasts, fcst.times,
  sims, sims.index,
  test.times, search.times,
  i.station, i.test.day, i.flt,
  parameter.names, num.analogs, weights,

  sim.ylim = NULL,
  add.sim = TRUE,

  cex.lab = 1.5,
  pch.selected = 16,
  pch.unselected = 16,
  pch.current = 1,

  col.selected = 'red',
  col.unselected = 'lightgrey',
  col.reference = 'black',
  col.current = 'black',

  lty.reference = 'longdash',
  lwd.reference = 2,

  as.POSIXct = T,
  origin = '1970-01-01',
  tz = 'UTC',

  single.figure = T,
  mar = c(1.5, 5, 1, 1) + 0.1,
  omi = c(.3, 0, 0, 0),

  use.plotly = F,
  hovermode = 'compare',
  spikemode = 'across',
  spikethickness = 1,
  spikedash = 'solid',
  spikecolor = 'grey',
  prevent.search.future = TRUE) {

  # Check input parameters
  if (length(parameter.names) != dim(forecasts)[1]) {
    stop("Parameter names and the first dimension of forecasts do not match.")
  }
  if (use.plotly) {
    check.package('plotly')
    check.package('magrittr')
    `%>%` <- magrittr::`%>%`
  }

  # Remove the parameters with weight equals to 0
  if (length(weights) == 0) {
    parameter.names.used <- parameter.names
  } else {
    parameter.names.used <- parameter.names[which(weights != 0)]
  }

  # Get the similarity matrix of this particular station, test day, and flt
  sims <- sims[i.station, i.test.day, i.flt, ]
  sims.index <- sims.index[i.station, i.test.day, i.flt, ]

  # Remove NA rows
  valid.pos <- which(!is.na(sims))
  sims <- sims[valid.pos]
  sims.index <- sims.index[valid.pos]

  # Get the start and end index for searching in the forecasts
  test.start <- which(fcst.times == test.times[1])
  test.end <- which(fcst.times == test.times[length(test.times)])

  search.start <- which(fcst.times == search.times[1])
  search.end <- which(fcst.times == search.times[length(search.times)])

  # Generate the x axis. The x axis will be the search days
  if (as.POSIXct) {
    x.days <- as.POSIXct(fcst.times[search.start:search.end], origin = origin, tz = tz)
    current.forecast.x <- as.POSIXct(fcst.times[test.start+i.test.day-1], origin = origin, tz = tz)
  } else {
    x.days <- fcst.times[search.start:search.end]
    current.forecast.x <- fcst.times[test.start+i.test.day-1]
  }

  # This is the day index associated with each row in the similarity matrix.
  # Note this is sorted by the similarity values.
  #
  days.index <- sims.index - search.start + 1

  if (use.plotly) {

    if (single.figure) {
      plotly.list <- list()
    }

    p <- plotly::plot_ly(x = x.days[days.index], y = sims, type = 'scatter', name = "Similarity",
                         mode = 'markers', marker = list(color = col.unselected, symbol= pch.unselected)) %>%
      plotly::add_markers(x = x.days[days.index[1:num.analogs]],
                          y = sims[1:num.analogs], name = "Selected",
                          marker = list(color = col.selected, symbol= pch.selected)) %>%
      plotly::layout(yaxis = list(title = 'Similarity', titlefont = cex.lab, showspikes = T, spikedash = spikedash,
                                  spikesnap = 'cursor', spikemode = spikemode, spikethickness = spikethickness,
                                  spikecolor = spikecolor),
                     xaxis = list(showspikes = T, spikesnap = 'cursor', spikemode = spikemode, spikedash = spikedash,
                                  spikethickness = spikethickness, spikecolor = spikecolor),
                     showlegend = F, hovermode = hovermode)

    if (single.figure) {
      plotly.list <- c(plotly.list, list(p))
    } else {
      print(p)
    }

    for (index in 1:length(parameter.names.used)) {
      i.parameter <- which(parameter.names.used[index] == parameter.names)

      current.forecast.value <- forecasts[
        i.parameter, i.station, (i.test.day+test.start-1), i.flt]

      # Extract the values for the forecasts for this particular parameter
      forecast.values <- forecasts[i.parameter, i.station, search.start:search.end, i.flt]

      # A sanity check
      stopifnot(length(x.days) == length(forecast.values))

      p <- plotly::plot_ly(type = 'scatter', mode = 'markers+lines') %>%
        plotly::add_markers(x = x.days, y = forecast.values, name = parameter.names[i.parameter],
                            marker = list(color = col.unselected, symbol= pch.unselected)) %>%
        plotly::add_markers(x = x.days[days.index[1:num.analogs]],
                            y = forecast.values[days.index[1:num.analogs]], name = 'Selected',
                            marker = list(color = col.selected, symbol= pch.selected)) %>%
        plotly::add_segments(x = x.days[1], xend = ifelse(prevent.search.future, current.forecast.x, max(x.days)),
                             y = current.forecast.value, yend = current.forecast.value,
                             name = 'Current forecast', line = list(
                               color = col.reference, width = lwd.reference, dash = lty.reference)) %>%
        plotly::add_markers(x = current.forecast.x, y = current.forecast.value,
                            name = 'Current forecast', marker = list(
                              color = col.current, symbol= pch.current)) %>%
        plotly::layout(yaxis = list(title = parameter.names[i.parameter], titlefont = cex.lab,
                                    showspikes = T, spikesnap = 'cursor', spikemode = spikemode,
                                    spikethickness = spikethickness, spikedash = spikedash,
                                    spikecolor = spikecolor),
                       xaxis = list(showspikes = T, spikesnap = 'cursor', spikemode = spikemode,
                                    spikethickness = spikethickness, spikedash = spikedash,
                                    spikecolor = spikecolor),
                       showlegend = F, hovermode = hovermode)

      if (single.figure) {
        plotly.list <- c(plotly.list, list(p))
      } else {
        print(p)
      }
    }

    p <- plotly::subplot(plotly.list, nrows = length(plotly.list), titleY = T, shareX = T)
    print(p)

  } else {

    if (single.figure) {
      # If you want to plot all figures in a single plot
      par(mfrow = c(length(parameter.names.used) + 1, 1), mar = mar, omi = omi)
    }

    # Plot the similairty and the selected ones
    if (add.sim) {
      if (is.null(sim.ylim)) {
        sim.ylim <- range(sims, na.rm = T)
      }

      plot(c(x.days[days.index], current.forecast.x),
           c(sims, NA), xlab = '', ylab = 'Similarity', xlim = range(x.days),
           ylim = sim.ylim, pch = pch.unselected, col = col.unselected, cex.lab = cex.lab)
      points(x.days[days.index[1:num.analogs]],
             sims[1:num.analogs], pch = pch.selected, col = col.selected)
    }

    for (index in 1:length(parameter.names.used)) {
      i.parameter <- which(parameter.names.used[index] == parameter.names)

      current.forecast.value <- forecasts[
        i.parameter, i.station, (i.test.day+test.start-1), i.flt]

      # Extract the values for the forecasts for this particular parameter
      forecast.values <- forecasts[i.parameter, i.station, search.start:search.end, i.flt]

      # A sanity check
      stopifnot(length(x.days) == length(forecast.values))

      plot(c(x.days, current.forecast.x),
           c(forecast.values, current.forecast.value), pch = pch.unselected, col = col.unselected,
           xlab = '', ylab = parameter.names[i.parameter], cex.lab = cex.lab)
      points(x.days[days.index[1:num.analogs]],
             forecast.values[days.index[1:num.analogs]],
             pch = pch.selected, col = col.selected)

      segments(x0 = x.days[1], y0 = current.forecast.value,
               x1 = ifelse(prevent.search.future, current.forecast.x, max(x.days)), y1 = current.forecast.value,
               col = col.reference, lty = lty.reference, lwd = lwd.reference)
      points(current.forecast.x, current.forecast.value, col = col.current, pch = pch.current)
    }
  }
}
