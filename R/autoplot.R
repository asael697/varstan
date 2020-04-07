#' Automatically create a ggplot for time series objects
#'
#' \code{autoplot} takes an object of type \code{ts} or \code{mts} and creates
#' a ggplot object suitable for usage with \code{stat_forecast}.
#'
#' \code{fortify.ts} takes a \code{ts} object and converts it into a data frame
#' (for usage with ggplot2).
#'
#' @param object Object of class \dQuote{\code{ts}} or \dQuote{\code{mts}}.
#' @param series Identifies the timeseries with a colour, which integrates well
#' with the functionality of \link{geom_forecast}.
#' @param facets If TRUE, multiple time series will be faceted (and unless
#' specified, colour is set to FALSE). If FALSE, each series will be assigned a
#' colour.
#' @param colour If TRUE, the time series will be assigned a colour aesthetic
#' @param model Object of class \dQuote{\code{ts}} to be converted to
#' \dQuote{\code{data.frame}}.
#' @param data Not used (required for \link{fortify} method)
#' @param ... Other plotting parameters to affect the plot.
#'
#'
#' @return None. Function produces a ggplot graph.
#'
#' @author Mitchell O'Hara-Wild
#'
#' @seealso \code{\link[stats]{plot.ts}}, \code{\link[ggplot2]{fortify}}
#'
#' @import ggplot2
#' @export
#'
autoplot.ts <- function(object, series=NULL, xlab = "Time", ylab = deparse(substitute(object)),
                        main = NULL,  ...) {
  if (!is.ts(object))
    stop("autoplot.ts requires a ts object, use object=object")

  # Create data frame with time as a column labelled x
  # and time series as a column labelled y.
  data <- data.frame(y = as.numeric(object), x = as.numeric(time(object)))
  if (!is.null(series)) {
    data <- transform(data, series = series)
  }

  # Initialise ggplot object
  p <- ggplot2::ggplot(ggplot2::aes_(y = ~y, x = ~x), data = data)

  # Add data
  if (!is.null(series)) {
    p <- p + ggplot2::geom_line(ggplot2::aes_(group = ~series, colour = ~series), na.rm = TRUE, ...)
  }
  else {
    p <- p + ggplot2::geom_line(na.rm = TRUE, ...)
  }

  # Add labels
  p <- p + ggplot2::labs(x = xlab, y = ylab, title = main)

  # Make x axis contain only whole numbers (e.g., years)
  p <- p + ggplot2::scale_x_continuous(breaks = ggtsbreaks)
   return(p)
}
#' @rdname autoplot.ts
#' @export
autoplot.numeric <-function(object, series=NULL, xlab = "Time", ylab = deparse(substitute(object)),
                            main = NULL,  ...) {
  if(is.numeric(object))
    y = ts(object)
  g = autoplot.ts(y,series=NULL, xlab = "Time", ylab = deparse(substitute(object)),
                  main = NULL,  ...)
  return(g)
}
#' @rdname autoplot.ts
#' @export
fortify.ts <- function(model, data, ...) {
  # Use ggfortify version if it is loaded
  # to prevent cran errors
  if (exists("ggfreqplot")) {
    tsp <- attr(model, which = "tsp")
    dtindex <- time(model)
    if (any(tsp[3] == c(4, 12))) {
      dtindex <- zoo::as.Date.yearmon(dtindex)
    }
    model <- data.frame(Index = dtindex, Data = as.numeric(model))
    return(ggplot2::fortify(model))
  }
  else {
    model <- cbind(x = as.numeric(time(model)), y = as.numeric(model))
    as.data.frame(model)
  }
}
#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot
#'
#'
ggAddExtras <- function(xlab=NA, ylab=NA, main=NA) {
  dots <- eval.parent(quote(list(...)))
  extras <- list()
  if ("xlab" %in% names(dots) || is.null(xlab) || any(!is.na(xlab))) {
    if ("xlab" %in% names(dots)) {
      extras[[length(extras) + 1]] <- ggplot2::xlab(dots$xlab)
    }
    else {
      extras[[length(extras) + 1]] <- ggplot2::xlab(paste0(xlab[!is.na(xlab)], collapse = " "))
    }
  }
  if ("ylab" %in% names(dots) || is.null(ylab) || any(!is.na(ylab))) {
    if ("ylab" %in% names(dots)) {
      extras[[length(extras) + 1]] <- ggplot2::ylab(dots$ylab)
    }
    else {
      extras[[length(extras) + 1]] <- ggplot2::ylab(paste0(ylab[!is.na(ylab)], collapse = " "))
    }
  }
  if ("main" %in% names(dots) || is.null(main) || any(!is.na(main))) {
    if ("main" %in% names(dots)) {
      extras[[length(extras) + 1]] <- ggplot2::ggtitle(dots$main)
    }
    else {
      extras[[length(extras) + 1]] <- ggplot2::ggtitle(paste0(main[!is.na(main)], collapse = " "))
    }
  }
  if ("xlim" %in% names(dots)) {
    extras[[length(extras) + 1]] <- ggplot2::xlim(dots$xlim)
  }
  if ("ylim" %in% names(dots)) {
    extras[[length(extras) + 1]] <- ggplot2::ylim(dots$ylim)
  }
  return(extras)
}
ggtsbreaks <- function(x) {
  # Make x axis contain only whole numbers (e.g., years)
  return(unique(round(pretty(floor(x[1]):ceiling(x[2])))))
}
