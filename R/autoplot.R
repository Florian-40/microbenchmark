#' Autoplot method for microbenchmark objects: Prettier graphs for
#' microbenchmark using ggplot2
#'
#' Uses ggplot2 to produce a more legible graph of microbenchmark timings
#'
#' @param object A microbenchmark object
#' @param expression Label of valued expression
#' @param \dots hyperparameter of scale_y_log10 or scale_y_continuous
#' @param log If \code{TRUE} the time axis will be on log scale.
#' @param y_min The lower limit of the y axis (defaults to 5 percent less than
#'   the minimum value)
#' @param y_max The upper limit of the y axis (defaults to 5 percent more than
#'   the maximum value)
#' @return A ggplot2 plot
#'
#' @author Ari Friedman, Olaf Mersmann, Florian Robert
#' 
autoplot.microbenchmark <- function(object, expression, 
                                    ...,
                                    log=TRUE,
                                    y_min=0.95*min(object$time),
                                    y_max=1.05 * max(object$time)) {
  if (!requireNamespace("ggplot2")){
    stop("Missing package 'ggplot2'.")
  }
  levels(object$expr)<-expression
  object$ntime <- convert_to_unit(object$time, "ms")
  plt <- ggplot2::ggplot(object)
  plt<-plt+ggplot2::stat_ydensity(ggplot2::aes(x=expr, y=ntime, fill=expr), show.legend = FALSE)
  plt<- plt+ ylim(y_min , y_max)
  plt <- plt + ggplot2::scale_x_discrete(name="Valued expression")
  y_label <- sprintf("Temps de calcul [%s]", attr(object$ntime, "unit"))
  plt <- if (log) {
    plt + ggplot2::scale_y_log10(name=y_label, ...)
  } else {
    plt + ggplot2::scale_y_continuous(name=y_label, ...)
  }
  plt
}
