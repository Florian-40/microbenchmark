#' Autoplot method for microbenchmark objects: Prettier graphs for
#' microbenchmark using ggplot2
#'
#' Uses ggplot2 to produce a more legible graph of microbenchmark timings
#'
#' @param object A microbenchmark object
#' @param expression Label of valued expression
#' @param breaks break of scale_y_log10 if \code{log=TRUE}. 
#' @param \dots hyperparameter of  scale_y_continuous
#' @param log If \code{TRUE} the time axis will be on log scale.
#' @param y_min The lower limit of the y axis (defaults to 5 percent less than
#'   the minimum value)
#' @param y_max The upper limit of the y axis (defaults to 5 percent more than
#'   the maximum value)
#' @return A ggplot2 plot
#' @import scales
#'
#' @author Ari Friedman, Olaf Mersmann, Florian Robert
#' 
autoplot.microbenchmark <- function(object, expression, 
                                    breaks=c(0,1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5),
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
    minor<-c()
    for (i in 1:length(breaks)){
      minor <- c(minor, seq(breaks[i],breaks[i]*10,length.out=10))
    }
    plt + ggplot2::scale_y_log10(name=y_label, 
                                 breaks=c(0,1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5),
                                 minor_breaks=minor)
    } else {
    plt + ggplot2::scale_y_continuous(name=y_label, ...)
  }
  plt
}
