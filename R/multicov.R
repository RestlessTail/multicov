#' @title multicov
#' @description Draw multiple covplot
#' @param peaks valCol chr start end ylim title xlab ylab
#' @return ggplot2 object
#' @examples multicov(c("./peak1.narrowPeak", "./peak2.narrowPeak"), 5, "NC_000071.7", 1, 1000)
#' @import ggplot2 cowplot
#' @export multicov
#' @author RestlessTail

multicov <- function(peakfiles, val.col, chr, start, end,
                     ylim = NULL,
                     title = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     palette = NULL) {

  #gen palette
  colors = rep(NULL, length(peakfiles))
  if(!is.null(palette)){
    colors = palette
  }

  #gen plot body
  p = genPlotBody(peakfiles, val.col, chr, start, end, ylim, colors)
  return (p)
}

genPlotBody <- function(peakfiles, val.col, chr, start, end, ylim, palette){
  len = length(peakfiles)
  if(len == 1){
    return (covplot(peakfiles, val.col, chr, start, end, ylim))
  }
  else{
    plot.list = list()

    #draw plots
    cur.index = 1
    for(i in peakfiles){
      plot.list = c(plot.list, list(covplot(i, val.col, chr, start, end, ylim, color = palette[cur.index])))
      cur.index = cur.index + 1
    }

    #align plots
    aligned = plot.list[[1]]
    for(i in 2:len){
      #aligned = plot_grid(aligned, plot.list[[i]], ncol = 1, align = "v")
      aligned = aligned / plot.list[[i]]
    }

    return (aligned)
  }
}
