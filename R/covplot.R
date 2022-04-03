#' @title covplot
#' @description Draw covplot more quickly
#' @param peak valCol chr start end ylim title xlab ylab
#' @return ggplot2 object
#' @examples covplot("./peak.narrowPeak", 5, "NC_000071.7", 1, 1000)
#' @import ggplot2
#' @export covplot
#' @author RestlessTail

covplot <- function(peak.file, val.col, chr, start, end,
                    ylim = NULL,
                    title = NULL,
                    xlab = NULL,
                    ylab = NULL,
                    color = NULL) {

  #read peak file
  table = read.delim(peak.file, F)
  table[, 2] = table[, 2] + 1

  #sort peaks
  index = unique(c(
    which(table[, 1] == chr & table[, 2] <= end & table[, 2] >= start),
    which(table[, 1] == chr & table[, 3] <= end & table[, 3] >= start)
    ))
  peaks = table[index, ]

  #gen data matrix
  data = data.frame(
    start = peaks[, 2],
    end = peaks[, 3],
    val = peaks[, val.col]
  )

  #select color
  color.bg = 'black'
  if(!is.null(color)){
    color.bg = color
  }

  #draw plot
  p = ggplot(data) + geom_rect(aes(xmin = start, xmax = end, ymin = 0, ymax = val), fill = color.bg, color = color.bg)
  p <- p + theme_classic()
  p <- p + xlab(xlab) + ylab(ylab) + ggtitle(title)
  p <- p + scale_x_continuous(limits = c(start, end))
  if(is.null(ylim)){
    p <- p + scale_y_continuous(expand=c(0,0))
  }
  else{
    p <- p + scale_y_continuous(expand=c(0,0), limits = c(0, ylim))
  }
  p <- p + theme(strip.text.y=element_text(angle=360))

  return (p)
}
