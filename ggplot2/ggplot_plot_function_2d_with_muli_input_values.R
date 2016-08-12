




# Plot function-2D(x, y) for different same range of x and
# different values of y


## Note: Other values for Palettes are:
# Diverging
# BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
#
# Qualitative
# Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
#
# Sequential
# Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd,
# Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd


rm(list = ls())

require('ggplot2')

function_2d <- function(x, y) {
  return(x + y * 10)
}

df.out <- expand.grid(x = seq(0, 10, 1),
                      group_y = c(1, 3, 5, 7))

df.out$y <- function_2d(df.out$x, df.out$group_y)

ggplot(df.out, aes(x = x, y = y)) +
  geom_line(aes(color = factor(group_y))) +
  xlab('Test label x') +
  ylab('Test label y') +
  ggtitle('testtitle') +
  scale_color_brewer(palette = 'Set1', name = 'color label')
