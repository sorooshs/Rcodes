


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


require('ggplot2')

function_3d <- function(x, y, z) {
  return(x + y * 10 + z * 100)
}

df.out <- expand.grid(
  x = seq(0, 10, 1),
  group_y = c(1, 3, 5, 7),
  group_z = c(2, 4, 6)
  
)

df.out$y <- function_3d(df.out$x, df.out$group_y, df.out$group_z)

ggplot(df.out, aes(x = x, y = y)) +
  geom_line(aes(color = factor(group_z), linetype = factor(group_y))) +
  xlab('Test label x') +
  ylab('Test label y') +
  ggtitle('testtitle') +
  scale_color_brewer(palette = 'Set1', name = 'color label') +
  scale_linetype_discrete(name = 'linetype label')
