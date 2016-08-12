


require('ggplot2')
library('extrafont')
library('ggthemes')
#fonts()
#loadfonts()

theme.fantasy <-
  theme_bw() +
  theme(
    axis.line = element_line(size = 1, colour = 'black'),
    panel.grid.major = element_line(colour = '#d3d3d3'),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(
      size = 14,
      family = 'xkcd',
      face = 'bold'
    ),
    text = element_text(family = 'xkcd'),
    axis.text.x = element_text(colour = 'black', size = 12),
    axis.text.y = element_text(colour = 'black', size = 12)
  ) 