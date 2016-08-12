


rm(list = ls())

require(ggplot2)
library(extrafont)
library(ggthemes)
library(dplyr)
library(RColorBrewer) 

source('ggplot_themes.R')

fonts()
loadfonts()

Precision_FScore <- function(fscore, beta, recall) {
  alpha <- 1 / (1 + beta ^ 2)
  return((alpha * (1 / (
    1 / fscore - (1 - alpha) * 1 / recall
  ))))
  
}

Recall_FScore <- function(fscore, beta, precision) {
  alpha <- 1 / (1 + beta ^ 2)
  return (alpha * (1 / (1 / fscore - (alpha) * 1 / precision)))
  
}

funcShaded <- function(fscore = .7,
                       beta = .3,
                       recall) {
  alpha <- 1 / (1 + beta ^ 2)
  precision = (alpha * (1 / (1 / fscore - (1 - alpha) * 1 / recall)))
  
  return(ifelse(precision > .83, precision, NA))
  
}


beta <- 0.3
df.out <- expand.grid(recall = seq(0, 1, .0001),
                      fscore = seq(0.1, .9, .05))

df.out$precision <- Precision_FScore(fscore = df.out$fscore,
                                     recall = df.out$recall,
                                     beta = beta)

df.out <- filter(df.out, precision < 1 & precision > 0)

colourCount <- length(unique(df.out$fscore))
getPalette  <- colorRampPalette(brewer.pal(9, "Set1"))

ggplot(df.out, aes(x = precision, y = recall)) +
  geom_line(aes(color = factor(fscore)), alpha = .9, size = 2) +
  xlab('Precision') +
  ylab('Recall') +
  ggtitle('Precision/Recall') +
  
  theme.fantasy +
  
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_color_brewer(palette = "Set1", name = 'FScore') +
  geom_vline(
    xintercept = .83,
    size = 1,
    colour = 'red',
    alpha = .7,
    linetype = 'twodash'
  ) +
  
  geom_hline(
    yintercept = .3,
    size = 1,
    colour = 'blue',
    alpha = .7,
    linetype = 'twodash'
  ) +
  
  geom_text(
    aes(
      x = 0.83,
      y = 0,
      label = paste('0.83')
    ),
    size = 6,
    colour = 'black',
    family = 'xkcd'
  ) +
  
  geom_text(
    aes(
      x = 0,
      y = 0.3,
      label = paste('0.3')
    ),
    size = 6,
    colour = 'black',
    family = 'xkcd'
  )

# geom_ribbon(data = subset(out_df, precision > .83),
#     aes(x = precision, ymax = recall), ymin = 0 , fill = 'red', alpha = .2)
