
library(pacman)
p_load(dplyr, ggplot2, data.table, extrafont, ggthemes)

impFeatures <- function(model.rf = model.rf,
                        type = 2,
                        plot = T) 
{

  # Importance Matrix
  # 1 = means decrease in accuracy # This the original one
  imp.accuracy <-
    importance(model.rf, type = 1)
  
  # 2 = mean decrease in node impurity
  imp.impurity <-
    importance(model.rf, type = 2) 
  
  imp <- imp.impurity
  
  # create importance data.frame
  imp.dplr <-
    as.data.table(as.table(imp)) %>%
    select(-V2) %>%
    dplyr::rename(feature = V1) %>%
    dplyr::rename(importance = N) %>%
    arrange(-importance)
  
  # Remove scientific notation and print
  imp.dplr$importance <- imp.dplr$importance
  
  # Plot importance
  if(plot)
    varImpPlot(model.rf, type = 2)
  
  imp.dplr
}


barPlot <- function(data, 
                    top_n = 0, # Plot to N data, 0 for all data
                    plot.path = '.', # Path to save the model
                    plot.name = ''   # Name of model to save
                    ) 
{
  # Select top_n features
  if( top_n != 0)
    imp.top <- data %>% slice(1:top_n)
  else
    imp.top <- data
  
  imp.top.name <- imp.top$feature
  
  # Create factor for the sake of odering
  imp.top$feature <- factor(imp.top$feature, levels = imp.top$feature)
  
  # ggplot importance matrix
  plot.bar.imp <-
    ggplot(imp.top ,
           aes(x = feature, y = importance, fill = feature)) +
    geom_bar(stat = "identity") + ggtitle("Variable Importance Plot") +
    xlab("Attributes") +
    ylab("Variable Importance") +
    guides(fill = guide_legend(ncol = 1)) +
    theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5,
        size = 12
      ),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 18),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 12)
    ) 

  # Load themes
  # theme.file.location.1 <- '../ggplot2/ggplot_themes.R'
  # theme.file.location.2 <- 'ggplot_themes.R'
  
  # if (file.exists(theme.file.location.1)) 
  # {
  #   source(theme.file.location.1)
  #   plot.bar.imp <- plot.bar.imp + theme.fantasy.simple
  # }
  # 
  # if (file.exists(theme.file.location.2))  
  # {
  #   source(theme.file.location.2)
  #   plot.bar.imp <- plot.bar.imp + theme.fantasy.simple
  # }

  if (plot.name != '')
    ggsave(file = file.path(plot.path,
                            paste0(plot.name, '.jpg')),
           plot = plot.bar.imp,
           width = 15, height = 12.7, units = 'in')
  
  plot.bar.imp
  
}


# ## TEST
# model.rf <- rf.model(formula = formula, 
#                      input = mtcars,
#                      importance = TRUE)
# 
# ## TEST
# impFeatures <- impFeatures(model.rf)
# 
# ## TEST
# barPlot(impFeatures)
