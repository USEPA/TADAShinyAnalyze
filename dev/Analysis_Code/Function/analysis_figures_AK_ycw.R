library(dplyr)
library(ggplot2)
library(viridis)


boxPlot <- function(data, WQS_table, AU_ID) {
  
  sysfonts::font_add_google("Open Sans", family = "Open_Sans") # for fonts
  showtext::showtext_auto() # for fonts
  
  relevant_constituents <- WQS_table %>%
    dplyr::select(TADA.CharacteristicName) %>%
    unique() %>%
    dplyr::pull()
  
  relevant_data <- data %>%
    dplyr::filter(TADA.CharacteristicName %in% relevant_constituents) %>%
    dplyr::filter(JoinToAU.AssessmentUnitIdentifier == AU_ID)
  
  constituents <- relevant_data %>%
    dplyr::select(TADA.CharacteristicName) %>%
    unique() %>%
    dplyr::pull()
  
  results <- list()
  counter <- 0
  #Loop through constituents
  for(j in constituents) {
    
    counter<- counter+1
    
    filt <- relevant_data %>%
      dplyr::filter(TADA.CharacteristicName == j)
    
    plt<-ggplot2::ggplot() +
      ggplot2::geom_boxplot(data = filt,
                            ggplot2::aes(x = JoinToAU.AssessmentUnitIdentifier,
                                         y = TADA.ResultMeasureValue),
                            color = 'gray30',
                            outlier.shape = NA) +
      ggplot2::geom_jitter(data = filt, ggplot2::aes(x = JoinToAU.AssessmentUnitIdentifier,
                                                     y = TADA.ResultMeasureValue,
                                                     fill = MonitoringLocationIdentifier),
                           color = 'black',
                           shape = 21,
                           size = 3.5,
                           width = 0.2,
                           alpha = 0.8) +
      ggplot2::xlab('AU ID') +
      ggplot2::ylab(paste0(str_to_title(j), ' (', tolower(filt$TADA.ResultMeasure.MeasureUnitCode), ')')) +
      ggplot2::scale_y_log10() +
      ggplot2::theme_bw() +
      viridis::scale_fill_viridis(discrete = T,
                                  option = "mako") +
      ggplot2::labs(fill = 'Monitoring Location ID') +
      ggplot2::theme(legend.position="top"
                     , legend.spacing.x = unit(0.5, 'cm')
                     , text = ggplot2::element_text(family = "Open_Sans", size = 24)
                     , axis.text = ggplot2::element_text(family = "Open_Sans", size = 22)
                     , legend.background = element_rect(colour = 'gray60', fill = 'white', linetype='dashed')
                     , plot.margin = unit(c(0.5,0.25,0.5,0.25), "cm")) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = ceiling(length(unique(filt$MonitoringLocationIdentifier))/3),
                                                   byrow=TRUE,
                                                   title.position="top",
                                                   title.hjust = 0.5))
    
    results[[counter]] <- plt
  }
  return(results)
}

# #Time Series function
# timeSeries <- function(data, WQS_table, AU_ID) {
#   sysfonts::font_add_google("Open Sans", family = "Open_Sans") # for fonts
#   showtext::showtext_auto() # for fonts
#   
#   relevant_constituents <- WQS_table %>%
#     dplyr::select(TADA.CharacteristicName) %>%
#     unique() %>%
#     dplyr::pull()
#   
#   relevant_data <- data %>%
#     dplyr::filter(TADA.CharacteristicName %in% relevant_constituents) %>%
#     dplyr::filter(AUID_ATTNS == AU_ID)
#   
#   constituents <- relevant_data %>%
#     dplyr::select(TADA.CharacteristicName) %>%
#     unique() %>%
#     dplyr::pull()
#   
#   results <- list()
#   counter <- 0
#   #Loop through constituents
#   for(j in constituents) {
#     
#     counter<- counter+1
#     
#     filt <- relevant_data %>%
#       dplyr::filter(TADA.CharacteristicName == j)
#     
#     plt<-ggplot2::ggplot() +
#       ggplot2::geom_point(data = filt,
#                           ggplot2::aes(x = ActivityStartDate,
#                                        y = TADA.ResultMeasureValue,
#                                        fill = MonitoringLocationIdentifier),
#                           color = 'black',
#                           shape = 21,
#                           size = 3.5,
#                           alpha = 0.8) +
#       ggplot2::xlab('Time') +
#       ggplot2::scale_y_log10() +
#       ggplot2::ylab(paste0(str_to_title(j), ' (', tolower(filt$TADA.ResultMeasure.MeasureUnitCode), ')')) +
#       ggplot2::theme_bw() +
#       viridis::scale_fill_viridis(discrete = T,
#                                   option = "mako") +
#       ggplot2::labs(fill = 'Monitoring Location ID') +
#       ggplot2::theme(legend.position="top"
#                      , legend.spacing.x = unit(0.5, 'cm')
#                      , text = ggplot2::element_text(family = "Open_Sans", size = 24)
#                      , axis.text = ggplot2::element_text(family = "Open_Sans", size = 22)
#                      , legend.background = element_rect(colour = 'gray60', fill = 'white', linetype='dashed')
#                      , plot.margin = unit(c(0.5,0.25,0.5,0.25), "cm")) +
#       ggplot2::guides(fill = ggplot2::guide_legend(nrow = ceiling(length(unique(filt$MonitoringLocationIdentifier))/3),
#                                                    byrow=TRUE,
#                                                    title.position="top",
#                                                    title.hjust = 0.5))
#     results[[counter]] <- plt
#   }
#   return(results)
# }
