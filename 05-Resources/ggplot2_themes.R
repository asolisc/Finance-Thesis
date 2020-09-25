# --- Load tidyverse ---
library(tidyverse)


# --- Setup fonts ---
library(extrafont)
# font_import(paths = "../02-RStudio-Fonts")
# loadfonts(device = "win")


# Decima Theme ------------------------------------------------------------


theme_decima <- function(){
  
  theme_minimal(base_family = "Decima Mono X", base_size = 12) +
    
    theme(
      # Remove minor grid lines
      panel.grid.minor = element_blank(),
      
      # Far-left title position
      plot.title.position = "plot",
      
      # Background color
      # plot.background = element_rect(fill = "#F4F3EE"),
      
      # Legends
      legend.position = "top",
      legend.direction = "horizontal"
    )
}


# Roboto Theme ------------------------------------------------------------


theme_roboto <- function(){
  
  theme_minimal(base_family = "Roboto Condensed", base_size = 13) +
    
    theme(
      # Remove minor grid lines
      panel.grid.minor = element_blank(),
      
      # Far-left title position
      plot.title.position = "plot",
      
      # Background color
      plot.background = element_rect(fill = "#F4F3EE"),
      
      # Legends
      legend.position = "top",
      legend.direction = "horizontal"
    )
}



# Plex Theme --------------------------------------------------------------



theme_plex <- function(){
  
  theme_minimal(base_family = "IBM Plex Sans Condensed", base_size = 13) +
    
    theme(
      # Remove minor grid lines
      panel.grid.minor = element_blank(),
      
      # Far-left title position
      plot.title.position = "plot",
      
      # Background color
      plot.background = element_rect(fill = "#F4F3EE"),
      
      # Legends
      legend.position = "top",
      legend.direction = "horizontal"
    )
}


# Fira Theme --------------------------------------------------------------


theme_fira <- function(){
  
  theme_minimal(base_family = "Fira Mono", base_size = 13) +
    
    theme(
      # Remove minor grid lines
      panel.grid.minor = element_blank(),
      
      # Far-left title position
      plot.title.position = "plot",
      
      # Background color
      plot.background = element_rect(fill = "#F4F3EE"),
      
      # Legends
      legend.position = "top",
      legend.direction = "horizontal"
    )
}


# Overpass Theme ----------------------------------------------------------

theme_overpass <- function(){
  
  theme_minimal(base_family = "Overpass Mono", base_size = 13) +
    
    theme(
      # Remove minor grid lines
      panel.grid.minor = element_blank(),
      
      # Far-left title position
      plot.title.position = "plot",
      
      # Background color
      plot.background = element_rect(fill = "#F4F3EE"),
      
      # Legends
      legend.position = "top",
      legend.direction = "horizontal"
    )
}




# Open Sans Theme ---------------------------------------------------------

theme_opensans <- function(){
  
  theme_minimal(base_family = "Open Sans", base_size = 13) +
    
    theme(
      # Remove minor grid lines
      panel.grid.minor = element_blank(),
      
      # Far-left title position
      plot.title.position = "plot",
      
      # Background color
      plot.background = element_rect(fill = "#F4F3EE"),
      
      # Legends
      legend.position = "top",
      legend.direction = "horizontal"
    )
}




# Set Theme & Colors ---------------------------------------------------------------

ggplot2::theme_set(
  theme_decima()
  )


# --- set colors ---
amazing_colors <- c(
  "#134162", "#F26F20", "#48A949", "#318FB5", "#DE4576", "#FFCD49", "#2C8697"
  # "#D14D38", "#4C91B7"
  # "#47802B", "#E23E57", "#532647",
  # "#2F4C58", "#63A583", "#6E93D6",
  # "#EF6101", "#47802B", "#FDAC07"
)









# Play --------------------------------------------------------------------


# palmerpenguins::penguins %>% 
#   as_tibble() %>% 
#   drop_na %>% 
#   ggplot(aes(bill_length_mm, flipper_length_mm, colour = species)) +
#   geom_point(size = 3) +
#   labs(
#     title = "Palmer Penguis",
#     x = "Bill Length (mm)",
#     y = "Flipper Length (mm)"
#   ) +
#   scale_colour_manual(values = amazing_colors)
#   
# 

# tidyquant::FANG %>%
#   mutate(symbol = fct_relevel(symbol, c("FB", "AMZN", "NFLX", "GOOG"))) %>%
#   ggplot(aes(date, close)) +
#   geom_line(aes(colour = symbol)) +
#   facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
#   scale_colour_manual(values = amazing_colors) +
#   scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
#   labs(
#     title = "FANG Stocks",
#     x = NULL,
#     y = "Close Price"
#   ) +
#   theme_opensans()



# # 
# # 
# ggsave(
#   filename = "amzn.png",
#   units = "in",
#   width = 8 * 1.61803398875,
#   height = 8,
#   path = "plots/",
#   dpi = 400,
#   device = "png"
# )
