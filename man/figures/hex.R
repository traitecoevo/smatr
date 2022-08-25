library(ggplot2)
library(wesanderson)
library(hexSticker)

str(wes_palette("Rushmore1"))


#' # Now consider low-nutrient sites (high and low rainfall):
 leaf.low.soilp <- subset(leaflife, soilp == 'low')
 
#' # Fit SMA's separately at each of high and low rainfall sites,
#' # and test for common slope:
 com.test <- sma(longev~lma*rain, log="xy", data=leaf.low.soilp)

#' # Plot longevity vs LMA separately for each group:
 
p <- ggplot(com.test$data, aes(x = lma, y = longev, group = rain)) + 
   geom_point(aes(color = rain), size = 2, shape = 21, stroke = 1) + 
   stat_smooth(method = "lm", se = FALSE, aes(color = rain), size = 1) +
   scale_color_manual(values = c("#0B775E", "#35274A")) +
   #theme_minimal() + 
   theme(legend.position = "none",
         axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         #rect = element_rect(fill = "transparent"),
         panel.border = element_rect(fill =  "transparent",
                                     colour = NA),
         panel.background = element_rect(fill = "transparent",
                                         color = NA), # necessary to avoid drawing panel outline
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         plot.background = element_rect(fill = "transparent",
                                        color = NA)) ; p

sticker(p,
        package="smatr", p_size=33,
        p_color = "black",
        p_x = 0.8, p_y = 1.2,
        h_fill = "white",
        s_x =1, s_y=.9,
        s_width=1.5, s_height=1.5,
        filename="inst/figures/smatr_hex.png")
