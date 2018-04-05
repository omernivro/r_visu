library(ggthemes) 
library(gridExtra)
library(dplyr)

data_1 = PickALL("2016-11-01", 6, 4, "BL", FALSE) 
data_1$CW = factor(data_1$CW)
#data_1$date = factor(data_1$date)
#data_1$Day_name = factor(data_1$Day_name)
data_1[,"cat"] = ("h")

data_2 = data_1[data_1$outboundprocess == "MANUAL_SORT",]

# plot title
title_ = c("Title", "Title_2", "Title_3")

# y-axis limits
y_limits = c(0, 75)
y_ax_title = "SPI"

# choose x-axis - date, cw, year
x_ax_title = "Day"

# choose background color - options: grey, white

# choose dot color. options: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
colo = c("coral4", "mediumorchid4", "gold", "salmon1", "seagreen4", "peru")

# choose dot width 
lin_wid = c(0.3, 0.3, 0.3, 1, 7, 2, 4, 6, 0.3)

# main title location -  choose a number between 0-1 
# -- 0 = left aligned, 0.5 = center aligned, 1 = right aligned
tit_loc = 0.5

# x-axis title/label location left/right - number between 0-1
x_loc = 0.5

# x-axis distance up/down from plot - number between 0-50
x_marg = 10

# y-axis title/label location left/right - number between 0-1
y_loc = 0.5

# y-axis distance up/down from plot - number between 0-50
y_marg = -30

leg_tit = NULL

## legend names: 
leg_nam = list("a", "b", "c", "d", "e", "f")

## http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
### legend position - none/left/right/top/bottom
leg_pos = "right"

### Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
leg_pos_xy = c(0.5, 1)

### "vertical"/"horizontal"
leg_direc = "horizontal"

######################################
x_y_limits = expand_limits(y = y_limits)

legend_pos  = (theme(legend.position = leg_pos, legend.direction = leg_direc) 
               + theme(legend.position = leg_pos_xy))


title_n_loc = (ggtitle(title_[1])  
               + theme(plot.title = element_text(hjust = tit_loc)) 
)

x_y_lab_loc =  ( theme(axis.title.x = element_text(hjust = x_loc)) 
                 + theme(axis.title.x = element_text(margin = margin(t = x_marg))) 
                 + theme(axis.title.y = element_text(hjust = y_loc)) 
                 + theme(axis.title.y = element_text(margin = margin(t = y_marg)))
                 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
)

#####################################

img <- image_graph(600, 400, res = 96)
datalist <- split(data_2, data_2$CW)
out <- lapply(datalist, function(data1){
  p <- (ggplot(data = data1, aes(x = Day_name, y = SPI, fill = Day_name))
        + theme_minimal()
        + ggtitle(title_) 
        + theme(plot.title = element_text(hjust = tit_loc)) 
        + title_n_loc
        + geom_bar(stat = "identity", aes(alpha = 0.6))
        + x_y_limits
        + xlab("Day")
        + ylab("SPI")
        + x_y_lab_loc
        + legend_pos
        + scale_fill_manual(name = leg_tit,
                            labels = leg_nam, values = colo)
        )
  print(p)
})
dev.off()
img <- image_background(image_trim(img), 'white')
animation <- image_animate(img, fps = 0.2)
print(animation)


