library(ggthemes) 
library(gridExtra)
library(dplyr)
library(ggplot2)


data_1 = PickALL("2016-11-01", 6, 4, "BL", FALSE) 
data_1$CW = factor(data_1$CW)
#data_1$date = factor(data_1$date)
#data_1$Day_name = factor(data_1$Day_name)
data_1[,"cat"] = ("h")

data_2 = data_1[data_1$outboundprocess == "MANUAL_SORT",]
data_2$freq  = 2
data_2$seq = seq(dim(data_2)[1])
data_2$end = cumsum(data_2$SPI)
data_2$end = c(head(data_2$end, -1), 0)
data_2$start = c(0, head(data_2$end, -1))


# plot title
title_ = c("Title", "Title_2", "Title_3")

# y-axis limits
y_limits = c(0, 75)
y_ax_title = "SPI"

# choose x-axis - date, cw, year
x_ax_title = "CW"

# choose background color - options: grey, white

# choose dot color. options: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
colo = c("white", rep("mediumorchid4",10))

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
y_loc = 0.2

# y-axis distance up/down from plot - number between 0-50
y_marg = -30

# choose plot theme
## ggthemes
### http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements

# theme_tufte : a minimalist theme
# theme_economist : theme based on the plots in the economist magazine
# theme_stata: theme based on Stata graph schemes.
# theme_wsj: theme based on plots in the Wall Street Journal
# theme_calc : theme based on LibreOffice Calc
# theme_hc : theme based on Highcharts JS

## ggplot themes
### https://www.r-bloggers.com/ggplot2-themes-examples/

# theme_bw : 
# theme_classic : 
# theme_dark : 
# theme_get : 
# theme_grey :
# theme_light : 
# theme_linedraw: 
# theme_minimal :

theme_var  = theme_minimal()

#######
# legend 
## legend title : your title or NULL
leg_tit = NULL

## legend names: 
leg_nam = list("a", "b", "c")


## http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
### legend position - none/left/right/top/bottom
leg_pos = "top"

### Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
leg_pos_xy = c(0.2, 0.8)

### "vertical"/"horizontal"
leg_direc = "horizontal"


################################
facets = facet_grid(.~Year, scales = "free", space = "free_x")

x_y_limits = expand_limits(y = y_limits)

legend_pos  = (theme(legend.position = leg_pos, legend.direction = leg_direc) 
               + theme(legend.position = leg_pos_xy))


title_n_loc = (ggtitle(title_[1])  
               + theme(plot.title = element_text(hjust = tit_loc)) 
)

x_y_lab_loc =  (theme(axis.title.x = element_text(hjust = x_loc)) 
                 + theme(axis.title.x = element_text(margin = margin(t = x_marg))) 
                 + theme(axis.title.y = element_text(hjust = y_loc)) 
                 + theme(axis.title.y = element_text(margin = margin(t = y_marg)))
                 + theme(axis.text.x = element_text(angle = 90, hjust = 1)))




################################
datalist <- split(df.expanded, df.expanded$grp)

(ggplot(data_2, aes(x = factor(date), fill = Day_name)) 
  + geom_rect(aes(x = factor(date), xmin = seq - 0.3,
                  xmax = seq + 0.3 ,ymin = end, ymax = start)))

