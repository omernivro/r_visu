
#install.packages("gridExtra")
#install.packages("ggthemes") 
library(ggthemes) 
library(gridExtra)
library(ggplot2)



data_1 = PickALL("2016-11-01", 10, 1, "BL", FALSE) 
data_1$CW = factor(data_1$CW)
#data_1$date = factor(data_1$date)
#data_1$Day_name = factor(data_1$Day_name)
data_1[,"cat"] = c("h", "s", "a", "h","h", "s", "a", "h", "s", "a", "s")


data_2 = PickALL("2016-11-01", 4, 1, "MG", FALSE) 
data_2$CW = factor(data_2$CW)
data_2[,"cat"] = "g"


data_3 = PickALL("2016-11-01", 4, 1, "EF", FALSE) 
data_3$CW = factor(data_3$CW)
data_3[,"cat"] = "f"

data_dots = list(dot1 = list(data = data_1, x_ax = "CW", y_ax = "SPI"), 
                  dot2 = list(data = data_2, x_ax = "CW", y_ax = "SPI"), 
                  dot3 = list(data = data_3, x_ax = "CW", y_ax = "SPI"))



plot_dots_sep = TRUE
# plot title
title_ = c("Title", "Title_2", "Title_3")

# y-axis limits
y_limits = c(0, 75)
y_ax_title = "SPI"

# choose x-axis - date, cw, year
x_ax_title = "CW"

# choose background color - options: grey, white

# choose dot color. options: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
colo = c("coral4", "mediumorchid4", "gold", "salmon1", "seagreen4", "peru")

# choose dot width 
lin_wid = c(1, 2, 1, 1, 7, 2, 4, 6, 0.3)

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
leg_nam = list(c("a","b","c"), "b", "c")


## http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
### legend position - none/left/right/top/bottom
leg_pos = "top"

### Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
leg_pos_xy = c(0.2, 0.8)

### "vertical"/"horizontal"
leg_direc = "horizontal"


## Add constant vertical/horizontal line --> comment lines otherwise
vline = geom_vline(xintercept = 2, lty = "dotted", colour = "red") 
hline = geom_hline(yintercept = 20) 

################################
facets = facet_grid(.~Year, scales = "free", space = "free_x")

x_y_limits = expand_limits(y = y_limits)

legend_pos  = (theme(legend.position = leg_pos, legend.direction = leg_direc) 
               + theme(legend.position = leg_pos_xy) 
)


title_n_loc = (ggtitle(title_[1])  
               + theme(plot.title = element_text(hjust = tit_loc)) 
)

x_y_lab_loc =  ( theme(axis.title.x = element_text(hjust = x_loc)) 
                 + theme(axis.title.x = element_text(margin = margin(t = x_marg))) 
                 + theme(axis.title.y = element_text(hjust = y_loc)) 
                 + theme(axis.title.y = element_text(margin = margin(t = y_marg)))
                 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
)

################################
num_dots = length(data_dots)

if(plot_dots_sep){
  all_dots_ = list()
  for (dot_num in 1:num_dots){
    all_dots_ = c(all_dots_, list((ggplot(data = data_dots[[dot_num]]$data,
                                            aes_string(x = data_dots[[dot_num]]$x_ax,
                                                       y = data_dots[[dot_num]]$y_ax,
                                                       group = "cat",
                                                       colour = "cat")) 
                                     + geom_point(stat = "identity", size = lin_wid[dot_num]) 
                                     + theme_var 
                                     + facets
                                     + expand_limits(y = y_limits) 
                                     + ggtitle(title_[dot_num]) 
                                     + theme(plot.title = element_text(hjust = tit_loc)) 
                                     + x_y_lab_loc
                                     + legend_pos
                                     + scale_colour_manual(name = leg_tit,
                                                           labels = leg_nam[[dot_num]], values = (colo))
                                     + hline 
                                     + vline
    ))
    )
    
  }
  dot_plot = multiplot(plotlist = all_dots_, cols = 1)
  
}else{
  all_dots_ = (ggplot(data = data_dots[[1]]$data, 
                       aes_string(x = data_dots[[1]]$x_ax, y = data_dots[[1]]$y_ax, 
                                  group = "cat", colour = shQuote(colo[1]))
  ) + geom_point(stat = "identity", size = lin_wid[1]))
  
  for (dot_num in 2:num_dots){
    dot_ = (geom_point(data = data_dots[[dot_num]]$data, 
                       aes_string(x = data_dots[[dot_num]]$x_ax,
                                  y = (data_dots[[dot_num]]$y_ax), 
                                  colour = shQuote(colo[dot_num])), 
                       size = lin_wid[dot_num] )
    )
    all_dots_  = all_dots_ + dot_
  }
  
  dot_plot =  (all_dots_ 
                       + theme_var 
                       + facets
                       + x_y_limits
                       + title_n_loc
                       + xlab(x_ax_title) 
                       + ylab(y_ax_title) 
                       + x_y_lab_loc
                       + legend_pos
                       + hline 
                       + vline
                       + scale_colour_manual(name = leg_tit,
                                             labels = leg_nam, 
                                             values = colo)
  )
}

dot_plot


