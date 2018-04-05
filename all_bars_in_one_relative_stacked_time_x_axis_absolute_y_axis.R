
#install.packages("gridExtra")
#install.packages("ggthemes") 
library(ggthemes) 
library(gridExtra)


data_1 = PickALL("2016-11-01", 4, 1, "BL", FALSE) 
data_1$CW = factor(data_1$CW)
#data_1$date = factor(data_1$date)
#data_1$Day_name = factor(data_1$Day_name)
data_1[,"cat"] = ("h")


data_2 = PickALL("2016-11-01", 4, 1, "MG", FALSE) 
data_2$CW = factor(data_2$CW)
data_2[,"cat"] = ("g")



data_3 = PickALL("2016-11-01", 4, 1, "EF", FALSE) 
data_3$CW = factor(data_3$CW)
data_3[,"cat"] = ("f")
# 

data_bars = list(bar1 = list(data = data_1, x_ax = "CW", y_ax = "SPI"),
                 bar2 = list(data = data_2, x_ax = "CW", y_ax = "SPI")
                 , bar3 = list(data = data_3, x_ax = "CW", y_ax = "SPI")
                 )


# plot title
title_ = "Title"

# y-axis limits
y_limits = c(0, 75)
y_ax_title = "SPI"

# choose x-axis - date, cw, year
x_ax_title = "CW"

# choose background color - options: grey, white

# choose dot color. options: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
colo = c("coral4", "mediumorchid4", "gold", "salmon1", "seagreen4", "peru")

# choose dot width 
lin_wid = c(0.3, 0.3, 0.3, 1, 7, 2, 4, 6, 0.3)

# main title location -  choose a number between 0-1 
# -- 0 = left aligned, 0.5 = center aligned, 1 = right aligned
tit_loc = 0

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


## Add constant vertical/horizontal line --> comment lines otherwise
vline = geom_vline(xintercept = 2, lty = "dotted", colour = "red") 
hline = geom_hline(yintercept = 20) 

################################
facets = facet_grid(.~Year, scales = "free", space = "free_x")

x_y_limits = expand_limits(y = y_limits)

legend_pos  = (theme(legend.position = leg_pos, legend.direction = leg_direc) 
               + theme(legend.position = leg_pos_xy))


title_n_loc = (ggtitle(title_[1])  + theme(plot.title = element_text(hjust = tit_loc)))

x_y_lab_loc =  ( theme(axis.title.x = element_text(hjust = x_loc)) 
                 + theme(axis.title.x = element_text(margin = margin(t = x_marg))) 
                 + theme(axis.title.y = element_text(hjust = y_loc)) 
                 + theme(axis.title.y = element_text(margin = margin(t = y_marg)))
                 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
)

################################
num_bars = length(data_bars)

all_bars_ = (ggplot(data = data_bars[[1]]$data, 
                    aes_string(x = data_bars[[1]]$x_ax, y = data_bars[[1]]$y_ax, 
                               group = "cat", fill = shQuote(colo[1]))
) + geom_bar(stat = "identity", width = lin_wid[1]))

for (bar_num in 2:num_bars){
  bar_ = (geom_bar(data = data_bars[[bar_num]]$data, 
                   aes_string(x = data_bars[[bar_num]]$x_ax,
                              y = (data_bars[[bar_num]]$y_ax), 
                              fill = shQuote(colo[bar_num]))
                   , stat = "identity",  width = lin_wid[bar_num])
  )
  all_bars_  = all_bars_ + bar_
}

bar_plot_ =  (all_bars_ 
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
              + scale_fill_manual(name = leg_tit,
                                  labels = leg_nam, 
                                  values = colo))

bar_plot_ 


# save plot to the desired format: http://www.cookbook-r.com/Graphs/Output_to_a_file/
## default PowerPoint presentation has a resolution of 1280 by 720 pixels -->
## 13.333 inch by 7.5 inch, 96 dots per inch (dpi)

## save to full pdf page
ggsave(paste0("bar_plot_full", Sys.Date(), ".pdf"), width = 7, height = 7)


## save to pdf page
ggsave(paste0("bar_plot", Sys.Date(), ".pdf"), width = 4, height = 4)

## save to png
ggsave(paste0("bar_plot", Sys.Date(), ".png"), width = 4, height = 4, dpi = 100)

