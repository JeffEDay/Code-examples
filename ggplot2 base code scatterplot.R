# GGplot2 examples from The Complete ggplot2 Tutorial
# turn off scientific notion 
library(ggplot2)
options(scipen = 999)

# Use midwest data
data("midwest", package = "ggplot2")

# initial plot

ggplot(midwest, aes(x=area, y=poptotal)) +  # area and poptotal are columns in 'midwest'
 geom_point() + geom_smooth(method="lm", se = FALSE)  # set se=FALSE to turnoff confidence bands

# Adjust the X and Y Axis

g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm", se = FALSE)  # set se=FALSE to turnoff confidence bands

# Delete the points outside the limits
g + xlim(c(0, 0.1)) + ylim(c(0, 1000000))   # deletes points
# g + xlim(0, 0.1) + ylim(0, 1000000)   # deletes points

# Method 2: Zooming In
# The other method is to change the X and Y axis limits 
# by zooming in to the region of interest without deleting 
# the points. This is done using coord_cartesian().

g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm", se = FALSE)  # set se=FALSE to turnoff confidence bands

# Zoom in without deleting the points outside the limits. 
# As a result, the line of best fit is the same as the original plot.
g1 <- g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))  # zooms in
plot(g1)

# add titles and axis labels
g1 + ggtitle("Area Vs Population", subtitle="From midwest dataset") + xlab("Area") + ylab("Population")

# How to Change the Color and Size To Static?
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
plot(gg)
# As an added benefit, the legend is added automatically.
# If needed, it can be removed by setting the legend.position to None 
# from within a theme() function.

gg + theme(legend.position="None")  # remove legend
# Also, You can change the color palette entirely.

gg + scale_colour_brewer(palette = "Set1")  # change color palette


# How to Change the X and Y Axis Text and its Location?
# This involves two aspects: breaks and labels.

# Step 1: Set the breaks
# The breaks should be of the same scale as the X axis variable. 
# Note that I am using scale_x_continuous because, the X axis variable is a 
# continuous variable. Had it been a date variable, scale_x_date could be 
# used. Like scale_x_continuous() an equivalent scale_y_continuous() 
# is available for Y axis.


# Base plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

# Change breaks
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01))
# Change breaks + label
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = letters[1:11])

# Reverse X Axis Scale
gg + scale_x_reverse()
# Change Axis Texts
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = sprintf("%1.2f%%", seq(0, 0.1, 0.01))) + 
  scale_y_continuous(breaks=seq(0, 1000000, 200000), labels = function(x){paste0(x/1000, 'K')})


# Themes

# Base plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

gg <- gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01))

# method 1: Using theme_set()
theme_set(theme_classic())  # not run
gg

# method 2: Adding theme Layer itself.
gg + theme_bw() + labs(subtitle="BW Theme")
gg + theme_classic() + labs(subtitle="Classic Theme")
