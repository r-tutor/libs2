## ---- include=FALSE------------------------------------------------------
library(ggforce)
set.seed(1)

## ---- eval=TRUE, echo=TRUE, fig.align='center'---------------------------
# We'll start by defining some dummy data
pie <- data.frame(
    state = c('eaten', 'eaten but said you didn\'t', 'cat took it', 
              'for tonight', 'will decompose slowly'),
    focus = c(0.2, 0, 0, 0, 0),
    start = c(0, 1, 2, 3, 4),
    end = c(1, 2, 3, 4, 2*pi),
    amount = c(4,3, 1, 1.5, 6),
    stringsAsFactors = FALSE
)

p <- ggplot() + theme_no_axes() + coord_fixed()

# For low level control you define the start and end angles yourself
p + geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1, start = start, end = end, 
                     fill = state),
                 data = pie)

# But often you'll have values associated with each wedge. Use stat_pie then
p + geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1, amount = amount, 
                     fill = state),
                 data = pie, stat = 'pie')

# The wedges can be exploded away from the centre using the explode aesthetic
p + geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1, amount = amount, 
                     fill = state, explode = focus),
                 data = pie, stat = 'pie')

# And a donut can be made by setting r0 to something > 0
p + geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.8, r = 1, amount = amount, 
                     fill = state, explode = focus),
                 data = pie, stat = 'pie')

## ---- eval=TRUE, echo=TRUE, fig.align='center'---------------------------
arcs <- data.frame(
    start = 0,
    end = runif(5) * 2*pi,
    r = seq_len(5)
)
p <- ggplot() + theme_no_axes() + coord_fixed()

p + geom_arc(aes(x0 = 0, y0 = 0, r = r, start = start, end = end, 
                 alpha = ..index.., colour = factor(r)), data = arcs)

# The 0 version will not properly expand the axes, as their extend is only
# known at draw time
p + geom_arc0(aes(x0 = 0, y0 = 0, r = r, start = start, end = end, 
                 colour = factor(r)), data = arcs, ncp = 50)

# The 2 version allow you to create gradients, but the input data format is
# different
arcs <- rbind(data.frame(end = 0, r = 1:5), arcs[, c('end', 'r')])
arcs$col <- sample(5, 10, TRUE)
p + geom_arc2(aes(x0 = 0, y0 = 0, r = r, group = r, end = end, 
                  colour = factor(col)), data = arcs, size = 3)


## ---- echo=TRUE, eval=TRUE, fig.align='center'---------------------------
# Here are some data describing some circles
circles <- data.frame(
    x0 = rep(1:3, 2),
    y0 =  rep(1:2, each=3),
    r = seq(0.1, 1, length.out = 6)
)
ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r, fill=r), data=circles)

# As it is related to the coordinate system, coord_fixed() is needed to ensure
# true circularity
ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r, fill=r), data=circles) +
    coord_fixed()

# Use n to set the smoothness of the circle
ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r, fill=r), data=circles, n=10) +
    coord_fixed()

## ---- echo=TRUE, eval=TRUE, fig.align='center'---------------------------
links <- data.frame(
    x = 0, y = 0, xend = runif(10), yend = runif(10)
)
ggplot() + geom_link(aes(x = x, y = y, xend = xend, yend = yend, 
                         alpha = ..index..), data = links)

# The 2 version also allows for drawing paths
links2 <- data.frame(
    x = runif(10), y = runif(10), group = rep(c(1,2), each = 5), 
    colour = sample(5, 10, TRUE)
)
ggplot() + geom_link2(aes(x = x, y = y, group = group, colour = factor(colour)), 
                      data = links2)

## ---- echo=TRUE, eval=TRUE, fig.align='center'---------------------------
beziers <- data.frame(
    x = c(1, 2, 3, 4, 4, 6, 6),
    y = c(0, 2, 0, 0, 2, 2, 0),
    type = rep(c('cubic', 'quadratic'), c(3, 4)),
    point = c('end', 'control', 'end', 'end', 'control', 'control', 'end')
)
help_lines <- data.frame(
    x = c(1, 3, 4, 6),
    xend = c(2, 2, 4, 6),
    y = 0,
    yend = 2
)
ggplot() + geom_segment(aes(x = x, xend = xend, y = y, yend = yend), 
                        data = help_lines, 
                        arrow = arrow(length = unit(c(0, 0, 0.5, 0.5), 'cm')), 
                        colour = 'grey') + 
    geom_bezier(aes(x= x, y = y, group = type, linetype = type), 
                data = beziers) + 
    geom_point(aes(x = x, y = y, colour = point), data = beziers)

## ---- echo=TRUE, eval=TRUE, fig.align='center'---------------------------
spline <- data.frame(
    x = runif(5), y = runif(5), group = 1
)
ggplot(spline) + geom_path(aes(x = x, y = y, group = group), colour = 'grey') + 
    geom_bspline(aes(x = x, y = y, group = group)) + 
    geom_point(aes(x = x, y = y))

## ---- echo=TRUE, eval=TRUE, fig.align='center'---------------------------
###Sample gaussian distributions with 1, 2 and 3 modes.
df <- data.frame(
  "Distribution" = c(rep("Unimodal", 500),
                     rep("Bimodal", 250),
                     rep("Trimodal", 600)),
  "Value" = c(rnorm(500, 6, 1),
              rnorm(200, 3, .7), rnorm(50, 7, 0.4),
              rnorm(200, 2, 0.7), rnorm(300, 5.5, 0.4), rnorm(100, 8, 0.4))
)

# Reorder levels
df$Distribution <- factor(df$Distribution,
                          levels(df$Distribution)[c(3, 1, 2)])

p <- ggplot(df, aes(Distribution, Value))
p + geom_violin(aes(fill = Distribution))
p + geom_sina(aes(color = Distribution), size = 1)

## ------------------------------------------------------------------------
# Standard facetting
ggplot(diamonds) +
  geom_point(aes(carat, price), alpha = 0.1) +
  facet_wrap(~cut:clarity, ncol = 3)

# Pagination
ggplot(diamonds) +
  geom_point(aes(carat, price), alpha = 0.1) +
  facet_wrap_paginate(~cut:clarity, ncol = 3, nrow = 3, page = 1)

# Works with grid as well
ggplot(diamonds) +
  geom_point(aes(carat, price), alpha = 0.1) +
  facet_grid_paginate(color~cut:clarity, ncol = 3, nrow = 3, page = 4)

## ------------------------------------------------------------------------
p <- ggplot(diamonds) +
  geom_point(aes(carat, price), alpha = 0.1) +
  facet_wrap_paginate(~cut:clarity, ncol = 3, nrow = 3, page = 1)
n_pages(p)

## ------------------------------------------------------------------------
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
    geom_point() +
    facet_zoom(x = Species == "versicolor")

## ------------------------------------------------------------------------
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
    geom_point() +
    facet_zoom(y = Species == "versicolor")

## ------------------------------------------------------------------------
# Zoom in on versicolor on both axes
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
    geom_point() +
    facet_zoom(xy = Species == "versicolor")
# Use different zoom criteria on each axis
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
    geom_point() +
    facet_zoom(x = Species != 'setosa', y = Species == 'versicolor')

## ------------------------------------------------------------------------
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
    geom_point() +
    facet_zoom(x = Species != 'setosa', y = Species == 'versicolor', 
               split = TRUE)

## ---- echo=TRUE, eval=TRUE, fig.align='center'---------------------------
p3 <- power_trans(3)
p3
p3$transform(1:5)
ggplot(mtcars) + geom_point(aes(mpg, cyl)) + scale_y_continuous(trans = p3)

## ---- echo=TRUE, eval=TRUE, fig.align='center'---------------------------
p3r <- trans_reverser(p3)
p3r
ggplot(mtcars) + geom_point(aes(mpg, cyl)) + scale_y_continuous(trans = p3r)

## ---- echo=TRUE, eval=TRUE, fig.align='center'---------------------------
line <- data.frame(
    x = seq(0, 10, length.out = 100), 
    y = seq(0, 10, length.out = 100)
)
r_trans <- radial_trans(r.range = c(0, 1), a.range = c(0, 2))
spiral <- r_trans$transform(r = line$x, a = line$y)
ggplot() + geom_path(aes(x, y), data = line, colour = 'red') + 
    geom_path(aes(x, y), data = spiral, colour = 'green')

## ------------------------------------------------------------------------
library(units)
miles <- make_unit('miles')
gallon <- make_unit('gallon')
horsepower <- make_unit('horsepower')
mtcars$consumption <- mtcars$mpg * (miles/gallon)
mtcars$power <- mtcars$hp * horsepower

ggplot(mtcars) +
    geom_point(aes(power, consumption))

## ------------------------------------------------------------------------
ggplot(mtcars) +
    geom_point(aes(power, 1/consumption))

## ------------------------------------------------------------------------
ggplot(mtcars) +
    geom_point(aes(power, consumption)) +
    scale_x_unit(unit = 'W') +
    scale_y_unit(unit = 'km/l')

## ---- echo=TRUE, eval=TRUE, fig.align='center'---------------------------
rocketData <- data.frame(
  x = c(1,1,2,2),
  y = c(1,2,2,3)
)
rocketData <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  rocketData$y <- rocketData$y - c(0,i/500);
  rocketData$group <- i+1;
  rocketData
}))
rocketData2 <- data.frame(
  x = c(2, 2.25, 2),
  y = c(2, 2.5, 3)
)
rocketData2 <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  rocketData2$x[2] <- rocketData2$x[2] - i*0.25/500;
  rocketData2$group <- i+1 + 500;
  rocketData2
}))
ggplot() + geom_link(aes(x=2, y=2, xend=3, yend=3, alpha=..index..,
                     size = ..index..), colour='goldenrod', n=500) +
           geom_bezier(aes(x=x, y=y, group=group, colour=..index..),
                       data=rocketData) +
           geom_bezier(aes(x=y, y=x, group=group, colour=..index..),
                       data=rocketData) +
           geom_bezier(aes(x=x, y=y, group=group, colour=1),
                       data=rocketData2) +
           geom_bezier(aes(x=y, y=x, group=group, colour=1),
                       data=rocketData2) +
           geom_text(aes(x=1.65, y=1.65, label='ggplot2', angle=45),
                     colour='white', size=15) +
           coord_fixed() +
           scale_x_reverse() +
           scale_y_reverse() +
           scale_alpha(range=c(1, 0), guide='none') +
           scale_size_continuous(range=c(20, 0.1), trans='exp',
                                 guide='none') +
           scale_color_continuous(guide='none') +
           xlab('') + ylab('') +
           ggtitle('ggforce: Accelerating ggplot2') +
           theme(plot.title = element_text(size = 20))

## ---- echo=FALSE---------------------------------------------------------
devtools::session_info()

