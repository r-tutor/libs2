

---
title: "Some plots and stuff"
author: "Foo Bar"
date: "June 30, 2015"
---


```r
library("ggplot2")
```

## Scatter plot


```r
ggplot(mtcars, aes(cyl, hp)) + 
  geom_point() + 
  theme_grey(base_size = 18)
```

![plot of chunk unnamed-chunk-3](https://i.imgur.com/qhAguQK.png)

## Bar plot


```r
ggplot(iris, aes(Species, Sepal.Length)) + 
  stat_boxplot() +
  theme_grey(base_size = 18)
```

![plot of chunk unnamed-chunk-4](https://i.imgur.com/byOPWhp.png)
