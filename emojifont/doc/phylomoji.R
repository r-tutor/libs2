## ----style, echo=FALSE, results="asis", message=FALSE--------------------
knitr::opts_chunk$set(tidy = FALSE,
		   message = FALSE)

## ----echo=FALSE, results="hide", message=FALSE---------------------------
library("ggplot2")
library("emojifont")
library("colorspace")

## ----fig.showtext=TRUE---------------------------------------------------
library(ggtree)
tree_text <- "(((((cow, (whale, dolphin)), (pig2, boar)), camel), fish), seedling);"
x <- read.tree(text=tree_text)
ggtree(x, linetype="dashed", color='firebrick') +
    xlim(NA, 7) + ylim(NA, 8.5) +
    geom_tiplab(aes(color=label), parse='emoji', size=14, vjust=0.25) +
    labs(title="phylomoji", caption="powered by ggtree + emojifont")


## ----fig.showtext=TRUE---------------------------------------------------
p <- ggtree(x, layout='circular') +
    geom_tiplab2(aes(color=label), parse='emoji', size=12, vjust=0.25)
print(p)

## ----fig.showtext=TRUE---------------------------------------------------
open_tree(p, angle=200)

open_tree(p, angle=60) %>% rotate_tree(-75)

## ----fig.showtext=TRUE---------------------------------------------------
set.seed(123)
tr <- rtree(30)
ggtree(tr) + xlim(NA, 5) +
    geom_cladelabel(node=41, label="chicken", parse="emoji",
                    fontsize=12, align=TRUE, color="firebrick") +
    geom_cladelabel(node=51, label="duck", parse="emoji",
                    fontsize=12, align=TRUE, color="steelblue") +
    geom_cladelabel(node=32, label="family", parse="emoji",
                    fontsize=12, align=TRUE, color="darkkhaki")

