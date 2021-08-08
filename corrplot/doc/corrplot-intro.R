## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  fig.align = 'center',
  fig.path = 'webimg/',
  fig.width = 6,
  fig.height = 6,
  dev = 'png')

get_os = function(){
  sysinf = Sys.info()
  if (!is.null(sysinf)){
    os = sysinf['sysname']
    if (os == 'Darwin')
      os = 'osx'
  } else { ## mystery machine
    os = .Platform$OS.type
    if (grepl('^darwin', R.version$os))
      os = 'osx'
    if (grepl('linux-gnu', R.version$os))
      os = 'linux'
  }
  tolower(os)
}
if(get_os() =='windows' & capabilities('cairo') | all(capabilities(c('cairo', 'X11')))){
  knitr::opts_chunk$set(dev.args = list(type='cairo'))
}

## ----intro--------------------------------------------------------------------
library(corrplot)
M = cor(mtcars)
corrplot(M, method = 'number') # colorful number
corrplot(M, method = 'color', order = 'alphabet') 
corrplot(M) # by default, method = 'circle'
corrplot(M, order = 'AOE') # after 'AOE' reorder
corrplot(M, method = 'shade', order = 'AOE', diag = FALSE) 
corrplot(M, method = 'square', order = 'FPC', type = 'lower', diag = FALSE)
corrplot(M, method = 'ellipse', order = 'AOE', type = 'upper')
corrplot.mixed(M, order = 'AOE')
corrplot.mixed(M, lower = 'shade', upper = 'pie', order = 'hclust')

## ----hclust-------------------------------------------------------------------
corrplot(M, order = 'hclust', addrect = 2)
corrplot(M, method = 'square', diag = FALSE, order = 'hclust', 
         addrect = 3, rect.col = 'blue', rect.lwd = 3, tl.pos = 'd')

## ----seriation----------------------------------------------------------------
library(seriation)
list_seriation_methods('matrix')
list_seriation_methods('dist')

data(Zoo)
Z = cor(Zoo[,-c(15, 17)])

dist2Order = function(corr, method, ...) {
  d_corr = as.dist(1 - corr)
  s = seriate(d_corr, method = method, ...)
  i = get_order(s)
  return(i)
}


# PCA_angle, same as 'AOE' in corrplot() and corrMatOrder()
i = get_order(seriate(Z, 'PCA_angle'))
corrplot(Z[i, i], cl.pos = 'n')

# Hierarchical clustering
i = dist2Order(Z, 'HC')
corrplot(Z[i, i], cl.pos = 'n')

# Rank-two ellipse seriation
i = dist2Order(Z, 'R2E')
corrplot(Z[i, i], cl.pos = 'n')

# Spectral seriation 
i = dist2Order(Z, 'Spectral')
corrplot(Z[i, i], cl.pos = 'n')

# TSP solver 
i = dist2Order(Z, 'TSP')
corrplot(Z[i, i], cl.pos = 'n')

## ----rectangles---------------------------------------------------------------
library(magrittr)

# use index parameter
i = get_order(seriate(Z, 'PCA_angle'))
corrplot(Z[i, i], cl.pos = 'n') %>% corrRect(c(1, 9, 15))

# use name parameter
# Since R 4.1.0, the following one line code works: 
# corrplot(M, order = 'AOE') |> corrRect(name = c('gear', 'wt', 'carb'))
corrplot(Z, order = 'AOE') %>% 
  corrRect(name = c('tail', 'airborne', 'venomous', 'predator'))


# use namesMat parameter
r = rbind(c('eggs', 'catsize', 'airborne', 'milk'),
          c('catsize', 'eggs', 'milk', 'airborne'))
corrplot(Z, order = 'hclust') %>% corrRect(namesMat = r)

## ----color--------------------------------------------------------------------
## diverging color example, suitable for matrix in [-N, N]
col1 = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', 'white',
                           'cyan', '#007FFF', 'blue', '#00007F'))
col2 = colorRampPalette(c('red', 'white', 'blue'))	

library(RColorBrewer)
# use RcolorBrewer color
corrplot(M, order = 'AOE', addCoef.col = 'black', tl.pos = 'd',  
         cl.pos = 'n', col = brewer.pal(n = 10, name = 'PRGn'))

## bottom color legend, diagonal text legend, rotate text label
corrplot(M, order = 'AOE', cl.pos = 'b', tl.pos = 'd', 
         col = col1(10), diag = FALSE)

## text labels rotated 45 degrees and  wider color legend with numbers right aligned
corrplot(M, type = 'lower', order = 'hclust', tl.col = 'black', 
         cl.ratio = 0.2, tl.srt = 45, col = col2(10))

## remove color legend, text legend and principal diagonal glyph
corrplot(M, order = 'AOE', cl.pos = 'n', tl.pos = 'n', 
         col = c('white', 'black'), bg = 'gold2')  

## ----non-corr-----------------------------------------------------------------
## color example, suitable for matrix in [-N, 0] or [0, N]
col3 = hcl.colors(10, "YlOrRd", rev = TRUE)

N = matrix(runif(80, 20, 30), 8)
corrplot(N, is.corr = FALSE, col.lim = c(20, 30), 
         col = col3, cl.pos = 'b', win.asp = 0.8)

## ----col-lim------------------------------------------------------------------
# when is.corr=TRUE, col.lim only affect the color legend display
corrplot(M/2)
corrplot(M/2, col.lim=c(-0.5, 0.5))

## ----NA-math------------------------------------------------------------------
M2 = M
diag(M2) = NA
colnames(M2) = rep(c('$alpha+beta', '$alpha[0]', '$alpha[beta]'), 
                   c(4, 4, 3))
rownames(M2) = rep(c('$Sigma[i]^n', '$sigma',  '$alpha[0]^100', '$alpha[beta]'), 
                   c(2, 4, 2, 3))
corrplot(10*abs(M2), col = col3, is.corr = FALSE, 
         col.lim = c(0, 10), tl.cex = 1.5)

## ----test---------------------------------------------------------------------
testRes = cor.mtest(mtcars, conf.level = 0.95)

## specialized the insignificant value according to the significant level
corrplot(M, p.mat = testRes$p, sig.level = 0.10, order = 'hclust', addrect = 2) 

## leave blank on no significant coefficient
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)

## add p-values on no significant coefficients
corrplot(M, p.mat = testRes$p, insig = 'p-value')

## add all p-values
corrplot(M, p.mat = testRes$p, insig = 'p-value', sig.level = -1)

## add significant level stars
corrplot(M, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9, 
         insig = 'label_sig', pch.col = 'grey20', order = 'AOE')

## add significant level stars and cluster rectangles
corrplot(M, p.mat = testRes$p, tl.pos = 'd', order = 'hclust', addrect = 2,
         insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 0.9, pch.col = 'grey20')

## ----confidence-interval------------------------------------------------------
# Visualize confidence interval
corrplot(M, lowCI = testRes$lowCI, uppCI = testRes$uppCI, order = 'hclust',
         tl.pos = 'd', rect.col = 'navy', plotC = 'rect', cl.pos = 'n')

# Visualize confidence interval and cross the significant coefficients
corrplot(M, p.mat = testRes$p, lowCI = testRes$lowCI, uppCI = testRes$uppCI,
         addrect = 3, rect.col = 'navy', plotC = 'rect', cl.pos = 'n')

