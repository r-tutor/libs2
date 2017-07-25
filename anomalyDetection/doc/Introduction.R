## ---- echo=FALSE, warning=FALSE, message=FALSE---------------------------
security_logs <- anomalyDetection::security_logs
security_logs <- tibble::as_tibble(security_logs)

## ---- collapse=TRUE, message=FALSE, warning=FALSE------------------------
# we'll use tidyverse for some common manipulations
library(tidyverse)
library(anomalyDetection)

security_logs

## ---- collapse=TRUE------------------------------------------------------
tabulate_state_vector(security_logs, 10)

## ---- collapse=TRUE------------------------------------------------------
(state_vec <- security_logs %>%
  tabulate_state_vector(10) %>%
  mc_adjust())

## ---- collapse=TRUE------------------------------------------------------
state_vec %>%
  mahalanobis_distance("both", normalize = TRUE) %>%
  as_tibble

## ---- fig.align='center', fig.width=9, fig.height=6----------------------
state_vec %>%
  mahalanobis_distance("both", normalize = TRUE) %>%
  as_tibble %>%
  mutate(Block = 1:n()) %>% 
  gather(Variable, BD, -c(MD, Block)) %>%
  ggplot(aes(factor(Block), Variable, color = MD, size = BD)) +
  geom_point()

## ---- collapse=TRUE------------------------------------------------------
state_vec %>%
  mahalanobis_distance("bd", normalize = TRUE) %>%
  bd_row(17, 10)

## ---- collapse=TRUE------------------------------------------------------
horns_curve(state_vec)

## ---- collapse=TRUE------------------------------------------------------
state_vec %>%
  horns_curve() %>%
  factor_analysis(state_vec, hc_points = .) %>%
  str

## ---- collapse=TRUE------------------------------------------------------
state_vec %>%
  horns_curve() %>%
  factor_analysis(state_vec, hc_points = .) %>%
  factor_analysis_results(4) %>%
  as_tibble

## ---- collapse=TRUE------------------------------------------------------
state_vec %>%
  horns_curve() %>%
  factor_analysis(data = state_vec, hc_points = .) %>%
  factor_analysis_results(fa_loadings_rotated) %>%
  kaisers_index()

## ---- fig.align='center', fig.height=7, fig.width=7----------------------
fa_loadings <- state_vec %>%
  horns_curve() %>%
  factor_analysis(state_vec, hc_points = .) %>%
  factor_analysis_results(fa_loadings_rotated)

row.names(fa_loadings) <- colnames(state_vec)

gplots::heatmap.2(fa_loadings, dendrogram = 'both', trace = 'none', 
            density.info = 'none', breaks = seq(-1, 1, by = .25), 
            col = RColorBrewer::brewer.pal(8, 'RdBu'))


## ---- fig.align='center', fig.height=7, fig.width=7----------------------
state_vec %>%
  horns_curve() %>%
  factor_analysis(state_vec, hc_points = .) %>%
  factor_analysis_results(fa_scores_rotated) %>%
  as_tibble() %>%
  mutate(Block = 1:n()) %>%
  gather(Factor, Score, -Block) %>%
  mutate(Absolute_Score = abs(Score)) %>%
  ggplot(aes(Factor, Absolute_Score, label = Block)) +
  geom_text() +
  geom_boxplot(outlier.shape = NA)


## ---- collapse=TRUE------------------------------------------------------
principal_components(state_vec) %>% str

## ---- collapse=TRUE------------------------------------------------------
state_vec %>%
  principal_components() %>%
  principal_components_result(pca_rotated) %>%
  as_tibble

