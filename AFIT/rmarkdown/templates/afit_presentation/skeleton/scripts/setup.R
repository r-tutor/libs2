knitr::knit_hooks$set(
  
  jkf_par = function(before, options, envir) {
  
  if (before) {
    
    par(cex.lab = 1.05, 
        cex.axis = 1.05, 
        mgp = c(3.25,0.7, 0), 
        tcl = -0.3, 
        font.lab = 2, 
        font = 2, 
        font.axis = 2, 
        tck = 0.025, 
        family = "serif",
        lwd = 2)  }
})

knitr::knit_hooks$set(
  
  source = function(x, options){
  if (!is.null(options$verbatim) && options$verbatim){
    opts = gsub(",\\s*verbatim\\s*=\\s*TRUE\\s*", "", options$params.src)
    bef = sprintf('\n\n    ```{r %s}\n', opts, "\n")
    stringr::str_c(
      bef, 
      knitr:::indent_block(paste(x, collapse = '\n'), "    "), 
      "\n    ```\n"
    )
  } else {
    stringr::str_c("\n\n```", tolower(options$engine), "\n", 
                   paste(x, collapse = '\n'), "\n```\n\n"
    )
  }
})

knitr::knit_hooks$set(
  
  latex = function(before, options, envir) {
  
  if (before) {
    
    if(output=='pdf') '\\vspace{7px}'
  }
})

knitr::opts_chunk$set(message = FALSE, 
                      warning = FALSE, 
                      echo = FALSE, 
                      results = "asis", 
                      jkf_par = TRUE,
                      fig.align = 'center',
                      fig.pos = 'h',
                      fig.width = 6.25,
                      fig.height = 4.5,
                      comment = NA)

getPackage <- function(pkg = NULL, repo = 'CRAN') {
  
  if(repo=='CRAN') {
  
eval(substitute(ifelse(!a%in%library()$results, 
                       {install.packages(a, repos = 'http://cran.rstudio.com') ; library(a)}, 
                       library(a)),
                list(a = pkg)))

  } else {
    
eval(substitute(ifelse(!a%in%library()$results, 
                       {devtools::install_github(paste(c(b,a), collapse = '/')) ; library(a) }, 
                       library(a)), 
                list(a = pkg, b = repo)))
  
}
}

