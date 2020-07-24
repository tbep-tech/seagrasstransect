# algae color palette
pal_alg <- colorFactor(
  palette = RColorBrewer::brewer.pal(8,  'Dark2'),#c('#004F7E', '#00806E', '#427355', '#5C4A42', '#958984'),
  na.color = 'yellow',
  levels = c('Bacillariophyta', 'Cyanobacteria', 'Karenia brevis', 'Pseudo-nitzschia pungens', 'Pseudo-nitzschia sp.', 'Pyrodinium bahamense', 'Tripos hircus', 'other')
)

# p value text formatting
p_ast <- function(x){
  
  sig_cats <- c('**', '*', '')
  sig_vals <- c(-Inf, 0.005, 0.05, Inf)
  
  out <- cut(x, breaks = sig_vals, labels = sig_cats, right = FALSE)
  out <- as.character(out)
  out <- ifelse(is.na(out), '', out)
  
  return(out)
  
}

# vertical lines in plots
vrtline_fun <- function(datin, sgty){
  
  dts <- datin %>% 
    group_by(Date) %>% 
    summarise(val = sum(val)) %>% 
    ungroup()
  
  if(sgty == 'Freq. Occ.')
    dts$val <- 100
  
  out <- list()

  for(i in 1:nrow(dts)){
    
    outi <- list(list(
      type = "line",
      y0 = 0,
      y1 = dts[[i, 'val']],
      xref = "x",
      x0 = dts[[i, 'Date']],
      x1 = dts[[i, 'Date']],
      line = list(color = 'grey', width = 0.5, opacity = 0.5)
    ))
    
    out <- c(out, outi)
    
  }

  return(out)
    
}
  