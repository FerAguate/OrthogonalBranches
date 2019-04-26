grow_tree <- function(data_df, tree_grw = NULL){
  
  eval(parse(text = RCurl::getURL('https://raw.githubusercontent.com/FerAguate/SmallRFunctions/master/fersource.R', ssl.verifypeer = FALSE)))
  fersource('create_color_vector')
  require(ape)
  
  if(!is.null(tree_grw)){
    PC_lst = tree_grw$PC_lst
    X_lst = tree_grw$X_lst
    level = tree_grw$level + 1
  }else{
    level = 1
  }
  
  data_hc <- hclust(dist(t(data_df)))
  if(level == 1) {
    PC_lst <- list(prcomp(data_df)$x[,1])
    X_lst <- list(data_df)
  }
  
  tree_cut <- cutree(data_hc, k = level)
  plot(as.phylo(data_hc), type = 'fan', 
       tip.color = create_color_vector(tree_cut, colors = rainbow(level, v = .7)), 
       edge.color = c("#4682B4", "#B47846"))
  
  newX <- X_lst
  for(w in seq_along(newX)){
    newX[[w]] <-  as.data.frame(sapply(newX[[w]], function(x) residuals(lm(x ~ PC_lst[[w]])))) 
  }
  
  Xlist <- list()
  for(i in unique(tree_cut)){
    lst_names <- lapply(lapply(newX, colnames), function(x) x %in% names(which(tree_cut == i)))
    newX_pos <- which(sapply(lst_names, any))
    Xlist[[i]] <- newX[[newX_pos]][, lst_names[[newX_pos]], drop = F]
  }
  
  PClist <- lapply(Xlist, function(x) prcomp(x)$x[,1])
  PCvar <- lapply(Xlist, function(x) prcomp(x)$sdev[1]^2)
  results <- list(PC_lst = PClist, X_lst = Xlist, level = level, PCvar = PCvar)                 
  class(results) <- 'Orthogonal tree'
  return(results)
}

