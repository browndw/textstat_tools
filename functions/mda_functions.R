
get_loadings <- function(m, n, cor_min=.20, get=c("loadings", "scores", "aov_scores")) {
  
  if (class(m) != "data.frame") stop ("your data must be formatted as a data.frame")
  if (sum(sapply(m, is.factor) == T) != 1) stop ("your data.frame must contain a single factor or grouping variable")
  if (ncol(m) - sum(sapply(m, is.numeric) == T) != 1) stop ("all columns except one must be numeric")
  
  d <- m[, sapply(ds_norm, is.numeric)]
  cat_id <- m[, sapply(ds_norm, is.factor)]
  m_cor <- cor(d, method = "pearson")
  diag(m_cor) <- 0
  threshold <- apply(m_cor, 1, function(x) max(abs(x), na.rm = T) > .2)
  m_trim <- d[, threshold]
  m_z <- data.frame(scale(m_trim, center = TRUE, scale = TRUE))
  
  fa1 <- factanal(m_trim, factors = n, rotation="promax")
  f_loadings <- as.data.frame(unclass(fa1$loadings))
  if(get=="loadings") return(f_loadings)
  
  idx <- seq(1:ncol(f_loadings))
  
  g_scores <- lapply(idx, function(i){
    pos <- row.names(f_loadings)[which(f_loadings[,i] > 0.35,arr.ind=T)]
    neg <- row.names(f_loadings)[which(f_loadings[,i] < -0.35,arr.ind=T)]
    pos_sums <- rowSums(m_z[pos])
    neg_sums <- rowSums(m_z[neg])
    dim_score <- mapply(function (x,y) x-y, pos_sums, neg_sums)
    dim_score <- data.frame(cbind(dim_score, as.character(cat_id)), stringsAsFactors = F)
    colnames(dim_score) <- c("score", "group")
    dim_score$score <- as.numeric(dim_score$score)
    if(get=="aov_scores") return(dim_score)
    group_score <- aggregate(score~group, dim_score, mean)
    return(group_score)
  })
  if(get=="aov_scores") a_scores <- lapply(idx, function(i) data.table::setnames(g_scores[[i]],  c(colnames(f_loadings[i]), paste0("group", i))))
  if(get=="scores") g_scores <- lapply(idx, function(i) data.table::setnames(g_scores[[i]],  c("group", colnames(f_loadings[i]))))
  if(get=="aov_scores") a_scores <- do.call("cbind", a_scores)
  if(get=="scores") g_scores <- suppressWarnings(Reduce(function(...) merge(..., by = "group", all=T), g_scores))
  if(get=="aov_scores") return(a_scores)
  if(get=="scores") return(g_scores)
}

plot_scree <- function(m, cor_min=.20, get=c("loadings", "scores")) {
  d <-  m[, sapply(ds_norm, is.numeric)]
  m_cor <- cor(d, method = "pearson")
  diag(m_cor) <- 0
  threshold <- apply(m_cor, 1, function(x) max(abs(x), na.rm = T) > .2)
  m_trim <- d[, threshold]
  ev <- eigen(cor(m_trim))
  ap <- parallel(subject=nrow(m_trim), var=ncol(m_trim), rep=100, cent=.05)
  nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  plotnScree(nS, legend = F)
}

plot_scores <- function(loadings, scores, f) {
  x <- loadings[order(loadings[,f], decreasing = T),]
  pos <- row.names(x)[which(x[,f] > 0.35,arr.ind=T)]
  neg <- row.names(x)[which(x[,f] < -0.35,arr.ind=T)]
  vegan::linestack(scores[,f+1], scores$group, axis=T,
                   air=1.3, hoff=6, at=-1, font=2)
  title(main = paste(pos, collapse='\n'), sub = paste(neg, collapse='\n'), 
        cex.main = .75,  font.main= 2, cex.sub = .75,  font.sub= 2)
}
