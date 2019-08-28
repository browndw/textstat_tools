
#These two functions calculate various dispersion measures from a quanteda dfm (document frequency matrix)
#Note that as written, they requrire quanteda, though that can easily be changed.

############
#dispersions_token() calculates the dispersion measures for a single token.
#For example: dispersions_token(my_dfm, "the")
#It returns a named list with all of the dispersion measures discussed by S.T. Gries:
#(http://www.stgries.info/research/dispersion/links.html)

############
#dispersions_all() calculates a subset of of the most common dispersion measures
#for all of the tokens in a document frequncy matrix and returns a dataframe.
#For example: dispersions_all(my_dfm)

dispersions_token <- function(df_matrix, token) {
  
  if (class(df_matrix) != "dfm") stop ("your target must be a quanteda dfm object")
  if (class(token) != "character") stop ("your token must be a character string")
  
  #select the token counts from the dfm
  t_counts <- dfm_select(df_matrix, pattern = token, valuetype = "fixed")
  if (length(t_counts) == 0) stop ("token not found in document feature matrix")
  #convert to a vector
  t_counts <- as.vector(t_counts)
  #calculate the total number of tokens
  total <- sum(ntoken(df_matrix))
  #calculte the relative sizes of the parts of the corpus (in percent)
  parts <- ntoken(df_matrix)/total
  #calculate the total number of tokens
  
  # The calc_disp1() function is baesed on a script written by Stefan Th. Gries 
  #It computes commonly used measures of dispersion that he discusses in these papers:
  #Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. /International Journal of Corpus Linguistics/ 13(4). 403-437.
  #Gries, Stefan Th. 2010. Dispersions and adjusted frequencies in corpora: further explorations. In Stefan Th. Gries, Stefanie Wulff, & Mark Davies (eds.), /Corpus linguistic applications: current studies, new directions/, 197-212. Amsterdam: Rodopi.
  
  calc_disp1 <- function(v, s=rep(1/length(v))) {
    
    n <- length(v) # n
    f <- sum(v) # f
    s <- s/sum(s) # s
    
    values <- list()
    
    values[["observed overall frequency"]] <- f
    #note that this is normalizing per million words
    values[["freq_norm"]] <- (f/total)*1000000
    values[["relative entropy of all sizes of the corpus parts"]] <- -sum(s*log(s))/log(length(s))
    
    values[["range"]] <- sum(v>0)
    values[["maxmin"]] <- max(v)-min(v)
    values[["standard deviation"]] <- sd(v)
    values[["variation coefficient"]] <- sd(v)/mean(v)
    values[["Chi-square"]] <- sum(((v-(f*s/sum(s)))^2)/(f*s/sum(s)))
    
    values[["Juilland et al.'s D (based on equally-sized corpus parts)"]] <- 1-((sd(v)/mean(v))/sqrt(n-1))
    values[["Juilland et al.'s D (not requiring equally-sized corpus parts)"]] <- 1-((sd(v/s)/mean(v/s))/sqrt(length(v/s)-1))
    values[["Carroll's D2"]] <- (log2(f)-(sum(v[v!=0]*log2(v[v!=0]))/f))/log2(n)
    values[["Rosengren's S (based on equally-sized corpus parts)"]] <- ((sum(sqrt(v))^2)/n)/f
    values[["Rosengren's S (not requiring equally-sized corpus parts)"]] <- sum(sqrt(v*s))^2/f
    values[["Lyne's D3 (not requiring equally-sized corpus parts)"]] <- 1-((sum(((v-mean(v))^2)/mean(v)))/(4*f))
    values[["Distributional consistency DC"]] <- ((sum(sqrt(v))/n)^2)/mean(v)
    values[["Inverse document frequency IDF"]] <- log2(n/sum(v>0))
    
    values[["Engvall's measure"]] <- f*(sum(v>0)/n)
    values[["Juilland et al.'s U (based on equally-sized corpus parts)"]] <- f*(1-((sd(v)/mean(v))/sqrt(n-1)))
    values[["Juilland et al.'s U (not requiring equally-sized corpus parts)"]] <- f*(1-((sd(v/s)/mean(v/s))/sqrt(length(v/s)-1)))
    values[["Carroll's Um (based on equally sized corpus parts)"]] <- f*((log2(f)-(sum(v[v!=0]*log2(v[v!=0]))/f))/log2(n))+(1-((log2(f)-(sum(v[v!=0]*log2(v[v!=0]))/f))/log2(n)))*(f/n)
    values[["Rosengren's Adjusted Frequency (based on equally sized corpus parts)"]] <- (sum(sqrt(v))^2)/n
    values[["Rosengren's Adjusted Frequency (not requiring equally sized corpus parts)"]] <- sum(sqrt(v*s))^2
    values[["Kromer's Ur"]] <- sum(digamma(v+1)+0.577215665) # C=0.577215665
    
    values[["Deviation of proportions DP"]] <- sum(abs((v/f)-s))/2
    values[["Deviation of proportions DP (normalized)"]] <- (sum(abs((v/f)-s))/2)/(1-min(s)) # corrected, see below
    
    return(values)
  }
  
  #execute the function and return the values
  dsp <- calc_disp1(t_counts, parts)
  return(dsp)
}

dispersions_all <- function(df_matrix) {
  
  if (class(df_matrix) != "dfm") stop ("your target must be a quanteda dfm object")
  #convert dfm to matrix object
  m <- as.matrix(df_matrix)
  #create a column index
  idx <- seq(1:ncol(m))
  #calculate the total number of tokens
  total <- sum(rowSums(m))
  #calculte the relative sizes of the parts of the corpus (in percent)
  parts <- rowSums(m)/total
  # The calc_disp() function is baesed on a script written by Stefan Th. Gries 
  #It computes commonly used measures of dispersion that he discusses in these papers:
  #Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. /International Journal of Corpus Linguistics/ 13(4). 403-437.
  #Gries, Stefan Th. 2010. Dispersions and adjusted frequencies in corpora: further explorations. In Stefan Th. Gries, Stefanie Wulff, & Mark Davies (eds.), /Corpus linguistic applications: current studies, new directions/, 197-212. Amsterdam: Rodopi.
  calc_disp2 <- function(v, s=rep(1/length(v))) {
    
    n <- length(v) # n
    f <- sum(v) # f
    s <- s/sum(s) # s
    
    values <- list()
    values[["freq"]] <- f
    #note that this is normalizing per million words
    values[["freq_norm"]] <- (f/total)*1000000
    values[["Carrolls_D2"]] <- (log2(f)-(sum(v[v!=0]*log2(v[v!=0]))/f))/log2(n)
    values[["Rosengrens_S"]] <- sum(sqrt(v*s))^2/f
    values[["Lynes_D3"]] <- 1-((sum(((v-mean(v))^2)/mean(v)))/(4*f))
    values[["DC"]] <- ((sum(sqrt(v))/n)^2)/mean(v)
    values[["Juillands_D"]] <- 1-((sd(v/s)/mean(v/s))/sqrt(length(v/s)-1))
    
    values[["DP"]] <- sum(abs((v/f)-s))/2
    values[["DP_norm"]] <- (sum(abs((v/f)-s))/2)/(1-min(s)) # corrected, see below
    
    values <- as.data.frame(t(as.matrix(unlist(values))))
    return(values)
  }
  
  #generate a list of dataframes for each token
  dsp <- lapply(idx, function(i){calc_disp2(m[,i], parts)})
  #use the rbindindlist() function to combine into a single dataframe
  dsp <- data.frame(data.table::rbindlist(dsp))
  #assign the tokens as row names
  rownames(dsp) <- colnames(m)
  dsp <- dsp[order(-dsp$freq),]
  return(dsp)
}
