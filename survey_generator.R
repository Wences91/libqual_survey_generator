# The number of surveys and range of respondents are established
surveys <- 20
respondents <- c(50, 100)

for(n in 1:surveys){
  
  # To randomize the results, a seed is assigned according to the number of survey and respondents
  i <- n
  set.seed(i)
  
  df <- data.frame(IDEncuesta=integer(),
                   Género=integer(),
                   AF1M=integer(),
                   AF2M=integer(),
                   AF3M=integer(),
                   AF4M=integer(),
                   AF5M=integer(),
                   AF6M=integer(),
                   AF7M=integer(),
                   AF8M=integer(),
                   AF9M=integer(),
                   ES1M=integer(),
                   ES2M=integer(),
                   ES3M=integer(),
                   ES4M=integer(),
                   ES5M=integer(),
                   CI1M=integer(),
                   CI2M=integer(),
                   CI3M=integer(),
                   CI4M=integer(),
                   CI5M=integer(),
                   CI6M=integer(),
                   CI7M=integer(),
                   CI8M=integer(),
                   AF1O=integer(),
                   AF2O=integer(),
                   AF3O=integer(),
                   AF4O=integer(),
                   AF5O=integer(),
                   AF6O=integer(),
                   AF7O=integer(),
                   AF8O=integer(),
                   AF9O=integer(),
                   ES1O=integer(),
                   ES2O=integer(),
                   ES3O=integer(),
                   ES4O=integer(),
                   ES5O=integer(),
                   CI1O=integer(),
                   CI2O=integer(),
                   CI3O=integer(),
                   CI4O=integer(),
                   CI5O=integer(),
                   CI6O=integer(),
                   CI7O=integer(),
                   CI8O=integer(),
                   AF1D=integer(),
                   AF2D=integer(),
                   AF3D=integer(),
                   AF4D=integer(),
                   AF5D=integer(),
                   AF6D=integer(),
                   AF7D=integer(),
                   AF8D=integer(),
                   AF9D=integer(),
                   ES1D=integer(),
                   ES2D=integer(),
                   ES3D=integer(),
                   ES4D=integer(),
                   ES5D=integer(),
                   CI1D=integer(),
                   CI2D=integer(),
                   CI3D=integer(),
                   CI4D=integer(),
                   CI5D=integer(),
                   CI6D=integer(),
                   CI7D=integer(),
                   CI8D=integer(),
                   stringsAsFactors = FALSE)
  
  # Probabilities are created to randomize the results
  probs <- sample(c(0.4,0.2,0.15,0.1,0.1,0.05,0.0,0.0,0.0), 9, replace = FALSE)
  posneg <- sample(c(-1,1),1)
  
  answers <- sample(50:100, 1)
  for(j in 1:answers){
    
    set.seed((j*i)+(i+j))
    genre <- sample(c(1,2), 1)
    
    m<-sample(1:9, 22, replace = TRUE, prob = probs)
    o<-sample(1:9, 22, replace = TRUE, prob = probs)+(posneg*sample(c(0,1,2),22, replace = TRUE))
    d<-sample(1:9, 22, replace = TRUE, prob = probs)+sample(c(2,3,4,5), 22, replace = TRUE)
    
    # Some adjustments to avoid that the results go out of the range
    o[o<1] <- 1
    o[o>9] <- 9
    d[d>9] <- 9
    
    df[j,] <- c(j, genre, m, o, d)
    
  }
  
  # The columns are sorted to facilitate the analysis
  df <- df[,c('IDEncuesta','Género','AF1M','AF1O','AF1D','AF2M','AF2O','AF2D','AF3M','AF3O','AF3D','AF4M','AF4O','AF4D','AF5M','AF5O','AF5D','AF6M','AF6O','AF6D','AF7M','AF7O','AF7D','AF8M','AF8O','AF8D','AF9M','AF9O','AF9D','ES1M','ES1O','ES1D','ES2M','ES2O','ES2D','ES3M','ES3O','ES3D','ES4M','ES4O','ES4D','ES5M','ES5O','ES5D','CI1M','CI1O','CI1D','CI2M','CI2O','CI2D','CI3M','CI3O','CI3D','CI4M','CI4O','CI4D','CI5M','CI5O','CI5D','CI6M','CI6O','CI6D','CI7M','CI7O','CI7D','CI8M','CI8O','CI8D')]
  
  
  write.table(df, paste0('survey_',i,'.tsv'), sep = '\t', quote = FALSE, fileEncoding = 'UTF8', row.names = FALSE)
  
}


