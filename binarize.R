#It's important to note that the observed levels of the training and testing data might be different. This function takes that into
#account, but it is necessary that the (potential) levels of the training and testing data are set to be the same.
#for example:
#potential_levels <- unique(c(levels(train),levels(test)))

binarize <- function(labels,data){
  if(!(class(data) %in% c('tbl_df','tbl','data.frame'))) print('Error: Data must be in the form of a data frame.')
  else{
    if(class(labels) == 'factor'){
      levs <- levels(labels)
      labels <- as.character(labels)
      levs <- gsub(pattern=' ',x=as.character(levs),rep='.')
      labels <- gsub(pattern=' ',x=as.character(labels),rep='.')
      for(i in 1:length(levs)){
        for(j in 1:nrow(data)){
          data[j,levs[i]] <- ifelse(labels[j] == levs[i],1,0)
        }
      }
    }
    else{
      labels <- as.character(labels)
      labels <- gsub(pattern=' ',x=labels,rep='.')
      uniques <- unique(labels)
      for(i in 1:length(uniques)){
        for(j in 1:nrow(data)){
          data[j,uniques[i]] <- ifelse(labels[j] == uniques[i],1,0)
        }
      }
    }
  }
  return(data)
}
