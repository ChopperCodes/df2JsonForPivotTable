df2Json<-
  function(df){
    
    #temporary function that will be called within df2Json
    #and will not be callable from the global environment
    fx<-
      function(i, df, colHeadings){
        
        #line up column headings as keys with appropriate values
        tmp.1<-
          paste('"', colHeadings, '" : "', as.vector(df[i,]), '" ', sep="" );
        
        #convert to a single string
        #separate the key value pairs by a comma
        tmp.2<-
          paste(tmp.1, collapse=",", sep="");
        
        return(tmp.2);
          
      } #end of fx()
    
    #similar to a for loop
    json.1 <-
      sapply(1:nrow(df),fx, df=df, colHeadings=names(df));
    
    #converts from a vector to a string.
    #each row is now divided by '},{'
    json.2<-
      paste(json.1, collapse="},{", sep="");
    
    #completes the transformation to json by prepending and appending the appropriate characters
    json.4<-
      paste("[{", json.3, "}]");
    
    return(json.2);
  }
