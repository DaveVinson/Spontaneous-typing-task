#http://datadebrief.blogspot.com/2011/03/ascii-code-table-in-r.html
asc <- function(x) { strtoi(charToRaw(x),16L) }
chr <- function(n) { rawToChar(as.raw(n)) }

cleanLastChar = function(w) { # get rid of punctuation at end of words
  if (substr(w,nchar(w),nchar(w)) %in% c('.',',')) {
    w = substr(w,1,nchar(w)-1)
  }
  return(w)
}

getWords = function() { 
    # get prior word; spaces contain indices of space chars
    .df$pw <<- unlist(apply(spaces,1,function(x) {
      if (x[3]==1) {
        w = 'STARTOFTHEPERF' # no prior word if start of performance
      } else {
        w = words[x[3]-1] # get word for this space char
      }      
      return(rep(cleanLastChar(w),x[1]-x[2]))    
    }))
    # get the current word
    .df$cw <<- unlist(apply(spaces,1,function(x) {
      return(rep(cleanLastChar(words[x[3]]),x[1]-x[2]))    
    }))    
    # get the next word
    .df$nw <<- unlist(apply(spaces,1,function(x) {
      if (is.na(words[x[3]+1])) {
        w = 'ENTOFTHEPERF' # if at the end, no next word
      } else {
        w = words[x[3]+1] # get the word for this space
      }
      return(rep(cleanLastChar(w),x[1]-x[2]))
    }))
    # let's make sure the words are all clean for stats processing
    words <<- lapply(words,function(x) {
      return(cleanLastChar(x))
    })
}

getBigramStats = function() { # gather statistics from COCA, build data sheet
  # inefficient... need to make this faster; apply slow, let's do dplyr sometime...
  .df$cwProb <<- unlist(lapply(.df$cw,function(x) { # current word frequency
    ix = which(w2Stats$w2==x)    
    if (length(ix)==1) {
      return(w2Stats[ix,]$fq/totalW2Count)
    } else {
      return(0)
    }
  }))
  .df$pwProb <<- unlist(lapply(.df$pw,function(x) { # prior word frequency
    ix = which(w1Stats$w1==x)    
    if (length(ix)==1) {
      return(w1Stats[ix,]$fq/totalW1Count)
    } else {
      return(0)
    }
  }))  
  .df$nwProb <<- unlist(lapply(.df$nw,function(x) { # prior word frequency
    ix = which(w2Stats$w2==x)    
    if (length(ix)==1) {
      return(w2Stats[ix,]$fq/totalW2Count)
    } else {
      return(0)
    }
  }))    
  .df$givenPrev <<- unlist(apply(.df,1,function(x) { # get probability given prior
    bg = paste(x['pw'],x['cw'])
    ix = which(bigramStats$bg==bg)
    if (length(ix)==1) {
      bigramFreq = bigramStats[ix,]$fq
      pwFreq = w1Stats[w1Stats$w1==x['pw'],]$fq
      return(bigramFreq/pwFreq) # bigram freq divided by frequency of prior word (GIVEN prior word)
    } else {
      return(0)
    }
  }))
  .df$givenNext <<- unlist(apply(.df,1,function(x) { # get probability given next
    bg = paste(x['cw'],x['nw'])
    ix = which(bigramStats$bg==bg)    
    if (length(ix)==1) {
      bigramFreq = bigramStats[ix,]$fq
      pwFreq = w2Stats[w2Stats$w2==x['nw'],]$fq
      return(bigramFreq/pwFreq) # bigram freq divided by frequency of prior word (GIVEN prior word)
    } else {
      return(0)
    }
  }))
  .df$jointProbPrior <<- unlist(apply(.df,1,function(x) { # get joint probability with PRIOR
    if (x['pw'] < x['cw']) {
      bg = paste(x['pw'],x['cw'])
    } else {
      bg = paste(x['cw'],x['pw'])
    }
    ix = which(jointCounts$bg==bg)
    if (length(ix)==1) {
      return(jointCounts[ix,]$fq/totalBigramCount)
    } else {
      return(0)
    }
  }))
  .df$jointProbNext <<- unlist(apply(.df,1,function(x) { # get joint probability with PRIOR
    if (x['nw'] < x['cw']) {
      bg = paste(x['nw'],x['cw'])
    } else {
      bg = paste(x['cw'],x['nw'])
    }
    ix = which(jointCounts$bg==bg)
    if (length(ix)==1) {
      return(jointCounts[ix,]$fq/totalBigramCount)
    } else {
      return(0)
    }
  }))  
}

getDataFrame = function(dataText) { # let's import the sheet from AMT/SONA (*.txt files)
  if (class(try(fromJSON(dataText),silent=TRUE))!="try-error") {
    dataBlob = fromJSON(dataText)    
    rtsClean = as.numeric(unlist(strsplit(gsub("\\]","",gsub("\\[","",dataBlob$rtsClean)),',')))
    charsClean <<- as.numeric(unlist(strsplit(gsub("\\]","",gsub("\\[","",dataBlob$charsClean)),',')))    
  } else {
    return(-1) # no JSON data!
  }
  
  if (sum(charsClean==0)>0 | length(charsClean)<10) {
    return(-1) # not typed enough OR no characters at all coded (0)
  }
  
  rtsClean = diff(rtsClean) # data is cumulative ms, so take diff for RT
  biChars = embed(unlist(strsplit(tolower(chr(charsClean)),"")),2) # get digraph
  biChars = biChars[,2:1] # reverse 'em to get prior / next
  spaces = cumsum(charsClean[2:length(charsClean)]==32) 
  sents = cumsum(charsClean[2:length(charsClean)]==46)
  dRT = rtsClean-mean(rtsClean[rtsClean<500]) # divergence from expected RT for z score
  sdRT = sd(rtsClean[rtsClean<500]) # dispersion of RT given 500 ms (for z score)
  if (length(dataBlob$sonaid)>0) { # handle slightly different naming for separate studies (sona/amt)
    dataBlob$email = dataBlob$sonaid
  }
  .df = data.frame(s=dataBlob$email,priorChar=biChars[,1], 
                   Char=biChars[,2],
                   RT=rtsClean,
                   zRT=dRT/sdRT,
                   wordN = spaces,
                   sentN = sents,
                   biChar = apply(biChars,1,function(x) {paste(x,collapse='')}))
  
  .df$fk = .df$priorChar==' ' # first key is always if prior is a space
  return(.df)
}






