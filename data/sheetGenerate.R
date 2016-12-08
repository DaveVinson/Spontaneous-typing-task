
rootFolder = '~/Dropbox/webstuff/plot-summary/analysis/final-to-share/data/amt.replication.mar.2016/' # replication (AMT)
#rootFolder = '~/Dropbox/webstuff/plot-summary/analysis/final-to-share/data/sona.apr.2016/' # second study (SONA / with genre)
#rootFolder = '~/Dropbox/webstuff/plot-summary/analysis/final-to-share/data/sona.dec.2015/' # first SONA study 

setwd(rootFolder)

source('../functions.R')
source('../load.R')

files <- list.files(pattern = "[.]txt$")

filen = 1
zeroSum = 0 # subjects whose keyboards produce 0 character codes (shift?); just omit (small %)
agged = 0
write('',file='baddata',append=F)
for (f in files[1:length(files)]) { # I like loops and I'm not ashamed of it
  dataTexts = readChar(f, file.info(f)$size) 
  # optional: do some stripping
  dataTexts = unlist(strsplit(dataTexts,'\n')) # \n could mess up word find, why not trim
  for (j in 1:length(dataTexts)) {
    print(c(f,j,filen))
    filen = filen + 1
    .df = getDataFrame(dataTexts[j]) # sheet for this iteration
    
    if (length(.df)[1]>1) { # if there's data, build the sheet!
      txt = tolower(chr(charsClean))
      words = unlist(strsplit(txt,' '))
      
      spaces = c(1,which(charsClean==32),length(charsClean))
      spaces = cbind(embed(spaces,2),1:(length(spaces)-1))    
      
      getWords() # uses <<- to get words and append to .df
      
      getBigramStats() # uses <<- to store new .df data.frame
      
      if (agged==0) { # bind the sheets to each other
        .df.all = .df        
        agged = 1
      } else {
        .df.all = rbind(.df.all,.df)
      }  
    }
    else {
      write(c(f,j),file='baddata',append=T) # store bad data in a file to consult WHY data is discarded
      write('--****--',file='baddata',append=T)
      zeroSum = zeroSum + 1
    }
  }
}

.df.all$givenPrevW = -log(.df.all$givenPrev) # information score
.df.all$givenNextW = -log(.df.all$givenNext)

save(.df.all,zeroSum,file='keystrokes.RData') # save to save time later