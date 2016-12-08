print("Loading libraries");

library(tm)
library(RWeka) # for NgramTokenizer
library(entropy) # pulls the shannon entropy measures
library(qdap) # for converting csv to term doc matrix
library(EntropyEstimation) # necessary for entropy.sd (but not necessary for current analysis)
library(reldist)
library(lme4)
library(dplyr)
library(ggplot2) # for obvious
library(stargazer) # for printing
library(rjson) 
library(gridExtra) # grid.arrange with ggplot2

print("Loading COCA");

bgs = read.csv('../coca/bigs.csv') # COCA large bigrams (Vinson & Davis)
#bgs = read.csv('../analysis/coca/1.million.bigrams.txt',sep='\t')

print("Prepping COCA");

colnames(bgs) = list('w1','w2','fq') # Vinson & Davis version
#colnames(bgs) = list('fq','w1','w2') # COCA 1 million version
bgs$w1 = as.character(bgs$w1);bgs$w2 = as.character(bgs$w2)

bgs = mutate(bgs,bg = paste(w1,w2)) # get one bigram string
 
# let's thin to make processing more efficient; only get bigrams we need!
files <- list.files(pattern = "[.]txt$")
filen = 1
words = c()
for (f in files[1:length(files)]) { # I like loops and I'm not ashamed of it
  dataTexts = readChar(f, file.info(f)$size) 
  # optional: do some stripping
  dataTexts = unlist(strsplit(dataTexts,'\n')) # \n could mess up word find, why not trim
  for (j in 1:length(dataTexts)) {
    print(paste('Thinning bigram list for efficiency; ',c(f,j)))
    .df = getDataFrame(dataTexts[j]) # sheet for this iteration
    if (length(.df)[1]>1) { # if there's data, build the sheet!
      txt = tolower(chr(charsClean))      
      words = c(words,unlist(strsplit(txt,' ')))
    }
  }  
}

words = gsub("\\.","",words)
words = gsub("\\,","",words)

allBigrams = unique(embed(words,2)[,c(2,1)])
colnames(allBigrams) = c('w1','w2')
allBigrams = mutate(data.frame(allBigrams),bg = paste(w1,w2)) # get one bigram string
bigramStats = bgs[bgs$bg %in% allBigrams$bg,]

allColocs = rbind(unique(embed(words,2)[,c(2,1)]),unique(embed(words,2)))
colnames(allColocs) = c('w1','w2')
allColocs = mutate(data.frame(allColocs),bg = paste(w1,w2)) # get one bigram string                  
jointCounts = bgs[bgs$bg %in% allColocs$bg,]
jointCounts$bg = unlist(apply(jointCounts,1,function(x) {
  if (x['w1']<x['w2']) {
    return(x['bg'])
  } else {
    return(paste(x['w2'],x['w1']))
  }
}))
jointCounts = jointCounts %>% group_by(bg) %>% summarise(fq = sum(fq)) # okay, no looping for this...

w1Stats = bgs[bgs$w1 %in% words,] %>% group_by(w1) %>% summarise(fq = sum(fq))
w2Stats = bgs[bgs$w2 %in% words,] %>% group_by(w2) %>% summarise(fq = sum(fq))

totalBigramCount = sum(bgs$fq)
totalW1Count = sum(w1Stats$fq)
totalW2Count = sum(w2Stats$fq)




