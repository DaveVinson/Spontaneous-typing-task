#########################################################
#
# this generates final data frame for analysis (.df.anl)
#
# outliers, get digraphs, within-word stroke, keystroke freq, etc. for analysis
#
#########################################################

# clean the file into analysis package (RT > 500 means hesitation by typing lit)
# hist(.df.all[.df.all$RT<500,]$zRT,100,xlab='IKI z-score',main=' ')
# hist(.df.all[.df.all$RT<500,]$RT,100)
print(paste('Proportion of keystrokes not pauses (> 500ms):',
            mean(.df.all$RT<500)))
.df.anl = .df.all[.df.all$RT<500,]

print('Getting character position k')
# get c_k -- within-word variable
wordIx = which(diff(.df.anl$wordN)!=0)
ks = unlist(lapply(diff(wordIx),function(x) {
  return(1:x)
}))
# let's get the corner cases: # issue with SONA 2016 replication // we dont use them anyways
# firstix = which(.df.anl$wordN==2)[1]-1
# lastix = dim(.df.anl)[1]-(firstix+length(ks))
.df.anl$k = 0
# .df.anl$k = c(1:firstix,ks,1:lastix) 

print(paste('removing all rows where Char = non-letter:',
      length(.df.anl[!.df.anl$Char %in% as.factor(letters),]$Char)))
.df.anl <- .df.anl[.df.anl$Char %in% as.factor(letters),]

print(paste('remove bichars that arent characters:', 
      length(.df.anl[!.df.anl$biChar %in% as.factor(digraph),]$biChar)))
.df.anl <- .df.anl[.df.anl$biChar %in% as.factor(digraph),]

print(paste('rows removed containing nonwords:',
            length(.df.anl[!.df.anl$cw %in% uniques$word,])))
.df.anl = .df.anl[.df.anl$cw %in% uniques$word,] 

print('add POS tags and cont/funct variable to .df')
.df.anl = merge(.df.anl, uniques, by.x="cw", by.y="word",all.x=TRUE, all.y=FALSE)

dim.RT = dim(.df.anl)
.df.anl = .df.anl[.df.anl$givenPrevW<Inf & .df.anl$givenNextW<Inf,] # only include words for which we have info
print(paste('Proportion of data left after discarding words not in bigram list:',
            dim(.df.anl)[1]/dim.RT[1]
))

print('Building digraph baselines by taking intercept of each bichar')
# get baseline model for zRT for eachs subject, using restricted set (RT<500)

#Average biomechanical motion ******** new model START 03.15.17 ************
#get digraph averages for each subject
digraphAvgs = aggregate(.df.anl$zRT,by=list(.df.anl$biChar, .df.anl$s),mean)
#assign classes to all digraphs 
digraphAvgs$class = 0
digraphAvgs[digraphAvgs$Group.1 %in% oneletter,]$class = "oneletter"
digraphAvgs[digraphAvgs$Group.1 %in% onefinger,]$class = "onefinger"
digraphAvgs[digraphAvgs$Group.1 %in% onehand,]$class = "onehand"
digraphAvgs[digraphAvgs$Group.1 %in% twohand,]$class = "twohand"
#assign classes for each digraph for each subject
subleveldigs <- aggregate(digraphAvgs$x, list(digraphAvgs$Group.2,digraphAvgs$class), mean)
#relabel columns to be same as digraphAvgs
colnames(subleveldigs) = c("Group.2","class","newx") #newx is the class level averge digraph unique for each subject
#replace digraphAvgs with merged data including digraph classes by subject
digraphAvgs <- merge(subleveldigs, digraphAvgs, by=c("Group.2","class"))

############# Dumb digraph model ################
# digraphAvgs[digraphAvgs$Group.1 %in% oneletter,]$x <- mean(digraphAvgs[digraphAvgs$Group.1 %in% oneletter,]$x)
# digraphAvgs[digraphAvgs$Group.1 %in% onefinger,]$x <- mean(digraphAvgs[digraphAvgs$Group.1 %in% onefinger,]$x)
# digraphAvgs[digraphAvgs$Group.1 %in% onehand,]$x <- mean(digraphAvgs[digraphAvgs$Group.1 %in% onehand,]$x)
# digraphAvgs[digraphAvgs$Group.1 %in% twohand,]$x <- mean(digraphAvgs[digraphAvgs$Group.1 %in% twohand,]$x)
#NOTE: this does not clear all the bichars, but that's because we don't need to, 
#and we don't need them/will never use them in the model because the letters we analyze have been cleaned. 
# ******** new model END 12.17.16 ************

#set this up by subject, too. 
.df.anl$digraphBL = apply(.df.anl,1,function(x) {
  return(digraphAvgs[digraphAvgs$Group.2==x['s'] & digraphAvgs$Group.1==x['biChar'],]$newx)  
# return(digraphAvgs[digraphAvgs$Group.1==x['biChar'],]$newx)  #use this for original and dumb models. 
  #to run the original model, simply use x instead of newx. 
  #to run the dumb digraph model, run the "dumb digraph model" code above, and then use x instead of newx
})


print('using data to estimate frequency of char')
# get f(c) -- frequency of a given keystroke
charFreqs = aggregate(.df.anl$sentN>-1,by=list(.df.anl$Char),sum)
.df.anl$charFreqs = apply(.df.anl,1,function(x) {
  return(charFreqs[charFreqs$Group.1==x['Char'],]$x)  
})

print('Using data to estimate frequency of a bichars')
# get f(c) -- frequency of a given keystroke
bicharFreqs = aggregate(.df.anl$sentN>-1,by=list(.df.anl$biChar),sum)
.df.anl$bicharFreq = apply(.df.anl,1,function(x) {
  return(bicharFreqs[bicharFreqs$Group.1==x['biChar'],]$x)  
})

.df.anl$uninfo = -log(.df.anl$cwProb)
.df.anl$mutualInfoPrior = log(.df.anl$jointProbPrior/(.df.anl$cwProb*.df.anl$pwProb))
.df.anl$mutualInfoNext = log(.df.anl$jointProbNext/(.df.anl$cwProb*.df.anl$nwProb))
.df.anl$wlen = nchar(.df.anl$cw)
.df.anl$ID = .df.anl$givenPrevW/.df.anl$uninfo

print(paste('Final number of trials:',
            dim(.df.anl)[1]
))

#.df.anl = .df.anl[.df.anl$Char!='.' & .df.anl$Char!=' ',] # only include alphanumeric (Rick code)
#.df.anl = .df.anl[.df.anl[as.factor(gsub('[^a-z\'.[:space:]]', '.', .df.anl$Char)),]$Char!=".",] #only include alphabet 
