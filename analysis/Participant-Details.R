#########################################################
#
# participant details
#
#########################################################

print(paste('Number of participants in the data in total:',
            length(unique(.df.all$s))+zeroSum # zeroSum is # of bad participants
))
print(paste('Number of subjects after bad data removed (done automatically, see sheetGenerate.R):',
            length(unique(.df.all$s)) # those left
))
print(paste('Percentage of participants who engaged the interface, remaining in the dataset:',
            round(100*length(unique(.df.all$s))/(length(unique(.df.all$s))+zeroSum)) # percentage included
))
print(paste('Number of bad participants (again, see sheetGenerate for this automatic process):',
            zeroSum 
))

# distribution of student contributions (using ALL participants)
nWords = aggregate(.df.all$wordN,by=list(.df.all$s),max)
print(paste('Mean words per summary:',mean(nWords$x)))
print(paste('Longest summary, words:',max(nWords$x)))
print(paste('Shortest summary, words:',min(nWords$x)))
nSentences = aggregate(.df.all$sentN,by=list(.df.all$s),max)
print(paste('Mean sentences per summary:',mean(nSentences$x)))
print(paste('Longest summary, sentences:',max(nSentences$x)))
print(paste('Shortest summary, sentences:',min(nSentences$x)))
nChars = aggregate(.df.all$RT>-100,by=list(.df.all$s),sum)
print(paste('Mean keystrokes per summary:',mean(nChars$x)))
print(paste('Longest summary, keystrokes:',max(nChars$x)))
print(paste('Shortest summary, keystrokes:',min(nChars$x)))

# typing speed of subjects
mRTs = aggregate(.df.all$RT,by=list(.df.all$s),sum)
print(paste('Mean words / minute:',
            mean(nWords$x/(mRTs$x/(1000*60)))
))
print(paste('Fastest typist (including pauses):',
            max(nWords$x/(mRTs$x/(1000*60)))
))
print(paste('Slowest typist (including pauses):',
            min(nWords$x/(mRTs$x/(1000*60)))
))