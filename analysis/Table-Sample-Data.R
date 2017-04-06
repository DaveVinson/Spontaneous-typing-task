# note: this script requires .df.anl (data frame for analysis)
# generated in the analysis.R, after cleaning, etc.

#########################################################
#
# print out a sample of data
#
#########################################################

.df.anl$priorChar = as.character(.df.anl$priorChar)
.df.anl$Char = as.character(.df.anl$Char)
.df.anl$priorChar[.df.anl$priorChar== ' '] = '` \''

sgtbl <- paste(stargazer(select(.df.anl[500:510,],c(Char,RT,zRT,fk,digraphBL,givenPrevW,uninfo,givenNextW,pw,cw,nw)),
                         summary=F,
                         float.env='table*',
                         covariate.labels=c('$c_{i\'}$' , '$IKI$', '$z_{IKI}$' , 
                                            'first key?' , 'digraph $\\bar{z}_{IKI}$' ,
                                            '$-logP(w|w_{prior})$' , '$-logP(w)$' , '$-logP(w|w_{next})$' , '$w_{t-1}$' , 
                                            '$w_{t}$' , '$w_{t+1}$'),
                         title="Example section of keystroke data used in analyses",
                         font.size='small',
                         column.sep.width = '-1pt',
                         rownames=F,colnames=T))
write(sgtbl,file=paste(rootFolder,'dataTable.tex',sep=''),append=F)


