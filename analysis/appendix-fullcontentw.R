######## center/scale all predictor variables
.df.anl$digraphBL <- scale(.df.anl$digraphBL)
.df.anl$charFreqs <- scale(.df.anl$charFreqs)
.df.anl$givenPrev <- scale(.df.anl$givenPrev)
.df.anl$uninfo    <- scale(.df.anl$uninfo)
.df.anl$givenNextW<- scale(.df.anl$givenNextW)
######### Build out the full model for content words 

cmat <- corr.test(x = .df.anl[.df.anl$category=="content",][c(20:21,25:26,28)])$r #psych package 
cmat <- corr.test(x = .df.anl[.df.anl$category=="content" & .df.anl$fk,][c(20:21,25:26,28)])$r #psych package 

lmo_baseline <- lmer(zRT~digraphBL+charFreqs+(1|cw)+(1|s),
                     data=.df.anl[.df.anl$category=="content",])

lmo <- lmer(zRT~digraphBL+charFreqs+givenPrevW+uninfo+givenNextW+(1|cw)+(1|s),
            data=.df.anl[.df.anl$category=="content",])
lmo_fk = lmer(zRT~digraphBL + charFreqs+givenPrevW+uninfo+givenNextW+(1|cw)+(1|s),
              data=.df.anl[.df.anl$category=="content" & .df.anl$fk,]) # first key predictions

lmo_ik = lmer(zRT~digraphBL+charFreqs+givenPrevW+uninfo+givenNextW+(1|cw)+(1|s),
              data=.df.anl[.df.anl$category=="content" & !.df.anl$fk,])

stats.table = stargazer(lmo_baseline,lmo,lmo_fk,lmo_ik,star.cutoffs=c(.001,.0001,.00001),
                        omit.stat=c('f','res.dev','adj.rsq','ser'),float.env='table*',
                        dep.var.labels=c('$z_{IKI}$','$Residual z_{IKI}$','$z_{IKI-first}$','$z_{IKI-internal}$'),
                        dep.var.labels.include=T,
                        covariate.labels=c('$\\bar{z}_{IKI,digraph}$',
                                           '$f(c_{i\'})$',
                                           '$p(w|w_{prior})$',
                                           '$p(w)$',
                                           '$p(w|w_{next})$',
                                           'Constant'))
write(stats.table,file=paste(rootFolder,'resultsTable_full-content-sona2.tex',sep=''),append=F)

########## adds to table. 
lmo_fk_baseline = lmer(zRT~digraphBL + charFreqs+(1|cw)+(1|s),
                       data=.df.anl[.df.anl$category=="content" & .df.anl$fk,])

lmo_ik_baseline = lmer(zRT~digraphBL+charFreqs+(1|cw)+(1|s),
                       data=.df.anl[.df.anl$category=="content" & !.df.anl$fk,])

intercept <- lmer(zRT~1+(1|cw)+(1|s),
                  data=.df.anl[ .df.anl$category=="content",])

BIC(intercept) # 36329.32
BIC(lmo_baseline)
BIC(lmo_fk_baseline) 
BIC(lmo_ik_baseline)
r.squaredGLMM(intercept) # R2c 0.03649369 


r.squaredGLMM(lmo_baseline)
r.squaredGLMM(lmo_fk_baseline)
r.squaredGLMM(lmo_ik_baseline)

r.squaredGLMM(lmo)
r.squaredGLMM(lmo_fk)
r.squaredGLMM(lmo_ik)

2^(BIC(intercept)-BIC(lmo_baseline))
2^(BIC(lmo_baseline)-BIC(lmo))
2^-(BIC(lmo_fk_baseline)-BIC(lmo_fk))
2^(BIC(lmo_ik_baseline)-BIC(lmo_ik))

#reported in text