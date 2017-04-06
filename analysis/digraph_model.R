#######################
#build out the digraph classes 
lp <- c("q","a","z")
lr <-  c("w","s","x")
lm <- c("e","d","c")
li <-  c("r","f","v","t","g","b")
lrt <-  c(" ")

rp <-  c("p")
rr <-  c("o","l")
rm <-  c("i","k")
ri <-  c("u","j","m","y","h","n")

#### same finger #### CORRECT
myLetters <- function(length.out) {
  a <- rep(letters, length.out = length.out)
  grp <- cumsum(a == "a")
  vapply(seq_along(a),
         function(x) paste(rep(a[x], grp[x]), collapse = ""),
         character(1L))
  }
samel <- unlist(myLetters(52)[27:52])

#### 'onefinger' digraph #### CORRECT
onef<-c()
one <- list(lp,lr,lm,li,rr,rm,ri)
for (i in 1:length(one)){
  onef <- c(onef,apply(utils::combn(one[[i]],2),2,paste,collapse=""))
}



#lefthand #CORRECT 120 combos
x <- cbind(unlist(lp),unlist(lr),unlist(lm),unlist(li),unlist(lrt))
x <- unique(Reduce(c,x))
left <- apply(utils::combn(x,2),2,paste,collapse="")

#righthand CORRECT 66
x <- cbind(unlist(rp),unlist(rr),unlist(rm),unlist(ri),unlist(lrt))
x <- unique(Reduce(c,x))
right <- apply(utils::combn(x,2),2,paste,collapse="")

onehandf <- unique(c(right,left)) #CORRECT 186 || remove "  " double space
onehand <- onehandf[!onehandf %in% onef] #remove all onefinger CORRECT - 41. boom: 
onehand <- onehand[!onehand %in% samel] #remove all same letter #nothing removed, because there's no dupes 

#### two hand ####
x <- cbind(unlist(lp),unlist(lr),unlist(lm),unlist(li),
           unlist(rp),unlist(rr),unlist(rm),unlist(ri),unlist(lrt))
x <- unique(Reduce(c,x))
twohand <- apply(combn(x,2),2,paste,collapse="")

#remove all one fingers from onehand
twohand <- twohand[!twohand %in% onehand]
twohand <- twohand[!twohand %in% onef]
twohand <- twohand[!twohand %in% samel]


#flip all letters for each and then you're done! per class
strReverse <- function(x)
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

twohandfull <- c(twohand,strReverse(twohand))
onehandfull <- c(onehand,strReverse(onehand))
onefingerfull <-c(onef,strReverse(onef))
digraph <- c(twohandfull,onehandfull,onefingerfull,samel) #correct number of strings


write.csv(twohandfull,"~/Desktop/twohand_dig.csv",row.names = F,quote=F)
write.csv(onehandfull,"~/Desktop/onehand_dig.csv",row.names = F,quote=F)
write.csv(onefingerfull,"~/Desktop/onefinger_dig.csv",row.names = F,quote=F)
write.csv(samel,"~/Desktop/sameletter_dig.csv",row.names = F,quote=F)
# 
# twohand<-as.factor(read.csv("~/Desktop/twohand_dig.csv")$x)
# onehand<-as.factor(read.csv("~/Desktop/onehand_dig.csv")$x)
# onefinger<-as.factor(read.csv("~/Desktop/onefinger_dig.csv")$x)
# oneletter<-as.factor(read.csv("~/Desktop/sameletter_dig.csv")$x)

#### Checks ######
# #full digraph digraph model
# x <- read.csv("~/Desktop/c.csv", header=F, sep=" ")
# x <- Reduce(c,as.character(unlist(as.list(x))))
# 
# lets <- combn(levels(factor(c(letters[c(1:26)]," "))),2) #+26 for reverse " "+letter 
# digs <- list()
# for (i in 1:26){
#   digs[i]<-paste(lets[,i], collapse="")
# }
# 
# ## all possible (reversed, too) combinations including spacebar
# lets <- tolower(c(as.character(unlist(as.list(x))),unlist(digs)))