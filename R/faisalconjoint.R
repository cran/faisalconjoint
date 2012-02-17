faisalconjoint <-
function(con,lal)
{
mf  <<- con
level1 <<- lal

uth <<- names(mf)
level2 <<- NA
Level = rbind (level1, level2)

## Convert data file into matrix
dm=data.matrix(mf)

## total variable and total data
tc=NCOL(mf)
tr=NROW(mf)
tc1=tc-1

## Takeing mean of every factor, level wise
mm=0
k=0
Factor=0

for (i in 2:tc1)
{
for (j in 1:max( dm [,i]))
{	
k=k+1
mm [k] <- mean(dm[,tc] [dm [,i]== j ] )
Factor [k] <- uth [i]		
}		
}					

Factor [k+1] <- "Constant"
# calculation of Path Worth Utilities
Utility=mm-mean(mm)			
Utility [k+1] <- mean(mm)
hd1=data.frame(Factor,Level,Utility)

# calculation of Factor Importance in Percentages
ut2=abs(mm-mean(mm))
ut2sum=sum(ut2)
k=0
kk=0
fsum=0
Factor1=' '
for (i in 2:tc1)
{
fsum1=0
for (j in 1:max( dm [,i]))
{
k=k+1
fsum1 = fsum1 + ut2 [k]
}
kk=kk+1
Factor1 [kk] <- Factor [k]
fsum [kk] <- fsum1
}

Percentage <- fsum/sum(ut2)*100

for (sr in 1:kk){}
hd2=data.frame(Factor1,Percentage)

# Calculation for bar plot in order
bar <- barplot( Percentage [order(-Percentage)], main = "Conjoint Factor Importance",
xlab="Factors", ylab="Percentage",
names.arg = Factor1 [order(-Percentage)],
border="blue",
density= sort(Percentage,decreasing = TRUE),
ylim = c(0, 100) )
imp <- Percentage [order(-Percentage)]

# display result
View (hd1, "Conjoint Parth Worth Utility")
View (hd2, "Conjoint Factor Importance")

# display Graph
text(bar, imp + 3, format(imp), xpd = TRUE)

}

