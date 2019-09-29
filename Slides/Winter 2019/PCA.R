rm(list = ls())


states=row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)
biplot(pr.out, scale=0)

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x

biplot(pr.out, scale=0)
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')


a=c(1,2,8,-3)
cumsum(a)




# 6,830 Gene Expression on 64 Cancer Cell Lines
rm(list = ls())

library (ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data


dim(nci.data)

nci.labs[1:4]

table(nci.labs)

pr.out=prcomp(nci.data, scale=TRUE)
summary (pr.out)

Cols=function (vec ){
  cols=rainbow (length (unique (vec )))
  return (cols[as.numeric (as.factor (vec))])
}

par(mfrow =c(1,2))
plot(pr.out$x [,1:2], col =Cols(nci.labs), pch =19,
     xlab ="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3) ], col =Cols(nci.labs), pch =19,
     xlab ="Z1",ylab="Z3")



plot(pr.out)


pve =100*pr.out$sdev^2/sum(pr.out$sdev^2)

par(mfrow =c(1,2))

plot(pve, type ="o", ylab="PVE", xlab="Principal Component",
     col ="blue")

plot(cumsum(pve), type="o", ylab ="Cumulative PVE", xlab="
Principal Component ", col =" brown3")







