data(iris)

getMode = function(v){
value = unique(v)
value[which.max(tabulate(match(v, value)))]
}

D = as.matrix(dist(iris[,1:4])) 
n = nrow(iris) 
classhat = rep(0,n)
class = as.numeric(iris[,5])
classKNN = rep(0,5) 
for (i in 1:n) {
	DOrdered = order(D[i,])
	for (j in 1:6) { 
		if(j == 1){
			next
		}
		else{ 
			k = DOrdered[j]
			classKNN[j-1] = iris[k,5] 
		}
	}
	mostCommonNN = getMode(classKNN) 
	classhat[i] = mostCommonNN
}

errorRate = sum(class != classhat)/n







