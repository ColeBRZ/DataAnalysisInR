dat = read.csv("ais.csv",stringsAsFactors=FALSE, sep=",")

X = data.matrix(dat[,3:12])
y = data.matrix(dat[,2])

ahat = solve(t(X) %*% X , t(X) %*% y)

error = y - yhat

sse = sum(error*error) 

X1 = matrix(nrow = 202, ncol = 9)

for (i in 1:10){
    X1 = X[,-i]
    ahat1 = solve(t(X1) %*% X1 , t(X1) %*% y)
    yhat1 = X1 %*% ahat1
    error1 = y - yhat1
    sse1 = sum(error1*error1)
    print(sse1)
}