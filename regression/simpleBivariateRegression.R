data = read.csv("Vocab.csv")

X = matrix(nrow = nrow(data), ncol = 2)
y = matrix(nrow = nrow(data), ncol = 1)

for(i in 1:30351){
    X[i, 1] = data[i, 4]
    X[i, 2] = 1
    y[i, 1] = data[i, 5]
}

alpha = solve(t(X) %*% X , t(X) %*% y)
a = alpha[1]
b = alpha[2]
