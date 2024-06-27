data(nottem) 
y = nottem 
n = length(y)
x = 1:n; 

plot(x,y,type="b")

# Model 1
X = cbind(x, rep(1,n)); 
ahat1 = solve(t(X) %*% X , t(X) %*% y);
yhat1 = X %*% ahat1;
lines(x, yhat1, col=2);

# Model 2
X1 = matrix(nrow = n, ncol = 3)

for(i in 1:n){
    X1[i,1] = cos((2*pi*x[i])/12)
    X1[i,2] = sin((2*pi*x[i])/12)
    X1[i,3] = 1
}

ahat2 = solve(t(X1) %*% X1 , t(X1) %*% y);
yhat2 = X1 %*% ahat2;
lines(x, yhat2, col=3);

## Model 3
X2 = matrix(nrow = n, ncol = 4)

for(i in 1:n){
    X2[i,1] = cos((2*pi*x[i])/12)
    X2[i,2] = sin((2*pi*x[i])/12)
    X2[i,3] = 1
    X2[i,4] = x[i]
}

ahat3 = solve(t(X2) %*% X2 , t(X2) %*% y);
yhat3 = X2 %*% ahat3;
lines(x, yhat3, col=4);
