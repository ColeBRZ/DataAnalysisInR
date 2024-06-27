data(AirPassengers)
y = log(AirPassengers)
n = length(y)
x = 1:n;

plot(x,y,type="b")

# Proposed Model 
X = matrix(nrow = n, ncol = 5)

for(i in 1:n){
    X[i,1] = cos((2*pi*x[i])/12)
    X[i,2] = sin((2*pi*x[i])/12)
    X[i,3] = x[i]
    X[i,4] = x[i]*x[i]
    X[i,5] = 1
}

ahat = solve(t(X) %*% X , t(X) %*% y);
yhat = X %*% ahat;
lines(x, yhat, col=3);
 
y = AirPassengers
ahat2 = solve(t(X) %*% X , t(X) %*% y);
yhat2 = X %*% ahat2;
plot(x,y,type="b")
lines(x, yhat2, col=2);