dieType = c('A', 'B', 'C')

rollDie = function(dieType) {
if(dieType == 'A') {
return(sample(1:6, 1, prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)))
}

else if(dieType == 'B'){
return(sample(c(1, 2, 3, 4, 5, 6), 1, prob = c(2/9, 1/9, 2/9, 1/9, 2/9, 1/9)))
}

else if(dieType == 'C'){
return(sample(c(1, 2, 3, 4, 5, 6), 1, prob = c(1/9, 1/9, 1/9, 2/9, 2/9, 2/9)))
}
}

AClassifier = 0
BClassifier = 0
CClassifier = 0

for(i in 1:1000){
Px1 = 0
Px2 = 0
Px3 = 0

for (die in dieType){

rolls = sapply(1:3, function(x) rollDie(die))
x1 = rolls[1]
x2 = rolls[2]
x3 = rolls[3]

if(die == 'A') {
PA = 1/2
prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
Px1 = Px1 + prob[x1]
Px2 = Px2 + prob[x2]
Px3 = Px3 + prob[x3]
PxnGA = Px1*Px2*Px3

PaGxn = (PxnGA*PA)/PxnGA
}

else if(die == 'B'){
PB = 1/4
prob = c(2/9, 1/9, 2/9, 1/9, 2/9, 1/9)
Px1 = Px1 + prob[x1]
Px2 = Px2 + prob[x2]
Px3 = Px3 + prob[x3]
PxnGB = Px1*Px2*Px3

PbGxn = (PxnGB*PB)/PxnGB
}

else if(die == 'C'){
PC = 1/4
prob = c(1/9, 1/9, 1/9, 2/9, 2/9, 2/9)
Px1 = Px1 + prob[x1]
Px2 = Px2 + prob[x2]
Px3 = Px3 + prob[x3]
PxnGC = Px1*Px2*Px3

PcGxn = (PxnGC*PC)/PxnGC
}

}

if(PcGxn > PbGxn && PcGxn > PaGxn){
CClassifier = CClassifier + 1
}
else if(PbGxn > PaGxn && PbGxn > PcGxn){
BClassifier = BClassifier + 1
}
else if(PaGxn > PbGxn && PaGxn > PcGxn){
AClassifier = AClassifier + 1
}

}

if (CClassifier > BClassifier && CClassifier > AClassifier) {
print("C is the classifier")
} else if (BClassifier > AClassifier && BClassifier > CClassifier) {
print("B is the classifier")
} else if (AClassifier > BClassifier && AClassifier > CClassifier) {
print(AClassifier)
print("A is the classifier")
}