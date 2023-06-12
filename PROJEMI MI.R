U1<-seq(0.001,0.99,0.01)
U1
#first Assumed a and b
Aa1=4 ; Ab1=2
X1=abs(Ab1*(1-(1-U1)^(-1/Aa1)))
X1
n=length(U1)
M1<-function(theta){(length(U1)*log(theta[1]))-(length(U1)*log(theta[2]))+sum(log(X1+1))+sum(X1)-((theta[1]+1)*sum(log(1+((X1*exp(X1))/theta[2]))))}
L1<-nlm(M1, theta <- c(0.5,0.8), hessian=TRUE)
L1
#first estimated a and b 
ss=(6.555346e-06*1000)
ss
b=(6.353147e-01*1000)
b
as_a1=(4580.007/1000) ; as_b1=(3082.498/1000)
es_X1=abs(as_b1*(1-(1-U1)^(-1/as_a1)))
es_X1
plot(es_X1,type="l",col="red",lwd=2)


#second Assumed a and b
U2<-seq(0.0012,0.99,0.012)
U2
Aa2=5 ; Ab2=3
X2=abs(Ab2*(1-(1-U2)^(-1/Aa2)))
X2
M2<-function(theta){(length(U2)*log(theta[1]))-(length(U2)*log(theta[2]))+sum(log(x+1))+sum(x)-((theta[1]+1)*sum(log(1+((x*exp(x))/theta[2]))))}
L2<-nlm(M2, theta <- c(0.5,0.5), hessian=TRUE)
L2
#second estimated a and b 
as_a2=(4559.411/1000) ; as_b2=(2319.435/1000)
as_X2=abs(as_b2*(1-(1-U2)^(-1/as_a2)))
as_X2
plot(as_X2,type="l",col="blue",lwd=2)
#third Assumed a and b


U3<-seq(0.0023,0.99,0.023)
U3
Aa3=7 ; Ab3=5
X3=abs(Ab3*(1-(1-U3)^(-1/Aa3)))
X3
M3<-function(theta){(length(U3)*log(theta[1]))-(length(U3)*log(theta[2]))+sum(log(x+1))+sum(x)-((theta[1]+1)*sum(log(1+((x*exp(x))/theta[2]))))}
L3<-nlm(M3, theta <- c(0.5,0.5), hessian=TRUE)
L3
#third estimated a and b 
as_a4=(5160.79008/1000) ; as_b4=(75.38138/1000)
as_X4=abs(as_b4*(1-(1-U4)^(-1/as_a4)))
as_X4
plot(as_X4,type="l",col="green",lwd=2)


#fourth Assumed a and b
U4<-seq(0.0013,0.99,0.013)
U4
Aa4=9 ; ba4=3
X4=abs(Ab4*(1-(1-U4)^(-1/Aa4)))
X4
M4<-function(theta){(length(U4)*log(theta[1]))-(length(U4)*log(theta[2]))+sum(log(x+1))+sum(x)-((theta[1]+1)*sum(log(1+((x*exp(x))/theta[2]))))}
L4<-nlm(M4, theta <- c(0.5,0.5), hessian=TRUE)
L4
#third estimated a and b 
as_a4=(4712.748/1000) ; as_b4=(94.90423/1000)
as_X4=abs(as_b4*(1-(1-U4)^(-1/as_a4)))
as_X4
plot(as_X4,type="l",col="brown",lwd=2)

#fifth Assumed a and b
U5<-seq(0.0013,0.99,0.013)
U5
Aa5=10 ; Ab5=6
X5=abs(Ab5*(1-(1-U5)^(-1/Aa5)))
X5
M5<-function(theta){(length(U5)*log(theta[1]))-(length(U5)*log(theta[2]))+sum(log(x+1))+sum(x)-((theta[1]+1)*sum(log(1+((x*exp(x))/theta[2]))))}
L5<-nlm(M5, theta <- c(0.5,0.5), hessian=TRUE)
L5
#fifth estimated a and b 
as_a4=(4712.748/1000) ; as_b4=(94.90423/1000)
as_X4=abs(as_b4*(1-(1-U4)^(-1/as_a4)))
as_X4
plot(as_X4,type="l",col="yellow",lwd=2)





