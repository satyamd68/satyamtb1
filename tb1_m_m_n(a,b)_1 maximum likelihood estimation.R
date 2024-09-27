n=50;rho=0.7;b=10
x<-c()
Mlerho<-c()
for(t in 1:1000){
for(i in 1:n){
  x[i]=rgeom(1,1/(1+(b*rho)))
  x[i]
}
a<-1:n
y=sum(x[a])
Mlerho[t]=y/(b*n)
print(Mlerho[t])
}
b<-1:1000
mean(Mlerho[b])
z1=sum((Mlerho[b]-rho)^2)
z1
mse1=(z1/1000)
mse1


