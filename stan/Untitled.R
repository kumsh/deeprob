library(rstan)
library(ggplot2)
library(bayesplot)
library(rethinking)
library(gridExtra)
library(kableExtra)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
N=500
size_t0=rnorm(N,mean=10, sd=1)
size_t1=rnorm(N, mean=35,sd=1)

x=runif(N,min=0,max=14)
y=runif(N,min=0,max=14)
distmat=as.matrix(dist(cbind(x,y),method="euclidean"))
sizemat=matrix(rep(size_t0,500),nrow=500,ncol=500,byrow=T)


#Here, the parameters are set to produce a biologically reasonable outcome:
alpha=0.8 # Intercept.
beta=0.2 # Size dependent growth (we hypothesize that bigger individuals grow faster).
a3=.035 # Neighbor effect parameter
a2=0.05 # Scaling parameter which determines the spatial reach of neighbor effect
a1=0.1 # Size dependent effect of the neighbor.
true_param=c(alpha,beta,a3,a2,a1,sd=0.01)
growth=rnorm(N,
             mean=alpha+
               size_t0*beta+
               a3*rowSums(exp(a1*log(sizemat)-(distmat^2*a2))),
             sd=0.01)

distmat=ifelse(distmat>10,0,distmat) #here, the cut-off is at 10m radius. 

sizemat=ifelse(distmat==0,0,sizemat)

case_stanlist=list(N=N,growth=growth,size_t0=size_t0, distmat=distmat,sizemat=sizemat)

fit_plant <- stan(
  file = "/Users/urgupta/Documents/deeprob_1/spatial_neighbour/Untitled.stan",
  data = case_stanlist
)