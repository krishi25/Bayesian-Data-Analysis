
# finding a0 and b0 in prior
body_count_prior=c(42,34,4,19,10,20)
time_min <- c(113,110,105,110,105,119)
time_hour <- time_min/60
mean_prior=mean(body_count_prior/time_hour)
variance_prior=var(body_count_prior/time_hour)
ao=mean_prior^2/variance_prior # a0=2.26 #2.31
bo=mean_prior/variance_prior #bo=0.105 #0.2
#data from tarantino's movie
set.seed(100)
body_count <- c(10,7,4,63,11,48,47,19)
mean(body_count)
length_hr <- c(1.65,2.57,2.57,1.85,2.26,2.55,2.75,3.12)
body_count
hist(body_count)
length_min <- (length_hr)*60

#prior
prior_theta=rgamma(100000,ao,bo) #a0=2.26,bo=0.10

hist(prior_theta,breaks =20,col="lightblue")
# posterior distribution

theta <- rgamma(100000,(ao+209))/(bo+19.33)# gamma posterior
hist(theta,col="lightblue")
hdi(theta,credMass = 0.95) # 9.44 ,12.37

# posterior predictive

shape=209+ao
prob=(1+bo+19.33)
# predictive y tilda comes from poisson likelihood
hist(theta)
plot(density(theta))

# Normal approximation for posterior
#from gamma
expected=(ao+91.23)/(bo+19.33)
sd=sqrt((ao+91.23)/(bo+19.33))
#from poisson
exp_poisson=theta
sd_poisson=sqrt(exp_poisson)
# mean and variance of posterior predictive ytilda
exposure=1-(1/(bo+19.33))
y_tilda=rnbinom(n.sim,a+sum(body_count),exposure)
hist(y_tilda)
hdi(y_tilda,credMass = 0.95)

# model checking

yrep <- mapply(rpois, n = n.obs,theta)
obs.min=min(body_count)
obs.max=max(body_count)
n.sim <- 100000
n.obs=8
dim(yrep)
sim.min <- apply(yrep, 2, min)
sim.max <- apply(yrep,2,max)
pvalmin <- length(sim.min[sim.min >= obs.min]) / n.sim #0.66
pvalmax <- length(sim.max[sim.max <=obs.max]) / n.sim #1

# replicated minimum
hist(sim.min,col = "lightblue", border = "black",ann=F,xlim=c(2,35),freq = F,right = F,axes=T)
lines(rep(obs.min, 2), c(0, 100000), col = "red", lwd = 3)
mtext(side = 3, paste0("p-value = ", round(pvalmin,3)))
title( "Replicated minimum")

# replicated maximum
hist(sim.max,col = "lightblue", border = "black",ann=F,xlim=c(2,70),freq = F,right = F,axes=T)
lines(rep(obs.max, 2), c(0, 100000), col = "red", lwd = 3)
mtext(side = 3, paste0("p-value = ", round(pvalmax,3)))
title("Replicated maximum")

# dispersion

sim.mean <- apply(yrep,2,mean)
sim.var <- apply(yrep,2,var)
dispersion=sim.var/sim.mean
hist(dispersion,xlim=c(0,20.1),col="lightblue",freq = F,right = F,axes=T,
     xlab=expression(paste("T(y,",theta, ")")),ann=T,main=NULL)
data_dispersion=var(body_count)/mean(body_count)
lines(rep(data_dispersion, 2), c(0, 100000), col = "red", lwd = 3)
title( "Variance to Mean ratio")
mtext(side = 3, paste0("p-value = ", round(pvalmax,3)))


