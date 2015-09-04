library(ggplot2)   # For plotting purposes
library(grid)      # For plotting purposes
library(gridExtra) # For plotting purposes
set.seed(500); m = 50000

# Uniform + Rejection Sampling Method
unifx = numeric(m); unify = numeric(m) # Storage vectors for uniform

for (i in 1:m){
  x = runif(1,min=-1,max=1); y = runif(1,min=-1,max=1); # Square uniform sample space
  while((x^2 + y^2) > 1) { # Resample until point falls inside circle
    x = runif(1,min=-1,max=1); y = runif(1,min=-1,max=1);
  }
  unifx[i] = x; unify[i] = y
}

dens1 = ggplot() +
  geom_point(aes(x = unifx, y = unify), alpha = 0.01) +
  coord_fixed() +
  theme(axis.text = element_text(size = 12), axis.title=element_blank()) +
  ggtitle("Uniform Rejection")

# Polar Sampling Method
polrx = numeric(m); polry = numeric(m) # Storage vectors for uniform

for (i in 1:m){
  r = runif(1); theta = runif(1,min=0,max=(2*pi))
  x = r*cos(theta); y = r*sin(theta); # Convert to (x,y)
  polrx[i] = x; polry[i] = y
}

dens2 = ggplot() +
  geom_point(aes(x = polrx, y = polry), alpha = 0.01) +
  coord_fixed() +
  theme(axis.text = element_text(size = 12), axis.title=element_blank()) +
  ggtitle("Polar Uniform")

grid.arrange(dens1, dens2, ncol = 2)
