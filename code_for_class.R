# Function for computing trajectory of H over time

H.prime <- function(H, N, u) { H - H/(2*N) + (1-H)*2*u}

H.eq <- function(N, u) {4*N*u / (4*N*u + 1)}

# initialize and run
# Various values of N and u
res <- vector();
t <- 1500
res[1] <- 0.8
N <- 500
u <- 10^-3

# compute trajectory
for(i in 2:t) {res[i] <- H.prime(res[i-1], N, u)}
plot(res)
abline(h=H.eq(N, u))


# Now a case with a population bottleneck:
res <- vector();
#N <- c(rep(500,499), 2, rep(500,490), rep(10,10), rep(500, 490), rep(10,10) )
N <- rep(c(rep(500, 490),rep(5,10)),3)
t <- 1500
res[1] <- 0.3
u <- 10^-3

# compute trajectory
for(i in 2:t) {res[i] <- H.prime(res[i-1], N[i], u)}
plot(res, type="l" )





