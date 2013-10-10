# startup messages:
cat('An implementation of Perceptron Learning Algorithm (PLA) in R.\n\n')
cat('Two random points will be taken in the plane to form a line that defines the target function (black line).\n\n')
cat('N data points of this target function will be randomely generated, and PLA will try to learn this target function, through a series of green lines.\n\n')

N<-readline('Please input N:') # read in N
N<-as.integer(N) # transform from string to integer

# Now implement the function that generates target function:
targetf <- function(){

    p <- runif(2,-1,1)
    q <- runif(2,-1,1)

    func <- function(x){
        bdy <- (p[2]-q[2])/(q[1]-p[1]) * (x[1] - p[1]) + x[2] - p[2]
        ifelse(bdy >= 0,1,-1)
    }
    func
}

# Now create the target function 'f':
f <- targetf()

# Now generate N random 2D points:
pts <- matrix(runif(N*2,-1,1),ncol=2)  #random points became 100*2 matrix
# plot these points:
results <- apply(pts,1,f) #apply for multivariable functions
a <- results == 1
plot(pts[a],col='red',xlim=c(-1,1),ylim=c(-1,1),xlab='X',ylab='Y')
points(pts[!a],col='blue')

## plot the target function in black: ##

# first, read the two points that defined the target function.
p <- get('p',environment(f))
q <- get('q',environment(f))

#The following is the standard way to plot line based on two points
rptx <- rbind(c(p[1],1),c(q[1],1))
rpty <- c(p[2],q[2])
ab <- solve(rptx,rpty) #ab[1]=slope, ab[2]=intercept  y = ax + b
plot(pts[results==1,1],pts[results==1,2],col='blue',xlim=c(-1,1),ylim=c(-1,1),xlab='x',ylab='y')
points(pts[results==-1,1],pts[results==-1,2],col='red',xlim=c(-1,1),ylim=c(-1,1))
abline(ab[2],ab[1]) #abline(intercept,slope)


# Now prepare PLA:
w <- vector('numeric',3)
z <- cbind(1,pts)
r <- sign(z %*% w)
hit <- results == r

cat('Iteration starts!  Press [Return] to iterate.\n')
while(!all(hit)){
    if(sum(!hit)>1){
        w <- w + f(pts[!hit,][1,]) * c(1,pts[!hit,][1,])
        } else w <- w + f(pts[!hit,]) * c(1,pts[!hit,])

    r <- sign(z %*% w)
    hit <- results == r

    abline(-w[1]/w[3], -w[2]/w[3],col='green')
    print(w)
    print(data.frame(pts,hit))

    readline()
}
cat('Iteration ends.  The PLA has just learnt the red line!\n')
abline(-w[1]/w[3], -w[2]/w[3],col='red')
