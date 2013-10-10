# startup messages:
cat('An implementation of Perceptron Learning Algorithm (PLA) in R.\n\n')
cat('Two random points will be taken in the [-1,1]X[-1,1] region to form a line that defines the target function (black line).\n\n')
cat('N data points of this target function will be randomely generated, and PLA will try to learn this target function, through a series of green lines.\n\n')

N<-readline('Please input N:') # read in N
N<-as.integer(N) # transform from string to integer

# Make sure that N is valid, such that the program does not terminate with error:
while(is.na(N) | N<1){
    N<-readline('Invalid N. Please input N:')
    N<-as.integer(N)
}

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

# Evaluate their value under the target function:
results <- apply(pts,1,f) #apply 'f' on every row of 'pts'


### ploting ###

## plot out the randomly-generated points and their value under target function ##
plot(pts[results==1,1],pts[results==1,2],col='blue',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),xlab='X',ylab='Y')
points(pts[results==-1,1],pts[results==-1,2],col='red',xlim=c(-1,1),ylim=c(-1,1))

## plot the target function with a black line: ##
# first, read the two points that defined the target function.
p <- get('p',environment(f))
q <- get('q',environment(f))
#The following is the standard way to plot line based on two points
rptx <- rbind(c(p[1],1),c(q[1],1))
rpty <- c(p[2],q[2])
ab <- solve(rptx,rpty) #ab[1]=slope, ab[2]=intercept  y = ax + b
abline(ab[2],ab[1]) #abline(intercept,slope)

## mark out the region where random data are generated with dashed-gray lines:
lines(c(-1,1),c(-1,-1),col='gray',lty=2)
lines(c(-1,1),c(1,1),col='gray',lty=2)
lines(c(-1,-1),c(-1,1),col='gray',lty=2)
lines(c(1,1),c(-1,1),col='gray',lty=2)


# Now prepare PLA:
w <- vector('numeric',3)
z <- cbind(1,pts)
r <- sign(z %*% w)
hit <- results == r
terminate<-''
counter <- 0
cat('Iteration starts!  Press [Return] to iterate.  Input \'q\' to terminate.\n')
while(!all(hit) & terminate!='q'){
    if(sum(!hit)>1){
        w <- w + f(pts[!hit,][1,]) * c(1,pts[!hit,][1,])
        } else w <- w + f(pts[!hit,]) * c(1,pts[!hit,])

    r <- sign(z %*% w)
    hit <- results == r

    abline(-w[1]/w[3], -w[2]/w[3],col='green')
    print(w)
    print(data.frame(pts,hit))

    terminate<-readline()
    counter <- counter + 1
}

abline(-w[1]/w[3], -w[2]/w[3],col='red')
if (terminate=='q') cat('WARNING: Manual termination.\n')
cat('Algorithm ends in',counter,'iterations.  The PLA has just learnt the red line!\n')
