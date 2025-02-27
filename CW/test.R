time <- (rep(c(0),10))
num <- c(1:10)
res <- double(10)

t0 <- strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")
t1 <- strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")
time[1]<-as.double(t1-t0)

#1

xA <- seq(100, 200, by =5)
res[1] <- sum(xA)
t2 <- strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")
time[2]<-as.double(t2-t0)

#2

t0 <- Sys.time()
res[2] <- length(xA)
res[3] <- mean(res[1])
t2 <- Sys.time()
time[2]<-as.double(t2-t0)

#3

norm_vec <- rnorm(length(xA), mean=5, sd=7)
res[4] <- round(sd(norm_vec))
t3 <- Sys.time()
time[3]<-as.double(t3-t2)

#4

arr <- array(xA, c(5,length(xA)/5))
sinsum <- sum(sin(arr))
RES[5] <- round(sinsum, 4)
t4 <- Sys.time()
time[4] <- t4-t3

#5

xA <- append(xA, seq(205, 220, by = 5))
length(xA)
matr <- matrix(xA, nrow = 5)
matr <- matr[-2,]
matr <- matr[-5,]
matr
RES[6] <- nrow(matr)+ncol(matr)
t5 <- Sys.time()
time[5] <- t5-t4

#6

new_list <-rep(c(TRUE, FALSE), 5)
new_list
new_list_sum <- any(new_list)
RES[7] <- new_list_sum
t6 <- Sys.time()
time[6] <- t6-t5


#7

RES[8] <- identical(arr, matr)
t7 <- Sys.time()
time[7] <- t7-t6

print(time)

arr
matr
matr <-array(matr, c(4,5))
arr <- arr[-2,]
arr <-arr[-5,]
arr
RES[9] <- identical(arr, matr)
t8 <- Sys.time()
time[8] <- t8-t7

#9

result <- data.frame(
  TIME <- time,
  TASK <- num,
  RESULTS <- res
)
result
t9<- Sys.time()
time[9]<-t9-t8















