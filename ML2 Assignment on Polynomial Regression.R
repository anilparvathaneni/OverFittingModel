install.packages("ggplot2")  # Installing package for plotting graphs
setwd("C:\\Users\\ANIL PARVATHANENI\\Desktop\\Others\\car-consume") #Setting path
df = read.csv(file = "cars consume.csv")  #Reading the file
#DATA:
set.seed(2)  
rand = sample(1:nrow(df),340)
train = df[rand, ]
test = df[-rand, ]
train_error <- c()
test_error <- c()
n <- c(30, 50, 70, 100, 200, 250, 300)  # Sample sizes
for(i in n){                    # Sample sizes for order 7
  rand1 = sample(1:nrow(train), i)
  samp1 = train[rand1, ]
  m30 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) + I(consume^5)+
              I(consume^6) + I(consume^7), samp1)   #Order 7
  m30
  #TRAIN AND TEST ACCURACY
  samp1train_error = sqrt(sum(m30$residuals^2))    #RMSE
  pred = predict(m30, newdata=test)
  samp1test_error = sqrt(sum((pred-test$distance)^2))
  train_error <- c(train_error, samp1train_error)
  test_error <- c(test_error, samp1test_error)
}
setwd("C:\\Users\\ANIL PARVATHANENI\\Desktop\\ML\\Graphs")  # Path for saving graphs
jpeg('Graph for order 7.jpeg')             # Saving the graph
plot(n, test_error, xlab="Sample Size", ylab="Test Error", main="Varying Sample size for order 7", pch=19, cex=0.5)
lines(n, test_error, col='black', type='l', pch=40)
dev.off()

t10 <- c()
te10 <- c()
for(k in n){                        # Different sample sizes for order 10
  rand1 = sample(1:nrow(train), i)
  samp1 = train[rand1, ]
  m30 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) + I(consume^5)+
              I(consume^6) + I(consume^7) + I(consume^8) + I(consume^9) + I(consume^10), samp1)#Order 10
  m30
  #TRAIN AND TEST ACCURACY
  samp1train_error = sqrt(sum(m30$residuals^2))   # RMSE
  pred = predict(m30, newdata=test)
  samp1test_error = sqrt(sum((pred-test$distance)^2))
  t10 <- c(t10, samp1train_error)
  te10 <- c(te10, samp1test_error)
}
setwd("C:\\Users\\ANIL PARVATHANENI\\Desktop\\ML\\Graphs")
jpeg('Graph for order 10.jpeg')
plot(n, te10, xlab="Sample Size", ylab="Test Error", main="Varying Sample size for order 10", pch=19, cex=0.5)
lines(n, te10, col='blue', type='l', pch=40)
dev.off()

library(ggplot2)   # Loading the package 
# SAMPLE OF 20     
set.seed(1)
rand = sample(1:nrow(df),340)
train = df[rand, ]
test = df[-rand, ]
ran1 = sample(1:nrow(train), 20)    # Sampling without replacement
tr1 = train[ran1, ]
ran2 = sample(1:nrow(train), 20)
tr2 = train[ran2, ]
ran3 = sample(1:nrow(train), 20)
tr3 = train[ran3, ]
ran4 = sample(1:nrow(train), 20)
tr4 = train[ran4, ]

#SAMPLE 1
tr1_error <- c()
ts1_error <- c()
order_no <- c(1, 2, 7, 8, 9, 10)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
m1 <- lm(distance ~ consume, tr1)
m1
jpeg('Sample of 20 size and order 1.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 20 size and order 1", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m1)[order(tr1$consume)], col='red', type='l')
dev.off()
#TRAIN AND TEST ACCURACY
tr_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
ts_error1 = sum((pred-test$distance)^2)

tr1_error <- c(tr1_error, tr_error1)
ts1_error <- c(ts1_error, ts_error1)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
m2 <- lm(distance ~ consume + I(consume^2), tr1)
m2
jpeg('Sample of 20 size and order 2.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 20 size and order 2", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m2)[order(tr1$consume)], col='brown2', type='l') 
dev.off()
tr_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
ts_error2 = sum((pred-test$distance)^2)

tr1_error <- c(tr1_error, tr_error2)
ts1_error <- c(ts1_error, ts_error2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
m7 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7), tr1)
m7
jpeg('Sample of 20 size and order 7.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 20 size and order 7", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m7)[order(tr1$consume)], col='darkblue', type='l')
dev.off()
#TRAIN AND TEST ACCURACY
tr_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
ts_error7 = sum((pred-test$distance)^2)

tr1_error <- c(tr1_error, tr_error7)
ts1_error <- c(ts1_error, ts_error7)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
m8 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8), tr1)
m8
jpeg('Sample of 20 size and order 8.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 20 size and order 8", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m8)[order(tr1$consume)], col='darkgoldenrod', type='l') 
dev.off()
#TRAIN AND TEST ACCURACY
tr_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
ts_error8 = sum((pred-test$distance)^2)

tr1_error <- c(tr1_error, tr_error8)
ts1_error <- c(ts1_error, ts_error8)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
m9 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^9) + I(consume^9), tr1)
m9
jpeg('Sample of 20 size and order 9.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 20 size and order 9", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m9)[order(tr1$consume)], col='darkslategray', type='l') 
dev.off()
#TRAIN AND TEST ACCURACY
tr_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
ts_error9 = sum((pred-test$distance)^2)

tr1_error <- c(tr1_error, tr_error9)
ts1_error <- c(ts1_error, ts_error9)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
m10 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
            I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8) + I(consume^9)
          + I(consume^10), tr1)
m10
jpeg('Sample of 20 size and order 10.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 20 size and order 10", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m10)[order(tr1$consume)], col='deeppink4', type='l')
dev.off()
#TRAIN AND TEST ACCURACY
tr_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
ts_error10 = sum((pred-test$distance)^2)

tr1_error <- c(tr1_error, tr_error10)
tr1_error
ts1_error <- c(ts1_error, ts_error10)
ts1_error

# All graphs in same plot
jpeg('Sample of 20 vs different order.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 20 size", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m1)[order(tr1$consume)], col='red', type='l')
lines(sort(tr1$consume), fitted(m2)[order(tr1$consume)], col='brown2', type='l')
lines(sort(tr1$consume), fitted(m7)[order(tr1$consume)], col='darkblue', type='l')
lines(sort(tr1$consume), fitted(m8)[order(tr1$consume)], col='darkgoldenrod', type='l')
lines(sort(tr1$consume), fitted(m9)[order(tr1$consume)], col='darkslategray', type='l')
lines(sort(tr1$consume), fitted(m10)[order(tr1$consume)], col='deeppink4', type='l')
legend("topright", legend = c("order 1", "order 2","order 7","order 8","order 9","order 10" ), 
       col = c("red","brown2","darkblue","darkgoldenrod","darkslategray","deeppink4"),lty=1:2,cex = 0.8)
dev.off()

#Test error Train error and model complexity
jpeg('Sample 20 model complexity vs errors.jpeg')
df1 <- data.frame(order_no, tr1_error, ts1_error)
g <- ggplot(df1, aes(x = order_no, y = ts1_error, col="Test Error"))
g <- g + geom_line(size  = 1.2)
g <- g + geom_line(aes(y = tr1_error, col="Train Error"),size = 1.2)
g <- g + xlab("Order Number") + ylab("Errors") + 
  ggtitle("Model Complexity vs Train and Test Errors for Sample Size 20") + 
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15))
g
dev.off()

jpeg('Sample20 first set.jpeg')  
plot(order_no, ts1_error, xlab="Order Number", ylab="Test Error", main="Sample 1 of 20 size", pch=19, cex=0.5)
lines(order_no, ts1_error, col='brown1', type='l')
dev.off()

# Sample 2
tr2_error <- c()
ts2_error <- c()
order_no <- c(1, 2, 7, 8, 9, 10)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
m1 <- lm(distance ~ consume, tr2)
m1
#TRAIN AND TEST ACCURACY
tr_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
ts_error1 = sum((pred-test$distance)^2)

tr2_error <- c(tr2_error, tr_error1)
ts2_error <- c(ts2_error, ts_error1)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
m2 <- lm(distance ~ consume + I(consume^2), tr2)
m2
#TRAIN AND TEST ACCURACY
tr_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
ts_error2 = sum((pred-test$distance)^2)

tr2_error <- c(tr2_error, tr_error2)
ts2_error <- c(ts2_error, ts_error2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
m7 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7), tr2)
m7
#TRAIN AND TEST ACCURACY
tr_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
ts_error7 = sum((pred-test$distance)^2)

tr2_error <- c(tr2_error, tr_error7)
ts2_error <- c(ts2_error, ts_error7)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
m8 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8), tr2)
m8
#TRAIN AND TEST ACCURACY
tr_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
ts_error8 = sum((pred-test$distance)^2)

tr2_error <- c(tr2_error, tr_error8)
ts2_error <- c(ts2_error, ts_error8)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
m9 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^9) + I(consume^9), tr2)
m9
#TRAIN AND TEST ACCURACY
tr_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
ts_error9 = sum((pred-test$distance)^2)

tr2_error <- c(tr2_error, tr_error9)
ts2_error <- c(ts2_error, ts_error9)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
m10 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
            I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8) + I(consume^9)
          + I(consume^10), tr2)
m10
#TRAIN AND TEST ACCURACY
tr_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
ts_error10 = sum((pred-test$distance)^2)

tr2_error <- c(tr2_error, tr_error10)
tr2_error
ts2_error <- c(ts2_error, ts_error10)
ts2_error

jpeg('Sample20 second set.jpeg')
plot(order_no, ts2_error, xlab="Order Number", ylab="Test Error", main="Sample 2 of 20 size", pch=19, cex=0.5)
lines(order_no, ts2_error, col='dodgerblue3', type='l')
dev.off()

# Sample 3
tr3_error <- c()
ts3_error <- c()
order_no <- c(1, 2, 7, 8, 9, 10)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
m1 <- lm(distance ~ consume, tr3)
m1
#TRAIN AND TEST ACCURACY
tr_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
ts_error1 = sum((pred-test$distance)^2)

tr3_error <- c(tr3_error, tr_error1)
ts3_error <- c(ts3_error, ts_error1)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
m2 <- lm(distance ~ consume + I(consume^2), tr3)
m2
#TRAIN AND TEST ACCURACY
tr_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
ts_error2 = sum((pred-test$distance)^2)

tr3_error <- c(tr3_error, tr_error2)
ts3_error <- c(ts3_error, ts_error2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
m7 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7), tr3)
m7
#TRAIN AND TEST ACCURACY
tr_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
ts_error7 = sum((pred-test$distance)^2)

tr3_error <- c(tr3_error, tr_error7)
ts3_error <- c(ts3_error, ts_error7)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
m8 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8), tr3)
m8
#TRAIN AND TEST ACCURACY
tr_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
ts_error8 = sum((pred-test$distance)^2)

tr3_error <- c(tr3_error, tr_error8)
ts3_error <- c(ts3_error, ts_error8)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
m9 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^9) + I(consume^9), tr3)
m9
#TRAIN AND TEST ACCURACY
tr_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
ts_error9 = sum((pred-test$distance)^2)

tr3_error <- c(tr3_error, tr_error9)
ts3_error <- c(ts3_error, ts_error9)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
m10 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
            I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8) + I(consume^9)
          + I(consume^10), tr3)
m10
#TRAIN AND TEST ACCURACY
tr_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
ts_error10 = sum((pred-test$distance)^2)

tr3_error <- c(tr3_error, tr_error10)
tr3_error
ts3_error <- c(ts3_error, ts_error10)
ts3_error

jpeg('Sample20 third set.jpeg')
plot(order_no, ts3_error, xlab="Order Number", ylab="Test Error", main="Sample 3 of 20 size", pch=19, cex=0.5)
lines(order_no, ts3_error, col='darkviolet', type='l')
dev.off()

# SAMPLE 4
tr4_error <- c()
ts4_error <- c()
order_no <- c(1, 2, 7, 8, 9, 10)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
m1 <- lm(distance ~ consume, tr4)
m1
#TRAIN AND TEST ACCURACY
tr_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
ts_error1 = sum((pred-test$distance)^2)

tr4_error <- c(tr4_error, tr_error1)
ts4_error <- c(ts4_error, ts_error1)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
m2 <- lm(distance ~ consume + I(consume^2), tr4)
m2
#TRAIN AND TEST ACCURACY
tr_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
ts_error2 = sum((pred-test$distance)^2)

tr4_error <- c(tr4_error, tr_error2)
ts4_error <- c(ts4_error, ts_error2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
m7 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7), tr4)
m7
#TRAIN AND TEST ACCURACY
tr_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
ts_error7 = sum((pred-test$distance)^2)

tr4_error <- c(tr4_error, tr_error7)
ts4_error <- c(ts4_error, ts_error7)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
m8 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8), tr4)
m8
#TRAIN AND TEST ACCURACY
tr_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
ts_error8 = sum((pred-test$distance)^2)

tr4_error <- c(tr4_error, tr_error8)
ts4_error <- c(ts4_error, ts_error8)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
m9 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^9) + I(consume^9), tr4)
m9
#TRAIN AND TEST ACCURACY
tr_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
ts_error9 = sum((pred-test$distance)^2)

tr4_error <- c(tr4_error, tr_error9)
ts4_error <- c(ts4_error, ts_error9)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
m10 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
            I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8) + I(consume^9)
          + I(consume^10), tr4)
m10
#TRAIN AND TEST ACCURACY
tr_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
ts_error10 = sum((pred-test$distance)^2)

tr4_error <- c(tr4_error, tr_error10)
tr4_error
ts4_error <- c(ts4_error, ts_error10)
ts4_error

jpeg('Sample20 fourth set.jpeg')
plot(order_no, ts4_error, xlab="Order Number", ylab="Test Error", main="Sample 4 of 20 size", pch=19, cex=0.5)
lines(order_no, ts4_error, col='gray7', type='l')
dev.off()

# SAMPLE OF 100
set.seed(1)
rand = sample(1:nrow(df),340)
train = df[rand, ]
test = df[-rand, ]
ran1 = sample(1:nrow(train), 100)  # Sampling without replacement
tr1 = train[ran1, ]
ran2 = sample(1:nrow(train), 100)
tr2 = train[ran2, ]
ran3 = sample(1:nrow(train), 100)
tr3 = train[ran3, ]
ran4 = sample(1:nrow(train), 100)
tr4 = train[ran4, ]

#SAMPLE 1
tr1_error <- c()
ts1_error <- c()
order_no <- c(1, 2, 7, 8, 9, 10)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
m1 <- lm(distance ~ consume, tr1)
m1
jpeg('Sample of 100 size and order 1.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 100 size and order 1", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m1)[order(tr1$consume)], col='red', type='l') 
dev.off()
#TRAIN AND TEST ACCURACY
tr_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
ts_error1 = sum((pred-test$distance)^2)

tr1_error <- c(tr1_error, tr_error1)
ts1_error <- c(ts1_error, ts_error1)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
m2 <- lm(distance ~ consume + I(consume^2), tr1)
m2
jpeg('Sample of 100 size and order 2.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 100 size and order 2", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m2)[order(tr1$consume)], col='green', type='l') 
dev.off()
tr_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
ts_error2 = sum((pred-test$distance)^2)

tr1_error <- c(tr1_error, tr_error2)
ts1_error <- c(ts1_error, ts_error2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
m7 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7), tr1)
m7
jpeg('Sample of 100 size and order 7.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 100 size and order 7", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m7)[order(tr1$consume)], col='indianred3', type='l') 
dev.off()
#TRAIN AND TEST ACCURACY
tr_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
ts_error7 = sum((pred-test$distance)^2)

tr1_error <- c(tr1_error, tr_error7)
ts1_error <- c(ts1_error, ts_error7)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
m8 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8), tr1)
m8
jpeg('Sample of 100 size and order 8.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 100 size and order 8", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m8)[order(tr1$consume)], col='mediumorchid4', type='l') 
dev.off()
#TRAIN AND TEST ACCURACY
tr_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
ts_error8 = sum((pred-test$distance)^2)

tr1_error <- c(tr1_error, tr_error8)
ts1_error <- c(ts1_error, ts_error8)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
m9 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^9) + I(consume^9), tr1)
m9
jpeg('Sample of 100 size and order 9.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 100 size and order 9", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m9)[order(tr1$consume)], col='magenta', type='l')
dev.off()
#TRAIN AND TEST ACCURACY
tr_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
ts_error9 = sum((pred-test$distance)^2)

tr1_error <- c(tr1_error, tr_error9)
ts1_error <- c(ts1_error, ts_error9)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
m10 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
            I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8) + I(consume^9)
          + I(consume^10), tr1)
m10
jpeg('Sample of 100 size and order 10.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 100 size and order 10", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m10)[order(tr1$consume)], col='mediumblue', type='l')
dev.off()
#TRAIN AND TEST ACCURACY
tr_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
ts_error10 = sum((pred-test$distance)^2)

tr1_error <- c(tr1_error, tr_error10)
tr1_error
ts1_error <- c(ts1_error, ts_error10)
ts1_error

# All graphs in same plot
jpeg('Sample100 vs all orders.jpeg')
plot(tr1$consume,tr1$distance, xlab="Consume", ylab="Distance", main="Sample of 100 size", pch=19, cex=0.5)
lines(sort(tr1$consume), fitted(m1)[order(tr1$consume)], col='red', type='l')
lines(sort(tr1$consume), fitted(m2)[order(tr1$consume)], col='green', type='l')
lines(sort(tr1$consume), fitted(m7)[order(tr1$consume)], col='indianred3', type='l')
lines(sort(tr1$consume), fitted(m8)[order(tr1$consume)], col='mediumorchid4', type='l')
lines(sort(tr1$consume), fitted(m9)[order(tr1$consume)], col='magenta', type='l')
lines(sort(tr1$consume), fitted(m10)[order(tr1$consume)], col='mediumblue', type='l')
legend("topright", legend = c("order 1", "order 2","order 7","order 8","order 9","order 10" ), 
       col = c("red","green","indianred3","mediumorchid4","magenta","mediumblue"),lty=1:2,cex = 0.8)
dev.off()

#Test error Train error and model complexity
jpeg('Sample 100 model complexity vs errors.jpeg')
df1 <- data.frame(order_no, tr1_error, ts1_error)
g <- ggplot(df1, aes(x = order_no, y = ts1_error, col="Test Error"))
g <- g + geom_line(size  = 1.2)
g <- g + geom_line(aes(y = tr1_error, col="Train Error"),size = 1.2)
g <- g + xlab("Order Number") + ylab("Errors") + 
  ggtitle("Model Complexity vs Train and Test Errors for Sample Size 100") + 
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15))
g
dev.off()

jpeg('Sample100 first set.jpeg')
plot(order_no, ts1_error, xlab="Order Number", ylab="Test Error", main="Sample 1 of 100 size")
lines(order_no, ts1_error, col='gray7', type='l')
dev.off()

# Sample 2
tr2_error <- c()
ts2_error <- c()
order_no <- c(1, 2, 7, 8, 9, 10)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
m1 <- lm(distance ~ consume, tr2)
m1
#TRAIN AND TEST ACCURACY
tr_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
ts_error1 = sum((pred-test$distance)^2)

tr2_error <- c(tr2_error, tr_error1)
ts2_error <- c(ts2_error, ts_error1)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
m2 <- lm(distance ~ consume + I(consume^2), tr2)
m2
#TRAIN AND TEST ACCURACY
tr_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
ts_error2 = sum((pred-test$distance)^2)

tr2_error <- c(tr2_error, tr_error2)
ts2_error <- c(ts2_error, ts_error2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
m7 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7), tr2)
m7
#TRAIN AND TEST ACCURACY
tr_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
ts_error7 = sum((pred-test$distance)^2)

tr2_error <- c(tr2_error, tr_error7)
ts2_error <- c(ts2_error, ts_error7)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
m8 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8), tr2)
m8
#TRAIN AND TEST ACCURACY
tr_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
ts_error8 = sum((pred-test$distance)^2)

tr2_error <- c(tr2_error, tr_error8)
ts2_error <- c(ts2_error, ts_error8)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
m9 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^9) + I(consume^9), tr2)
m9
#TRAIN AND TEST ACCURACY
tr_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
ts_error9 = sum((pred-test$distance)^2)

tr2_error <- c(tr2_error, tr_error9)
ts2_error <- c(ts2_error, ts_error9)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
m10 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
            I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8) + I(consume^9)
          + I(consume^10), tr2)
m10
#TRAIN AND TEST ACCURACY
tr_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
ts_error10 = sum((pred-test$distance)^2)

tr2_error <- c(tr2_error, tr_error10)
tr2_error
ts2_error <- c(ts2_error, ts_error10)
ts2_error

jpeg('Sample100 second set.jpeg')
plot(order_no, ts2_error, xlab="Order Number", ylab="Test Error", main="Sample 2 of 100 size")
lines(order_no, ts2_error, col='orange4', type='l')
dev.off()

# Sample 3
tr3_error <- c()
ts3_error <- c()
order_no <- c(1, 2, 7, 8, 9, 10)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
m1 <- lm(distance ~ consume, tr3)
m1
#TRAIN AND TEST ACCURACY
tr_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
ts_error1 = sum((pred-test$distance)^2)

tr3_error <- c(tr3_error, tr_error1)
ts3_error <- c(ts3_error, ts_error1)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
m2 <- lm(distance ~ consume + I(consume^2), tr3)
m2
#TRAIN AND TEST ACCURACY
tr_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
ts_error2 = sum((pred-test$distance)^2)

tr3_error <- c(tr3_error, tr_error2)
ts3_error <- c(ts3_error, ts_error2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
m7 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7), tr3)
m7
#TRAIN AND TEST ACCURACY
tr_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
ts_error7 = sum((pred-test$distance)^2)

tr3_error <- c(tr3_error, tr_error7)
ts3_error <- c(ts3_error, ts_error7)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
m8 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8), tr3)
m8
#TRAIN AND TEST ACCURACY
tr_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
ts_error8 = sum((pred-test$distance)^2)

tr3_error <- c(tr3_error, tr_error8)
ts3_error <- c(ts3_error, ts_error8)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
m9 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^9) + I(consume^9), tr3)
m9
#TRAIN AND TEST ACCURACY
tr_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
ts_error9 = sum((pred-test$distance)^2)

tr3_error <- c(tr3_error, tr_error9)
ts3_error <- c(ts3_error, ts_error9)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
m10 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
            I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8) + I(consume^9)
          + I(consume^10), tr3)
m10
#TRAIN AND TEST ACCURACY
tr_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
ts_error10 = sum((pred-test$distance)^2)

tr3_error <- c(tr3_error, tr_error10)
tr3_error
ts3_error <- c(ts3_error, ts_error10)
ts3_error

jpeg('Sample100 third set.jpeg')
plot(order_no, ts3_error, xlab="Order Number", ylab="Test Error", main="Sample 3 of 100 size")
lines(order_no, ts3_error, col='orangered2', type='l')
dev.off()

# SAMPLE 4
tr4_error <- c()
ts4_error <- c()
order_no <- c(1, 2, 7, 8, 9, 10)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
m1 <- lm(distance ~ consume, tr4)
m1
#TRAIN AND TEST ACCURACY
tr_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
ts_error1 = sum((pred-test$distance)^2)

tr4_error <- c(tr4_error, tr_error1)
ts4_error <- c(ts4_error, ts_error1)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
m2 <- lm(distance ~ consume + I(consume^2), tr4)
m2
#TRAIN AND TEST ACCURACY
tr_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
ts_error2 = sum((pred-test$distance)^2)

tr4_error <- c(tr4_error, tr_error2)
ts4_error <- c(ts4_error, ts_error2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
m7 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7), tr4)
m7
#TRAIN AND TEST ACCURACY
tr_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
ts_error7 = sum((pred-test$distance)^2)

tr4_error <- c(tr4_error, tr_error7)
ts4_error <- c(ts4_error, ts_error7)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
m8 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8), tr4)
m8
#TRAIN AND TEST ACCURACY
tr_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
ts_error8 = sum((pred-test$distance)^2)

tr4_error <- c(tr4_error, tr_error8)
ts4_error <- c(ts4_error, ts_error8)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
m9 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
           I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^9) + I(consume^9), tr4)
m9
#TRAIN AND TEST ACCURACY
tr_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
ts_error9 = sum((pred-test$distance)^2)

tr4_error <- c(tr4_error, tr_error9)
ts4_error <- c(ts4_error, ts_error9)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
m10 <- lm(distance ~ consume + I(consume^2) + I(consume^3) + I(consume^4) +
            I(consume^5)+ I(consume^5) + I(consume^6) + I(consume^7) + I(consume^8) + I(consume^9)
          + I(consume^10), tr4)
m10
#TRAIN AND TEST ACCURACY
tr_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
ts_error10 = sum((pred-test$distance)^2)

tr4_error <- c(tr4_error, tr_error10)
tr4_error
ts4_error <- c(ts4_error, ts_error10)
ts4_error

jpeg('Sample100 fourth set.jpeg')
plot(order_no, ts4_error, xlab="Order Number", ylab="Test Error", main="Sample 4 of 100 size")
lines(order_no, ts4_error, col='yellowgreen', type='l')
dev.off()
