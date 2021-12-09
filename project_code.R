################ AVOCADO-REPORT CODE ##########
#### Coded By: Arnaud Laprais
#### Last Updated: 11-30-2021
###############################################
personal_path <- "C:\\Users\\Arnaud Laprais\\Desktop\\SJSU\\264 bayesian\\project"
setwd(personal_path)

library(LaplacesDemon) # HPDs
library(knitr) #tables
library(e1071) # skew and kurtosis

set.seed(1412)

############# IMPORT DATA ###############
data_total <- read.csv("data/avocado.csv") # total data

data <- data_total[c("Date","Total.Volume", "type","region")]

data$Total.Volume.scaled <- data$Total.Volume/1e5 # adjust units conventional - 100,000
data$Total.Volume.scaled.2 <- data$Total.Volume/1e3 # adjust units organic ,- 1,000

years <- as.numeric(format(as.Date(data$Date), "%Y"))
months <- as.numeric(format(as.Date(data$Date), "%m"))
data$year <- years
data$month <- months

# sf_data 
sf_data <- data[data$region == "SanFrancisco",]

# sf_data conventional and organic
sf_data_conv <- sf_data[sf_data$type == "conventional",]
sf_data_org <- sf_data[sf_data$type == "organic",]
row.names(sf_data_conv) <- NULL
row.names(sf_data_org) <- NULL

# sf_data off-season
sf_data_off <- sf_data[sf_data$month %in% c(9,10,11,12,1),]  # sept,oct,nov,dec,jan off-season in cali

sf_data_off_org <- sf_data_off[sf_data_off$type == "organic",]
sf_data_off_conv <- sf_data_off[sf_data_off$type == "conventional",]
row.names(sf_data_off_conv) <- NULL
row.names(sf_data_off_org) <- NULL

# sf_data in-season
sf_data_in <- sf_data[!(sf_data$month %in% c(9,10,11,12,1)),]  # NOT sept,oct,nov,dec,jan 

sf_data_in_org <- sf_data_in[sf_data_in$type == "organic",]
sf_data_in_conv <- sf_data_in[sf_data_in$type == "conventional",]
row.names(sf_data_in_conv) <- NULL
row.names(sf_data_in_org) <- NULL

# train & test sets for off-season paradigm
train_off_conv <- sf_data_off_conv[sf_data_off_conv$year != 2018, ]
test_off_conv <- sf_data_off_conv[sf_data_off_conv$year == 2018, ]
train_off_conv <- train_off_conv[order(train_off_conv$Date),]           # order by date
test_off_conv <- test_off_conv[order(test_off_conv$Date),]

train_off_org <- sf_data_off_org[sf_data_off_org$year != 2018, ]
test_off_org <- sf_data_off_org[sf_data_off_org$year == 2018, ]
train_off_org <- train_off_org[order(train_off_org$Date),]            # order by date
test_off_org <- test_off_org[order(test_off_org$Date),]

row.names(train_off_conv) <- NULL  # reset row indices
row.names(test_off_conv) <- NULL
row.names(train_off_org) <- NULL  
row.names(test_off_org) <- NULL

# train & test sets for full data paradigm
train_conv <- sf_data_conv[sf_data_conv$year != 2018, ]
test_conv <- sf_data_conv[sf_data_conv$year == 2018, ]
train_conv <- train_conv[order(train_conv$Date),]           # order by date
test_conv <- test_conv[order(test_conv$Date),]

train_org <- sf_data_org[sf_data_org$year != 2018, ]
test_org <- sf_data_org[sf_data_org$year == 2018, ]
train_org <- train_org[order(train_org$Date),]            # order by date
test_org <- test_org[order(test_org$Date),]


row.names(train_conv) <- NULL  # reset row indices
row.names(test_conv) <- NULL
row.names(train_org) <- NULL  
row.names(test_org) <- NULL
############# END IMPORT DATA ###############

############# EXPLORATORY DATA ANALYSIS ###############

# Conventional Plot over Time (2015-2018)
sf_data_conv$Date <- as.Date(sf_data_conv$Date, "%Y-%m-%d")
sf_data_conv <- sf_data_conv[order(sf_data_conv$Date),]
plot(sf_data_conv$Date, sf_data_conv$Total.Volume.scaled, 
     main = "SF Conventional Weekly Sales Volume", 
     col=ifelse(sf_data_conv$month %in% c(9,10,11,12,1), "red","black"), pch=20,
     xlab = "Date", ylab= "Units Sold (100k)")
lines(sf_data_conv$Date, sf_data_conv$Total.Volume.scaled)
legend("topleft",inset=c(0,0), legend=c("Off-Season", "In-Season"),
       col=c("red", "black"), pch = 20, cex=0.8)

# Organic Plot over Time (2015-2018)
sf_data_org$Date <- as.Date(sf_data_org$Date, "%Y-%m-%d")
sf_data_org <- sf_data_org[order(sf_data_org$Date),]
plot(sf_data_org$Date, sf_data_org$Total.Volume.scaled.2, 
     main = "SF Organic Weekly Sales Volume", 
     col=ifelse(sf_data_org$month %in% c(9,10,11,12,1), "red","black"), pch=20,
     xlab = "Date", ylab= "Units Sold (1k)" )
lines(sf_data_org$Date, sf_data_org$Total.Volume.scaled.2)
legend("topright",inset=c(0,0), legend=c("Off-Season", "In-Season"),
       col=c("red", "black"), pch = 20, cex=0.8)

# summary stats
mat <- matrix(data=NA,nrow=4, ncol=7)
summary_stats_table <- data.frame(mat,row.names=c("**In-Season Conventional (100k)**","**Off-Season Conventional (100k)**", "**In-Season Organic (1k)**", "**Off-Season Organic (1k)**"))
colnames(summary_stats_table) <- c("Mean","SD","Min","Q1","Q2","Q3","Max")
# quantiles
summary_stats_table[1,3:7] <- round(quantile(sf_data_in_conv$Total.Volume.scaled),2)
summary_stats_table[2,3:7] <- round(quantile(sf_data_off_conv$Total.Volume.scaled),2)
summary_stats_table[3,3:7] <- round(quantile(sf_data_in_org$Total.Volume.scaled.2),2)
summary_stats_table[4,3:7] <- round(quantile(sf_data_off_org$Total.Volume.scaled.2),2)
# mean
summary_stats_table[1,1] <- round(mean(sf_data_in_conv$Total.Volume.scaled),2)
summary_stats_table[2,1] <- round(mean(sf_data_off_conv$Total.Volume.scaled),2)
summary_stats_table[3,1] <- round(mean(sf_data_in_org$Total.Volume.scaled.2),2)
summary_stats_table[4,1] <- round(mean(sf_data_off_org$Total.Volume.scaled.2),2)
# sd
# mean
summary_stats_table[1,2] <- round(sd(sf_data_in_conv$Total.Volume.scaled),2)
summary_stats_table[2,2] <- round(sd(sf_data_off_conv$Total.Volume.scaled),2)
summary_stats_table[3,2] <- round(sd(sf_data_in_org$Total.Volume.scaled.2),2)
summary_stats_table[4,2] <- round(sd(sf_data_off_org$Total.Volume.scaled.2),2)

kable(summary_stats_table, caption = "Summary Stats")

# boxplots
par(mfrow=(c(1,2)))
boxplot(sf_data_off_conv$Total.Volume.scaled,sf_data_in_conv$Total.Volume.scaled, 
        main= "Conventional", horizontal= T, xlab = "Units Sold (100k)", 
        names = c("Off", "In"), col=c("red","grey"), las = 1)
boxplot(sf_data_off_org$Total.Volume.scaled.2,sf_data_in_org$Total.Volume.scaled.2, 
        main= "Organic", horizontal= T, xlab = "Units Sold (1k)", 
        names = c("Off", "In"), col=c("red","grey"), las = 1)

# sample sizes
mat <- matrix(data=NA, nrow = 3, ncol=2)
sample_sizes <- data.frame(mat,row.names=c("**Full-Season Organic**", "**Off-Season Organic**", "**Full-Season Conventional**"))
colnames(sample_sizes) <- c("Model Data", "Validation Data")
sample_sizes[1,1] <- dim(train_org)[1]
sample_sizes[1,2] <- dim(test_org)[1]
sample_sizes[2,1] <- dim(train_off_org)[1]
sample_sizes[2,2] <- dim(test_off_org)[1]
sample_sizes[3,1] <- dim(train_conv)[1]
sample_sizes[3,2] <- dim(test_conv)[1]
kable(sample_sizes, caption = "Sample Sizes")

############# END EXPLORATORY DATA ANALYSIS ###############

############# MODEL SELECTION AND INTERPRETATION ############
## posterior
posterior <- function(theta,y){
  n <- length(y)
  out <- dgamma(theta, n*mean(y)+1/2, n)
  return(out)
}

##### off-season paradigm
# conventional
n <- dim(train_off_conv)[1]
y <- train_off_conv$Total.Volume.scaled
curve(posterior(x,y), from = 5.5, to=14, xlab = expression(theta))

##### full season paradigm
# conventional
n <- dim(train_conv)[1]
y <- train_conv$Total.Volume.scaled
curve(posterior(x,y), from = 5.5, to=10, xlab = expression(theta)) 

#### BAYESIAN UPDATE
updated_posterior <- function(theta,y,z){
  n_y <- length(y)
  n_z <- length(z)
  out <- dgamma(theta, n_y*mean(y) + n_z*mean(z)+1/2, n_y+n_z)
  return(out)
}

##### Off - Season paradigm
#test_off_conv <- test_off_conv[order(test_off_conv$Date),]

n_y <- dim(train_off_conv)[1]
y <- train_off_conv$Total.Volume.scaled
z <- test_off_conv$Total.Volume.scaled[1] # update this number
n_z <- length(z)

curve(posterior(x,y), from = 5.5, to=10, col ='red')
curve(updated_posterior(x,y,z), from = 5.5, to=10, col ='purple', add = TRUE)

##### Full-Season paradigm
y <- train_conv$Total.Volume.scaled
z <- test_conv$Total.Volume.scaled[1] # update this number


curve(posterior(x,y), from = 5.5, to=10, col ='red')
curve(updated_posterior(x,y,z), from = 5.5, to=10, col ='purple', add = TRUE)

################  POSTERIOR PREDICTIVE
post_predict_vec <- function(y_tilde,y){ # vector capable
  n_y <- length(y)
  n_y_tilde <- length(y_tilde)
  out <- (n_y^(1/2+sum(y)) * gamma(1/2+sum(y_tilde)+sum(y))) / 
    ( prod(factorial(y_tilde))*gamma(1/2+sum(y))*(n_y_tilde + n_y)^(1/2+sum(y_tilde)+sum(y)) )
  return(out)
}

post_predict <- function(y_tilde,y){
  n_y <- length(y)
  out <- exp((1/2+sum(y))*log(n_y) + lgamma(1/2+y_tilde+sum(y)) - log(factorial(y_tilde)) - lgamma(1/2+sum(y)) -(1/2+y_tilde+sum(y))*log(1+n_y))
  return(out)
}

#### means and vars

# off-season organic
mean(train_off_org$Total.Volume.scaled.2)
var(train_off_org$Total.Volume.scaled.2)

# full-season organic
mean(train_org$Total.Volume.scaled.2)
var(train_org$Total.Volume.scaled.2)

# full-season conventional
mean(train_conv$Total.Volume.scaled)
var(train_conv$Total.Volume.scaled)

mat <- matrix(data=NA, nrow=3, ncol=3)
mean_var_table <- data.frame(mat, row.names = c("**Off-Season Organic**","**Full-Season Organic**", "**Full-Season Conventional**"))

colnames(mean_var_table) <- c("Sample Mean", "Sample Variance", "Sample Size")

mean_var_table[1,] <- c( mean(train_off_org$Total.Volume.scaled.2), 
                         var(train_off_org$Total.Volume.scaled.2),
                         dim(train_off_org)[1] )
mean_var_table[2,] <- c( mean(train_org$Total.Volume.scaled.2), 
                         var(train_org$Total.Volume.scaled.2),
                         dim(train_org)[1] )
mean_var_table[3,] <- c( mean(train_conv$Total.Volume.scaled), 
                         var(train_conv$Total.Volume.scaled),
                         dim(train_conv)[1] )
kable(mean_var_table, caption = "Summary of Model Data")



# disc measures
par(mfrow=c(1,3))

# off-season organic
hist(train_off_org$Total.Volume.scaled.2, main = "Off-Season Organic", xlab= "Weekly Sales (1k)", breaks = seq(5,45,by=3))

#skewness(train_off_org$Total.Volume.scaled.2) # skew, Normal has 0
#kurtosis(train_off_org$Total.Volume.scaled.2)

# full-season organic
hist(train_org$Total.Volume.scaled.2, main = "Full-Season Organic", xlab= "Weekly Sales (1k)")

#skewness(train_org$Total.Volume.scaled.2) # skew, Normal has 0
#kurtosis(train_org$Total.Volume.scaled.2)

# full-season conv
hist(train_conv$Total.Volume.scaled, main = "Full-Season Conventional", xlab= "Weekly Sales (100k)", breaks = c(4:18))

#skewness(train_conv$Total.Volume.scaled) # skew, Normal has 0
#kurtosis(train_conv$Total.Volume.scaled)

########## Generating y reps for discrepancy measures
n_sims <- 5000

# off-season organic y^{rep}
n_reps <- dim(train_off_org)[1]
off_season_org_reps <- matrix(data=NA, nrow=n_sims, ncol = n_reps)
y <- train_off_org$Total.Volume.scaled.2
for (k in 1:n_sims){
  sample <- c()
  i <- 1
  while (length(sample) < n_reps){
    x_star <- 1.8*rgamma(1,8,0.4)
    u <- runif(1)
    if (u <= post_predict(x_star,y)/(1.8*dgamma(x_star,8,0.4))){
      sample[i] <- x_star
      i <- i + 1
    }
  }
  off_season_org_reps[k,] <- sample
}

# full-season organic y^{rep}
n_reps <- dim(train_org)[1]
full_season_org_reps <- matrix(data=NA, nrow=n_sims, ncol = n_reps)
y <- train_org$Total.Volume.scaled.2
for (k in 1:n_sims){
  sample <- c()
  i <- 1
  while (length(sample) < n_reps){
    x_star <- 1.5*rgamma(1,12,0.5)
    u <- runif(1)
    if (u <= post_predict(x_star,y)/(1.5*dgamma(x_star,12,0.5))){
      sample[i] <- x_star
      i <- i + 1
    }
  }
  full_season_org_reps[k,] <- sample
}


# full-season conv y^{rep}
n_reps <- dim(train_conv)[1]
full_season_conv_reps <- matrix(data=NA, nrow=n_sims, ncol = n_reps)
y <- train_conv$Total.Volume.scaled
for (k in 1:n_sims){
  sample <- c()
  i <- 1
  while (length(sample) < n_reps){
    x_star <- 1.5*rgamma(1,4,0.5)
    u <- runif(1)
    if (u <= post_predict(x_star,y)/(1.5*dgamma(x_star,4,0.5))){
      sample[i] <- x_star
      i <- i + 1
    }
  }
  full_season_conv_reps[k,] <- sample
}


########### Discrepancy Measure (Outliers %)

##### Realized value
# off-season organic
Q3 <- quantile(train_off_org$Total.Volume.scaled.2)[[4]]
Q1 <- quantile(train_off_org$Total.Volume.scaled.2)[[2]]
realized_off_season_org <- mean(train_off_org$Total.Volume.scaled.2 > 
                                  (Q3 + 1.5*(Q3-Q1))) * 100 # percent outliers
# full-season organic
Q3 <- quantile(train_org$Total.Volume.scaled.2)[[4]]
Q1 <- quantile(train_org$Total.Volume.scaled.2)[[2]]
realized_full_season_org <- mean(train_org$Total.Volume.scaled.2 > 
                                   (Q3 + 1.5*(Q3-Q1))) * 100 # percent outliers
# full-season conventional
Q3 <- quantile(train_conv$Total.Volume.scaled)[[4]]
Q1 <- quantile(train_conv$Total.Volume.scaled)[[2]]
realized_full_season_conv <- mean(train_conv$Total.Volume.scaled > 
                                    (Q3 + 1.5*(Q3-Q1))) * 100 # percent outliers

###### simmed 
# off-season organic
Q3 <- apply(off_season_org_reps, 1, function(x) quantile(x, probs=0.75)[[1]])
Q1 <- apply(off_season_org_reps, 1, function(x) quantile(x, probs=0.25)[[1]])
cutoffs <- Q3 + 1.5*(Q3-Q1) 

t_outlier_off_org <- c()
for (i in 1:length(cutoffs)){
  t_outlier_off_org[i] <- mean(off_season_org_reps[i,] > cutoffs[i])*100
}

# full-season organic
Q3 <- apply(full_season_org_reps, 1, function(x) quantile(x, probs=0.75)[[1]])
Q1 <- apply(full_season_org_reps, 1, function(x) quantile(x, probs=0.25)[[1]])
cutoffs <- Q3 + 1.5*(Q3-Q1) 

t_outlier_org <- c()
for (i in 1:length(cutoffs)){
  t_outlier_org[i] <- mean(full_season_org_reps[i,] > cutoffs[i])*100
}

# full-season conventional
Q3 <- apply(full_season_conv_reps, 1, function(x) quantile(x, probs=0.75)[[1]])
Q1 <- apply(full_season_conv_reps, 1, function(x) quantile(x, probs=0.25)[[1]])
cutoffs <- Q3 + 1.5*(Q3-Q1) 

t_outlier_conv <- c()
for (i in 1:length(cutoffs)){
  t_outlier_conv[i] <- mean(full_season_conv_reps[i,] > cutoffs[i])*100
}

# plots
par(mfrow = c(1,3))
hist(t_outlier_off_org, main = "Off-Season Organic Model", xlab = "% Large Outliers")
abline(v=realized_off_season_org, col='red')
hist(t_outlier_org, main = "Full-Season Organic Model", xlab = "% Large Outliers" )
abline(v=realized_full_season_org, col='red')
hist(t_outlier_conv, main = "Full-Season Conventional Model", xlab = "% Large Outliers")
abline(v=realized_full_season_conv, col='red')

####### Discrepancy Measure (Skew)

#### realized
# off-season org
realized_off_season_org <- skewness(train_off_org$Total.Volume.scaled.2)

# full-season org
realized_full_season_org <- skewness(train_org$Total.Volume.scaled.2)

# full-season conv
realized_full_season_conv <- skewness(train_conv$Total.Volume.scaled)

#### simmed
# off-season organic
t_skew_off_org <- apply(off_season_org_reps,1, function(x) skewness(x))
# full-season organic
t_skew_org <- apply(full_season_org_reps,1, function(x) skewness(x))
# full-season conventional
t_skew_conv <- apply(full_season_conv_reps,1, function(x) skewness(x))

# plots
par(mfrow= c(1,3))
# off-season org
hist(t_skew_off_org, main = "Off-Season Organic", xlab = "Skew")
abline(v = realized_off_season_org, col= 'red')
# full-season org
hist(t_skew_org, xlim = c(-.75,1.8),main = "Full-Season Organic", xlab = "Skew")
abline(v=realized_full_season_org, col='red')
# full-season conv
hist(t_skew_conv, xlim = c(-.5, 1.8))
abline(v=realized_full_season_conv, col = 'red', main = "Full-Season Conventional", xlab = "Skew")


########################### END MODEL SELECTION/INTERPRETATION #########################

############################## PREDICTION ###########################################
############# HPDS
samp_off_org <- as.vector(unlist(off_season_org_reps))
samp_org <- as.vector(unlist(full_season_org_reps))
samp_conv <- as.vector(unlist(full_season_conv_reps))

hpd_off_org <- p.interval(samp_off_org, HPD=T, MM=F, prob=.9)
hpd_org <- p.interval(samp_org, HPD=T, MM=F, prob=.9)
hpd_conv <- p.interval(samp_conv, HPD=T, MM=F, prob=.9)

mat <- matrix(data=NA, nrow=3,ncol=2)
hpd_table <- data.frame(mat,row.names = c("**Off-Season Organic (1k)**", "**Full-Season Organic (1k)**", "**Full-Season Conventional (100k)**"))

colnames(hpd_table) <- c("Lower Bound", "Upper Bound")
hpd_table[1,] <- round(as.vector(hpd_off_org),2)
hpd_table[2,] <- round(as.vector(hpd_org),2)
hpd_table[3,] <- round(as.vector(hpd_conv),2)

kable(hpd_table, caption = "90% HPD Credible Interval for Future Avocado Sales")

par(mfrow = c(1,3))

# off-season org
y <- train_off_org$Total.Volume.scaled.2
curve(post_predict(x,y), from = 0, to=60, main= "PPD of Off-Season Organic",
      xlab = "Weekly Units Sold (1k)") 
points(x=test_off_org$Total.Volume.scaled.2, y=rep(0, dim(test_off_org)[1]), col = "red")

# full-season org
y <- train_org$Total.Volume.scaled.2
curve(post_predict(x,y), from = 0, to=50, main= "PPD of Full-Season Organic",
      xlab = "Weekly Units Sold (1k)") 
points(x=test_org$Total.Volume.scaled.2, y=rep(0, dim(test_org)[1]), col = "red")

# full-season conv
y <- train_conv$Total.Volume.scaled
curve(post_predict(x,y), from = 0, to=25, main= "PPD of Full-Season Conventional",
      xlab = "Weekly Units Sold (100k)")
points(x=test_conv$Total.Volume.scaled, y=rep(0, dim(test_conv)[1]), col = "red")

################### Prediction Accuracy
####75th percentile prediction point
# off-season org
prediction_off_org <- quantile(as.vector(unlist(off_season_org_reps)), probs = 0.75)[[1]]
# full-season org
prediction_org <- quantile(as.vector(unlist(full_season_org_reps)), probs = 0.75)[[1]]
# full-seson conv
prediction_conv <- quantile(as.vector(unlist(full_season_conv_reps)), probs= 0.75)[[1]]

# mse off-season org
mse_off_org <- mean((test_off_org$Total.Volume.scaled.2 - prediction_off_org)^2)
mse_org_compare <- mean((test_off_org$Total.Volume.scaled.2 - prediction_org)^2)
mse_org <- mean((test_org$Total.Volume.scaled.2 - prediction_org)^2)
mse_conv <- mean((test_conv$Total.Volume.scaled - prediction_conv)^2)

# root mse (to compare to mean distance, rootmse punishes larger errors than meandist)
root_mse_off_org <- sqrt(mse_off_org)
root_mse_org_compare <- sqrt(mse_org_compare)
root_mse_org <- sqrt(mse_org)
root_mse_conv <- sqrt(mse_conv)

# mean distance (mean absolute error)
dist_off_org <- mean(abs(test_off_org$Total.Volume.scaled.2 - prediction_off_org))
dist_org_compare <- mean(abs(test_off_org$Total.Volume.scaled.2 - prediction_org))

dist_org <- mean(abs(test_org$Total.Volume.scaled.2 - prediction_org))
dist_conv <- mean(abs(test_conv$Total.Volume.scaled - prediction_conv))

# mean absolute percentage error (mean( abs(actual-predicted)/abs(actual) ))*100
mape_off_org <- mean(  abs(test_off_org$Total.Volume.scaled.2 - prediction_off_org) /
                         test_off_org$Total.Volume.scaled.2  ) * 100
mape_org_compare <- mean(  abs(test_off_org$Total.Volume.scaled.2 - prediction_org) /
                             test_off_org$Total.Volume.scaled.2  ) * 100

mape_org <- mean(  abs(test_org$Total.Volume.scaled.2 - prediction_org) /
                     test_org$Total.Volume.scaled.2  ) * 100

mape_conv <- mean(  abs(test_conv$Total.Volume.scaled - prediction_conv) /
                      test_conv$Total.Volume.scaled  ) * 100


# overestimate or underestimate?
diffs <- prediction_off_org - test_off_org$Total.Volume.scaled.2 
diffs <- diffs > 0 # 1 if overestimate, 0 if underestimate
percent_over_off_org <- mean(diffs)*100

diffs <- prediction_org - test_off_org$Total.Volume.scaled.2 
diffs <- diffs > 0 # 1 if overestimate, 0 if underestimate
percent_over_org_compare <- mean(diffs)*100

diffs <- prediction_org - test_org$Total.Volume.scaled.2
diffs <- diffs > 0 # 1 if overestimate, 0 if underestimate
percent_over_org <- mean(diffs)*100

diffs <- prediction_conv - test_conv$Total.Volume.scaled
diffs <- diffs > 0 # 1 if overestimate, 0 if underestimate
percent_over_conv <- mean(diffs)*100

####### Prediction Tables
# full season
mat <- matrix(data=NA, nrow=2, ncol = 6)
table_full <- data.frame(mat,row.names = c("**Full-Season Organic (1k)**", "**Full-Season Conventional (100k)**"))
colnames(table_full) <- c("Predicted Val","MSE","root MSE", 
                          "MAE", "MAPE (%)", "Overestimate (%)")
table_full[1,] <- round(c(prediction_org, mse_org, root_mse_org, dist_org, 
                          mape_org, percent_over_org),2)
table_full[2,] <- round(c(prediction_conv, mse_conv, root_mse_conv, dist_conv, 
                          mape_conv, percent_over_conv),2)

# off-season
mat <- matrix(data=NA, nrow=2, ncol = 6)
table_off <- data.frame(mat,row.names = c("**Off-Season Organic (1k)**", "**Full-Season Organic (1k)**"))
colnames(table_off) <- c("Predicted Val","MSE","root MSE", 
                         "MAE", "MAPE (%)", "Overestimate (%)")
table_off[1,] <- round(c(prediction_off_org, mse_off_org, root_mse_off_org, 
                         dist_off_org, mape_off_org, percent_over_off_org),2)
table_off[2,] <- round(c(prediction_org, mse_org_compare, root_mse_org_compare, 
                         dist_org_compare, mape_org_compare, percent_over_org_compare),2)

kable(table_full, caption = "Full-Season Performance")

kable(table_off, caption = "Off-Season Performance")

###################################### END PREDICTION ###################################
