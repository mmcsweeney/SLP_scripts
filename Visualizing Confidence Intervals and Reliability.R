# Visualizing the relationships between confidence intervals, standard error of measurement, and reliability in standardized tests 

# Written by Melissa McSweeney, Mackenzie Kephart, and Betsy Barrett for Speech@NYU - Clinical Practicum I, May 2023 

###########################

# There are three imaginary standardized tests which each have a mean score of 100 and a standard deviation of 15 according to their norms
# The same person takes all three tests and happens to obtain a score of 110 on each test.
# The three tests have different levels of reliability (internal consistency), each measured by Cronbach's alpha. Higher value = more reliable test

rm(list=ls())

library(scales)

tests<-as.data.frame(matrix(data=NA, nrow = 3, ncol=5))
colnames(tests)<-c('Test','Reliability','SEM','90% Conf. Int.','95% Conf. Int.')
tests$Test<-c('Test 1', 'Test 2', 'Test 3')
tests$Reliability<-c(0.95,0.8,0.65)


pt_score<-110 #patient's observed score on each test 
mu=100 #population mean of test
sigma=15 #population standard deviation of test

#Calculate the standard error of measurement (SEM) using the tests' standard deviation (15 for all of them) and their respective reliability
tests$SEM<-sigma*(sqrt(1-tests$Reliability))

#critical value for 90% CI = 1.645
#critical value for 95% CI = 1.96

#calculate 90% and 95% confidence intervals for each test

tests$CI_90_low<-round((pt_score-(1.645*tests$SEM)),2)
tests$CI_90_high<-round((pt_score+(1.645*tests$SEM)),2)
tests$CI_95_low<-round((pt_score-(1.96*tests$SEM)),2)
tests$CI_95_high<-round((pt_score+(1.96*tests$SEM)),2)

tests$`90% Conf. Int.`<-paste('[',tests$CI_90_low,', ',tests$CI_90_high,']',sep = '')
tests$`95% Conf. Int.`<-paste('[',tests$CI_95_low,', ',tests$CI_95_high,']',sep = '')

dev.off()

#allow plots to be visualized in a 3x1 array 
par(mfrow=c(3,1))

reli<-c('High Reliability',"Medium Reliability",'Lower Reliability') #note, these are not official classifications of Cronbach's alpha magnitude, but comparative placeholders for simplicity

#initialize normal distribution for plotting 
x <- seq(-4, 4, length = 1000) * sigma + mu
y<-dnorm(x,mean=mu,sd=sigma)

#iteratively plot normal distribution with confidence intervals for each of the tests 
for (i in 1:3){
  #plot normal distribution and label numbers on x-axis 
  plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "Score", ylab = "",main=paste(tests[i,'Test'],' (',reli[i],')',sep=''))
  sd_axis_bounds = 5
  axis_bounds <- seq(-sd_axis_bounds * sigma + mu,
                     sd_axis_bounds * sigma + mu,
                     by = sigma)
  axis(side = 1, at = axis_bounds, pos = 0)
  
  #plot 95% confidence interval
  lowlim=tests[i,'CI_95_low']
  uplim=tests[i,'CI_95_high']
  
  dx = seq(lowlim, uplim, 0.01)
  polygon(x = c(lowlim, dx, uplim), y = c(0, dnorm(dx, mean = mu, sd = sigma), 0), border = NA, col = alpha('coral1',1))
  
  #plot 90% confidence interval 
  lowlim=tests[i,'CI_90_low']
  uplim=tests[i,'CI_90_high']
  
  dx = seq(lowlim, uplim, 0.01)
  polygon(x = c(lowlim, dx, uplim), y = c(0, dnorm(dx, mean = mu, sd = sigma), 0), border = NA, col = alpha('cornflowerblue',1))
  
  #dotted line to visualize the patient's obtained score
  abline(v=pt_score,lty=2,lwd=2)
  
  #label and delineate the width of each confidence interval 
  segments(x0=tests[i,'CI_95_low'],y0=0.003,x1=tests[i,'CI_95_high'],y1=0.003,col='darkred',lwd=1.3)
  segments(x0=tests[i,'CI_90_low'],y0=0.006,x1=tests[i,'CI_90_high'],y1=0.006,col='darkblue',lwd=1.3)
  text(x=(tests[i,'CI_95_low'])-4.5,y=0.003,labels="95% CI",col='darkred',cex=1.2)
  text(x=(tests[i,'CI_90_low'])-4.5,y=0.006,labels="90% CI",col='darkblue',cex=1.2)
  
}


#the part of the code for plotting specific areas under a normal curve in R was adapted from https://chem.libretexts.org/Bookshelves/Analytical_Chemistry/Chemometrics_Using_R_(Harvey)/06%3A_Uncertainty_of_Data/6.03%3A_Using_R_to_Model_Properties_of_a_Normal_Distribution
