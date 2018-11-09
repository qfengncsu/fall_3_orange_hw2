######## ########## ############# ########
########### Simulation HW 1 ##############
######## ########## ############# ########

library(graphics)
library(ks)
library(readxl)
library(ggplot2)
library(triangle)
library(reshape2)


#####load the dataset####
path = '/Users/vea/Desktop/NCSU/Fall3/Simulation/'
file = 'Analysis_Data.xlsx'

price = read_excel(paste(path, file, sep = '') , sheet ='Price Projections', col_types = 'numeric',skip =2 )
drilling = read_excel(paste(path, file, sep = '') , sheet ='Drilling Cost', col_types = 'numeric', skip =2 )

########  EDA and subset dataset ########
#View(price)

#View(drilling)

drilling$Date =as.Date(drilling$Date - 25569, origin = "1970-01-01")
inital_oil = drilling[[47,2]]
inital_gas = drilling[[47,3]]
inital_dry = drilling[[47,4]]
inital_avg = (inital_dry + inital_gas + inital_oil)/3

drilling = drilling[32:47,c(1,5,6,7)]
change = c(drilling[[2]], drilling[[3]], drilling[[4]])

########## Distribution Selection - Kernel Estimation from 2006 to 2012 ########
hist(change)

density.change <- density(change, bw="SJ-ste")
density.change
#Bandwidth 'bw' = 0.07935
set.seed(12345)
Est.change <- rkde(fhat = kde(change, h = 0.07935), n = 10000)
hist(Est.change,  breaks=50)

qqnorm(change)
qqline(change)
#normal distribution

########## Simulation of 2019 drilling costs #############

########## 1. Use normal distribution ##########
mean = mean(change)
standard = sd(change)
a = rep(0, 100000)
b = rep(0, 100000)

for(i in 1:100000){
        #2006 - 2012
        p_nor = rnorm(6, mean, standard)
        p_nor = p_nor+1
        p1 = prod(p_nor)
        
        #2012 - 2015
        p_tri = rtriangle(3, -0.22, -0.07, -0.0917)  
        p_tri = p_tri + 1
        p2 = p1 * prod(p_tri)
        
        #2015 - 2019
        p_tri2 = rtriangle(4, 0.02, 0.06, 0.05)  
        p_tri2 = p_tri2 + 1
        p3 = p2 * prod(p_tri2)
        
        a[i] = p3

}
        
hist(a, breaks = 50)      

########## 2. Use Kernel Estimation ##########

for(i in 1:100000){
        #2006 - 2012
        p_nor = rkde(fhat = kde(change, h = 0.07935), n = 6)
        p_nor = p_nor+1
        p1 = prod(p_nor)
        
        #2012 - 2015
        p_tri = rtriangle(3, -0.22, -0.07, -0.0917)  
        p_tri = p_tri + 1
        p2 = p1 * prod(p_tri)
        
        #2015 - 2019
        p_tri2 = rtriangle(4, 0.02, 0.06, 0.05)  
        p_tri2 = p_tri2 + 1
        p3 = p2 * prod(p_tri2)
        
        b[i] = p3
        
}

hist(b, breaks = 50) 

############## Visualization #############

way1 = data.frame(a*inital_oil, a*inital_gas, a*inital_dry)
colnames(way1) = c('Oil Well', 'Gas Well', 'Dry Well')
way1$Avg = (way1[,1] + way1[,2] + way1[,3])/3

way2 = data.frame(b*inital_oil, b*inital_gas, b*inital_dry)
colnames(way1) = c('Oil Well', 'Gas Well', 'Dry Well')
way2$Avg = (way2[,1] + way2[,2] + way2[,3])/3

combine = data.frame(way1$Avg, way2$Avg)
colnames(combine) = c('Normality', 'Kernel Density')
combine2 = melt(combine)
colnames(combine2) = c('Method','Price')

plot1 = ggplot(combine2) + 
        geom_histogram(aes(x = Price, y = (..count..)/100000,
                           fill = Method), alpha=0.5, bins =50, position="identity") +
        ylab('\nRelative Frequency\n')  +
        xlab('\n2019 Drilling Costs') +
        ggtitle('2019 Drilling Costs by Two Simulation Methods') +
        geom_vline(aes(xintercept = inital_avg), colour="black", linetype="dashed") +
        annotate("text", x = 1500, y = 0.08, label = '2006 Costs', angle = 90)

plot1

plot2 = ggplot(way2) + 
        geom_histogram(aes(x = Avg, y = (..count..)/100000), alpha=0.7, fill = 'turquoise3', bins =50, position="identity") +
        ylab('Relative Frequency') +
        xlab('2019 Drilling Costs') +
        ggtitle('Simulation of 2019 Drilling Costs by Kernel Density') +
        geom_vline(aes(xintercept = inital_avg), colour="black", linetype="dashed") +
        annotate("text", x = 1500, y = 0.08, label = '2006 Costs', angle = 90)

plot2

inital_oil*quantile(a, probs = c( 0.05,0.5, 0.95)) 
quantile(b, probs = c(0, 0.05,0.5, 0.95))
