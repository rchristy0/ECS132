overt <- read.csv("Traffic_data_orig.csv")
library("graphics")

toBinaryletter = function(string){
  vector = unlist(strsplit(string, ""))
  number = as.numeric(charToRaw(vector))
  binary = c()
  track = 0
  while(track != 8){
    result = number %% 2
    binary = c(result,binary)
    number = floor(number/2)
    track = length(binary)
  }
  binary
}

convertString = function(string){
  vector = unlist(strsplit(string, ""))
  unlist(lapply(vector, toBinaryletter))
}

delayCalculationTable = function(message){
  secret = unlist(strsplit(message, ""))
  binary = convertString(message)
  delay = sapply(binary, function(x){
    if(x == 0){
     0.25
    }
    else{
     0.75
    }
  })
  TotalTime = sapply(1:length(delay), function(x) sum(delay[1:x]))
  PacketNum = c(1:length(delay))
  InterPacketDelay = c(TotalTime[1], sapply(2:length(PacketNum), function(x){
    TotalTime[x] - TotalTime[x-1]
  }))
  data.frame(binary, PacketNum, delay, TotalTime, InterPacketDelay)
}

data = delayCalculationTable("this is a secret message")

hist(overt$InterPacketDelay)
hist(data$InterPacketDelay)

mini = 0.00001 #computed via excel
medi = 0.0769
maxi = 0.73199

delayCalculationTableBetter = function(message, mini, medi, maxi){
  secret = unlist(strsplit(message, ""))
  binary = convertString(message)
  delay = sapply(binary, function(x){
    if(x == 0){
      runif(1, mini, medi)
    }
    else{
      runif(1, medi, maxi)
    }
  })
  TotalTime = sapply(1:length(delay), function(x) sum(delay[1:x]))
  PacketNum = c(1:length(delay))
  InterPacketDelay = c(TotalTime[1], sapply(2:length(PacketNum), function(x){
    TotalTime[x] - TotalTime[x-1]
  }))
  data.frame(binary, PacketNum, delay, TotalTime, InterPacketDelay)
}

data1 = delayCalculationTableBetter("this is a secret message", mini, medi, maxi)
hist(data1$InterPacketDelay)

#Question 5
#1. The method can be improved by weighting the "1" delay toward the lower end 
#of the range, so that it can more closely resemble the curve of the overt 
#stream.
#2. It is unrealistic especially in a Skype call that is real time. There would 
#be excessive amounts of lag and the video quality would drop dramatically 
#because the packets are not being sent wen they should, but rather when Alice 
#wants them to.
#3. If the network altered the interpacket delays then any delay that is close 
#to the median could possibly be flipped if the delay is longer or shorter than 
#expected and the result would be loss of data in the secret message. The effect
#could be mitigated by adding a slight buffer between the two ranges when 
#generating delay so that timings are unlikely to cause a flipped bit due to 
#changes from the network.