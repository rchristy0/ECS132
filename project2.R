# 1.
NumBeforeACollision = function(){
  ListofVectors = {}
  SampleVector = sample(0:1, size = 8, prob = c(0.5, 0.5), replace = TRUE)
  comparison = TRUE
  count = 1
  while(comparison == TRUE){
    ListofVectors[[length(ListofVectors) + 1]] = SampleVector
    SampleVector = sample(0:1, size = 8, prob = c(0.5, 0.5), replace = TRUE)
    count = count + 1
    i = 0
    repeat{
      i = i + 1
      if(all(ListofVectors[[i]] == SampleVector)){
        comparison = FALSE
        break
      } else if(i==length(ListofVectors)){
        break
      }
    }
  }
  count
}

# NumBeforeACollision()

mean(sapply(1:10000, function(x) NumBeforeACollision()))

# 2.
NumBeforeSCollision = function(SomeVector){
  comparison = TRUE
  count = 0
  while(comparison == TRUE){
    SampleVector = sample(0:1, size = 8, prob = c(0.5, 0.5), replace = TRUE)
    count = count + 1
    if(all(SampleVector == SomeVector)){
      comparison = FALSE
    }
  }
  count
}

# NumBeforeSCollision()

mean(sapply(1:10000, function(x) NumBeforeSCollision(c(0,0,0,0,0,0,0,0))))

# 3.
pACollision = function(numVectors, trials){
  Collisions = sapply(1:trials, function(x){
    FirstVectorList = lapply(1:numVectors, function(i) sample(0:1, size = 8, prob = c(0.5, 0.5), replace = TRUE))
    SecondVectorList = unique(FirstVectorList)
    if(length(FirstVectorList) >  length(SecondVectorList)){
      TRUE
    }
    else{
      FALSE
    }
  })
  mean(Collisions)
}

SCollision = function (n, SomeVector){
  Collision = FALSE
  while(n > 0){
    n = n - 1
    SampleVector = sample(0:1, size = 8, prob = c(0.5, 0.5), replace = TRUE)
    if(all(SampleVector == SomeVector)){
      Collision = TRUE
      n = 0
    }
  }
  Collision
}

pSCollision = function(numVectors, SomeVector, trials){
  success = sapply(1:trials, function(x) SCollision(numVectors, SomeVector))
  return(sum(success)/trials)
}

# Probability of A-Collision given 20 vectors
pACollision(20, 10000)
# Probability of S-Collision given 200 vectors
pSCollision(200, c(0,0,0,1,0,0,1,1), 10000)

# 4
NumBeforeSCollision = function(n, SomeVector){
  comparison = TRUE
  count = 0
  while(comparison == TRUE){
    SampleVector = sample(0:1, size = n, prob = c(0.5, 0.5), replace = TRUE)
    count = count + 1
    if(all(SampleVector == SomeVector)){
      comparison = FALSE
    }
  }
  count
}

# Mean trials for S-Collision
mean(sapply(1:1000, function(x) NumBeforeSCollision(10, c(1,0,0,1,1,0,0,1,1,0))))
mean(sapply(1:1000, function(x) NumBeforeSCollision(12, c(1,0,0,1,1,0,1,0,0,1,1,0))))
mean(sapply(1:1000, function(x) NumBeforeSCollision(14, c(1,0,0,1,1,0,0,0,1,0,0,1,1,0))))
mean(sapply(1:1000, function(x) NumBeforeSCollision(16, c(1,0,0,1,1,1,1,0,1,1,1,0,0,1,1,0))))


# Each bit increases the number of trials by about a factor of 2. A 10-bit 
# takes about 1000 trials and we estimate that a 24 bit will take about 2^14
# more trials on average. So, a 24-bit IV should have, on average, around
# 16384000 trials before an S-Collision.

# Given a 52 Mbps connection and a 1500 byte packet size we have 4333 packets/s.
# With a counter-based approach there are 2^8 possible vectors for an 8-bit IV.
# There is a maximum of 256 vectors we need to test so it will take at most .06 
# seconds. With an 8-bit IV we estimated about 255 packets on average until 
# collision, this means it will take on average .06 seconds to collide with an 
# 8-bit. By going up to a 16-bit IV there will be 2^16 possible for a 
# counter-based approach. There will be 65536 possible vectors so at most 15.12 
# seconds for a collision. With the probabalistic approach it will take on 
# average 64000 packets until collision, meaning it will take about 14.77 
# seconds to collide with a 16-bit IV. The counter approach will be slightly 
# faster than the probabalistic approach because the max time for the counter
# is close to the same as the average time for the random approach.
