#sample two dice 
replicate(2, sample(6, 10, TRUE))
#       [,1] [,2]
# [1,]    4    3
# [2,]    4    6
# [3,]    5    6
# [4,]    2    1
# [5,]    5    5
# [6,]    1    2
# [7,]    6    3
# [8,]    2    5
# [9,]    1    2
# [10,]    2    4
#row sums to get sums from 10 rolls of 2 dice
rowSums(replicate(2, sample(6, 10, TRUE)))
# [1]  8  6  3  8  5  8  4  4 12  5

#simulate 10,000 rolls of two dice to get accurate estimate (Monte Carlo)
rollsOfDice <- rowSums(replicate(2, sample(6, 10000, TRUE)))

#near certain we will have values 2-12 in throws but to be sure we test
length(unique(rollsOfDice))
# [1] 11

#we see 11 throws is not enough to gell all 11 outcomes (2-12)
length(unique(rollsOfDice[1:11]))
# [1] 10

#we take sample of 100
length(unique(rollsOfDice[1:100]))
# [1] 11

#between 11 and 100 throws is our expected number of throws 
#we iterate through throws to find min point where all 11 sums have been obtained 
for(i in 11:100)
{
  if(length(unique(rollsOfDice[1:i])) == 11){
    break;
  }
}

# [1] 32

#loop stops when i = 23 telling us it took 23 rolls to get all 11 sums
#from our two die

#logic can be in a function
numRollsNeeeded <- function() {
  throws <- rowSums(replicate(2, sample(6, 10000, TRUE)))
  for(i in 11:10000)
  {
    if(length(unique(throws[1:i])) == 11){
      break;
    }
  }
  return(i)
}

#various runs of experiment with different answers for each experiment
numRollsNeeeded()
# [1] 50
numRollsNeeeded()
# [1] 81
numRollsNeeeded()
# [1] 59

#distribution of results 
result <- replicate(10000, numRollsNeeeded())

#mean number of throws needed
mean(result)
#[1] 60.897

#histogram 
hist(result)

#density plot
plot(density(result))

