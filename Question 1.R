
#num rolls to get each face at least once after 500 rolls
numRollsReq <- rep(0,500)

#run experiment
for (i in 1:500){
  #set simulates die being rolled 10,000 times
  sampleSet <- sample(1:12, 10000, TRUE)
  
  #faces of die all set to false will be set to true when face seen on die
  facesOfDie <- rep(FALSE,12)
  
  #num of rolls needed to get each face at least once (i.e. result)
  resRollOfDie <-0
  
  for(j in 1:10000){
    
    #checking face has been rolled and setting respective index based on answer
    if(facesOfDie[sampleSet[j]]==FALSE){
      facesOfDie[sampleSet[j]]<-TRUE
    }
    #checking if all faces rolled if yes we exit with number of rolls it 
    #took; if no we continue rolling
    else if(all(facesOfDie)==TRUE){
      resRollOfDie <- (j-1)
      break
    }
  }
  # we note how many die rollings it took for us this time into the list
  numRollsReq[i] <- resRollOfDie 
}

#print how many times needed to roll to get all faces
print(numRollsReq)

#mean of result allows us to estimate the number of required die rolling to get
#each face at least once. 
print(mean(numRollsReq))

#histogram
hist(numRollsReq)

#density plot
plot(density(numRollsReq))
