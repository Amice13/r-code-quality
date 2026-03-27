# code for estimating plurality pivotal probabilities via numerical integration given independence assumption 

library(gtools)

napotffay = function(alpha.vec, y){
	# naive.analytical.probability.of.tie.for.first.at.y 
	# probability that parties 1 and 2 tie at y can be calculated without specifying the values of the other parties, by the aggregation property.
	prob.of.tie.at.y = ddirichlet(x = c(y,y,1-2*y), alpha = c(alpha.vec[1], alpha.vec[2], sum(alpha.vec) - alpha.vec[1] - alpha.vec[2]))
	# now, what is the probability that _no other party_ would have gotten above y?
	# we do it separately for each party -- a naive approach not recognizing how the vote shares for the other parties are related.
	# obvious they are correlated because they must sum to 1-2*y.  
	probs = c()
	for(i in 3:length(alpha.vec)){
		this.prob = pbeta(y/(1-2*y), alpha.vec[i], sum(alpha.vec[-c(1,2,i)])) 
		probs = c(probs, this.prob)
	}
	list(prob.of.tie.at.y = prob.of.tie.at.y, probs.of.being.below.y = probs, probability = prod(c(prob.of.tie.at.y, probs))) 
}

# now we aggregate this up (integrate over values of y)
probability.of.tie.for.first = function(alpha.vec, increments = 50){
	ys = seq(1/length(alpha.vec), .5, length = increments) # the least a pair of parties can get and be tied for first is 1/k each. the most they can get is .5.  We are going to calculate the probability of a tie for first at each value in that sequence and then integrate.
	probs = c() # this holds the probability of the two parties being tied for first at each y
	tie.vec = c()  # this holds the probability of the two parties being tied at a given y 
	prob.mat = matrix(NA, nrow = 0, ncol = length(alpha.vec)-2) # this holds the probabilities of being below y for each of the parties other than the ones who are tied.
	increment = ys[2] - ys[1]
	for(i in 1:(length(ys) - 1)){
		this = napotffay(alpha.vec, (ys[i] + ys[i+1])/2)
		probs = c(probs, increment*this$probability) 
		tie.vec = increment*c(tie.vec, this$prob.of.tie.at.y)  
		prob.mat = rbind(prob.mat, this$probs.of.being.below.y) 
	}
	list(estimate = sum(probs), 
  	ys = ys, tie.vec = tie.vec, prob.mat = prob.mat) # I include these for diagnostic purposes
}

## and here's a function that yields pivotal probabilities in a vector, given an alpha input.
pivotal.probabilities.analytical = function(alpha.vec, increments = 50, normalize = F, as.matrix = F, n = 50000){
	out.vec = name.vec = c()
	out.matrix = matrix(0, nrow = length(alpha.vec), ncol = length(alpha.vec))
	rownames(out.matrix) = colnames(out.matrix) = names(alpha.vec)
	for(i in 1:length(alpha.vec)){
		for(j in 1:length(alpha.vec)){
			if(i <= j){next}
			this.entry = probability.of.tie.for.first(alpha.vec = c(alpha.vec[i], alpha.vec[j], alpha.vec[-c(i,j)]), increments = increments)$estimate/n
			out.matrix[i,j] = out.matrix[j,i] = this.entry
			out.vec = c(out.vec, this.entry)
			name.vec = c(name.vec, paste0("p.", names(alpha.vec)[i], ".", names(alpha.vec)[j]))
		}
	}
	if(normalize){
		out.vec = out.vec/max(out.vec)		
	}
	names(out.vec) = name.vec
	if(as.matrix){return(out.matrix)}
	out.vec
}

# and a function that turns this into a P matrix
plurality.P.matrix.analytical <- function(alpha.vec, increments = 50, normalize = F, as.matrix = T, n = 50000){
  ppa = pivotal.probabilities.analytical(alpha.vec, increments, normalize, as.matrix = T, n)
  P = matrix(NA, nrow = length(alpha.vec), ncol = length(alpha.vec))
  colnames(P) = rownames(P) = names(alpha.vec)
  for(i in 1:nrow(P)){
    for(j in 1:ncol(P)){
      if(i == j){
        P[i,j] = 2*sum(ppa[i,])
      }else{
        P[i,j] = sum(ppa[i,-j])
      }
    }
  }
  P
} 


