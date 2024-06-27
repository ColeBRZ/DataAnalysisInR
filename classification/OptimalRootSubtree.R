X = matrix(scan("tree_data.dat"),byrow=T,ncol = 3)

splitPenalty = .03

n = 100
optVal = 0

optimalRootSubtree = function(i){
	rootPlus = X[i,1]
	rootMinus = X[i,2]
	rootTerminal = X[i,3]

	if(rootPlus <= rootMinus){
		rootErr = rootPlus/n
	}

	else if(rootPlus >= rootMinus){
		rootErr = rootMinus/n
	}


	if(rootTerminal == 1){
		if(rootErr > optVal){
			optVal = rootErr
		}
		else if(rootErr < optVal){
			optVal = optVal
		}
	} 

	else if(rootTerminal == 0){
		ch1 = 2*i
		ch2 = 2*i+1
		ch1Plus = X[ch1,1]
		ch1Minus = X[ch1,2]
		ch1Terminal = X[ch1,3]

		ch2Plus	= X[ch2,1]
		ch2Minus = X[ch2,2]
		ch2Terminal = X[ch2,3]
		
		if(ch1Plus > ch1Minus){
			ch1Err = ch1Minus/n
		}
		else if(ch1Plus < ch1Minus){
			ch1Err = ch1Plus/n
		}

		if(ch2Plus > ch2Minus){
			ch2Err = ch2Minus/n
		}
		else if(ch2Plus < ch2Minus){
			ch2Err = ch2Plus/n
		}

		optCostCh = splitPenalty + ch1Err + ch2Err
	
		
		if(optCostCh > rootErr){
			if(optCostCh > optVal){
				optVal = optCostCh
			}
			else if(optCostCh < optVal){
				optVal = optVal
			}
		}
	
		else if(optCostCh < rootErr){
			if(rootErr > optVal){
				optVal = rootErr
			}
			else if(rootErr < optVal){
				optVal = optVal
			}
		}
		
		if(ch1Terminal == 0 && ch2Terminal == 0){
			optimalRootSubtree(ch1)
			optimalRootSubtree(ch2)
		}
		else if(ch1Terminal == 0 && ch2Terminal == 1){
			optimalRootSubtree(ch1)
		}
		else if(ch1Terminal == 1 && ch2Terminal == 0){
			optimalRootSubtree(ch2)
		}
	}
    return(optVal)
}

