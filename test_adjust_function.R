# adjust the knn results by known probabilities
knn_adjust <- function(orig_factor, k, num_no=1, num_yes=1)
{
  # original probabilities
  orig_win    <- attr(orig_factor,'prob')
  orig_lose   <- 1.0 - orig_win
  
  # What was original answer
  orig_answer <- as.character(orig_factor)
  other_answer <- ifelse(orig_answer == 'No','Yes','No')
  
  win_votes <- k * orig_win
  lose_votes <- k - win_votes
  
  # if the original answer was no, then divide win by total number of no's
  win  <- win_votes  / ifelse (orig_answer == 'No', num_no, num_yes)
  
  # if the original answer was yes, then divide lose by total number of no's
  lose <- lose_votes / ifelse (orig_answer == 'Yes',num_no,num_yes)
  
  # alter the answer if lose > win
  new_answer <- as.factor(ifelse( (lose > win), other_answer, orig_answer))
  
  levels(new_answer) <- levels(orig_factor)
  return(new_answer)
}

a <- rep('No',10)
a[7] <- 'Yes'

raw <- as.factor(a)
attr(raw,'prob') <- runif(10)


classifications <- knn_adjust(raw,9,1)

cat('raw\n',raw,'\n')
summary(raw)
    
cat ('new\n',classifications,'\n')
summary(classifications)



