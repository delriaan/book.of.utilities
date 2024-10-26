# Helper Functions for `ratio()`: ----
of.max <- \(.i) .i/max(.i, na.rm = TRUE);

of.sum <- \(.i) .i/sum(.i, na.rm = TRUE);

cumulative <- \(.i){
	if (any(is.na(.i))){
		.i[is.na(.i)] <- 0
	}
	cumsum(.i)/sum(.i, na.rm = TRUE);
}

dens <- \(.i, .type){
  # Given `.i`, .kernel defines <-1, 0, 1> positions of 
  # the current position in indexing vector `.idx:
  .idx <- seq_len(length(.i))
  .p <- sapply(.idx, \(n) mean(.i <= .i[n], na.rm = TRUE))
  
  if (.type == "of.max"){
    .p <- exp(.p)/sum(exp(.p))
  }  
  of.sum(.p * (1-.p))
}
