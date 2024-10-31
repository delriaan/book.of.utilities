# Helper Functions for `ratio()`: ----
softmax <- \(.p) exp(.p)/sum(exp(.p), na.rm = TRUE)

of.max <- \(.i) .i/max(.i, na.rm = TRUE);

of.sum <- \(.i) .i/sum(.i, na.rm = TRUE);

cumulative <- \(.i){
  sapply(1:length(.i), \(n) mean(.i <= .i[n], na.rm = TRUE))
}

dens <- \(.i, .type = "of.sum"){
  .p <- .i

  if (.type == "of.max"){
    .p <- softmax(.i) |> cumulative()
  } else if (.type == "of.sum"){
    .p <- cumulative(.i)
  }
  
  of.sum(.i) * .p * (1 - .p)
}
