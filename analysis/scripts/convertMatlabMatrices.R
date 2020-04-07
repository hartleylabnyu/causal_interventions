
#functions for converting matlab matrices into R format
mat2r_matrices3 <- function(matlab_matrix) {
  m2 <- gsub("\ ", ",", matlab_matrix)
  m3 <- gsub(";", "", m2)
  m4 <- gsub("\\[","t(array(c(",m3)
  m5 <- gsub("\\]","),dim=c(3,3)))",m4)
  m6 <- eval(parse(text=m5))
  return(m6)
}

mat2r_matrices4 <- function(matlab_matrix) {
  m2 <- gsub("\ ", ",", matlab_matrix)
  m3 <- gsub(";", "", m2)
  m4 <- gsub("\\[","t(array(c(",m3)
  m5 <- gsub("\\]","),dim=c(4,4)))",m4)
  m6 <- eval(parse(text=m5))
  return(m6)
}
