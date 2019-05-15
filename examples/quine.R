## string-manupulation quine (version 1)
s1 <- "s2 <- c(\"s1 <- \", deparse(s1))\ncat(s2, \"\\n\", s1, \"\\n\", sep=\"\")"
s2 <- c("s1 <- ", deparse(s1))
cat(s2, "\n", s1, "\n", sep="")

## string-manipulation quine (version 2)
s <- "cat(\"s <- \", deparse(s), \"\\n\", s, \"\\n\", sep=\"\")"
cat("s <- ", deparse(s), "\n", s, "\n", sep="")


## string-manipulation quine (functional version)
quine <-
function()
{
  s <- "cat(\"function()\\n{\\n  s <- \", deparse(s), \"\\n  \", s, \"\\n}\\n\", sep=\"\")"
  cat("function()\n{\n  s <- ", deparse(s), "\n  ", s, "\n}\n", sep="")
}


## substitute quine
(function(x) substitute((x)(x)))(function(x) substitute((x)(x)))

## error quine
Error: unexpected symbol in "Error: unexpected symbol"
#Error: syntax error in "Error: syntax error"
