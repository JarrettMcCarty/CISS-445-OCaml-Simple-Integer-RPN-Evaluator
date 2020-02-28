# Simple RPN Evaluator

Very simple Reverse polish Notation evaluator using OCaml. The implementation uses a custom "linked list" type to build to Stack for calculating the output.

Simple parse function that takes in a reverse polish notation string and builds a "linked list" from it. 

Here you can call "eval_rpn" with your list as a parameter and an return value of type "Inttok" will be given. This will be the value of the given rpn string.

Note this is very simple and needs work to be more efficent. 

TODO: 
  * Add checks to prompt user if input is not a rpn string
  * Clean up repeatitiveness of certain parts of the code in "eval_rpn"/"parse"
