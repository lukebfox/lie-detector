# lie-detector

##### Simple propositional logic calculator written in Haskell.


## Installation
Make sure you have the Stack tool chain installed on your system. In the project directory run:

`$ stack build`

`$ stack exec lie-detector`


## Usage
With this tool you can assign values to variables and combine these variables into statements, which will be evaluated for you. For example,
if you evaluate  `p = 1`, followed by `q = 0`, you will then be able to evaluate longer expressions such as `¬(p^(q→p))`. True is 1; False is 0. Variables must start with letter, although they may contain digits. Use the Alt-Gr key with 'y' or 'i' respectively for implication symbols. Heavy use of parentheses is advised.

##### Recognised Symbols/Operators
* And : ^
* Or : |
* Not : ¬
* Right Imply : →
* Left Imply : ←
* Assign : = 


#### To-do
* ~~Write a README~~
* ~~Swap nasty imperative style for pure functional monads~~
* Use more friendly operator symbols
* Make a prompt
