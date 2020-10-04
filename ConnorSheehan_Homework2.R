#Question 1 - Pythagorean Theorem Function

a = as.integer(readline(prompt = "Please enter a number"))
b = as.integer(readline(prompt = "Please enter a second number"))
c = as.integer(readline(prompt = "Please enter a third number"))

is_pythagorean = function(a,b,c){
  (a^2)+(b^2) == (c^2)
}
is_pythagorean(a,b,c)


#Question 2 - Loops

is_prime <- function(n) {
  if (n >= 2) {
    x = seq(2, n)
    prime_nums = c()
    for (i in seq(2, n)) {
      if (any(x == i)) {
        prime_nums = c(prime_nums, i)
        x = c(x[(x %% i) != 0], i)
      }
    }
    return(prime_nums)
  }
  else 
  {
    stop("Input number should be at least 2.")
  }
}
rev(is_prime(1000)[26:168])


#Extra Credit

is_prime_two = function(n){
  if(sum(n/1:n==n%/%1:n)==2)
    print("prime")
  if(sum(n/1:n==n%/%1:n)!=2)
    print("not prime")
}


#Question 3 - Professor Xavi's Wine Emporium

inventory = matrix(data = 0, nrow = 3, ncol = 3)
colnames(inventory) = c("Wine", "Vodka", "Lemon Juice")
rownames(inventory) = c("A", "B", "C")    
inventory["A", "Wine"] = 20  
inventory["A", "Vodka"] = 30
inventory["A", "Lemon Juice"] = 50
inventory["B", "Wine"] = 30
inventory["B", "Vodka"] = 20
inventory["B", "Lemon Juice"] = 60
inventory["C", "Wine"] = 30
inventory["C", "Vodka"] = 30
inventory["C", "Lemon Juice"] = 32

price = matrix(data = 0, nrow = 3, ncol = 1)
colnames(price) = c("Price")
rownames(price) = c("Wine", "Vodka", "Lemon Juice")
price["Wine", "Price"] = 5
price["Vodka", "Price"] = 45
price["Lemon Juice", "Price"] = 10

#3a

Blend_A_Price = sum(inventory["A",] * price[,"Price"])
Blend_B_Price = sum(inventory["B",] * price[,"Price"])
Blend_C_Price = sum(inventory["C",] * price[,"Price"])

#3b

Total_Price = (sum(inventory["A",] * price[,"Price"]) * 10) + 
  (sum(inventory["B",] * price[,"Price"]) * 4) +
  (sum(inventory["C",] * price[,"Price"]) * 5)
