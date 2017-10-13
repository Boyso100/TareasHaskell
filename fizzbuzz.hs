lessThan20 :: Int -> String
lessThan20 n
    | n>0 && n<20 =
    let answers = ["one", "two", "three","four", "five", "six"]
    in answers!!(n-1)

tens :: Int -> String
tens n 
    | n>=2 && n<=9 = 
    answers!!(n-2) 
    where answers = words("twenty thirty forty fifty sixty seventy eighty ninety")

numbers :: Int -> String
numbers n 
    | n < 20 = lessThan20 n
    | n < 100 && ((n `mod` 10)==0) = tens (n `div` 10)
    | n < 100 = tens (n `div` 10)++" "++lessThan20 (n `mod` 10)
    | n == 100 = "one hundred"

fizzBuzz :: Int -> String
fizzBuzz n
    |(n `mod` 3)==0 = "Fizz"
    |((n `mod` 5)==0) = "Buzz"
    |(((n `mod` 3)==0) && ((n `mod`5)==0)) = "fizzBuzz"
