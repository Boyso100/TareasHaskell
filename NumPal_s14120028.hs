lessThan20 :: Int -> String
lessThan20 n
	| n < 0 = "rango no valido"
	| n >= 0 && n < 20 = answers !! (n) 
	where answers = words("cero uno dos tres cuatro cinco seis siete ocho nueve diez once doce trece catorce quince dieciseis diecisiete dieciocho diecinueve")
numbers :: Int -> String
numbers n
	| n >= 30 && n < 100 && ( (n `mod` 10) == 0 ) = answers !! ((n `div` 10)-3)
	| n == 20 = "veinte"
	| n > 20 && n < 30 = "veinti" ++ lessThan20(n`mod`10)
	| n > 30 && n < 100 && ((n`mod`10)/=0)= answers !! ((n`div`10)-3)++" y "++lessThan20(n`mod`10) 
	| otherwise = lessThan20(n)
	where answers = words("treinta cuarenta cincuenta sesenta setenta ochenta noventa")

cientos :: Int -> String
cientos n
	| n == 100 = "cien"
	| n > 100 && n < 200 = "ciento " ++ numbers(n`mod`100)
	| n >=200 && n < 1000 && ((n`mod`100)==0) = answers!! ((n `div`100)-2)
	|n >200 && n < 1000 && ((n`mod`100)/=0) = answers !! ((n `div`100)-2) ++" " ++ numbers(n`mod`100)
	|otherwise = numbers(n)
	where answers = words("doscientos trescientos cuatrocientos quinientos seiscientos setecientos ochocientos novecientos")

miles :: Int -> String
miles n
	| n == 1000 = "mil"
	| n > 1000 && n < 2000 = "mil " ++ cientos(n`mod`1000)
	| n >= 2000 && n < 1000000 && ((n`mod` 1000)==0) = cientos(n`div`1000) ++ " mil"
	| n >= 2000 && n < 1000000 && ((n`mod` 1000)/=0) = cientos(n`div`1000) ++ " mil " ++ cientos(n`mod`1000)
	| otherwise = cientos(n)


elmillon :: Int -> String
elmillon n 
	| n == 1000000 = "un elmillon"
	| n > 1000000 = "numero no valido"
	| otherwise = miles(n)

numbers2::Int -> String
numbers2 n

    |n < 20=lessThan20(n)
    |n < 100 = numbers(n)
    |n < 1000 = cientos(n)
    |n < 1000000= miles(n) 
    |n > 1000000 = elmillon(n)