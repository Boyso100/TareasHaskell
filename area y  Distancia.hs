
area a b c = sqrt (s*(s-a)*(s-b)*(s-c))
 where s= (a+b+c)/2

distancia (x1,y1) (x2,y2) = sqrt ((x1-x2)^2+(y1-y2)^2)


 
ciclo [] = []

ciclo xs = last xs : init xs