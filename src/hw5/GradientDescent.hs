module GradientDescent
(
errorfunc,
nIterations,
solution5,
solution6,
solution7,
coordinateDescent
)
where
--Homework problems 5 of Learning from Data
--Problems can be found here:
--https://work.caltech.edu/homeworks.html

--error surface as defined in the exercise
errorfunc u v = (u * exp v - 2 * v* exp (-u))^2
--partial derivatives
partialByU u v = 2*(exp v + 2*v * exp (-u))*(u * exp v -2*v*exp(-u))
partialByV u v = 2 * (u* exp v - 2 * exp (-u)) * (u * exp v - 2 *v*exp(-u))
--update rules
updateU alpha (u, v) = u - alpha * partialByU u v
updateV alpha (u, v)  = v - alpha * partialByV u v
--update rules for coordinate descent
updateFirst alpha (u,v) = (updateU alpha (u,v),v)
updateSecond alpha (u,v) = (u,updateV alpha (u,v))
updateBoth alpha (u,v) = (updateU alpha (u, v), updateV alpha (u, v) )
updateCoordinate  alpha = updateSecond alpha . updateFirst alpha
--iterations for gradient descent and coordinate descent
iterations alpha = iterate (updateBoth alpha)
nIterations n alpha point = iterations alpha point !! n 
coordinateDescent n alpha point = iterate (updateCoordinate alpha) point !! n

--finding the number of iterations it takes to minimize the error
numIterations n 
    |err n <= 10**(-14) = n
    |otherwise = numIterations (n+1)
        where err n = uncurry errorfunc $ nIterations n 0.1 (1,1)

--Solutions to the exercises 5 -7 of homework 5

solution5 = numIterations 1
solution6 = nIterations (numIterations 1) 0.1 (1,1)
solution7 = uncurry errorfunc $ coordinateDescent 15 0.1 (1,1)
