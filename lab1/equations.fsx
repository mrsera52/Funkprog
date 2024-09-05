let eps = System.Double.Epsilon 

let rec dichotomy f a b =
    let fa = f a
    let fb = f b
    let rec loop a b =
        let c = (a + b) / 2.0
        let fc = f c
        if abs(fc) < eps 
            then c
        elif fa * fc < 0.0 then 
            loop a c
        else 
            loop c b
    loop a b

let rec iterations f x0 =
    let x1 = f x0
    if abs(x1 - x0) < eps then
        x1
    else
        iterations f x1


let f1 x = (3.0 * x) - 4.0 * log x - 5.0
let f2 x = cos(2.0 / x) - 2.0 * sin(1. / x) + 1.0 / x

let f3 x = sqrt(1.0 - 0.4 * x ** 2.0) - asin(x)

let f1' x = 3.0 - 4.0 / x
let f2' x = (-2.0 * sin(2.0 / x) + 2.0 * cos(1.0 / x) - 1.0) / (x ** 2.0)
let f3' x = - (2.0 * sqrt(25.0 - 10.0 * x ** 2.0) * x) / (25.0 - 10.0 * x ** 2.0) - (sqrt(1.0 - x ** 2.0) / 1.0 - x ** 2.0)


let g1 x = x - f1 x / f1' x
let g2 x = x - f2 x / f2' x
let g3 x = x - f3 x / f3' x

let phi1 x = (-4.0 * log(x) - 5.0) / 3.0
let phi2 x = 1.0 / (sin(1.0 + 1.0/x) / (2.0 + 2.0 * cos(2.0/x)))
let phi3 x = sin(sqrt(1.0 - 0.4 * x ** 2.0))


let main =

    printfn " | %10.5f | %10.5f | %10.5f|" (dichotomy f1 2. 4.) (iterations phi1 4.) (iterations g1 2.)
    printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f2  1. 2.) (iterations phi2 1.) (iterations g2 2.)
    printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f3 0. 1.) (iterations phi3 0.) (iterations g3 0.)


main
