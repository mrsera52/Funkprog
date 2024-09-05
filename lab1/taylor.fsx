//вариант 23 

let main_func x = atan(x)

let a = 0.
let b = 0.5
let n = 10

let eps = System.Double.Epsilon 

let rec power x n =
    if n = 0.0 then 1.0 
    else x * power x (n - 1.0) 


let func (x, n) = (power -1. n) * (power x (2. * n + 1.)) / (2. * n + 1.)


let rec taylor_naive x =
    let rec loop cnt sum =
        let current_term = func(x, cnt)
        if abs current_term > eps then
            loop (cnt + 1.) (sum + current_term)
        else
             sum 
    loop 0. 0.


let taylor_smart x =
    let initialTerm = x 
    let rec loop n term sum =
        if abs term < eps then sum
        else
            let nextTerm = -term * x * x  / (2. * n + 1.) * (2. * (n - 1.) + 1.) 
            loop (n + 1.) nextTerm (sum + nextTerm)
    loop 1 initialTerm (initialTerm)



let main =
    for i = 0 to n do
        let x = a + float i / float n * (b - a)
        printfn "%5.2f  %10.6f  %10.6f %10.6f " x (main_func x) (taylor_naive x) (taylor_smart x)

main
