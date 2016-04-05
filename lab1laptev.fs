module lab1

open System
open System.Net
open System.IO
open System.Collections.Specialized
 
// почтовый адрес
let email = "hoderu@yandex.ru"
// общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = float * int
let delta = 1e-10
 
// *** Первая часть
 
let fTailor x : float = 3. ** x // функция, которую раскладываем
let n, a, b = 20., 0., 1. // интервал
 
let tailor x : Result = 
    let log3 = log 3.
    let rec mytailor last n result = 
        let current = (last * log3 * x) / float(n)
        if abs current < delta then (result + last, n)
            else mytailor current (n + 1) (result + last)
    mytailor 1. 1 0.

let tailorA x : Result = 
    let log3 = log 3.
    let rec iter f result n = 
        match n with
        | 0 -> result
        | _ -> iter f (f result n) (n - 1)
    let rec mytailor result n = 
        let current = iter (fun current c -> (current * x * log3) / float c) 1. n
        if abs current < delta then (result, n)
            else mytailor (result + current) (n + 1)
    mytailor 0. 0
 
let printTailor () = 
    [a .. (b-a)/n .. b] 
    |> List.map (fun x -> let (firstRes, firstCou), (secondRes, secondCou) = tailor x, tailorA x in (x, firstRes, firstCou, secondRes, secondCou, fTailor x))
    |> List.iter (fun (a,b,c,d,e,f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )
 
// *** Вторая часть
let ctg x = cos x / (sin x)
 
let fSolve12 = fun x ->  log x - x + 1.8
let fSolve13 = fun x ->  x * tan x - 1./3.
let fSolve14 = fun x ->  tan (x / 2.) - ctg (x / 2.) + x
 
let iterations f a b l =
    let rec iterationsA f i x = 
        let f_x = f x
        if abs f_x < delta then (x, i)
            else iterationsA f (i + 1) (x - (l x) * f_x)
    iterationsA f 1 ((a + b) / 2.)
 
let iter f a b : Result = 
    let l = 0.1 * float (sign (f n - (f b)))
    iterations f a b (fun x->l)
 
let newton f a b : Result = 
    iterations f a b (fun x->2. * delta / (f (x + delta) - (f (x - delta))))
 
let dichotomy =
    let rec dichotomyA i (f:float->float) (a:float) (b:float) : Result =
        let middle = (a+b)/2.
        let f_m = f middle
        if abs f_m < delta then (middle, i + 1)
            else
            let next = dichotomyA (i + 1) f 
            if f a * f_m < 0. then next a middle
                else next middle b
    dichotomyA 0 
 
let printSolveFor (fSolve, a, b) =
    [iter; newton; dichotomy] 
    |> List.map (fun f -> f fSolve a b) 
    |> List.iter (fun (res, cou) -> printf "%f\t%d\t" res cou)
    printfn ""
 
 
let printSolve () =
    [(fSolve12, 2., 3.); (fSolve13, 0.2, 1.); (fSolve14, 1., 2.)] 
    |> List.iter printSolveFor
 
let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://mipt.eu01.aws.af.cm/lab1slug"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString
main()
