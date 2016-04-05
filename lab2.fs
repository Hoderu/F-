open System
open System.Net
open System.IO
open System.Collections.Specialized

// почтовый адрес
let email = "~~~"
// общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = float * int
let delta = 1e-10

// *** Первая часть

let fTailor x : float = 3. ** x // функция, которую раскладываем
let n, a, b = 20., 0., 1. // интервал

let tailor x : Result =
let ln3 = log 3.
let rec calculate last n result =
let next = last * x * ln3 / float(n)
if abs next < delta then (result + last, n)
else calculate next (n + 1) (result + last)
calculate 1. 1 0.
let tailorA x : Result =
let ln3 = log 3.
let rec iterate f result n =
match n with
| 0 -> result
| _ -> iterate f (f result n) (n-1)
let rec calculate result n =
let value = iterate (fun value c -> value * x * ln3 / float c) 1. n
if abs value < delta then (result, n)
else calculate (result+value) (n+1)
calculate 0. 0

let printTailor () =
[a .. (b-a)/n .. b]
|> List.map (fun x -> let (firstRes, firstCou), (secondRes, secondCou) = tailor x, tailorA x in (x, firstRes, firstCou, secondRes, secondCou, fTailor x))
|> List.iter (fun (a,b,c,d,e,f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )

// *** Вторая часть
let ctg x = cos x / (sin x)

let fSolve12 = fun x ->  log x - x + 1.8 // функция, решение которой ищем (12)
let fSolve13 = fun x ->  x * tan x - 1./3. // функция, решение которой ищем (13)
let fSolve14 = fun x ->  tan (x / 2.) - ctg (x / 2.) + x // функция, решение которой ищем (14)

let iterations f a b l =
let rec iterationsA i f x =
let fx = f x
if abs fx < delta then (x, i)
else iterationsA (i + 1) f (x - (l x) * fx)
iterationsA 1 f ((a + b) / 2.)

let iter f a b : Result = //метод итераций
let l = 0.1 * float (sign (f n - (f b))) //знак производной * 0.1
iterations f a b (fun x->l)

let newton f a b : Result = //метод Ньютона
iterations f a b (fun x->2. * delta / (f (x + delta) - (f (x - delta))))

let dichotomy = //метод дихотомии
let rec dichotomyA i (f:float->float) (a:float) (b:float) : Result =
let m = (a+b)/2.
let fm = f m
if abs fm < delta then (m, i + 1)
else
let next = dichotomyA (i + 1) f
if f a * fm < 0. then next a m
else next m b
dichotomyA 0

let printSolveFor (fSolve, a, b) =
[iter; newton; dichotomy]
|> List.map (fun f -> f fSolve a b)
|> List.iter (fun (res, cou) -> printf "%f\t%d\t" res cou)
printfn ""


let printSolve () =
[(fSolve12, 2., 3.); (fSolve13, 0.2, 1.); (fSolve14, 1., 2.)]
|> List.iter printSolveFor

[<EntryPoint>]
let main argv =
let values = new NameValueCollection()
values.Add("email", email)
values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + Path.DirectorySeparatorChar.ToString() + __SOURCE_FILE__))

let client = new WebClient()
let response = client.UploadValues(new Uri("http://mipt.eu01.aws.af.cm/lab1"), values)
let responseString = Text.Encoding.Default.GetString(response)

printf "%A\n" responseString
0

(*[<EntryPoint>]
 let main argv =
 printTailor ()
 printfn ""
 printSolve ()
 0*)