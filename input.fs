open System

let rec pascal c r =
    if (r = 0) then 1
    elif (r > c) then 0
    else pascal (c - 1) (r - 1) + pascal(c - 1) r

let main() =
    //printfn "%i" (pascal 4 4)
    Console.WriteLine(pascal 4 2)

main()
