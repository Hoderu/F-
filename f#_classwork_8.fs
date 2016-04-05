let wReturn (x: 'a) : ('a * string) = (x, " got " + x.ToString() + ".")
let wReturnFrom (x: 'a * string) : 'a * string = x
let wBind (mx: ('a * string)) (f: 'a -> 'b * string) : 'b * string = 
    let res = f (fst mx)
    (fst res, snd mx + snd res)
                                                       
let (>>=) = wBind

type WriterMonad() =
    member x.Bind(p, f) = wBind p f
    member x.Return(y) = wReturn y
    member x.ReturnFrom(y) = wReturnFrom y

let writer = new WriterMonad()

let squared x = (x * x, " was squared.")
let halved x = (x / x, " was halved.")
 
let a = writer {
    let! a = wReturn 4
    let! b = squared a
    let! c = halved a
    return c
} // (1, " got 4. was squared. was halved. got 1.")