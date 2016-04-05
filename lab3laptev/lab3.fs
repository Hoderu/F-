module lab3
open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = "echelon2@mail.ru"

let explode (s:string) =
  [for c in s -> c]

type Token =
  | OpenBrace | CloseBrace
  | OpenBracket | CloseBracket
  | Colon | Comma
  | String of string
  | Number of int
  | Boolean of bool
  | Null

let tokenize source =
  let rec parseString acc = function
    | '\\' :: '"' :: t -> parseString (acc + "\"") t
    | '\\' :: 'n' :: t -> parseString (acc + "\n") t
    | '"' :: t -> acc, t
    | c :: t -> parseString (acc + c.ToString()) t
    | _ -> failwith "Malformed string."

  let rec token acc = function
    | (x :: _) as t when List.exists ((=)x) [')'; ':'; ','; ']'] -> acc, t
    | w :: t when Char.IsWhiteSpace(w) -> acc, t
    | [] -> acc, [] // end of list terminates
    | c :: t -> token (acc + (c.ToString())) t

  let rec tokenize' acc = function
    | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t
    | '{' :: t -> tokenize' (OpenBrace :: acc) t
    | '}' :: t -> tokenize' (CloseBrace :: acc) t
    | '[' :: t -> tokenize' (OpenBracket :: acc) t
    | ']' :: t -> tokenize' (CloseBracket :: acc) t
    | ':' :: t -> tokenize' (Colon :: acc) t
    | ',' :: t -> tokenize' (Comma :: acc) t
    | '"' :: t -> // start of string
      let s, t' = parseString "" t
      tokenize' (String s :: acc) t'
    | 'n' :: 'u' :: 'l' :: 'l' :: t -> tokenize' (Null :: acc) t
    | 't' :: 'r' :: 'u' :: 'e' :: t -> tokenize' (Boolean true :: acc) t
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t -> tokenize' (Boolean false :: acc) t
    | d :: t -> // остались числа
      let n, t' = token (d.ToString()) t
      tokenize' (Number (try Convert.ToInt32 n with e -> 0)  :: acc) t'
    | [] -> List.rev acc
    | _ -> failwith "Tokinzation error"
  tokenize' [] source

type JSON =
  | Object of (string * JSON) list
  | Array of JSON list
  | Number of int
  | String of string
  | Boolean of bool
  | Null

let rec parse json =
  let rec parse' json =
    let rec parseObject list = function
      | CloseBrace :: t -> (Object (List.rev list)), t
      | Comma :: t -> parseObject list t
      | Token.String s :: Colon :: t ->
        let a, t = parse' t
        parseObject ((s, a) :: list) t
      | _ -> failwith "Incorrect object"
    let rec parseArray list = function
      | CloseBracket :: t -> (Array (List.rev list)), t
      | Comma :: t -> parseArray list t
      | ob ->
        let a, t = parse' ob
        parseArray (a :: list) t
    match json with
      | OpenBrace :: t -> parseObject [] t
      | OpenBracket :: t -> parseArray [] t
      | Token.Null :: t -> JSON.Null, t
      | Token.String s :: t -> JSON.String s, t
      | Token.Number s :: t -> JSON.Number s, t
      | Token.Boolean s :: t -> JSON.Boolean s, t
      | _ -> failwith "Incorrect identification"
  match parse' json with
    | res, [] -> res
    | _ -> failwith "Wrong JSON structure"


let rec sum (acc : int) (node : JSON) =
    match node with
    | JSON.Null -> acc
    | JSON.Number x -> acc + x
    | JSON.String _ -> acc
    | JSON.Boolean _ -> acc
    | JSON.Array list ->
        list |> List.sumBy (sum 0)
    | JSON.Object list ->
        list |> List.sumBy (fun x -> x |> snd |> sum 0)

let parse_str = explode >> tokenize >> parse >> (sum 0)

let s1 = "[false, {\"a\": [[1, 2]]}]"

let s2 = """
{
  "a": 1
}
"""
let s3 = """
[
    "a";
    {
      "a": 1
      "b": {"c": true}
     }
]
"""

let s4 = """
[
    false;
    {
        "a": [[1, 2]]
        "b": 6
    }
]
"""

let s5 = """
[
    "b";
    {
        "c": [[[1000], [2000]]]
        "b": false
    }
]
"""

let tests = [s1; s2; s3; s4; s5]
let expected = [3; 1; 1; 9; 3000]

let test_passed = List.forall (fun (s, exp) -> parse_str s = exp) (List.zip tests expected)

let test() =
    if (test_passed = true) then
        printfn "Tests passed: OK\n"
    else
        printfn "Tests failed: FAIL\n"
    0

let rec get_json () : JSON =
    let random_word =
        let R = System.Random()
        fun n -> System.String [|for _ in 1..n -> R.Next(26) + 97 |> char|]
    let rnd = new Random()
    match rnd.Next(6) with
    0 -> JSON.Null
    |1 -> JSON.Boolean(rnd.Next(2) = 1)
    |2 -> JSON.String( random_word(rnd.Next(10)) )
    |3 -> JSON.Number(rnd.Next(5555))
    |4 -> JSON.Array [for i in [0..rnd.Next(12)] -> get_json()]
    |5 -> JSON.Object [for i in [0..rnd.Next(12)] -> ( random_word(rnd.Next(10)) , get_json())]



let toString (obj:JSON) =
    let rec toString' (obj:JSON) =
        match obj with
        JSON.Array list -> List.fold (fun s x -> if List.head list = x then
                                                    s + toString' x
                                                 else s + "; " + (toString' x)) "[ " list + "] "
        |JSON.Object list ->
            List.fold (fun s (k,v) -> s + "\"" + k + "\": " + toString' v + " ") "{ " list + "} "
        |JSON.Null _ -> "Null "
        |JSON.Boolean obj -> obj.ToString()
        |JSON.Number obj -> obj.ToString()
        |JSON.String obj -> obj
    toString' obj

let sample() =
  let sample_json = get_json()
  let tmp = toString sample_json
  printfn "test = %s\n" tmp
  printfn "ans = %d\n" (parse_str tmp)
  0

let main () =
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://mipt.eu01.aws.af.cm/lab3"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString
  0


