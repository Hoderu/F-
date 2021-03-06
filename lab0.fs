module lab0

open System
open System.Net
open System.Collections.Specialized

let (email, name) = ("hoderu@yandex.ru", "Решетников А.Л.") // адрес почты и фамилия с инициалами

let rec pascal c r =
    if (c = r) || (c = 0) then 1
    else pascal (c - 1) (r - 1) + pascal c (r - 1)

let printIt n = 
  "[" +
  ([for x in 0..n do for y in 0..x do yield pascal y x] 
    |> List.map (fun x -> x.ToString())
    |> List.reduce (fun x y -> x + "," + y) )
  + "]"

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("name", name)
  values.Add("lang", "fsharp")
  values.Add("content", printIt 20)

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://mipt.eu01.aws.af.cm/lab0"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString
main()