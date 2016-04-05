open FSharp.Data
open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized
let email = "leofish@list.ru"
let upgrade link = "http://wikimipt.org" + link
let get (link : string) =
    let bases = HtmlDocument.Load (link)
    let a = 
        bases.Descendants ["table"]
            |> Seq.filter (fun (x : HtmlNode) -> x.HasClass "wikitable card") 
            |> Seq.collect (fun (x : HtmlNode) -> x.Descendants ["tr"])
            |> Seq.collect (fun (x : HtmlNode) -> x.Descendants ["td"])
            |> Seq.filter(fun x -> (x.InnerText()).StartsWith "Факультет") 
    let list = Seq.toList a
    if (List.isEmpty list) then "non" else (list.[0]).InnerText()
let chair =
    let bases = HtmlDocument.Load(@"http://wikimipt.org/wiki/Категория:Кафедры_по_алфавиту")
    bases.Descendants["td"]
        |> Seq.filter (fun (x : HtmlNode) -> x.HasAttribute("style", "width: 33.3%;"))
        |> Seq.collect (fun (x:HtmlNode) -> x.Descendants ["a"])
        |> Seq.map (fun (x : HtmlNode) -> upgrade (x.AttributeValue "href"))
        |> Seq.toList
let dep =
    chair 
        |> Seq.map (fun x -> get x) 
        |> Seq.filter (fun x -> not (String.Equals("non", x)))
let compare (name1, r1) (name2, r2) =
    if (r1 < r2) then 1
    elif (r1 > r2) then  -1
    else 0
let lab2 () =
    let dep_num = Seq.countBy(fun x -> x) dep
    let list_dep = Seq.toList dep_num
    let mostPopular =  List.sortWith compare list_dep
    fst (mostPopular.[0])

let main () =
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("result", lab2().ToString())
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))
  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://mipt.eu01.aws.af.cm/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString
main()