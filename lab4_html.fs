//#r "../packages/FSharp.Data.2.2.1/lib/net40/FSharp.Data.dll"
open FSharp.Data
open System
open System.IO
 
let dampingFactor = 0.85
let eps = 0.00001
 
let getDomain (site:string) = // получить доменное имя сайта http: wiki.ru
    let tokens = site.Split('/')
    tokens.[0] + "//" + tokens.[2]
 
let isHtmlPage (link:string) = // проверяет является ли ссылка html страницей
    let tokens = link.Split('/')
    let lastToken = tokens.[Array.length tokens - 1] // кончается на
    (link = getDomain link) || lastToken.EndsWith(".html") || lastToken.EndsWith(".htm") || lastToken.EndsWith(".php")
        || (not (lastToken.Contains(".")) && not (lastToken.StartsWith("javascript")) && not (link.Contains("javascript"))
            && not (lastToken.Contains("?")) && not (lastToken.Contains("#")) && not (lastToken.Contains(":")))

let getLinks (link:string) = async {
    try
        printfn "Inspecting %s" link
        let! html = HtmlDocument.AsyncLoad(link) // асинхронно загружаем код страницы html
        let domain = getDomain link
        let links = html.Descendants["a"] // получить все узлы html дерева с тегом а это ссылка в html
                      |> Seq.map (fun x -> x.AttributeValue("href")) // получить значение атрибута href там хранится ссылка
                      |> Seq.map (fun x -> x.Trim()) // удалить ледирующие и конечные пробелы
                      |> Seq.map (fun x -> x.TrimStart('.').TrimStart('/')) // удалить из начала
                      |> Seq.map (fun x -> if (x.StartsWith("http")) then x else domain + "/" + x.TrimStart('.').TrimStart('/'))
                      |> Seq.map (fun x -> x.TrimEnd('/'))
                      |> Seq.filter isHtmlPage // оставляет только html страницы
                      |> Seq.filter(fun x -> x.StartsWith(domain)) // убираем все ссылки ведущие на другие сайты
                      |> Seq.distinct // оставляем различные ссылки
                      |> Seq.toList
        return link, links
    with
        | :? System.Net.WebException -> return link, []
        | :? System.Exception -> return link, []
}
 // откуда и куда
let addLinks (map, candidates) = fun (link, links) -> // текущий map и текущий список кондидатов на bfs на слудеющий уровень
    printfn "adding links from %s" link
    let linksAdder map element = // говорит что на тебя ведет ссылка с таким адресом
        let previousValue = Map.find element map // было число исходящих и список входящих
        let newValue = match previousValue with
                        | (a, b) -> (a, (link::b))
        Map.add element newValue map
    let elementAdder pair element = // добавляет новые узлы в графе
        let map = fst pair
        let lst = snd pair
        if (Map.containsKey element map) then map, lst
                                         else Map.add element (0, []) map, element::lst // есть ли эта ссылка в текущем мапе если нет то добавляем в голову списка
    let previousValue = Map.find link map // нужно обновить количество ссылок идущих из текущего узла что мы ищем где мы ищем
    let newValue = match previousValue with // записываем в мап старое число входящих ссылок 
                    | (a, b) -> (List.length links, b) 
    let (newMap, newCandidates) = List.fold elementAdder ((Map.add link newValue map), candidates) links // fold - свертка списка
    (List.fold linksAdder newMap links), newCandidates // 
 
 //взяли страницу - 1-ый уровень bfs
let getAllPages startLink = // поиск в ширину в графе // получили мап и список всех страниц оторбажение из адреса страницы в пару
//количество исходящих ребер и список входящих
    let rec addTasks linkGetTasks = fun (map, allPages) -> // принимаем на вход текущий map-граф и список страницу
        let newMap, newCandidates = linkGetTasks |> Async.RunSynchronously |> Array.toList |> List.fold addLinks (map, [])
        if (List.length newCandidates = 0)
            then (newMap, allPages)
            else      
                let tasks = newCandidates |> List.map getLinks |> Async.Parallel // создаем задание список асинков по получению ссылок с этих страниц
                addTasks tasks (newMap, allPages @ newCandidates) // все добавили
    addTasks ([getLinks startLink] |> Async.Parallel) (Map.empty.Add(startLink, (0, [])), [startLink])
// получили список из одного асинка на получение задач с исходной страницы
 
 
let localPr map pr page =
    let leavePr = Map.find page map |> snd
                  |> List.map (fun x ->  (Map.find x pr) / float(fst (Map.find x map))) // список отношения pagerk к количеству элементов в списке
                  |> List.sum
    (1. - dampingFactor) + dampingFactor * leavePr // количество ссылок с данной страницы
 
 
let recalulatePr map links pr numberOfPages = // пересчет pagerk для каждой страницы итеративно пока дельта станет не достаточно маленькой
    let folder =
        fun (mp, delta) el ->
            let newPr = localPr map pr el
            let oldPr = Map.find el pr
            (Map.add el (localPr map pr el) mp, min delta (abs (oldPr - newPr)))
    links |> List.fold folder (Map.empty, numberOfPages)
   
let rec calculatePr map links pr numberOfPages = // рекурсивный пересчет
    let newPr, delta = recalulatePr map links pr numberOfPages
    printfn "Caclucalting pagerank: current delta is %f" delta
    if (delta < eps) then newPr
                     else calculatePr map links newPr numberOfPages
 
let pageRank initialPage =  
    let map, list = getAllPages initialPage
    printfn "Scheme was built"
    let numberOfPages = float (List.length list)
    let startPr = list |> List.map (fun x -> (x, 1.)) |> Map.ofList
    calculatePr map list startPr numberOfPages
 
 
[<EntryPoint>]
let main args =
    let page = args.[0]
    use file = new StreamWriter(args.[1])
    let pr = pageRank page
    printfn "Pagerank calculated"
    pr |> Map.toList |> List.sortBy snd |> List.rev
    |> List.iter (fun x -> file.WriteLine(sprintf "%s - %f" (fst x) (snd x)))
    0