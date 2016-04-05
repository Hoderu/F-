open System
open System.IO

let originalText = File.ReadAllText @"/Users/Hoderu/f#/1.txt"
let vocabluary = File.ReadAllText @"/Users/Hoderu/f#/123.txt"

let searchNames (sentence : String) index (names : Map<string, Set<int>>) : Map<string, Set<int>> =  
    let words = sentence.Split([|' '; ','; ';'; '"'|])
    let rec loop (namesTemp : Map<string, Set<int>>) i =
        let sep = [|'.'; '!'; '?'; ' '; char(13); char(10); ')'; '('; ','; ';'; '"'; '\''|]
        match i with
        | a when a = words.Length -> namesTemp
        | _ -> words.[i].Trim(sep) |> (fun word -> if word.Length > 1 && Char.IsUpper(word.[0]) then
                                                        let w = if word.[word.Length - 2] = '\'' && word.[word.Length - 1] = 's' then word.Remove(word.Length - 2) else word
                                                        if namesTemp.ContainsKey(w) then loop (namesTemp.Add(w, namesTemp.[w].Add(index))) (i + 1) 
                                                        else loop (namesTemp.Add(w, Set.empty.Add(index))) (i + 1)
                                                   else loop namesTemp (i + 1))
    loop names 0

type relation = string * string * int                            

let main_function (text : String) : relation list =
    let sentences = text.Split([|'.'; '!'; '?'|]);
    let rec loop (names : Map<string, Set<int>>) index : Map<string, Set<int>> =
        if index < sentences.Length then loop (searchNames (sentences.[index].Trim([|'.'; '!'; '?'; ' '|])) index names) (index + 1)
        else names
    let map = loop Map.empty 0

    let commonWords = ["An";"The";"He";"She";"It";"We";"They";"If";"In";"One";"Dr";"Who";"What";"And";"Many";"But";"That";"There";"With";"When";"You";"On";"Own";"World";"Valley";"Some";"So";"Not";"North";"South";"River";"Lake";"Forest";"Great";"Mr";"As";"East";"King";"Then";"Anyway";"Approach";"April";"Battle";"Birds";"Yet";"Never";"Master";"At";"New";"Durings";"Winter";"Autumn";"Year";"Day";"Mountain";"Lonely";"Holiday";"Hobbits";"Back";"Bag-End";"Esquire";"Can";"By";"Just";"June";"After";"Again";"April-and";"All";"Already";"Actually";"Across";"About";"Be";"The";"Of";"Most";"Next";"Soon";"Wild";]

    let dictionary = vocabluary.Split([|' '; ','; ';'; '"'|]) |> Array.toList
    
    let filteredMap = Map.filter (fun word _ -> not (List.exists (fun elem -> elem = word) dictionary)) map
    
    let listOfRelations = [ for n1 in filteredMap do
                                for n2 in filteredMap do
                                    let c = Set.count(Set.intersect n1.Value n2.Value)
                                    if n1.Key < n2.Key && c > 0 then yield (n1.Key, n2.Key, c) ]

    let sortFunction (r1 : relation) (r2 : relation) = 
        let third (_, _, c) = c
        if third r1 > third r2 then -1 else 1

    List.sortWith sortFunction listOfRelations

for s in (main_function originalText) do 
    printfn "%s" (s.ToString())