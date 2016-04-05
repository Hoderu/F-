open System

let artASCII = 
    [
    ['@';'.';'.';'@';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.']
    ['.';'.';'@';'.';'.';'@';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.']
    ['.';'.';'.';'.';'@';'.';'.';'@';'.';'.';'@';'@';'@';'@';'@';'@';'@';'@';'@';'@';'@';'@']
    ['.';'.';'.';'.';'.';'.';'@';'.';'.';'@';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.']
    ['.';'.';'.';'.';'.';'.';'.';'.';'@';'.';'.';'@';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.']
    ['.';'.';'.';'.';'.';'.';'@';'.';'.';'@';'.';'.';'.';'@';'.';'.';'@';'@';'@';'@';'@';'@']
    ['.';'.';'.';'.';'@';'.';'.';'@';'.';'.';'.';'.';'.';'.';'.';'@';'.';'.';'.';'.';'.';'.']
    ['.';'.';'@';'.';'.';'@';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'@';'.';'.';'.';'.']
    ['@';'.';'.';'@';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'.';'@';'.';'.']
    ]

let println line =
    List.iter (fun x -> printf "%c" x) line
    printf "\n"

let print im = 
    List.iter (fun x -> println x) im

let horizontalReverse im =
    List.rev im

let verticalReverse im =
    List.map List.rev im

let turn180 im =
    horizontalReverse(verticalReverse im)

let makeColumn im n =
    let rec make im n acc = 
        if (List.length acc) >= (List.length im) then acc
        else make im n ( (List.nth (List.nth im (List.length acc)) n) :: acc)
    make im n []

let turnLeft90 im =
    let rec collect im acc =
        if List.length acc >= List.length (List.head im) then acc
        else collect im ((makeColumn im (List.length acc)) :: acc)
    verticalReverse(collect im [])

let turnRight90 im =
    turn180(turnLeft90 im)

let main() =
    printf "initial:\n"
    print artASCII

    printf "\nverticalReverse:\n"
    print (verticalReverse artASCII)
    
    printf "\nhorisontalReverse:\n"
    print (horizontalReverse artASCII)

    printf "\nturnRight90:\n"
    print (turnRight90 artASCII)

    printf "\nturnLeft90:\n"
    print (turnLeft90 artASCII)

    printf "\nturn180:\n"
    print (turn180 artASCII)

main()