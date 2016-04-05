open System

type Tree =
    | Tip
    | Node of int * Tree * Tree

let rec sumTree tree =
    match tree with
    | Tip -> 0
    | Node(value, left, right) ->
        value + sumTree(left) + sumTree(right)

let rec mirrorEquals 

let myTree = Node(0, Node(1, Node(2, Tip, Tip), Node(3, Tip, Tip)), Node(4, Tip, Tip))
let resultSumTree = sumTree myTree

let main() =
    Console.WriteLine(resultSumTree)
main()



boolean mirrorEquals(BTree left, BTree right) {
  if (left == null || right == null) return left == null && right == null;
  return left.value == right.value && mirrorEquals(left.left, right.right) && mirrorEquals(left.right, right.left);
}