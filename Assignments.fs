module Assignments

// Assignment 1
let rec count (xs: List<int>) (x: int) =
    match xs with
    | [] -> 0
    | h :: t when h < x -> count t x
    | h :: t when h = x -> 1 + (count t x)
    | _ -> 0

// Assignment 2
let rec insert (xs: List<int>) (x: int) =
    match xs with
    | [] -> [ x ]
    | h :: t when x <= h -> [ x ] @ xs
    | h :: t when x > h -> [ h ] @ insert t x
    | _ -> failwith "insert failed!"

// Assignment 3
let rec intersect (xs: List<int>) (xs': List<int>) =
    match xs, xs' with
    | [], _
    | _, [] -> []
    | ha :: ta, hb :: tb when ha = hb -> [ ha ] @ intersect ta tb
    | ha :: ta, hb :: _ when ha < hb -> intersect ta xs'
    | ha :: _, hb :: tb when ha > hb -> intersect xs tb
    | _ -> failwith "intersect failed!"

// Assignment 4
let rec plus (xs: List<int>) (xs': List<int>) =
    match xs, xs' with
    | [], _ -> xs @ xs'
    | _, [] -> xs' @ xs
    | ha :: ta, hb :: _ when ha = hb -> [ ha ] @ plus ta xs'
    | ha :: _, hb :: tb when ha > hb -> [ hb ] @ plus xs tb
    | ha :: ta, hb :: _ when ha < hb -> [ ha ] @ plus ta xs'
    | _ -> failwith "plus failed!"

// Assignment 5
// [1;1;2;3] [1;1;1;2;2] = []
// [1;2;3] [1;1;2;2] = []
// [2;3] [1;2;2] = []
// [2;3] [2;2] = []
// [3] [2] = []
// [3] [] = []
// [] [] = [3]

// [1;1;1;2;2] [1;1;2;3] = []
// [1;1;2;2] [1;2;3] = []
// [1;2;2] [2;3] = []
// [2;2] [2;3] = [1]
// [2] [3] = [1]
// [] [3] = [1;2]
let rec minus (xs: List<int>) (xs': List<int>) =
    match xs, xs' with
    | [], _
    | _, [] -> xs
    | ha :: ta, hb :: tb when ha = hb -> minus ta tb
    | ha :: _, hb :: tb when ha > hb -> minus xs tb
    | ha :: ta, hb :: _ when ha < hb -> [ ha ] @ minus ta xs'
    | _ -> failwith "minus failed!"
