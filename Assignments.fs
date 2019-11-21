module Assignments

// Assignment 1
let rec count numList key =
    match numList with
    | [] -> 0
    | h :: t when h < key -> count t key
    | h :: t when h = key -> 1 + (count t key)
    | _ -> 0

// Assignment 2
let rec insert numList key =
    match numList with
    | [] -> [ key ]
    | h :: t when key <= h -> [ key ] @ numList
    | h :: t when key > h -> [ h ] @ insert t key
    | _ -> failwith "insert failed!"

// Assignment 3
let rec intersect listA listB =
    match listA, listB with
    | [], _
    | _, [] -> []
    | ha :: ta, hb :: tb when ha = hb -> [ ha ] @ intersect ta tb
    | ha :: ta, hb :: _ when ha < hb -> intersect ta listB
    | ha :: _, hb :: tb when ha > hb -> intersect listA tb
    | _ -> failwith "intersect failed!"

// Assignment 4
let rec plus listA listB =
    match listA, listB with
    | [], _ -> listA @ listB
    | _, [] -> listB @ listA
    | ha :: ta, hb :: tb when ha = hb -> [ ha ] @ plus ta listB
    | ha :: ta, hb :: tb when ha > hb -> [ hb ] @ plus listA tb
    | ha :: ta, hb :: tb when ha < hb -> [ ha ] @ plus ta listB
    | _ -> failwith "plus failed!"

// Assignment 5
let rec minus listA listB =
    match listA, listB with
    | [], _ -> []
    | _, [] -> listA
    | ha :: ta, hb :: tb when ha = hb -> minus ta tb
    | ha :: ta, hb :: tb when ha > hb -> minus listA tb
    | ha :: ta, hb :: tb when ha < hb -> [ ha ] @ minus ta listB
    | _ -> failwith "minus failed!"
