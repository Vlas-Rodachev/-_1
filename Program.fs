open System

let rec vvod_znach_f (mes:string) = 
    printf "%s: " mes
    let str = Console.ReadLine()
    match Double.TryParse(str) with
    | (true, value) -> Some value
    | _ -> vvod_znach_f (mes)

let rec vvod_znach_i (mes:string) = 
    printf "%s: " mes
    let str = Console.ReadLine()
    match Int32.TryParse(str) with
    | (true, value) -> Some value
    | _ -> vvod_znach_i(mes)

let print_fun (p:int) mes = 
    if p = 1 then 
        printf $"{mes} "
    else 
        printfn $"{mes} "


(*// задание 1
let x = vvod_znach_i("Введите количество вводимых чисел").Value
let list1 = [for i in 1 .. x -> if (vvod_znach_i("Введите значение").Value % 2) = 1 then true
                                else false]

printf "%A" list1*)


(*// задание 2
let x = vvod_znach_i("Введите число").Value

let rec fu n list=
    if n % 10 <> 0 then
        let list1 = n % 10 :: list
        fu (n / 10) list1
    else 
        list

let list3 = []
let list2 = fu x []
printfn "%A" list2*)


//задание 3
// добавление элемента в начало списка 
let add_el x l = 
    let l2 = x :: l
    l2

// удаление элемента по индексу
let rec del_el i l =
    match i, l with
    | 0, x::xs -> xs
    | i, x::xs -> x::del_el (i - 1) xs
    | i, [] -> []

    // поиск элемента, возвращает индекс
let rec find_el i l =
    match l with
    | x::xs -> 
        if x = i then 
            0
        else 
            1 + find_el i xs
    | [] -> 0

    //соединение списков
let scep_two_list l1 l2 = 
    let l3 = l1 @ l2
    l3

    // возвращает значение по индексу
let rec select_el i l =
    match i, l with
    | 0, x::xs -> x
    | i, x::xs -> select_el (i - 1) xs
    | i, [] -> -1
                                                                                    
       
print_fun 1 "Начальный список:"
let list4 = [1; 2; 4; 5]
let list5 = add_el 3 list4
printfn "%A" list4
print_fun 1 "Добавили значение в начало:"
printfn "%A" list5
let list6 = del_el 4 list5
print_fun 1 "Удаление элемента:"
printfn "%A" list6

print_fun 1 "поиск элемента 3 (его индекс):"
printfn "%d" (find_el 3 list6)

print_fun 1 "соединение списков:"
let list7 = scep_two_list list6 list5
printfn "%A" list7

print_fun 1 "получение элемента по индексу (1):"
printfn "%d" (select_el 1 list7)