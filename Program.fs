open System

let rec vvod_znach_f (mes:string) = 
    printf "%s: " mes  // вывод сообщения для контекста вводимой информации
    let str = Console.ReadLine()  // ввод строки
    match Double.TryParse(str) with  // проверка на то является ли строка числом
    | (true, value) -> Some value  // если да то возвращается значение типа float option
    | _ ->     // иначе заново просим ввести значение
        printfn "Неверный ввод!!!!!"
        vvod_znach_f (mes)

let rec vvod_znach_i (mes:string) = 
    printf "%s: " mes  // вывод сообщения для контекста вводимой информации
    let str = Console.ReadLine()  // ввод строки
    match Int32.TryParse(str) with  // проверка на то является ли строка числом
    | (true, value) -> Some value  // если да то возвращается значение типа int option
    | _ ->   // иначе заново просим ввести значение
        printfn "Неверный ввод!!!!!"
        vvod_znach_i(mes)

let print_fun (p:int) mes =   // функция вывода строки или значения, если первый параметр 1, то вывод без переноса строки, иначе с переносом 
    if p = 1 then 
        printf $"{mes} "
    else 
        printfn $"{mes} "


let zap_spisok () = 
    let n = vvod_znach_i("Введите количество элементов").Value
    let sp = [for i in 1..n do vvod_znach_i("Введите значение").Value]  
    sp



// задание 1
let fu1 () = 
    let x = vvod_znach_i("Введите количество вводимых чисел").Value
    let list1 = [for i in 1 .. x -> if (vvod_znach_i("Введите значение").Value % 2) = 1 then true
                                    else false]
    printf "%A" list1


// задание 2

let rec fu n list=
    if n % 10 <> 0 then
        let list1 = n % 10 :: list
        fu (n / 10) list1
    else 
        list

let fu2 () = 
    let x = vvod_znach_i("Введите число").Value
    let list2 = fu x []
    printfn "%A" list2


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

let rec fu3 list4 = 
    print_fun 1 "Данный список: "
    printfn "%A" list4
    print_fun 2 "1 - добавление элемента в начало списка"
    print_fun 2 "2 - удаление элемента по индексу"
    print_fun 2 "3 - поиск элемента, возвращает индекс"
    print_fun 2 "4 - соединение списков"
    print_fun 2 "5 - возвращает значение по индексу"
    print_fun 2 "0 - Выход"
    match vvod_znach_i("Ведите номер функции").Value with
    | 1 -> 
        let x = vvod_znach_i("Ведите значение добавляемого элемента").Value
        let list5 = add_el x list4
        printfn "%A" list5
        print_fun 2 ""
        fu3 list4
    | 2 -> 
        let list5 = del_el (vvod_znach_i("Ведите индекс удаляемого элемента").Value) list4
        printfn "%A" list5
        print_fun 2 ""
        fu3 list4
    | 3 -> 
        let list5 = find_el (vvod_znach_i("Ведите значение искомого элемента").Value) list4
        printfn $"{list5}"
        print_fun 2 ""
        fu3 list4
    | 4 -> 
        let list5 = scep_two_list list4 list4
        printfn "%A" list5
        print_fun 2 ""
        fu3 list4
    | 5 -> 
        let list5 = select_el (vvod_znach_i("Ведите индекс элемента").Value) list4
        printfn $"{list5}"
        print_fun 2 ""
        fu3 list4
    | 0 -> ()

                                                                                    
       
let rec fuuuuuu () = 
    print_fun 2 "1 - Сформировать список из значений true и false"
    print_fun 2 "2 - Сформировать список из цифр числа"
    print_fun 2 "3 - Создайте собственные функции для выполнения основных операций над списками"
    print_fun 2 "0 - Выход"
    match vvod_znach_i("Ведите номер задания").Value with
    | 1 -> 
        fu1()
        print_fun 2 ""
        print_fun 2 ""
        fuuuuuu ()
    | 2 -> 
        fu2()
        print_fun 2 ""
        print_fun 2 ""
        fuuuuuu ()
    | 3 -> 
        let list4 = zap_spisok ()
        fu3 list4
        print_fun 2 ""
        print_fun 2 ""
        fuuuuuu ()
    | 0 -> ()
    | _ -> 
        print_fun 2 ""
        print_fun 2 ""
        fuuuuuu ()
    
fuuuuuu ()
