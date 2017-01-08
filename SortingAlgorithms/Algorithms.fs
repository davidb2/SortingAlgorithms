namespace SortingAlgorithms

open Queue

module List = 
    let private (++) collection0 collection1 = 
        [collection0; collection1] |> List.toSeq |> List.concat

    let private maxi collection = 
        collection |> List.findIndex ((=) (List.max collection))

    let private replace element i collection =  
        let rec repl element i n collection =
            match i, n, collection with
            | _, _, [] -> [element]
            | ip, np, x::xs when ip = np -> element :: xs
            | ip, np, x::xs -> x :: (xs |> repl element (i+1) n)
        collection |> repl element 0 i

    let private swap i j collection = 
        let a, b = collection |> List.item i, collection |> List.item j
        collection |> replace a j |> replace b i

    let private decompose collection = 
        List.head collection, List.tail collection

    let rec private sinsert element collection =
        match element, collection with
        | x, [] -> [x]
        | x, y::ys when x < y -> x::y::ys
        | x, y::ys -> y :: (ys |> sinsert x)
    
    let bubbleSort collection = 
        let rec bsort acc collection =
            match collection, acc with
            | [], ys       -> ys
            | [x], ys      -> List.rev (x :: ys)
            | x::y::xs, ys -> let a, b = min x y, max x y in bsort (a :: ys) (b :: xs)
        let rec iter i n collection = 
            match i, n with
            | x, y when x = y -> collection
            | _, _            -> collection |> bsort [] |> iter (i+1) n
        collection |> iter 0 (List.length collection)
       
    let selectionSort collection =
        let rec ssort acc collection = 
            match collection, acc with
            | [], zs -> zs
            | xs, zs -> let y, ys = xs |> swap 0 (maxi xs) |> decompose in ssort (y :: acc) ys
        collection |> ssort []

    let insertionSort collection = 
        let rec isort acc collection = 
            match collection, acc with
            | [], _ -> acc
            | x::xs, ys -> xs |> isort (sinsert x ys)
        collection |> isort []

    let radixSort collection =
        let extractElementsFromBucket bucket = 
            bucket 
            |> Map.toList
            |> List.map (fun (d, q) -> Queue.toList q)
            |> List.reduce (++)
        let rec addElementsToBucket collection k bucket = 
            match collection with
            | [] -> extractElementsFromBucket bucket
            | x::xs -> 
                let rem = (abs (x % k)) / (k/10) in 
                    bucket 
                    |> Map.add rem (bucket.[rem] |> Queue.push x)
                    |> addElementsToBucket xs k
        let rec rsort i k collection = 
            let emptyBucket = 
                [0..9] 
                |> List.map (fun x -> x, Queue.empty) 
                |> Map.ofList
            match i, k with
            | x, y when x > y -> collection
            | _, _ -> 
                emptyBucket 
                |> addElementsToBucket collection (pown 10 i) 
                |> rsort (i+1) k
        let negatives, positives = List.partition ((>) 0) collection
        let nk = 
            if negatives = List.empty then 1 
            else 
                let i = negatives |> List.min in
                    if i = 0 then 1 
                    else i |> abs |> float |> log10 |> ceil |> int
        let pk = 
            if positives = List.empty then 1 
            else 
                let i = positives |> List.max in
                    if i = 0 then 1 
                    else i |> abs |> float |> log10 |> ceil |> int
        (rsort 1 nk negatives |> List.rev) ++ (rsort 1 pk positives)

    let rec quickSort = function
    | [] -> []
    | x::xs -> 
        let left, right = List.partition ((>) x) xs in 
            (quickSort left) ++ (x :: quickSort right)

    let mergeSort collection = 
        let rec merge acc collection0 collection1 =
            match collection0, collection1 with
            | [], [] -> List.rev acc
            | [], y::ys -> ([], ys) ||> merge (y :: acc)
            | x::xs, [] -> (xs, []) ||> merge (x :: acc)
            | x::xs, y::ys -> 
                if x < y then (xs, y :: ys) ||> merge (x :: acc)
                else (x :: xs, ys) ||> merge (y :: acc)
        let rec msort = function
        | [] -> []
        | [x] -> [x]
        | [x; y] -> let a, b = min x y, max x y in [a; b]
        | xs -> 
            let left, right = xs |> List.splitAt ((List.length xs) / 2) in 
                ((msort left), (msort right)) ||> merge []
        msort collection