namespace SortingAlgorithms

module Queue = 
    let private decompose collection = 
        List.head collection, List.tail collection

    let private (++) collection0 collection1 = 
        [collection0; collection1] |> List.toSeq |> List.concat

    type queue<'a> = 
        | Queue of 'a list * 'a list 

    let empty = Queue([], [])

    let push element = function
    | Queue(fs, ss) -> Queue(element :: fs, ss)

    let pop = function
    | Queue([], []) -> failwith "Queue is empty."
    | Queue(fs, s::ss) -> s, Queue(fs, ss)
    | Queue(fs, []) -> let s, ss = fs |> List.rev |> decompose in s, Queue([], ss)

    let toList = function
    | Queue(fs, ss) -> ss ++ List.rev fs