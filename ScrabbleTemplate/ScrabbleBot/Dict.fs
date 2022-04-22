module internal Dictionary

    open System

    type Dictionary = Node of (Map<char, Dictionary>*bool)
    
    let empty (u:unit) = Node (Map.empty, false)

    let insert (s: string) dict : Dictionary =
        let rec aux (Node (m, b)) = function
            | [] -> Node (m, true)
            | c::tail ->
                match Map.tryFind c m with
                | None ->
                    let child = aux (empty ()) tail
                    Node (Map.add c child m, b)
                | Some child ->
                    let nchild = aux child tail
                    Node (Map.add c nchild m, b)            
        aux dict (List.ofSeq s)
        
    let rec lookup (s : string) (dict : Dictionary) = 
        match dict, s with
        |Node (_, b), "" ->  b
        |Node (m, _), _ ->  if Map.containsKey s.[0] m then if s.Length = 1 then lookup "" m.[s.[0]] 
                                                            else lookup s.[1..] m.[s.[0]]
                            else false
    
    let step (c: char) (Node(child, _)) =
    match Map.tryFind c child with
    | None -> None
    | Some (Node(child, word)) -> Some (word, Node(child, word))