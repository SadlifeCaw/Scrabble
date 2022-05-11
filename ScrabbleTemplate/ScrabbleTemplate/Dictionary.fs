namespace Dictionary
module internal Dictionary =

    open System

    type Dictionary = Node of (Map<char, Dictionary>*bool)
    
    let empty (u:unit) = Node (Map.empty, false)

    let insert (s: String) dict : Dictionary =
        let cl = Seq.toList s
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
        aux dict cl
    
    (*
    IF WE WANT A GADACT THEN:
    
    let esc = '#'
    
    let gadact (s: string) =
        let rec aux acc pre = function
            |[] -> pre::acc
            |c::tail -> aux ((c::pre@tail)::acc) (c::pre) tail
        aux [] [esc] (List.ofSeq s)
    
    let insert (s : string) (dict : Dictionary) : Dictionary = gadact s |> List.fold (fun acc li -> insertToDict li acc) dict
     *)
    let step (c: char) (Node(child, _)) =
        match Map.tryFind c child with
        | None -> None
        | Some (Node(child, word)) -> Some (word, Node(child, word))
    
    //TODO: MAKE THIS A TRIE INSTEAD