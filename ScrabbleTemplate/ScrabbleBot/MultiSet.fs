// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

   type MultiSet<'a when 'a : comparison> = Map<'a, uint32> 

   let empty : MultiSet<'a> = Map.empty

   let isEmpty (map: MultiSet<'a>) = map.IsEmpty

   let size (s: MultiSet<'a>) : uint32 = Map.fold (fun x _ v -> x + v) 0u s

   let contains a (s : MultiSet<'a>) = Map.exists (fun k _ ->  k = a) s

   let numItems a (s : MultiSet<'a>) = if contains a s then s.[a] else 0u

   let add a (n : uint32) (s : MultiSet<'a>) : MultiSet<'a> = s |> Map.add a ((numItems a s) + n)

   let addSingle a (s : MultiSet<'a>) = s |> Map.add a ((numItems a s) + 1u)

   let remove a n (s : MultiSet<'a>) : MultiSet<'a> = 
       let value = numItems a s
       if value >= n then Map.add a (value - n) s else Map.remove a s
    
   let removeSingle a (s : MultiSet<'a>) = remove a 1u s

   let fold f acc (multiset : MultiSet<'b>) = Map.fold f acc multiset

   let foldBack f (multiset : MultiSet<'b>) acc = Map.foldBack f multiset acc

   let ofList lst = List.fold(fun acc element -> addSingle element acc) empty lst
   let toList (s:MultiSet<'a>) : 'a list = fold (fun acc key value -> acc @ (List.init (int32 value) (fun _ -> key))) List.empty s
   let map f (s:MultiSet<'a>) : MultiSet<'b> = fold (fun acc key value -> add (f key) value acc) empty s
   let union (s1:MultiSet<'a>) (s2:MultiSet<'a>) = fold (fun acc key value -> if contains key acc && numItems key acc > value then acc else Map.add key value acc) s2 s1
   let sum (s1:MultiSet<'a>) (s2:MultiSet<'a>) = fold (fun acc key value -> add key value acc) s2 s1
   let subtract (s1:MultiSet<'a>) (s2:MultiSet<'a>) = fold (fun acc key value -> remove key value acc) s1 s2
   let intersection (s1:MultiSet<'a>) (s2:MultiSet<'a>) = fold (fun acc key value -> if contains key acc && numItems key acc > value then Map.add key value acc elif contains key acc && numItems key acc < value then Map.add key (numItems key acc) acc else acc) s2 s1