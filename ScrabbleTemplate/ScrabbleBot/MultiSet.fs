// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> =  Map<'a,uint32> // replace with your type
    let empty : MultiSet<'a> = Map.empty

    let isEmpty (s : MultiSet<'a>) = s.IsEmpty

    let size (s : MultiSet<'a>) = Map.fold (fun sum _ y -> y + sum) 0u s
    
    let rec contains (a : 'a) (s : MultiSet<'a>) = 
         match Map.tryFind a s with
         | Some _ -> true
         | None -> false

    let numItems (a : 'a) (s : MultiSet<'a>) =
         match Map.tryFind a s with
           |Some a -> a
           |None -> 0u

    let add (a : 'a) (n : uint32) (s : MultiSet<'a>) : MultiSet<'a> =  
     match Map.tryFind a s with
      | Some x -> Map.add a (x+n) s
      | None -> Map.add a n s

    let addSingle (a : 'a) (s : MultiSet<'a>) : MultiSet<'a> = Map.add a (uint32 1) s
    
    let remove (a : 'a) (n : uint32) (s  : MultiSet<'a>) : MultiSet<'a> = 
          match Map.tryFind a s with
            | Some x -> if x <= n then Map.remove a s else Map.add a (x-n) s 
            | None -> s

    let removeSingle (a : 'a) (s : MultiSet<'a>) : MultiSet<'a>= 
      match Map.tryFind a s with
      | Some x -> if x = 1u then Map.remove a s else Map.add a (x-1u) s 
      | None -> s


    let fold (f : 'a -> 'b -> uint32 -> 'a) (acc : 'a) (s : MultiSet<'b>) =  Map.fold f acc s 

    
    let foldBack (f : 'a -> uint32 -> 'b -> 'b) (s : MultiSet<'a>) (acc : 'b) = Map.foldBack f s acc
   
    let ofList (_ : 'a list) : MultiSet<'a> = empty
    let toList (_ : MultiSet<'a>) : 'a list = []

    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = empty

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty