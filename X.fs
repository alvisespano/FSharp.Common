(*
 * F# Common Library
 * X.ml: extensions of F# stdlib types
 * (C) 2007-2012 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
[< RequireQualifiedAccess >]
module FSharp.Common.X

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open Microsoft.FSharp
open FSharp.Common

module Assembly =

    let get_assembly_attribute<'T when 'T :> System.Attribute> (asm : System.Reflection.Assembly) =
        let t = typeof<'T>
        try
            let rex = new Text.RegularExpressions.Regex ("(System.Reflection.Assembly)(\w+)(Attribute)")
            let name = rex.Match(t.FullName).Groups.[2].Value
            let atts = asm.GetCustomAttributes (t, false)
            let att = atts.[0] :?> System.Attribute
            in
                att.GetType().GetProperty(name).GetValue(att, [||]).ToString ()
        with _ -> ""

    let retrieve_linker_timestamp () =
        let filePath = System.Reflection.Assembly.GetCallingAssembly().Location
        let c_PeHeaderOffset = 60
        let c_LinkerTimestampOffset = 8
        let b = Array.create 2048 (byte 0)
        use s = new IO.FileStream (filePath, System.IO.FileMode.Open, System.IO.FileAccess.Read)
        ignore <| s.Read (b, 0, 2048)
        let i = System.BitConverter.ToInt32(b, c_PeHeaderOffset)
        let secondsSince1970 = System.BitConverter.ToInt32 (b, i + c_LinkerTimestampOffset)
        let dt = new DateTime (1970, 1, 1, 0, 0, 0)
        let dt = dt.AddSeconds (float secondsSince1970)
        dt.AddHours (float (let x = TimeZone.CurrentTimeZone.GetUtcOffset dt in x.Hours))
        

module Threading =

    open System.Threading

    /// polymorphic synchronized box accessible via application
    //

    type sync<'a> (x : 'a) = 
        let m = new Mutex ()
        let mutable x = x

        member private __.lock = ignore <| m.WaitOne ()         
        member private __.unlock = m.ReleaseMutex ()
                
        member this.value
          with set x' =
            this.lock
            x <- x'
            this.unlock
        
        member this.apply_and_set f =
            this.lock
            try x <- f x
            with _ -> this.unlock; reraise ()
            this.unlock
        
        member this.set_and_apply f =
            this.lock
            let (x', r) =
                try f x
                with _ -> this.unlock; reraise ()
            x <- x'
            this.unlock
            r
        
        member this.apply f =
            this.lock
            let ret = try f x with _ -> this.unlock; reraise ()
            this.unlock
            ret

    /// refinement of [sync] with the [get] method. Useful for creating quick synchronized mutable fields in objects or records.
    //
    
    type mut<'a> (x) =
        inherit sync<'a> (x)
        member this.value 
            with get () = base.apply identity
            and set x = base.value <- x
      

module Regex =

    let (|Match|_|) regex s =
        let m = Regex.Match (s, regex) in
        if m.Success then Some (List.tail [ for g in m.Groups -> g.Value ]) else None


module Collection =

    /// polymorphic utilities
    //

    let inline exists f l =
        let e = (^t : (member GetEnumerator : unit -> ^e) l)
        let mutable exit = false
        while (not exit && (^e : (member MoveNext : unit -> bool) e)) do
            let x = (^e : (member Current : ^a) e)
            if f x then exit <- true
        exit

    let inline occurs x = exists ((=) x)

    /// NOTE: is constraining ^t to have an Item indexed property (which we don't actually invoke) really needed?
    let inline internal infer_cast sq (s (* : ^t when ^t : (member Item : int -> ^v with get)*)) = 
        let e = (^t : (member GetEnumerator : unit -> ^e) s)
        in
            sq e 


    /// special collection types
    //

    type cyclic_queue<'a> (max_len) =
        inherit Queue<'a> ()
        member this.Enqueue x =
            if this.Count >= max_len then this.Dequeue () |> ignore
            base.Enqueue x
        

module Seq =
 
    open Collection

    let inline infer_cast s =
        infer_cast (fun e ->    
            seq { while (^e : (member MoveNext : unit -> bool) e) do
                      yield (^e : (member Current : obj) e) :?> ^v }) s        


module List =

    open Collection

    /// Split a list on the pivot found by predicate [p]. 
    let rec pivotp p = function
        | x :: xs -> if p x then ([], x, xs) else let (h, c, t) = pivotp p xs in (x :: h, c, t)
        | _       -> raise (ArgumentException ())

    /// Split list [l] on the pivot [x]. An equality operator [eq] can be optinally passed. 
    let pivot x l = let (a, _, b) = pivotp ((=) x) l in (a, b)

    /// Discards elements from list [l] starting from [x]. An equality operator [eq] can be optinally passed. 
    let trim x l = 
        let rec skip = function [] -> [] | x' :: xs as l -> if x = x' then skip xs else l
        in
            List.rev (skip (List.rev (skip l)))

    /// Checks whether [x] occurs in [l]. An equality operator [eq] can be optinally passed. 
    let occurs x l = List.exists (fun x' -> x = x') l

    /// Replaces [x] with [y] in [l]. An equality operator [eq] can be optinally passed. 
    let replace x y l = List.map (fun x' -> if x = x' then y else x) l

    /// Removes all occurences of [x] in [l]. An equality operator [eq] can be optinally passed. 
    let remove_all x l = List.filter (fun x' -> not (x = x')) l

    /// Evaluates the difference between [l1] and [l2]. An equality operator [eq] can be optinally passed. 
    let diff l1 l2 = List.filter (fun x -> not (occurs x l2)) l1

    /// Evaluates the intersection between [l1] and [l2]. An equality operator [eq] can be optinally passed. 
    let intersect l1 l2 = List.filter (fun x -> occurs x l2) l1

    /// Checks whether [x] occurs in [l] and in case it does removes it. An equality operator [eq] can be optinally passed. 
    let occurs_and_remove x l = try let (h, t) = pivot x l in (h @ t, true) with :? ArgumentException -> (l, false)

    /// Removes the first occurence of [x] in [l]. An equality operator [eq] can be optinally passed. 
    let remove x l = fst (occurs_and_remove x l)

    /// Checks whether [x] occurs as first element of the list of pair [l]. An equality operator [eq] can be optinally passed. 
    let occurs_fst x l = List.exists (fun (x', _) -> x = x') l

    /// Checks whether [x] occurs as second element of the list of pair [l]. An equality operator [eq] can be optinally passed. 
    let occurs_snd x l = List.exists (fun (_, x') -> x = x') l

    /// Checks whether [x] occurs as first element of the list of triples [l]. An equality operator [eq] can be optinally passed. 
    let occurs3_fst x l = List.exists (fun (x', _, _) -> x = x') l

    /// Checks whether [x] occurs as second element of the list of triples [l]. An equality operator [eq] can be optinally passed. 
    let occurs3_snd x l = List.exists (fun (_, x', _) -> x = x') l

    /// Checks whether both [x] and [y] occur as first and second element of the list of triples [l]. Equality operators [eq_fst] and [eq_snd] can be optinally passed. 
    let occurs3 x y l = List.exists (fun (x', y', _) -> x = x' && y = y') l

    /// Returns the second element of the pair whose first element is [x] in the list of pairs [l]. An equality operator [eq] can be optinally passed. 
    let find_assoc_fst x l = snd (List.find (fun (x', _) -> x = x') l)
    let try_find_assoc_fst x = trap2 find_assoc_fst x

    /// Returns the first element of the pair whose second element is [x] in the list of pairs [l]. An equality operator [eq] can be optinally passed. 
    let find_assoc_snd x l = fst (List.find (fun (_, x') -> x = x') l)
    let try_find_assoc_snd x = trap2 find_assoc_snd x

    /// Returns the second and third element of the triple whose first element is [x] in the list of triples [l]. An equality operator [eq] can be optinally passed. 
    let find_assoc3_fst x l = match List.find (fun (x', _, _) -> x = x') l with (_, a, b) -> (a, b)
    let try_find_assoc3_fst x = trap2 find_assoc3_fst x

    /// Returns the first and third element of the triple whose second element is [x] in the list of triples [l]. An equality operator [eq] can be optinally passed. 
    let find_assoc3_snd x l = match List.find (fun (_, x', _) -> x = x') l with (a, _, b) -> (a, b)
    let try_find_assoc3_snd x = trap2 find_assoc3_snd x

    /// Returns the third element of the triple whose first and second element are [x] and [y] in the list of triples [l]. Equality operators [eq_fst] and [eq_snd] can be optinally passed. 
    let find_assoc3 x y l =
        match List.find (fun (x', y', _) -> x = x' && y = y') l with (_, _, z) -> z

    /// Returns all third elements of the triples whose first element is [x] in the list of triples [l]. An equality operator [eq] can be optinally passed. 
    let find_assoc3_fst' x l =
        let l = List.filter (fun (x', _, _) -> x = x') l
        in
            List.map (fun (_, _, z) -> z) l
    
    /// Returns all second elements of the triples whose first element is [x] in the list of triples [l]. An equality operator [eq] can be optinally passed. 
    let find_assoc3_snd' x l =
        let l = List.filter (fun (_, x', _) -> x = x') l
        in
            List.map (fun (_, _, z) -> z) l

    /// Maps the second elements of the pairs whose first element is [x] in the list of pairs [l]. An equality operator [eq] can be optinally passed. 
    let map_assoc_fst f x l = List.map (fun ((x', y) as t) -> if x = x' then (x, f y) else t) l

    /// Maps the first elements of the pairs whose second element is [x] in the list of pairs [l]. An equality operator [eq] can be optinally passed. 
    let map_assoc_snd f x l = List.map (fun ((y, x') as t) -> if x = x' then (f y, x) else t) l

    /// Replaces with [y] all the second elements of the pairs whose first element is [x] in the list of pairs [l]. An equality operator [eq] can be optinally passed. 
    let replace_assoc_fst x y l = List.map (fun ((x', _) as t) -> if x = x' then (x, y) else t) l

    /// Replaces with [y] all the first elements of the pairs whose second element is [x] in the list of pairs [l]. An equality operator [eq] can be optinally passed. 
    let replace_assoc_snd x y l = List.map (fun ((_, x') as t) -> if x = x' then (y, x) else t) l

    /// Replaces with [f z] all third elements of the triples whose first and second elements are [x] and [y]. Equality operators [eq_fst] and [eq_snd] can be optinally passed. 
    let replace_assoc3 f x y l =
        List.map (fun ((x', y', z) as t) -> if x = x' && y = y' then (x, y, f z) else t) l

    /// Replaces with [z] all the third elements of the triples whose first and second elements are [x] and [y]. An equality operator [eq] can be optinally passed. 
    let replace_assoc3_fst x y z = replace_assoc3 (fun _ -> z) x y

    /// Removes from the list of pairs [l] all pairs whose first element is [x]. An equality operator [eq] can be optinally passed. 
    let remove_assoc_fst x l = List.filter (fun (x', _) -> not (x = x')) l

    /// Removes from the list of pairs [l] all pairs whose second element is [x]. An equality operator [eq] can be optinally passed. 
    let remove_assoc_snd x l = List.filter (fun (_, x') -> not (x = x')) l

    /// Removes from the list of triples [l] all triples whose first element is [x]. An equality operator [eq] can be optinally passed. 
    let remove_assoc3_fst x l = List.filter (fun (x', _, _) -> not (x = x')) l

    /// Removes from the list of triples [l] all triples whose second element is [x]. An equality operator [eq] can be optinally passed. 
    let remove_assoc3_snd x l = List.filter (fun (_, x', _) -> not (x = x')) l

    /// Removes from the list of triples [l] all triples whose first element is [x] and second element is [y]. Equality operators [eq_fst] and [eq_snd] can be optinally passed. 
    let remove_assoc3 x y l = List.filter (fun (x', y', _) -> not (x = x' && y = y')) l

    /// Returns the second element of the list of pairs [l] given a predicate [p] on the first element. 
    let find_assocp p l = snd (List.find (fun (x, _) -> p x) l)

            
    /// Creates a list given a function [f], to which the increasing counter [i] is passed, that returns [None] when for ending the list creation
    /// or [Some x] for storing [x] into the list at the current position.    
    let tabo f =
        let rec g l i =
            match f i with
                None   -> l
              | Some x -> g (x :: l) (i + 1)
        in
            List.rev (g [] 0)
                
    /// Creates a list of length [n] given a function [f], to which the increasing counter [i] is passed.    
    let tab f n = tabo (fun i -> if i < n then Some (f i) else None)
        
    /// [List.map] variation with index. 
    let mapi f l = List.rev (fst (List.fold (fun (l', i) x -> (f i x :: l', i + 1)) ([], 0) l))

    /// [List.iter] variation with index. 
    let iteri f l = let i = ref 0 in List.iter (fun x -> f i x; incr i) l

    /// [List.fold_left] variation with index. 
    let foldi f z l = snd (List.fold (fun (i, z) x -> (i + 1, f z x i)) (0, z) l)

    /// Remove element [i]-th from list [l]. 
    let remi i l = 
        if (List.length l) <= i then l else
        let rec remi' cur acc l =
            match (cur, l) with
                (x, _ :: l) when x = i    -> (List.rev acc) @ l
              | (x, elt :: l) when x <> i -> remi' (x + 1) (elt :: acc) l
              | _                         -> raise (Unexpected "remi: index less than zero?")
        in
            remi' 0 [] l

    /// Seek list [l] for [x] and returns its position. 
    let index x l =
        let rec f i = function
            []       -> raise (ArgumentException ())
          | x' :: xs -> if x = x' then i else f (i + 1) xs
        in
            f 0 l

    /// Extracts the first [n] elements from a list and returns both them and the remainder. 
    let rec heads_with_tail n = function
        | [] -> ([], [])
        | x :: xs as l ->
            if n = 0 then ([], l)
            else let (h, t) = heads_with_tail (n - 1) xs in (x :: h, t)
        
    /// Extracts the first [n] elements from a list. 
    let heads n l = fst (heads_with_tail n l)

    /// Extracts the first [n] elements from a list and returns the remainder. 
    let tail n l =
        let len = List.length l
        in
            snd (heads_with_tail (len - (crop (0, len) n)) l)

    let cast l = Seq.cast l |> Seq.toList

    let inline infer_cast s =
        infer_cast (fun e ->
            [ while (^e : (member MoveNext : unit -> bool) e) do
                      yield (^e : (member Current : obj) e) :?> ^v ]) s
    

    /// Finds an element in list [l] according to the predicate [p]; if none is found, returns the best element according to the comparison
    /// function [is_better] over the type returned by the projection function [eval].
    
    type find_or_best_result<'a, 'b> = Best of 'a * 'b | Found of 'a * 'b | Empty

    let find_or_best p f is_better l =
        let rec R (best, bestv) = function
            | []      -> Best (best, bestv)
            | x :: xs -> match p x with
                            | Some y -> Found (x, y)
                            | None   -> let v = f x in R (if is_better v bestv then (x, v) else (best, bestv)) xs                          
        in
            match l with
                | []              -> Empty
                | best :: _ as l  -> R (best, f best) l
             
    /// Picks up an random element from a list of (float, 'a), according to the weight designated by the first element 
    let weight_assoc def l =
        let l = List.filter (fun (w, _) -> w > 0. && w <= 1.) l
        let (W, l) = List.fold (fun (W, l) (w, x) -> let W = W + w in (W, (W, x) :: l)) (0., []) l
        if W > 1. then invalidArg "l" "probability distribution is higher than 1.0"
        let r = (new Random ()).NextDouble ()
        in
            try find_assocp (fun p -> r <= p) (List.rev l)
            with :? KeyNotFoundException -> def
  
    