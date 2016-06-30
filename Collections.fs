(*
 * F# Common Library
 * Collections.fs: special collections and containers
 * (C) 2007-2012 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
module FSharp.Common.Collections

open System.Collections.Generic

/// Pure set type supporting absolute complement.
/// For example, it can represent the universe set without actually knowing all elements.
type cset<'a when 'a : comparison> (set, ?is_complemented) =

    let (|Inc|Exc|) (x : cset<_>) =
        let set = x.set
        in
            if x.is_complemented then Exc set
            else Inc set

    member val is_complemented = defaultArg is_complemented false
    member val set = set

    static member complemented (set : Set<'a>) = new cset<'a> (set, true)
    static member universe = cset<'a>.complemented Set.empty
    static member empty = new cset<'a> (Set.empty)

    member private this.apply inc exc = new cset<'a> ((if this.is_complemented then exc else inc) set, this.is_complemented)            
    member this.add x = this.apply (Set.add x) (Set.remove x)
    member this.remove x = this.apply (Set.remove x) (Set.add x)
    member this.contains x = (if this.is_complemented then not else id) (Set.contains x set)
    member this.is_universe = this.is_complemented && Set.isEmpty set
    member this.is_empty = not this.is_complemented && Set.isEmpty set

    member this.complement = new cset<_> (set, not this.is_complemented) 

    static member op_Implicit (set : Set<_>) = new cset<_> (set)

    member a.difference (b : cset<'a>) : cset<'a> =
        match a, b with
        | Inc X, Inc Y -> !> (X - Y)
        | Inc X, Exc Y -> !> (Set.intersect X Y) 
        | Exc X, Exc Y -> !> (Y - X)
        | Exc X, Inc Y -> cset<_>.complemented (X + Y)

    member a.union b =
        match a, b with
        | Inc X, Inc Y -> !> (X + Y)
        | Inc X, Exc Y -> cset<_>.complemented (Y - X)
        | Exc X, Exc Y -> cset<_>.complemented (Set.intersect X Y)
        | Exc X, Inc Y -> cset<_>.complemented (X - Y)

    member a.intersect b =
        match a, b with
        | Inc X, Inc Y -> !> (Set.intersect X Y)
        | Inc X, Exc Y -> !> (X - Y)
        | Exc X, Exc Y -> cset<_>.complemented (X + Y)
        | Exc X, Inc Y -> !> (Y - X)
                        
    static member (-) (a : cset<'a>, b) = a.difference b
    static member (+) (a : cset<'a>, b) = a.union b

    member this.pretty = this.ToString ()
    override this.ToString () = sprintf "%s{ %s }" (if this.is_complemented then "~" else "") (flatten_stringables "; " this.set)



/// Cyclic queue type.
type cyclic_queue<'a> (max_len) =
    inherit Queue<'a> ()
    member this.Enqueue x =
        if this.Count >= max_len then this.Dequeue () |> ignore
        base.Enqueue x