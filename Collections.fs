﻿(*
 * F# Common Library
 * Collections.fs: special collections and containers
 * (C) 2007-2016 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
module FSharp.Common.Collections

open System.Collections.Generic

/// Pure set type supporting absolute complement.
/// For example, it can represent the universe set as a complemented empty set, without actually knowing all elements.
[< System.Diagnostics.DebuggerDisplay("{ToString()}") >]
type cset<'a when 'a : comparison> (set : 'a Set, ?is_complemented) =

    let (|Inc|Exc|) (x : _ cset) =
        let set = x.set
        in
            if x.is_complemented then Exc set
            else Inc set

    new (sq : 'a seq, ?is_complemented) = cset (Set sq, ?is_complemented = is_complemented)

    member val is_complemented = defaultArg is_complemented false
    member val private set = set
    member this.get = this.set, this.is_complemented

    override x.Equals yobj =
        match yobj with
        | :? cset<'a> as y -> x.is_complemented = y.is_complemented && x.set = y.set
        | _ -> false
 
    override x.GetHashCode () = hash (x.is_complemented, x.set)
 
    interface System.IComparable with
      member x.CompareTo yobj =
          match yobj with
          | :? cset<'a> as y -> if x = y then 0 elif x.intersect y = x then -1 else 1
          | _ -> invalidArg "yobj" "cannot compare values of different types"

    static member complemented (set : 'a Set) = cset (set, true)
    static member universe = cset<'a>.complemented Set.empty
    static member empty = cset Set.empty
    static member singleton x = cset <| Set.singleton x

    member private this.apply inc exc = new cset<'a> ((if this.is_complemented then exc else inc) set, this.is_complemented)            
    member this.add x = this.apply (Set.add x) (Set.remove x)
    member this.remove x = this.apply (Set.remove x) (Set.add x)
    member this.contains x = (if this.is_complemented then not else id) (Set.contains x set)
    member this.is_universe = this.is_complemented && Set.isEmpty set
    member this.is_empty = not this.is_complemented && Set.isEmpty set
    member this.complement = cset (set, not this.is_complemented) 
    member this.size = match this with Inc set -> Set.count set | Exc _ -> System.Int32.MaxValue

    static member op_Implicit (set : _ Set) = cset set

    member a.difference (b : 'a cset) =
        match a, b with
        | Inc X, Inc Y -> !> (X - Y)
        | Inc X, Exc Y -> !> (Set.intersect X Y) 
        | Exc X, Exc Y -> !> (Y - X)
        | Exc X, Inc Y -> cset<_>.complemented (X + Y)

    member a.union b =
        match a, b with
        | Inc X, Inc Y -> !> (X + Y)
        | Exc Y, Inc X // commutative case
        | Inc X, Exc Y -> cset<_>.complemented (Y - X)
        | Exc X, Exc Y -> cset<_>.complemented (Set.intersect X Y)
        
    member a.intersect b =
        match a, b with
        | Inc X, Inc Y -> !> (Set.intersect X Y)
        | Exc Y, Inc X // commutative case
        | Inc X, Exc Y -> !> (X - Y)
        | Exc X, Exc Y -> cset<_>.complemented (X + Y)
                        
    static member (-) (a : 'a cset, b) = a.difference b
    static member (+) (a : 'a cset, b) = a.union b

    member this.pretty = this.ToString ()
    override this.ToString () = sprintf "cset [%s%s]" (if this.is_complemented then "! " else "") (flatten_stringables "; " this.set)



/// Cyclic queue type.
type cyclic_queue<'a> (max_len) =
    inherit Queue<'a> ()
    member this.Enqueue x =
        if this.Count >= max_len then this.Dequeue () |> ignore
        base.Enqueue x