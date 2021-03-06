﻿(*
 * F# Common Library
 * Env.fs: environment definitions
 * (C) 2007-2012 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
[< RequireQualifiedAccess >]
module FSharp.Common.Env

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open FSharp.Common

open FSharp.Common.Log

// generic environment reporting
    
exception UnboundSymbol of string

[< RequireQualifiedAccess >]
module Report =
    let unbound_symbol x = throw_formatted UnboundSymbol "%O" x
    let unbound_symbol_in_named_env name x = throw_formatted UnboundSymbol "%O in %s envinronment" x name

// choice type used by some env manipulation higher-order functions

[< RequireQualifiedAccess >]
type choice<'k, 'a, 'b> =
    | Existant of 'k * 'a * 'b
    | New1 of 'k * 'a
    | New2 of 'k * 'b

// map-based environment class
//

type t< 'k, [< EqualityConditionalOn; ComparisonConditionalOn >] 'a when 'k : comparison> (m : Map<'k, 'a>) =

    static member ofMap (m : Map<'k, 'a>) = new t<_, _> (m)
    static member ofSeq (sq : seq<'k * 'a>) = new t<_, _> (Map.ofSeq sq)

    member __.to_map = m

    override x.Equals yobj =
        match yobj with
        | :? t<'k, 'a> as y -> Unchecked.equals x.to_map y.to_map
        | _ -> false
 
    override x.GetHashCode () = Unchecked.hash x.to_map
 
    interface System.IComparable with
      member x.CompareTo yobj =
          match yobj with
          | :? t<'k, 'a> as y -> Unchecked.compare x.to_map y.to_map
          | _                  -> invalidArg "yobj" "cannot compare values of different types"

    interface IEnumerable<'k * 'a> with
        member __.GetEnumerator () = (Map.toSeq m).GetEnumerator ()

    interface Collections.IEnumerable with
        member this.GetEnumerator () = (this :> IEnumerable<_>).GetEnumerator () :> Collections.IEnumerator

    new (env : t<_, _>) = new t<'k, 'a> (env.to_map)
    new () = new t<'k, 'a> (Map.empty)

    static member empty = new t<'k, 'a> ()

    member __.is_empty = m.IsEmpty
    member __.length = m.Count
    member __.map f = t<_, _>.ofMap <| Map.map f m
    member __.filter f = t<_, _>.ofMap <| Map.filter f m
    member __.remove x = t<_, _>.ofMap <| Map.remove x m
    member this.remove (xs : #seq<'k>) = Seq.fold (fun (env : t<_, _>) (x : 'k) -> env.remove x) this xs
    member __.find f = Map.findKey f m
    member __.forall p = Map.forall p m
    member __.find_key p = Map.findKey p m
    member __.exists p = Map.exists p m
    member __.contains_key x = Map.containsKey x m
    member __.fold f z = Map.fold f z m
    member __.fold_back f z = Map.foldBack f z m
    member __.toSeq = Map.toSeq m
    member __.toList = Map.toList m
    member __.toArray = Map.toArray m

    member this.keys = seq { for x, _ in this do yield x }
    member this.values = seq { for _, x in this do yield x }
    member this.dom = this.keys |> Set.ofSeq

    member __.search_by p = Map.tryPick (fun x v -> if p x v then Some (x, v) else None) m

    member __.search x = Map.tryFind x m

    member __.lookup x =
        try Map.find x m
        with :? KeyNotFoundException -> Report.unbound_symbol x

    member __.bind x v = t<_, _>.ofMap (Map.add x v m)

    member env.binds bs = Seq.fold (fun (env : t<_, _>) (x, v) -> env.bind x v) env bs
   
    member this.update x f = this.bind x (f (this.lookup x))

    member this.effect x f = f (this.lookup x)

    member env1.diff1 f z (env2 : t<_, _>) =
        let f z x v2 =
            let choice = 
                match env1.search x with
                    | Some v1 -> Choice1Of2 (x, v1, v2)
                    | None    -> Choice2Of2 (x, v2)
            in
                f z choice
        in
            env2.fold f z

    member env1.diff f z env2 =
        let z =
            let f z = function
                | Choice1Of2 (x, v1, v2) -> f z (choice.Existant (x, v1, v2))
                | Choice2Of2 (x, v2)     -> f z (choice.New2 (x, v2))
            in
                env1.diff1 f z env2
        let f z = function
            | Choice1Of2 (x, _, _)   -> z
            | Choice2Of2 (x, v1)     -> f z (choice.New1 (x, v1))
        in
            env2.diff1 f z env1

    member env1.compose f env2 =
        env1.diff (fun (env : t<_, _>) ->
                        function choice.Existant (x, _, _)
                               | choice.New1 (x, _)
                               | choice.New2 (x, _) as choice ->
                                    match f choice with
                                        | Some v -> env.bind x v
                                        | None   -> env)
            t<_, _>.empty env2

    static member (+) (env1 : t<_, _>, env2 : t<_, _>) = env1.binds env2.toSeq

    static member (-) (env1 : t<_, _>, env2) =
        env1.compose (function choice.Existant (_, _, _)
                             | choice.New2 (_, _) -> None
                             | choice.New1 (_, y) -> Some y)
                        env2

    member this.pretty_by_binding p sep =
        if m.IsEmpty then "<empty>"
        else this.fold (fun ss x v -> ss @ [p x v]) [] |> flatten_strings sep

    member this.pretty bsep sep = this.pretty_by_binding (fun x v -> sprintf "%O %s %O" x bsep v) sep

    override this.ToString () = this.pretty "=" "; "


// shortcut for polymorphic empty env
let empty<'id, 'a when 'id : comparison> = new t<'id, 'a> ()

[< CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix) >]
module t =
    // computation expression builder
    let B<'id, 'a when 'id : comparison> = new Computation.Builder.itemized_collection<_, t<'id, 'a>> (empty, (fun (x, v) (env : t<_, _>) -> env.bind x v), (+))


// pure functional environment
//

[< RequireQualifiedAccess >]
module Functional =

    type t<'a, 'b> = 'a -> 'b

    let empty = fun _ -> failwithf "empty environment"

    let bind x v env = fun y -> if x = y then v else env y

    let binds env1 env = fun x -> try env1 x with Failure _ -> env x

    // computation expression builder
//    let B<'a, 'b> = new Computation.Builder.itemized_collection<'a * 'b, t<'a, 'b>> (empty = empty, plus1 = (fun (x, v) env -> bind x v env), plus = binds)
