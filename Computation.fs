
(*
 * F# Common Library
 * Computation.ml: computation expression builders
 * (C) 2007-2014 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
   
module FSharp.Common.Computation

open System
open System.Collections.Generic
open FSharp.Common.Prelude

module Bits =

    module Std =

        let __Delay f = f ()

        let __Combine _Bind (a, b) = _Bind (a, fun _ -> b)

        let __TryWith (body, handler) =
            try body
            with e -> handler e

        let __TryFinally (body, compensation) =
            try body
            finally compensation ()

        let __Using _TryFinally (disp : #System.IDisposable, body) =
            _TryFinally (body disp, fun () -> match disp with null -> disp.Dispose () | _ -> ())

        let rec __While _Bind _Zero (guard, body) =
            let __While = __While _Bind _Zero
            in
                if guard () then _Bind (body, fun _ -> __While (guard, body))
                else _Zero () 

        let __For _Bind _Zero _Using (sq : #IEnumerable<_>, body) =
            let rec R (e : IEnumerator<_>) = 
                if e.MoveNext () then _Bind (body e.Current, fun _ -> R e)
                else _Zero ()
            in
                _Using (sq.GetEnumerator (), R)


    module FunBody =
        let __TryWith (body, handler) s =
            try body s
            with e -> handler e

        let __TryFinally (body, compensation) s =
            try body s
            finally compensation () 


    module EnclosedBody =
        let __Delay f = f

        let __TryWith = FunBody.__TryWith

        let __TryFinally = FunBody.__TryFinally

        let __Using _TryFinally (disp : #System.IDisposable, body) =
            _TryFinally (fun () -> body disp, fun () -> match disp with null -> disp.Dispose () | _ -> ())

        let rec __While _Bind _Zero (guard, body) =
            let __While = __While _Bind _Zero
            in
                if guard () then _Bind (body (), fun _ -> __While (guard, body))
                else _Zero () 

        let __For _Using _While _Delay (sq : #IEnumerable<_>, body) =
            _Using (sq.GetEnumerator (), fun (e : IEnumerator<_>) -> _While (e.MoveNext, _Delay (fun () -> body e.Current))) 


    module LazyBody =
        let __TryWith (body : _ Lazy, handler) =
            try body.Value
            with e -> handler e

        let __TryFinally (body : _ Lazy, compensation) =
            try body.Value
            finally compensation () 

        let __Using _TryFinally (disp : #System.IDisposable, body) =
            _TryFinally (body disp, fun () -> match disp with null -> disp.Dispose () | _ -> ())

        let rec __While _Bind _Zero (guard, body : _ Lazy) =
            let __While = __While _Bind _Zero
            in
                if guard () then _Bind (body.Value, fun _ -> __While (guard, body))
                else _Zero () 


module Builder =
    open Bits

    type collection<'a> (empty : 'a, plus : 'a -> 'a -> 'a) =
        member __.Delay f = Std.__Delay f
        member __.Run f : 'a = f empty
        member __.Bind (a, b) = fun s0 -> let s1 = a s0 in b s1 s1
        member __.Zero () = fun (s : 'a) -> s
        member this.Combine (a, b) = Std.__Combine this.Bind (a, b)
        member this.Using (a, b) = Std.__Using this.TryFinally (a, b)
        member __.TryWith (a, b) = FunBody.__TryWith (a, b)
        member __.TryFinally (a, b) = FunBody.__TryFinally (a, b)
        member this.While (a, b) = Std.__While this.Bind this.Zero (a, b)
        member this.For (a, b) = Std.__For this.Bind this.Zero this.Using (a, b)
        member __.YieldFrom f = fun s -> plus s f
        member __.get = fun (s : 'a) -> s

    type itemized_collection<'e, 'a> (empty, plus1, plus) =
        inherit collection<'a> (empty, plus)
        member __.Yield x = plus1 x
        new (empty, singleton : 'e -> 'a, plus) = new itemized_collection<'e, 'a> (empty = empty, plus1 = (fun x s -> plus s (singleton x)), plus = plus)

    type enumerable<'e, 'a when 'a :> IEnumerable<'e>> (empty, plus1, plus) =
        inherit itemized_collection<'e, 'a> (empty = empty, plus1 = plus1, plus = plus)
        new (empty, plus1) = new enumerable<_, _> (empty = empty,
                                                   plus1 = plus1,
                                                   plus = fun a b -> Seq.fold (fun z x -> plus1 x z) a b)

    type net_collection<'e, 'a when 'a :> ICollection<'e> and 'a : (new : unit -> 'a)> () =
        inherit enumerable<'e, 'a> (empty = new 'a (),
                                    plus1 = (fun x (c : 'a) -> ignore <| c.Add x; c))


// some useful builder instances

module B =
    let string = new Builder.itemized_collection<_, _> ("", (fun (c : char) -> new string [|c|]), (+))

    let set<'a when 'a : comparison> = new Builder.itemized_collection<'a, Set<'a> > (Set.empty, Set.singleton, Set.union)

    let hashset prj = new Builder.itemized_collection<_, _> (empty = new HashSet<_> ({ new IEqualityComparer<_> with
                                                                                          member __.Equals (x, y) = CustomCompare.equals_by prj x y
                                                                                          member __.GetHashCode x = CustomCompare.hash_by prj x }),
                                                             plus1 = (fun x (a : HashSet<_>) -> ignore <| a.Add x; a),
                                                             plus = (fun (a : HashSet<_>) b -> a.UnionWith b; a))

    let net_collection<'e, 'a when 'a :> ICollection<'e> and 'a : (new : unit -> 'a)> = new Builder.net_collection<'e, 'a> ()

