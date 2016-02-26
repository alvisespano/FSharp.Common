(*
 * F# Common Library
 * Cache.ml: caching facilities
 * (C) 2007-2012 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
  
module FSharp.Common.Cache

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open Microsoft.FSharp



// functions to cached function
//

module Functional =        


    module Dictionaried =

        let internal make_structural by_comparer f = by_comparer (HashIdentity.Structural) f

        let internal make_by_compare by_comparer compare =
            by_comparer ({ new IEqualityComparer<'k> with
                            member this.Equals (x, y) = compare x y = 0
                            member this.GetHashCode x = hash x })

        let by_comparer (cmp : IEqualityComparer<'k>) f =
            let m = new Collections.Concurrent.ConcurrentDictionary<'k, 'v> (cmp)            
            in
                fun k x -> m.GetOrAdd (k, (fun k -> f k x))

        let structural x = make_structural by_comparer x
        let by_compare x = make_by_compare by_comparer x


    // cache recursive algorithms
    //

    module Recursive =

        let rec generic Y lookup store x =
            match lookup x with
                | Some r -> r
                | None   -> let r = Y (generic Y lookup store) x
                            store x r
                            r

        let inline stored Y =
            generic Y
                (fun (ctx, x) -> match (^x : (member cached : _ option) x) with Some r -> Some r | None -> None)
                (fun (ctx, x) r -> (^x : (member set_cached : _ option -> unit) x, Some r))


        module Dictionaried =

            let by_comparer (cmp : IEqualityComparer<'k>) Y =
                let m = new Collections.Concurrent.ConcurrentDictionary<'k, 'v> (cmp)            
                in
                    generic (fun R (k, v) -> Y R k v)
                        (fun (k, _) -> try Some m.[k] with :? KeyNotFoundException -> None)
                        (fun (k, _) v -> m.[k] <- v)

            let structural x = Dictionaried.make_structural by_comparer x
            let by_compare x = Dictionaried.make_by_compare by_comparer x




    module Pure =

        let empty = fun _ _ -> None

        let cps f k x m =
            let h = hash k 
            match m h x with
                | Some r -> r, m
                | None   -> let r = f k x
                            in
                                r, fun h' x -> if h = h' then Some r else m h x

        let effected f =
            let c = cps f
            let mx = new Threading.Mutex ()
            let m = ref empty
            in
                fun k v ->
                    ignore <| mx.WaitOne ()
                    let r, m' = c k v !m
                    m := m'
                    mx.ReleaseMutex ()
                    r


            
