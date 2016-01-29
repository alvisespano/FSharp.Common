(*
 * F# Common Library
 * Prelude.fs: misc stuff
 * (C) 2007-2012 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
module FSharp.Common.Monad

open Computation.Bits

type M<'a, 's> = 's -> 'a * 's


// state monad
//

// TODO: try to reuse Computation.Bits modules
module Bits =
    let __Using = Std.__Using //_TryFinally (v : #System.IDisposable, f) = _TryFinally (f v, fun () -> if v <> null then v.Dispose () else ())

    let __For _Bind _Zero _Using (sq : seq<_>, f) =
        let rec R (e : System.Collections.Generic.IEnumerator<_>) = 
            if e.MoveNext () then _Bind (f e.Current, fun _ -> R e)
            else _Zero ()
        in
            _Using (sq.GetEnumerator (), R)

    let rec __While _Bind _Zero (cond, f) =
        let _While = __While _Bind _Zero
        in
            if cond () then _Bind (f, fun _ -> _While (cond, f)) else _Zero ()


type state_builder<'s> () =
    member __.Delay f = f ()
    member __.Run f = f
    member __.Bind (e1, e2) = fun (s : 's) -> let r, s' = e1 s in e2 r s'
    member __.Return x = fun (s : 's) -> x, s
    member this.ReturnFrom f = this { let! r = f in return r }
    member __.Zero () = fun (s : 's) -> (), s
    member this.Combine (a, b) = Std.__Combine this.Bind (a, b)
    member this.Using (v, f) = Bits.__Using this.TryFinally (v, f)
    member this.While (cond, f) = if cond () then this.Bind (f, fun _ -> this.While (cond, f)) else this.Zero ()
    member this.For (sq : seq<_>, f) =
        let rec R (e : System.Collections.Generic.IEnumerator<_>) = 
            if e.MoveNext () then this.Bind (f e.Current, fun _ -> R e)
            else this.Zero ()
        in
            this.Using (sq.GetEnumerator (), R)
    member __.TryWith (e, catch) = fun (s : 's) -> try e s with exn -> catch exn s
    member __.TryFinally (e, fin) = fun (s : 's) -> try e s finally fin ()
     
    // old version
//    member __.Delay f = fun (s : 's) -> f () s
//    member __.Run f = fun (s : 's) -> f s
//    member __.Bind (e1, e2) = fun (s : 's) -> let r, s' = e1 s in e2 r s'
//    member __.Return x = fun (s : 's) -> x, s
//    member M.ReturnFrom f = M { let! r = f in return r }
//    member __.Zero () = fun (s : 's) -> (), s
//    member __.For (sq : seq<_>, f) =
//        let rec R (e : System.Collections.Generic.IEnumerator<_>) = 
//            if e.MoveNext () then __.Bind (f e.Current, fun _ -> R e)
//            else __.Zero ()
//        in
//            __.Using (sq.GetEnumerator (), R)
//    member __.Combine (e1, e2) = __.Bind (e1, fun _ -> e2)
//    member __.TryWith (e, catch) = fun (s : 's) -> try e s with exn -> catch exn s
//    member __.TryFinally (e, fin) = fun (s : 's) -> try e s finally fin ()
//    member __.Using (v, f) = __.TryFinally (f v, v.Dispose)
//    member __.While (cond, f) = if cond () then __.Bind (f, fun _ -> __.While (cond, f)) else __.Zero ()

    member __.get_state = fun (s : 's) -> s, s
    member __.set_state (s : 's) = fun (_ : 's) -> (), s
    member __.lift_state f = fun (s : 's) -> (), f s

    member M.fork f =
        M {
            let! s = M.get_state
            let! r = f
            do! M.set_state s
            return r
        }

    member M.ignore f =
      M {
          let! _ = f
          return ()
      }


// DEPRECATE: class methods should be used instead
let get_state = fun s -> (s, s)
let set_state s = fun _ -> ((), s)
let lift_state f = fun s -> ((), f s)
let lift f x = fun s -> (f x, s)

let fork (M : _ state_builder) = M.fork
let ignore (M : _ state_builder) = M.ignore


// binary operator monadic wrappers

let internal short_circuited_logic_binop (M : _ state_builder) op short_circuit_when_true f g =
  M {
    let! a = f
    if a = short_circuit_when_true then return a
    else
        let! b = g
        return op a b
  }

let (|||) M x = short_circuited_logic_binop M (||) true x
let (&&&) M x = short_circuited_logic_binop M (&&) false x


type Option<'s> (M : 's state_builder) =
    member __.something f def o =
      M {
        match o with
            | Some x -> return! f x
            | None   -> return def
      }

    member this.either def o = M { let! r = o in return! this.something (fun x -> M { return x }) def r }

    member this.map f = this.something (fun x -> M { let! r = f x in return Some r }) None

    member __.iter f o =
      M {
        match o with
            | None   -> return ()
            | Some x -> return! f x
      }

    member this.bind f = this.something f None

type List<'s> (M : 's state_builder) =
    member this.fold f z l =
      M {
        match l with
            | []      -> return z
            | x :: xs -> let! z = f z x in return! this.fold f z xs
      }

    member this.fold2 f z l1 l2 =
      M {
          return! this.fold (fun z (x1, x2) -> f z x1 x2) z (List.zip l1 l2)
      }

    member this.map f l =
      M {
        match l with
            | []      -> return []
            | x :: xs -> let! r = f x
                         let! l = this.map f xs
                         return r :: l

      }

    member __.iter f (l : 'a list) = 
      M {
        for e in l do
          do! f e
      }
      
    member __.iteri f (l : 'a list) = 
      M {
        for i, e in List.zip [0..l.Length - 1] l do
            do! f i e
      }

    member this.tryPick f l =
        M {
           match l with
                | [] -> return None
                | x :: xs ->
                    let! o = f x
                    match o with
                        | Some _ as r -> return r
                        | None        -> return! this.tryPick f xs
        }

    member this.tryFind p l = this.tryPick (fun x -> M { let! r = p x in if r then return Some x else return None }) l

    member this.filter f l = 
      M {
        return! this.choose (fun x -> M { let! b = f x in return if b then Some x else None }) l
      }

    member this.choose f l = this.fold (fun l x -> M { let! o = f x in match o with Some x -> return x :: l | None -> return l }) [] l

    member this.collect f l = this.fold (fun l x -> M { let! r = f x in return l @ r }) [] l


type state_builder<'s> with
    member this.List = new List<'s> (this)
    member this.Option = new Option<'s> (this)
