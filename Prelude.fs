(*
 * F# Common Library
 * Prelude.fs: misc stuff
 * (C) 2007-2012 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
module FSharp.Common.Prelude

open System
open System.Collections.Generic
open System.Diagnostics
open System.Threading

exception Quit

type unexpected_exception (msg) =
    inherit Exception (msg)

let Unexpected s = new unexpected_exception (s)
let (|Unexpected|_|) (e : Exception) = match e with :? unexpected_exception -> Some e.Message | _ -> None

let identity = id

let throw_formatted exnf fmt = Printf.kprintf (fun s -> raise (exnf s)) fmt

let (%+%%) (fmt1 : PrintfFormat<'a -> 'r, _, _, 'r>) (fmt2 : PrintfFormat<'b, _, _, 'r>) =
    new PrintfFormat<'a -> 'b, _, _, 'r> (fmt1.Value + fmt2.Value)

let lexnf exnf fmt = exnf ("%s(" %+%% ("%s): " %+%% fmt))
let uunexpected fmt = throw_formatted Unexpected fmt
let unexpected fmt = lexnf uunexpected fmt

let unot_implemented fmt = throw_formatted (fun s -> new NotImplementedException (s)) fmt
let not_implemented fmt = lexnf unot_implemented fmt

let equal_float (x : float) y = Math.Abs (x - y) <= Double.Epsilon

let mappen_strings_or_nothing f empty sep xs =
    match Seq.toList xs with
      | []      -> empty
      | [x]     -> f x
      | x :: xs -> Seq.fold (fun r x -> r + sep + (f x)) (f x) xs

let flatten_strings_or_nothing empty = mappen_strings_or_nothing identity empty

let flatten_strings sep = flatten_strings_or_nothing "" sep
let mappen_strings f = mappen_strings_or_nothing f ""

let mappen_stringables f = mappen_strings (f >> sprintf "%O")
let flatten_stringables sep = mappen_stringables identity sep

let separate2 f = List.fold (fun (aa, bb) x -> match f x with Choice1Of2 a -> (a :: aa, bb) | Choice2Of2 b -> (aa, b :: bb)) ([], [])

let crop (a, b) n = if n < a then a elif n > b then b else n

let char_of_byte n = Text.ASCIIEncoding.ASCII.GetChars([|n|]).[0]
let byte_of_char c = Text.ASCIIEncoding.ASCII.GetBytes([|c|]).[0]

let spaces n = new String (' ', n)

let mutable private fresh_int_cnt = 0
let fresh_int () = let r = fresh_int_cnt in fresh_int_cnt <- fresh_int_cnt + 1; r

let something f def = function
    None   -> def
  | Some x -> f x

let rec try_map f = function
    | [] -> Some []
    | x :: xs -> match f x with Some y -> Option.map (fun ys -> y :: ys) (try_map f xs) | None -> None

let soprintf fmt = function None -> "" | Some x -> sprintf fmt x

let either def o = something (fun x -> x) def o

let split_string_on_size n (s : string) =
    let m = s.Length % n
    in
        Array.init (s.Length / n + (if m > 0 then 1 else 0)) (fun i -> s.Substring (i * n, if i * n + n > s.Length then m else n))

let truncate_string_with_ellipsis n (s : string) =
    if s.Length > n then sprintf "%s..." (s.Substring (0, n - 3))
    else s

let capitalize (s : string) =
    if s.Length > 1 then s.Substring(0, 1).ToUpper() + s.Substring(1) else s.ToUpper()

let (|Capitalized|) s = capitalize s
let (|Uppercased|) (s : string) = s.ToUpper ()
let (|Lowercased|) (s : string) = s.ToLower ()

let cputime f x = 
    let proc = Process.GetCurrentProcess ()
    let cpu_time_stamp = proc.TotalProcessorTime
    let r = f x
    let span = proc.TotalProcessorTime - cpu_time_stamp
    in
        r, span


type syncbox<'a> (x : 'a) =
    let mx = new Mutex ()
    let mutable x = x

    member __.apply_and_set f =
        ignore <| mx.WaitOne ()
        x <- f x
        mx.ReleaseMutex ()
        
    member __.apply f =
        ignore <| mx.WaitOne ()
        let r = f x
        mx.ReleaseMutex ()
        r

    member this.value
        with set x' = this.apply_and_set <| fun _ -> x'


type delayed<'a> (f) =
    let mutable x = None

    member __.Value
        with get () : 'a =
            match x with
                | None ->
                    try
                        let r = f ()
                        x <- Some (Choice1Of2 r)
                        r
                    with e ->
                        x <- Some (Choice2Of2 e)
                        reraise ()

                | Some (Choice1Of2 r) -> r
                | Some (Choice2Of2 e) -> raise e

    override this.ToString () = this.Value.ToString ()


let Delayed f = new delayed<_> (f)
let (|Delayed|) (d : _ delayed) = d.Value

let delay_value x = new delayed<_> (fun () -> x)

let async_delayed f =
    let t = Async.StartAsTask (async {
                try return Choice1Of2 (f ())
                with e -> return Choice2Of2 e
              })
    in
        Delayed (fun () ->
            t.Wait ()
            match t.Result with
                | Choice1Of2 r -> r
                | Choice2Of2 e -> raise e)


let trap f x = try Some (f x) with _ -> None
let private trapn trap f a = trap (f a)
let trap2 f = trapn trap f
let trap3 f = trapn trap2 f
let trap4 f = trapn trap3 f
let trap5 f = trapn trap4 f

let uncurry2 f (a, b) = f a b 
let uncurry3 f (a, b, c) = f a b c 
let uncurry4 f (a, b, c, d) = f a b c d 
let uncurry5 f (a, b, c, d, e) = f a b c d e

let curry2 f a b = f (a, b)
let curry3 f a b c = f (a, b, c)
let curry4 f a b c d = f (a, b, c, d)
let curry5 f a b c d e = f (a, b, c, d, e)

let inline (!>) (x : ^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)
let inline (!>>) x = !> (!> x)

let rec pretty_exn_and_inners (e : exn) =
    e.Message + (match e.InnerException with null -> "" | _ -> "\nThis is due to an inner exception: " + pretty_exn_and_inners e.InnerException)


module Breakpoint =
    open System.Diagnostics

    let tbl = new HashSet<string> (HashIdentity.Structural)
    
    let init name = ignore <| tbl.Add name

    let inline conditional name p = if tbl.Contains name && p then Debugger.Break ()

    let inline normal name = conditional name true


module CustomCompare =

    let equals_with equals (x : 'a) (yobj : obj) =
        match yobj with
            | :? 'a as y -> equals x y
            | _          -> false

    let equals_by f = equals_with (fun x y -> f x = f y)
     
    let hash_by f x = hash (f x)
 
    let compare_with compare (x : 'a) (yobj: obj) =
        match yobj with
            | :? 'a as y -> compare x y
            | _          -> invalidArg "yobj" "cannot compare values of different types"

    let compare_by f = compare_with (fun x y -> compare (f x) (f y))
    
    type [< AbstractClass >] based_on_compare<'this> () =
        interface IComparable with
            member x.CompareTo yobj =
                match yobj with
                    | :? 'this as y -> (x : based_on_compare<'this>).compare y
                    | _             -> invalidArg "yobj" "cannot compare values of different types"
        override x.Equals yobj = (x :> IComparable).CompareTo yobj = 0
        override x.GetHashCode () = hash_by identity x
        abstract compare : 'this -> int
        
    type [< AbstractClass >] project_by_property<'a when 'a : comparison> () =
        inherit based_on_compare<project_by_property<'a>> ()
        let p (x : project_by_property<'a>) = x.project_to_comparable
        override x.Equals yobj = equals_by p x yobj
        override x.GetHashCode () = hash_by p x
        override x.compare y = compare_by p x y
        abstract project_to_comparable : 'a

    type [< AbstractClass >] project_by_function<'a when 'a : comparison> (f : project_by_property<'a> -> 'a) =
        inherit project_by_property<'a> ()
        override this.project_to_comparable = f this

module Logic =

    type boolean (b : bool) =
        member val value = b 
        static member op_Implicit (this : boolean) = this.value
        static member op_Implicit b = new boolean (b)
        static member ( * ) (a : boolean, b : boolean) : boolean = !> (!> a && !> b)
        static member ( + ) (a : boolean, b : boolean) : boolean = !> (!> a || !> b)
        static member get_Zero () = boolean false
    
    let inline (==>) p q = -(p * -q)

    type _then = Then
    type _else = Else

    let inline If a Then b Else c = (a ==> b) * (-a ==> c)

    let inline crop (a, b) n = If !> (n < a) Then a Else (If !> (n > b) Then b Else n)
