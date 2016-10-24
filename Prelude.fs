(*
 * F# Common Library
 * Prelude.fs: misc stuff
 * (C) 2007-2016 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
[< AutoOpen >]
module FSharp.Common.Prelude

open System
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open Microsoft.FSharp.Reflection
open Printf

// some basic exceptions and exception formatting
//

exception Quit

type unexpected_exception (msg) =
    inherit Exception (msg)

let Unexpected s = new unexpected_exception (s)
let (|Unexpected|_|) (e : Exception) = match e with :? unexpected_exception -> Some e.Message | _ -> None

let throw_formatted exnf fmt = ksprintf (fun s -> raise (exnf s)) fmt

let located_exn exnf (fmt : StringFormat<'a, 'r>) = exnf (StringFormat<string -> string -> 'a, 'r> ("%s(%s): " + fmt.Value))
let unlocated_unexpected fmt = throw_formatted Unexpected fmt
let unexpected fmt = located_exn unlocated_unexpected fmt
let unlocated_not_implemented fmt = throw_formatted NotImplementedException fmt
let not_implemented fmt = located_exn unlocated_not_implemented fmt
let unexpected_case __source_file__ __line__ x = unexpected "unexpected pattern case: %O" __source_file__ __line__ x

let rec pretty_exn_and_inners (e : exn) =
    e.Message + (match e.InnerException with null -> "" | _ -> "\nThis is due to an inner exception: " + pretty_exn_and_inners e.InnerException)


// printf format processing
//

let rec get_flattened_function_arguments (functionType : Type) = 
    let domain, range = FSharpType.GetFunctionElements functionType 
    in
        domain :: if not (FSharpType.IsFunction range) then [range] else get_flattened_function_arguments range

[< System.ObsoleteAttribute("This function is not entirely working and may throw a bad cast exception.") >]
let process_format (f : string * obj list -> 'r) (fmt : PrintfFormat<'a, _, _, 'r>) : 'a = 
    if not (FSharpType.IsFunction typeof<'a>) then unbox (f (fmt.Value, [])) 
    else 
        let types = get_flattened_function_arguments typeof<'a> 
        let rec proc (types : Type list) (values : obj list) (a : obj) : obj = 
            let values = a :: values 
            match types with
            | [_; _] -> box (f (fmt.Value, List.rev values)) 
            | _ :: (y :: z :: _ as l) -> 
                let cont = proc l values
                let ft = FSharpType.MakeFunctionType (y, z)
                let cont = FSharpValue.MakeFunction (ft, cont) 
                in
                    box cont 
            | x -> unexpected_case __SOURCE_FILE__ __LINE__ x
        let handler = proc types [] 
        in
            unbox (FSharpValue.MakeFunction (typeof<'a>, handler))


// list of strings mappers
//

let mappen_strings_or_nothing f empty sep xs =
    match Seq.toList xs with
      | []      -> empty
      | [x]     -> f x
      | x :: xs -> Seq.fold (fun r x -> r + sep + (f x)) (f x) xs

let flatten_strings_or_nothing empty = mappen_strings_or_nothing id empty

let flatten_strings sep = flatten_strings_or_nothing "" sep
let mappen_strings f = mappen_strings_or_nothing f ""

let mappen_stringables f = mappen_strings (f >> sprintf "%O")
let flatten_stringables sep = mappen_stringables id sep


// misc stuff
//

let separate2 f = List.fold (fun (aa, bb) x -> match f x with Choice1Of2 a -> (a :: aa, bb) | Choice2Of2 b -> (aa, b :: bb)) ([], [])

/// Given the value x within the source interval (a1, b1), perforom a linear projection of x into the destination interval (a2, b2).
/// Does not crop, thus a value lying outside the source interval is projected outside the destination interval;
/// similarly it does not check order of interval boundaries, thus projection may perform a flip.
let project (a1, b1) (a2, b2) x =
    let d1 = b1 - a1
    let d2 = b2 - a2
    in
        (x - a1) * d2 / d1 + a2

let crop (a, b) n = if n < a then a elif n > b then b else n

let char_of_byte n = Text.ASCIIEncoding.ASCII.GetChars([|n|]).[0]
let byte_of_char c = Text.ASCIIEncoding.ASCII.GetBytes([|c|]).[0]

let spaces n = new String (' ', n)

let mutable private fresh_int_cnt = 0
let fresh_int () = let r = fresh_int_cnt in fresh_int_cnt <- fresh_int_cnt + 1; r

let something f def = function
    None   -> def
  | Some x -> f x

let (|Mapped|) f x = f x        // useful for mapping data on pattern matching site

[< CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix) >]
module List =
    let takeButLast = function
        | Mapped List.rev (last :: Mapped List.rev heads) -> heads, last
        | _ -> unexpected "empty list has no heads" __SOURCE_FILE__ __LINE__

[< CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix) >]
module Seq =
    let takeButLast l = Seq.take (Seq.length l - 1) l, Seq.last l

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


let cputime f x = 
    let proc = Process.GetCurrentProcess ()
    let cpu_time_stamp = proc.TotalProcessorTime
    let r = f x
    let span = proc.TotalProcessorTime - cpu_time_stamp
    in
        r, span

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


// disposable facilities
//

type [< AbstractClass >] disposable_base () =
    let mutable disposed = false

    interface IDisposable with
        member this.Dispose () =
            this.Dispose true
            GC.SuppressFinalize this
    
    override this.Finalize () = this.Dispose false

    abstract Dispose : bool -> unit
    default this.Dispose disposing =
        if not disposed then
            disposed <- true
            if disposing then this.cleanup_managed ()
            this.cleanup_unmanaged ()

    abstract cleanup_managed : unit -> unit
    abstract cleanup_unmanaged : unit -> unit
    default __.cleanup_unmanaged () = ()  


let disposable_by f =
    { new disposable_base () with
        override __.cleanup_managed () = f () } :> IDisposable


// delayed stuff
//

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


// debugger stuff
//

module Breakpoint =
    open System.Diagnostics

    let tbl = new HashSet<string> (HashIdentity.Structural)
    
    let init name = ignore <| tbl.Add name

    let inline conditional name p = if tbl.Contains name && p then Debugger.Break ()

    let inline normal name = conditional name true


// comparison facilities
//

module CustomCompare =

    /// Perform typed equality by using argument equals as binary equality operator.
    let equals_with equals (x : 'a) (yobj : obj) =
        match yobj with
            | :? 'a as y -> equals x y
            | _          -> false

    /// Performs typed equality by mapping both operands via the given projection function f to some target type supporting equality and F# polymorphic (=) operator.
    let equals_by f = equals_with (fun x y -> f x = f y)
     
    /// Performs hash by mapping operand x via the given projection function f to some target type supporting F# polymorphic hash function.
    let hash_by f x = hash (f x)
 
    /// Perform typed comparison by using argument compare as binary comparison function (does not use polymorphic compare).
    let compare_with compare (x : 'a) (yobj: obj) =
        match yobj with
            | :? 'a as y -> compare x y
            | _          -> invalidArg "yobj" "cannot compare values of different types"

    /// Performs typed comparison by mapping operand via the given projection function f to some target type supporting F# polymorphic compare function.
    let compare_by f = compare_with (fun x y -> compare (f x) (f y))
    
    type [< AbstractClass >] based_on_compare<'this> () =
        interface IComparable with
            member x.CompareTo yobj =
                match yobj with
                    | :? 'this as y -> (x : based_on_compare<'this>).compare y
                    | _             -> invalidArg "yobj" "cannot compare values of different types"
        override x.Equals yobj = (x :> IComparable).CompareTo yobj = 0
        override x.GetHashCode () = hash_by id x
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


// logic
//

[< System.Obsolete("Logic module is not very useful and will probably be deprecated soon.") >]
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
