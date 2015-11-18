(*
 * F# Common Library
 * Log.fs: log facilities
 * (C) 2007-2012 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
module FSharp.Common.Log

open System
open System.IO
open Prelude
open Printf


// log line priority type
//
//

[<CustomEquality; CustomComparison>]
type pri = Unmaskerable | High | Normal | Low | Min
with
    static member Parse = function
        "U" | "Unmaskerable" -> Unmaskerable
      | "H" | "High" -> High
      | "N" | "Normal" -> Normal
      | "L" | "Low" -> Low
      | "M" | "Min" -> Min
      | s   -> invalidArg "s" (sprintf "invalid string '%s'" s)

    static member op_Explicit p =
        match p with
            Unmaskerable    -> 4
          | High            -> 3
          | Normal          -> 2
          | Low             -> 1
          | Min             -> 0

    override this.Equals yobj = CustomCompare.equals_by int this yobj
    override this.GetHashCode () = CustomCompare.hash_by int this
    interface IComparable with
        member this.CompareTo yobj = CustomCompare.compare_by int this yobj

    member this.pretty =
        match this with
            Unmaskerable    -> "U"
          | High            -> "H"
          | Normal          -> "N"
          | Low             -> "L"
          | Min             -> "M"


// dynamic configuration
//
//

type config (?filename : string) =
    let mutable swo : StreamWriter option = Option.map (fun (s : string) -> new StreamWriter (s)) filename

    interface IDisposable with
        member this.Dispose () =
            this.Dispose true
            GC.SuppressFinalize this

    abstract Dispose : bool -> unit
    default this.Dispose disposing =
        if disposing then this.close

    abstract close : unit
    default __.close =
        Option.iter (fun (sw : StreamWriter) -> sw.Flush (); sw.Close (); swo <- None) swo

    member this.filename
        with get () = Option.map (fun (sw : StreamWriter) -> (sw.BaseStream :?> FileStream).Name) swo

        and set newnameo =
            this.close
            swo <- Option.map (fun newname ->
                                if this.move_logfile_on_name_change then
                                    Option.iter (fun oldname ->
                                                    File.Copy (oldname, newname, true)
                                                    File.Delete oldname)
                                        this.filename
                                new StreamWriter (newname))
                            newnameo

    member internal this.StreamWriter = something id StreamWriter.Null swo

    member val move_logfile_on_name_change = true with get, set

    member val debug_threshold = Min with get, set
    member val perf_threshold = Min with get, set
    member val msg_threshold = Min with get, set
    member val hint_threshold = Min with get, set
    member val warn_threshold = Min with get, set

    member val has_console = (try (ignore Console.CursorLeft; true) with _ -> false)

    member val suppress_duplicates_window = Some 80 with get, set

    abstract all_thresholds : pri with set
    default this.all_thresholds
        with set pri =
            this.debug_threshold <- pri
            this.msg_threshold <- pri
            this.hint_threshold <- pri
            this.warn_threshold <- pri

    member val show_thread = false with get, set
    member val show_milliseconds = false with get, set
    member val show_elapsed = false with get, set
    member val show_datetime = true with get, set
    member val show_priority = false with get, set
    member val show_urgency = false with get, set
    member val text_shade_by_urgency = true with get, set

    member this.show_body_only
        with set b =
            let b = not b
            this.show_datetime <- b
            this.show_priority <- b
            this.show_urgency <- b
    
    member val padding_enabled = true with get, set
    member val multiline_tab_width = 2 with get, set
    member val datetime_max_len = 22 with get, set
    member val thread_max_len = 5 with get, set
    member val priority_max_len = 4 with get, set
    member val header_max_len = 7 with get, set

    member val debug_header = Some "DEBUG" with get, set
    member val perf_header = Some "PERF" with get, set
    member val msg_header = Some "INFO" with get, set
    member val warn_header = Some "WARN" with get, set
    member val hint_header = Some "HINT" with get, set

    member val datetime_color = ConsoleColor.Gray with get, set
    member val thread_color = ConsoleColor.Blue with get, set
    member val priority_color = ConsoleColor.Blue with get, set
    member val urgency_color = ConsoleColor.White with get, set
    member val square_bracket_color = ConsoleColor.White with get, set
    member val msg_color = ConsoleColor.Gray with get, set
    member val perf_color = ConsoleColor.Blue with get, set
    member val debug_color = ConsoleColor.Cyan with get, set
    member val hint_color = ConsoleColor.Green with get, set
    member val warn_color = ConsoleColor.Yellow with get, set
    member val fatal_error_color = ConsoleColor.Red with get, set
    member val unexpected_error_color = ConsoleColor.Magenta with get, set


module Color =
    type private C = ConsoleColor
    
    let darken col =
        try
            let colenumty = col.GetType ()
            in
                C.Parse (colenumty, "dark" + C.GetName (colenumty, col), true) :?> C
        with _ -> if col = C.Black then C.DarkGray else C.Black

    let shade col darkcol n =
        let fg, bg =
            match n with
                | 0 -> darkcol, C.Black // darker
                | 1 -> col, C.Black     // normal
                | 2 -> col, darkcol     // brighter
                | 3 -> C.White, darkcol
                | 4 -> darkcol, col
                | _ -> C.White, col
        in
            fg, if fg = bg then C.Gray else bg

    let closest_console_color (r, g, b) =
        let rec R delta z = function
            | [] -> z
            | cc :: ccs ->
                let n = Enum.GetName (typeof<ConsoleColor>, cc)
                let c = System.Drawing.Color.FromName (if n = "DarkYellow" then "Orange" else n)
                let t = Math.Pow (float (c.R - r), 2.0) + Math.Pow (float (c.G - g), 2.0) + Math.Pow (float (c.B - b), 2.0)
                in
                    if t = 0.0 then cc
                    elif t < delta then R t cc ccs
                    else R delta z ccs
        in
            R Double.MaxValue (new ConsoleColor ()) (X.List.cast (Enum.GetValues typeof<ConsoleColor>))
        


// public API
//

type prompt =  int option -> pri option -> string -> unit

type [< AbstractClass >] logger (cfg : config) =
    member this.debug_prompt = this.prompter cfg.debug_header cfg.debug_color
    member this.hint_prompt = this.prompter cfg.hint_header cfg.hint_color
    member this.warn_prompt = this.prompter cfg.warn_header cfg.warn_color
    member this.msg_prompt = this.prompter cfg.msg_header cfg.msg_color
    member this.perf_prompt = this.prompter cfg.perf_header cfg.perf_color
    member this.error_prompt fgcol s = this.visible_prompt_printf (this.prompter (Some s) fgcol) None None

    member this.msg lv fmt = this.print this.msg_prompt cfg.msg_threshold lv fmt
    member this.perf lv fmt = this.print this.perf_prompt cfg.perf_threshold lv fmt
    member this.debug lv fmt = this.print this.debug_prompt cfg.debug_threshold lv fmt
    member this.hint lv fmt = this.print this.hint_prompt cfg.hint_threshold lv fmt
    member this.warn lv fmt = this.print this.warn_prompt cfg.warn_threshold lv fmt
    member this.custom fgcol header threshold lv fmt = this.print (this.prompter (Some header) fgcol) threshold lv fmt
    member this.custom_debug fgcol header lv fmt = this.custom fgcol header cfg.debug_threshold lv fmt
    member this.custom_error fgcol header fmt = this.error_prompt fgcol header fmt
    member this.fatal_error fmt = this.custom_error cfg.fatal_error_color "FATAL" fmt
    member this.unexpected_error fmt = this.custom_error cfg.unexpected_error_color "UNEXPECTED" fmt
    member __.line_feed = Console.WriteLine ""

    member val cfg = cfg

    abstract prompter : string option -> ConsoleColor -> prompt

    abstract visible_prompt_printf : prompt -> int option -> pri option -> StringFormat<'a, unit> -> 'a
    default __.visible_prompt_printf prompt marks lvo fmt = kprintf (prompt marks lvo) fmt    

    abstract hidden_prompt_printf : prompt -> int option -> pri option -> StringFormat<'a, unit> -> 'a
    default __.hidden_prompt_printf _ _ _ fmt = kprintf (fun _ -> ()) fmt

    abstract print : prompt -> pri -> pri -> StringFormat<'a, unit> -> 'a
    default this.print prompt thre lv fmt =
        (if lv >= thre then this.visible_prompt_printf else this.hidden_prompt_printf) prompt (Some (int lv - int thre)) (Some lv) fmt



// console logger
//

type console_logger (cfg) =
    inherit logger (cfg)

    let mutex = new Threading.Mutex ()
    let now0 = DateTime.Now
//    let history = Option.map (fun n -> new X.Collection.cyclic_queue<int> (n)) cfg.suppress_duplicates_window
    let calling_thread_id = Threading.Thread.CurrentThread.ManagedThreadId
    let mutable another_thread_has_logged = false
    let tab = new String (' ', cfg.multiline_tab_width)
    let tablen = ref 0
    let out (s : string) =
        if cfg.has_console then Console.Write s
        cfg.StreamWriter.Write s
        tablen := !tablen + s.Length
    let outcol fg bg (s : string) =
        Console.ForegroundColor <- fg
        Console.BackgroundColor <- bg
        out s
    let outsq fg bg len s =
        outcol cfg.square_bracket_color ConsoleColor.Black "["
        outcol fg bg s
        outcol cfg.square_bracket_color ConsoleColor.Black "]"
        let dlen = len - (s.Length + 2)
        if dlen > 0 then Console.ResetColor (); out (new String (' ', dlen))
    let outsqfg fg len s = outsq fg ConsoleColor.Black len s
    let pad n =
        Console.ResetColor ()
        if cfg.padding_enabled then out (new String (' ', n))
     
    override this.prompter (hdo : string option) (col : ConsoleColor) =
        let darkcol = Color.darken col
        in
          fun markso lvo ->
            let print_on_console (s : string) =
                tablen := 0
                // datetime              
                if cfg.show_datetime then
                    let now = DateTime.Now
                    outsqfg cfg.datetime_color cfg.datetime_max_len
                        (if cfg.show_milliseconds then sprintf "%O.%d" now now.Millisecond else sprintf "%O" now)
                // elapsed              
                if cfg.show_elapsed then
                    let span = DateTime.Now - now0
                    let len, f = if cfg.show_milliseconds then (10, fun h m s -> sprintf "%02d:%02d:%02d.%03d" h m s span.Milliseconds)
                                    else 6, sprintf "%02d:%02d:%02d"
                    outsqfg cfg.datetime_color len (f span.Hours span.Minutes span.Seconds)
                // thread
                if cfg.show_thread then
                    let th = Threading.Thread.CurrentThread
                    let id = th.ManagedThreadId
                    if id <> calling_thread_id then another_thread_has_logged <- true
                    if another_thread_has_logged then
                        let name = th.Name
                        outsqfg cfg.thread_color cfg.thread_max_len (if th.IsThreadPoolThread then sprintf ":%d" id else sprintf "%s#%d" (if String.IsNullOrEmpty name then "" else name.Trim ()) id)
                // header
                match hdo with
                    | Some hd -> outsq col darkcol cfg.header_max_len hd
                    | None    -> pad cfg.header_max_len
                // priority
                if cfg.show_priority then
                    match lvo with
                        | Some (lv : pri) -> outsqfg cfg.priority_color cfg.priority_max_len lv.pretty
                        | None            -> pad cfg.priority_max_len
                // urgency
                if cfg.show_urgency then
                    Option.iter (fun marks -> outcol cfg.urgency_color ConsoleColor.Black (new String ('!', marks) + new String (' ', 5 - marks))) markso
                else out " "
                // body
                let at = Console.CursorLeft
                let (bodyfgcol, bodybgcol) =
                    if cfg.text_shade_by_urgency then Color.shade col darkcol (either 1 markso)
                    else (col, ConsoleColor.Black)
                let outbody tabn (s : string) =
                    Console.CursorLeft <- at + tabn
                    Console.ForegroundColor <- bodyfgcol
                    Console.BackgroundColor <- bodybgcol
                    Console.Write s
                    Console.ResetColor ()
                    Console.WriteLine ()
                let p (s : string) =
                    let s = s.Replace ("\t", tab)
                    let tabn = cfg.multiline_tab_width + (new Text.RegularExpressions.Regex ("[^ ]+")).Match(s).Index
                    let sa = split_string_on_size (Console.BufferWidth - at - 1 - cfg.multiline_tab_width) s
                    if sa.Length > 0 then
                        outbody 0 sa.[0]
                        for i = 1 to sa.Length - 1 do
                            outbody tabn sa.[i]
                Array.iter p (s.Split [|'\n'|])

            in
                fun (s : string) ->
                    let s = s.TrimEnd [|' '; '\t'|]
                    if s.Length > 0 then this.locked_apply <| fun () -> print_on_console s

    member __.locked_apply f = 
        ignore <| mutex.WaitOne ()
        try f ()
        finally mutex.ReleaseMutex ()                            

    member this.print_line s = this.locked_apply <| fun () -> printfn "%s" s

    interface IDisposable with
        member this.Dispose () =
            this.Dispose true
            GC.SuppressFinalize this

    abstract Dispose : bool -> unit
    default __.Dispose disposing =
        if disposing then (cfg :> IDisposable).Dispose ()

    abstract close : unit
    default __.close = cfg.close

    new () = new console_logger (new config ())



(* other loggers *)

type null_logger (cfg) =
    inherit logger (cfg)
    override __.prompter _ _ = fun _ _ _ -> ()

