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

[< CustomEquality; CustomComparison >]
type pri = Unmaskerable | High | Normal | Low | Min
with
    static member Parse = function
        | "U" | "Unmaskerable" -> Unmaskerable
        | "H" | "High" -> High
        | "N" | "Normal" -> Normal
        | "L" | "Low" -> Low
        | "M" | "Min" -> Min
        | s -> invalidArg "s" (sprintf "invalid string '%s'" s)

    static member op_Explicit pri =
        match pri with
        | Unmaskerable    -> 4
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
        | Unmaskerable    -> "U"
        | High            -> "H"
        | Normal          -> "N"
        | Low             -> "L"
        | Min             -> "M"

    override this.ToString () = this.pretty


// dynamic configuration
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
        with get () =
            Option.map (fun (sw : StreamWriter) -> (sw.BaseStream :?> FileStream).Name) swo

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

    member internal __.StreamWriter = something id StreamWriter.Null swo

    member val move_logfile_on_name_change = true with get, set

    member val debug_threshold = Min with get, set
    member val custom_debug_threshold = Min with get, set
    member val msg_threshold = Min with get, set
    member val hint_threshold = Min with get, set
    member val warn_threshold = Min with get, set

    member val has_console = (try (ignore Console.WindowHeight; true) with _ -> false)

    abstract all_thresholds : pri with set
    default this.all_thresholds        
        with set pri =
            this.debug_threshold <- pri
            this.msg_threshold <- pri
            this.hint_threshold <- pri
            this.warn_threshold <- pri

    member this.thresholds 
        with get () = this.debug_threshold, this.custom_debug_threshold, this.msg_threshold, this.hint_threshold, this.warn_threshold
        and set (d, p, m, h, w) =
            this.debug_threshold <- d
            this.custom_debug_threshold <- p
            this.msg_threshold <- m
            this.hint_threshold <- h
            this.warn_threshold <- w

    member val show_thread = false with get, set
    member val show_milliseconds = false with get, set
    member val show_elapsed = false with get, set
    member val show_datetime = true with get, set
    member val show_priority = false with get, set
    member val show_urgency = false with get, set
    member val show_header = true with get, set

    member this.show_message_body_only
        with set b =
            let b = not b
            this.show_datetime <- b
            this.show_priority <- b
            this.show_urgency <- b
            this.show_elapsed <- b
            this.show_thread <- b
            this.show_milliseconds <- b
            this.show_header <- b

    member val text_shade_by_urgency = true with get, set
    member val show_header_only_when_changes = false with get, set

    member val tab = 0 with get, set    
      
    member val padding_enabled = true with get, set
    member val multiline_left_pad = 3 with get, set
    member val tab_to_spaces = Some 4 with get, set
    member val datetime_max_len = 22 with get, set
    member val thread_max_len = 5 with get, set
    member val priority_max_len = 4 with get, set
    member val header_max_len = 7 with get, set

    member val debug_header = "DEBUG" with get, set
    member val msg_header = "INFO" with get, set
    member val warn_header = "WARN" with get, set
    member val hint_header = "HINT" with get, set

    member val datetime_color = ConsoleColor.Gray with get, set
    member val thread_color = ConsoleColor.Blue with get, set
    member val priority_color = ConsoleColor.Blue with get, set
    member val urgency_color = ConsoleColor.White with get, set
    member val square_bracket_color = ConsoleColor.White with get, set
    member val msg_color = ConsoleColor.Gray with get, set
    member val debug_color = ConsoleColor.Cyan with get, set
    member val custom_debug_color = ConsoleColor.Blue with get, set
    member val hint_color = ConsoleColor.Green with get, set
    member val warn_color = ConsoleColor.Yellow with get, set
    member val fatal_error_color = ConsoleColor.Red with get, set
    member val unexpected_error_color = ConsoleColor.Magenta with get, set


module Color =
    type t = ConsoleColor
    
    let darken col =
        try
            let colenumty = col.GetType ()
            in
                t.Parse (colenumty, "dark" + t.GetName (colenumty, col), true) :?> t
        with _ -> if col = t.Black then t.DarkGray else t.Black

    let shade col darkcol n =
        let fg, bg =
            match n with
                | 0 -> darkcol, t.Black // darkest
                | 1 -> col, t.Black     // when threshold = Min, Low = 2; but when threshold = Low (which should be the default threshold), Normal = 1
                | 2 -> col, darkcol
                | 3 -> darkcol, col
                | _ -> t.White, darkcol
//                | _ -> C.White, col   // white on bright color is barely visible
        in
            fg, if fg = bg then t.Gray else bg

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

module private FakeFormat =
    let rec private make_curried_fun (ty : System.Type) =
        if Reflection.FSharpType.IsFunction ty then
            let _, ran = Reflection.FSharpType.GetFunctionElements ty
            let f = make_curried_fun ran    // do not delay invocation until runtime
            in
                Reflection.FSharpValue.MakeFunction (ty, fun _ -> f)
        else box ()

    [< Sealed >]
    type Format<'T> private () =
        static let instance : 'T = unbox (make_curried_fun typeof<'T>)
        static member Instance = instance


type [< AbstractClass >] logger (cfg : config) =
    let tab_history = new System.Collections.Generic.Stack<_> ()

    member val cfg = cfg

    member this.log_leveled header fgcol threshold pri fmt = this.printf_leveled header fgcol threshold pri fmt
    member this.log_unleveled header fgcol fmt = this.printf_unleveled header fgcol fmt
    member this.log_line s = lock this <| fun () -> this.write_line s
    member this.line_feed = this.log_line ""

    abstract write_line : string -> unit
    default __.write_line s = Console.WriteLine s

    member this.msg pri fmt = this.log_leveled cfg.msg_header cfg.msg_color cfg.msg_threshold pri fmt
    member this.debug pri fmt = this.log_leveled cfg.debug_header cfg.debug_color cfg.debug_threshold pri fmt
    member this.custom_debug header pri fmt = this.log_leveled header cfg.custom_debug_color cfg.custom_debug_threshold pri fmt // custom_debug can be used as secondary debug channel, specifying a custom header
    member this.hint pri fmt = this.log_leveled cfg.hint_header cfg.hint_color cfg.hint_threshold pri fmt
    member this.warn pri fmt = this.log_leveled cfg.warn_header cfg.warn_color cfg.warn_threshold pri fmt
    member this.fatal_error fmt = this.log_unleveled "FATAL" cfg.fatal_error_color fmt
    member this.unexpected_error fmt = this.log_unleveled "UNEXPECTED" cfg.unexpected_error_color fmt

    abstract actually_print : string * ConsoleColor * int option * pri option * string -> unit

    abstract visible_printf : string -> ConsoleColor -> int option -> pri option -> StringFormat<'a, unit> -> 'a
    default this.visible_printf header fgcol markno prio fmt = ksprintf (fun s -> lock this <| fun () -> this.actually_print (header, fgcol, markno, prio, s)) fmt

    abstract hidden_printf : string -> ConsoleColor -> int option -> pri option -> StringFormat<'a, unit> -> 'a
    default __.hidden_printf _ _ _ _ _ = FakeFormat.Format<_>.Instance

    member internal this.printf_leveled header fgcol threshold pri fmt =
        (if pri >= threshold then this.visible_printf else this.hidden_printf) header fgcol (Some (int pri - int threshold)) (Some pri) fmt

    member internal this.printf_unleveled header fgcol fmt =
        this.visible_printf header fgcol None None fmt
               
    member this.tab
        with get () = cfg.tab
        and set n =
            lock this <| fun () ->
                ignore <| tab_history.Push cfg.tab
                cfg.tab <- n

    member this.tabulate n = 
        lock this <| fun () ->
            this.tab <- this.tab + n

    member this.undo_tabulate =
        lock this <| fun () ->
            if tab_history.Count > 0 then
                cfg.tab <- tab_history.Pop ()


// Common formatting. Destinations supply only colored text output, flushing and
// an optional line width. The optional file in config receives the same text.
//

[< AbstractClass >]
type formatted_logger (cfg : config) =
    inherit logger (cfg)

    let mutable last_header = ""
    let now0 = DateTime.Now
    let calling_thread_id = Threading.Thread.CurrentThread.ManagedThreadId
    let mutable another_thread_has_logged = false

    abstract write_text : ConsoleColor * ConsoleColor * string -> unit

    abstract flush : unit -> unit
    default __.flush () = ()

    abstract line_width : int option
    default __.line_width = None

    member private this.out fg bg s =
        this.write_text (fg, bg, s)
        cfg.StreamWriter.Write (s : string)

    member private this.end_line () =
        this.out cfg.msg_color ConsoleColor.Black Environment.NewLine
        this.flush ()
        cfg.StreamWriter.Flush ()

    override this.write_line s =
        this.out cfg.msg_color ConsoleColor.Black s
        this.end_line ()

    override this.actually_print (header, fgcol, markso, prio, s) =
        let darkcol = Color.darken fgcol
        let mutable prefix_length = 0
        let emit fg bg (text : string) =
            this.out fg bg text
            prefix_length <- prefix_length + text.Length
        let pad n =
            if cfg.padding_enabled then emit cfg.msg_color ConsoleColor.Black (spaces (max 0 n))
        let field fg bg width (value : string) =
            emit cfg.square_bracket_color ConsoleColor.Black "["
            emit fg bg value
            emit cfg.square_bracket_color ConsoleColor.Black "]"
            pad (width - value.Length - 2)
        let fieldfg fg width value = field fg ConsoleColor.Black width value
        if cfg.show_datetime then
            let now = DateTime.Now
            fieldfg cfg.datetime_color cfg.datetime_max_len
                (if cfg.show_milliseconds then sprintf "%O.%03d" now now.Millisecond else sprintf "%O" now)
        if cfg.show_elapsed then
            let span = DateTime.Now - now0
            fieldfg cfg.datetime_color (if cfg.show_milliseconds then 10 else 6)
                (if cfg.show_milliseconds then sprintf "%02d:%02d:%02d.%03d" span.Hours span.Minutes span.Seconds span.Milliseconds
                 else sprintf "%02d:%02d:%02d" span.Hours span.Minutes span.Seconds)
        if cfg.show_thread then
            let th = Threading.Thread.CurrentThread
            let id = th.ManagedThreadId
            if id <> calling_thread_id then another_thread_has_logged <- true
            if another_thread_has_logged then
                fieldfg cfg.thread_color cfg.thread_max_len
                    (if th.IsThreadPoolThread then sprintf ":%d" id
                     else sprintf "%s#%d" (if String.IsNullOrEmpty th.Name then "" else th.Name.Trim ()) id)
        if cfg.show_header then
            if not (String.IsNullOrWhiteSpace header) &&
               (not cfg.show_header_only_when_changes || last_header <> header) then
                last_header <- header
                field fgcol darkcol cfg.header_max_len header
            else pad cfg.header_max_len
        if cfg.show_priority then
            match prio with
            | Some pri -> fieldfg cfg.priority_color cfg.priority_max_len pri.pretty
            | None -> pad cfg.priority_max_len
        if cfg.show_urgency then
            match markso with
            | Some marks ->
                emit cfg.urgency_color ConsoleColor.Black (String ('!', max 0 marks))
                pad (5 - marks)
            | None -> ()
        elif prefix_length > 0 then emit cfg.msg_color ConsoleColor.Black " "
        let body =
            match cfg.tab_to_spaces with
            | Some n -> s.Replace ("\t", spaces n)
            | None -> s
        let lines = (body.Replace ("\r\n", "\n")).Split [|'\n'|]
        let bodyfg, bodybg =
            if cfg.text_shade_by_urgency then Color.shade fgcol darkcol (either 1 markso)
            else fgcol, ConsoleColor.Black
        let width = this.line_width
        let body_indent = prefix_length + max 0 cfg.tab
        let wrap_indent = body_indent + max 0 cfg.multiline_left_pad
        // Explicit newlines align with the body; only wrapped continuations get
        // the extra padding. Plain writers never split a long source line.
        for i = 0 to lines.Length - 1 do
            let line = lines.[i]
            let mutable offset = 0
            let mutable indent = if i = 0 then max 0 cfg.tab else body_indent
            let mutable column = if i = 0 then prefix_length + indent else indent
            let mutable more = true
            while more do
                let count =
                    match width with
                    | Some width -> min (line.Length - offset) (max 1 (width - column - 1))
                    | None -> line.Length - offset
                this.out cfg.msg_color ConsoleColor.Black (spaces indent)
                this.out bodyfg bodybg (line.Substring (offset, count))
                this.end_line ()
                offset <- offset + count
                more <- offset < line.Length
                indent <- wrap_indent
                column <- indent

    abstract close : unit
    default __.close = cfg.close

    abstract Dispose : bool -> unit
    default this.Dispose disposing =
        if disposing then this.close

    interface IDisposable with
        member this.Dispose () =
            this.Dispose true
            GC.SuppressFinalize this


// Caller-owned writers stay open. File loggers own and close their writer.
//

type text_logger (writer : TextWriter, cfg : config, ?leave_open : bool) =
    inherit formatted_logger (cfg)

    let leave_open = defaultArg leave_open true
    let mutable closed = false

    override __.write_text (_, _, s) =
        if closed then raise (ObjectDisposedException "text_logger")
        writer.Write s

    override __.flush () = writer.Flush ()

    override this.close =
        lock this <| fun () ->
            if not closed then
                try
                    if leave_open then writer.Flush () else writer.Dispose ()
                finally
                    closed <- true
                    cfg.close

type std_logger (cfg : config, ?stderr : bool) =
    inherit text_logger ((if defaultArg stderr false then Console.Error else Console.Out), cfg, leave_open = true)

    new () = new std_logger (new config ())

type file_logger (filename : string, cfg : config, ?append : bool) =
    inherit text_logger (new StreamWriter (filename, defaultArg append false, new Text.UTF8Encoding (false)), cfg, leave_open = false)

    new (filename : string) = new file_logger (filename, new config ())


// console logger
//

type console_logger (cfg) =
    inherit formatted_logger (cfg)

    let interactive () = cfg.has_console && not Console.IsOutputRedirected

    override __.write_text (fg, bg, s) =
        if interactive () then
            try
                Console.ForegroundColor <- fg
                Console.BackgroundColor <- bg
                Console.Write s
            finally Console.ResetColor ()
        else Console.Write s

    override __.flush () = Console.Out.Flush ()

    override __.line_width =
        if interactive () then Some Console.BufferWidth else None

    new () = new console_logger (new config ())



// null loggers that actually prints nothing
//

type null_logger (cfg) =
    inherit logger (cfg)
    override __.write_line _ = ()
    override __.actually_print (_, _, _, _, _) = ()
