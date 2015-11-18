(*
 * F# Common Library
 * Threading.ml: multithreading facilities
 * (C) 2015 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
module FSharp.Common.Threading

open System
open System.Threading

/// Helper that can be used for writing CPS-style code that resumes
/// on the same thread where the operation was started.
let internal synchronize f = 
    let ctx = System.Threading.SynchronizationContext.Current 
    in
        f <| fun g ->
            let nctx = System.Threading.SynchronizationContext.Current 
            in
                if ctx <> null && ctx <> nctx then ctx.Post ((fun _ -> g ()), null)
                else g ()

#nowarn "21"
#nowarn "40"
type Microsoft.FSharp.Control.Async with 
    /// Behaves like AwaitObservable, but calls the specified guarding function
    /// after a subscriber is registered with the observable.
    static member GuardedAwaitObservable (ev1 : IObservable<_>) guardFunction =
        synchronize <| fun f ->
            Async.FromContinuations <| fun (cont, econt, ccont) ->
                let rec finish cont value = 
                    remover.Dispose()
                    f (fun () -> cont value)
                and remover : IDisposable = 
                    ev1.Subscribe
                        { new IObserver<_> with
                            member __.OnNext v = finish cont v
                            member __.OnError e = finish econt e
                            member __.OnCompleted () = finish ccont (new System.OperationCanceledException ("Cancelling the workflow, because the Observable awaited using AwaitObservable has completed.")) }
                in
                    guardFunction ()

module Async = 
    /// Returns an asynchronous workflow 'Async<Async<unit>>'. When called
    /// using 'let!', it starts the workflow provided as an argument and returns
    /// a token that can be used to cancel the started work - this is an
    /// (asynchronously) blocking operation that waits until the workflow
    /// is actually cancelled 
    let StartCancellable work =
        async {
            let cts = new CancellationTokenSource ()
            let evt = new Event<_> ()
            Async.Start (Async.TryCancelled (work, ignore >> evt.Trigger), cts.Token)
            let waitForCancel = Async.GuardedAwaitObservable evt.Publish cts.Cancel
            return async.TryFinally (waitForCancel, cts.Dispose)
        }


