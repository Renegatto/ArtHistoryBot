﻿module Infrastructure
open FSharpPlus
let flip f x y = f y x
let constant x _ = x
let apply x f = f x
let uncurry f (x,y) = f x y
let tryMatch p x = if p x then Some x else None
module InterfaceTools =

    let freeIndex (xs:'a option []):int option =
        Array.tryFindIndex (function None -> true |_ -> false) xs

    let addInto (xs: 'a option [] ref) (x:'a): int =
        let xs' = xs.contents
        match freeIndex xs' with
        |Some index -> 
            Array.set xs' index (Some x)
            xs.contents <- xs'
            index
        |None ->
            xs.contents <- Array.append [|Some x|] xs'
            //printfn "appending %A %A" x xs
            Array.length xs'
module Result =

    [<System.Obsolete("Result.map2 in FSharpPlus")>]
    let liftM2 (fn:'a -> 'b -> 'c) (a:Result<'a,_>) (b:Result<'b,_>): Result<'c,_> =
        (Result.map fn a |> Result.apply) b
    let fromOption (err:'err):'a option -> Result<'a,'err> =
        function Some x -> Ok x |_ -> Error err
    let seqList (xs: Result<'a,'b> list): Result<'a list,'b> =
        let fn = liftM2 (fun acc x -> x::acc)
        in List.fold fn (Ok []) xs
type IO<'a> = IO of 'a with

    //this kind of IO can join to Async to avoid complicated code
    //a main proposal of this IO is to mark 'dirty' code
    //it can join to Async because Async marks 'dirty' code too, here is no sense to
    //double 'marking' layers

    static member private extract<'a> (IO x): 'a = x

    static member bind (fn:'a -> 'b IO) : 'a IO -> 'b IO = fn<< IO.extract
    static member map (fn: 'a -> 'b) : 'a IO -> 'b IO = IO.bind (IO<<fn)
    static member ap (fn: ('a -> 'b) IO) (x:'a IO): 'b IO = IO.bind (flip IO.map x) fn

    static member map2 (fn:'a -> 'b -> 'c) (x:'a IO): 'b IO -> 'c IO = IO.ap (IO.map fn x)
    static member map3 (fn:'a->'b->'c->'d) (a:'a IO) (b: 'b IO): 'c IO -> 'd IO = IO.ap (IO.map2 fn a b)
    static member map4 (fn:'a->'b->'c->'d->'e) (a:'a IO) (b: 'b IO) (c: 'c IO): 'd IO -> 'e IO = IO.ap (IO.map3 fn a b c)

    static member join: 'a IO IO -> 'a IO = IO.extract
    static member joinAsync: Async<'a> IO -> Async<'a> = IO.extract // <-- non monadic
    static member sequence :'a IO list -> 'a list IO =
        IO << List.map IO.extract
    static member traverse (fn: 'a -> 'b IO): 'a list -> 'b list IO =
        IO << List.map (IO.extract<<fn)
    static member unwrapInsideAsync<'a> (x: 'a IO ): 'a = IO.extract x
    static member compose (f: 'b -> 'c IO) (g: 'a -> 'b IO) (x:'a): 'c IO =
        IO.bind f <| g x
    static member andThen (f: 'a -> 'b IO) (g: 'b -> 'c IO) : 'a -> 'c IO =
        IO.compose g f
type IOComprehension() =
    member x.Bind(a,fn) = IO.bind fn a 
    member x.Return(a) = IO a 
    member x.Zero() = IO ()
let io = new IOComprehension()

type ResultComprehension() =
    member x.Bind(a,fn) = Result.bind fn a 
    member x.Return(a) = Ok a 
    member x.Zero() = Ok ()
let result = new ResultComprehension()

type OptionComprehension() = 
    member x.Bind(a,fn) = Option.bind fn a 
    member x.Return(a) = Some a 
    member x.Zero() = Some ()
    member x.ReturnFrom(a) = a
let option = new OptionComprehension()

type Asyncresult<'a,'b> = Async<Result<'a,'b>>
module Async =
    let wrap (x:'a):'a Async = async {return x}
module Asyncresult =
    let bind (fn: 'a -> Asyncresult<'b,'c>) (x:Asyncresult<'a,'c>): Asyncresult<'b,'c> = async {
        let! arg1 = x
        match arg1 with
        |Ok a ->
            let! solvedfn = fn a
            return solvedfn
        |Error b ->
            return Error b
    }
    let ok (x:'a): Asyncresult<'a,_> = Ok x |> Async.wrap
    let map (f:'a -> 'b) (x: Asyncresult<'a,'c>): Asyncresult<'b,'c> = bind (f >> ok) x
    let ap (fn:Asyncresult<'a -> 'b,_>) (x:Asyncresult<'a,_>): Asyncresult<'b,_> = bind (map >> apply x) fn
    let liftM2 fn x y = (map fn x |> ap) y
    let error (x:'a): Asyncresult<_,'a> = Error x |> Async.wrap
    let okAsync (x:'a Async): Asyncresult<'a,_> = Async.map Ok x
    let errorAsync (x:'a Async): Asyncresult<_,'a> = Async.map Error x
    let okIO (x:'a IO) : Asyncresult<'a,_> = IO.unwrapInsideAsync x |> ok
    let fromResult (x:Result<'a,'b>): Asyncresult<'a,'b> = Async.wrap x
    let fromOption (err:'err) (x:'a option): Asyncresult<'a,'err> = Option.toResultWith err x |> fromResult

    type M<'a,'b> = Asyncresult<'a,'b>
    let flatten (x: M<M<'a,'c>,'c>):M<'a,'c> = bind id x 
    let zero (): Asyncresult<unit,_> = ok ()
    let next (a: M<_,_>) (b:M<'a,'b>): M<'a,'b> = bind (constant b) a
    let compose (f: 'b -> M<'c,_>) (g: 'a -> M<'b,_>) (x:'a): M<'c,_> = bind f (g x)
    let andThen (f: 'a -> M<'b,_>) (g: 'b -> M<'c,_>): 'a -> M<'c,_> = compose g f
    let seqList (a: M<'a,'b> list): M<'a list,'b> = 
        List.fold (liftM2 List.cons |> flip) (ok []) a 
type AsyncResultComprehension() = 
    member x.Bind(a,fn) = Asyncresult.bind fn a 
    member x.Return(a) = Asyncresult.ok a 
    member x.Zero() = Asyncresult.zero ()
    member x.ReturnFrom(a) = a
    //member x.TryWith(y:Asyncresult<'a,'b>,fn:('a -> Asyncresult<'a,'b>)): Asyncresult<'a,'b> = async {
    //    try
    //        return fn y
    //    with
    //    |ex -> return Error
    //}
    //member x.Delay(fn) = fn ()

let asyncresult = new AsyncResultComprehension()
//let bar x = x+1
//let foo: Asyncresult<int,float> = asyncresult {
//    try
//        return bar 2 
//    with
//    |_ -> Asyncresult.error 7.44
//}
