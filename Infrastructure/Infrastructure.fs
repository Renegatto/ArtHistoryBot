﻿module Infrastucture
let flip f x y = f y x
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
type IOComprehension() =
    member x.Bind(a,fn) = IO.bind fn a 
    member x.Return(a) = IO a 
    member x.Zero() = IO ()
let io = new IOComprehension()

//let k = IO 5
//let bar (x:int):int IO = IO (x+3)
//let foo (x:int): int IO = io {
//    let! g = k
//    let k = 5 + g 
//    return k
//}
