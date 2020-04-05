module Infrastucture
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
