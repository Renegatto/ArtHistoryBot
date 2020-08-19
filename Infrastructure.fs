module Infrastructure
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
    let ok (x:'a): Asyncresult<'a,_> = async {
        return Ok x
    }
    let map (f:'a -> 'b) (x: Asyncresult<'a,'c>): Asyncresult<'b,'c> =
        bind (f >> ok) x
    let error (x:'a): Asyncresult<_,'a> = async {
        return Error x
    }
    let okAsync (x:'a Async): Asyncresult<'a,_> = async {
        let! y = x
        return Ok y
    }
    let errorAsync (x:'a Async): Asyncresult<_,'a> = async {
        let! y = x
        return Error y
    }
    let okIO (x:'a IO) : Asyncresult<'a,_> = async {
        return IO.unwrapInsideAsync x |> Ok
    } 
    let zero (): Asyncresult<unit,_> = ok ()
    let fromResult (x:Result<'a,'b>): Asyncresult<'a,'b> = async { return x }
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
