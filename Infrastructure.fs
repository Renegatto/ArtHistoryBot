module Infrastructure

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
    open FSharpPlus
    [<System.Obsolete("Result.map2 in FSharpPlus")>]
    let liftM2 (fn:'a -> 'b -> 'c) (a:Result<'a,_>) (b:Result<'b,_>): Result<'c,_> =
        (Result.map fn a |> Result.apply) b
    let fromOption (err:'err):'a option -> Result<'a,'err> =
        function Some x -> Ok x |_ -> Error err
    let seqList (xs: Result<'a,'b> list): Result<'a list,'b> =
        let fn = liftM2 (fun acc x -> x::acc)
        in List.fold fn (Ok []) xs
    let result (pred:'a -> bool) (err:'err) (x:'a): Result<'a,'err> =
        if pred x then Ok x else Error err
    //let mapM (f: 'a -> '``f b``) (x:Result<'a,'c>): '``f (Result b c)`` =
    let seqPair (pair:Result<'a,'c> * Result<'b,'c>):Result<'a*'b,'c> =
        pair |> uncurry (Result.map2 (fun x y -> x,y))
    let splitPair (pair:Result<'a*'b,'c>):Result<'a,'c> * Result<'b,'c> =
        match pair with
        |Ok (x,y) -> (Ok x, Ok y)
        |Error err -> (Error err,Error err)
module List =
    let rec seqResult (xs: Result<'a list,'b>): Result<'a,'b> list =
        match xs with
        |Error err  -> [Error err]
        |Ok []      -> []
        |Ok (x::xs) -> (Ok x) :: seqResult (Ok xs)
        
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
    let bind (f:'a -> 'b Async) (x:'a Async): 'b Async =
        async {
            let! x' = x
            return! f x'
        }
    let zero x: unit Async = wrap x
type AResult<'a,'err> = AResult of Asyncresult<'a,'err> 
module Asyncresult =
    open FSharpPlus
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
    let errorAsync (x:'a Async): Asyncresult<_,'a> = Async.map Error x

    let ap (fn:Asyncresult<'a -> 'b,_>) (x:Asyncresult<'a,_>): Asyncresult<'b,_> = bind (map >> apply x) fn
    let liftM2 fn x y = (map fn x |> ap) y
    let error (x:'a): Asyncresult<_,'a> = Error x |> Async.wrap
    let okAsync (x:'a Async): Asyncresult<'a,_> = Async.map Ok x
    let okIO (x:'a IO) : Asyncresult<'a,_> = IO.unwrapInsideAsync x |> ok
    let fromResult (x:Result<'a,'b>): Asyncresult<'a,'b> = Async.wrap x
    let fromOption (err:'err) (x:'a option): Asyncresult<'a,'err> = Option.toResultWith err x |> fromResult

    type M<'a,'b> = Asyncresult<'a,'b>
    let flatten (x: M<M<'a,'c>,'c>):M<'a,'c> = bind id x 
    let zero (): Asyncresult<unit,_> = ok ()
    let next (a: M<_,_>) (b:M<'a,'b>): M<'a,'b> = bind (constant b) a
    let compose (f: 'b -> M<'c,_>) (g: 'a -> M<'b,_>) (x:'a): M<'c,_> = bind f (g x)
    let andThen (f: 'a -> M<'b,_>) (g: 'b -> M<'c,_>): 'a -> M<'c,_> = compose g f
    let sequential (xs: M<'a,'b> list): M<'a list,'b> = 
        List.fold (liftM2 List.cons |> flip) (ok []) xs
        //xs |> List.toSeq |> Async.Sequential
        //|> Async.map (Seq.toList >> Result.seqList)       <- is a also valid (and built-in) but... So verbose >.<

    let asyncEndpoint (x:M<'a,'b>):Async<unit> = Async.map ignore x
    let endpoint (x:M<'a,'b>):M<unit,'b> = map ignore x // Async.sequential :: [m (Result a b)] -> m [Result a b]

    //let sequentially (xs:M<'a,'b> list):M<unit,'b> =
    //    List.fold (fun acc x -> acc *> endpoint x) (zero ()) xs
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
type Foo<'a> = Foo of 'a with
    static member map f (Foo x) = Foo (f x)
    static member bind (f:'a -> 'b Foo) (Foo (x:'a)): 'b Foo = 
        match Foo (f x) with
        |Foo y -> y 
type M<'a,'b> = AResult<'a,'b>
type AR<'a,'b> = Asyncresult<'a,'b>
module AResult =
    let private unwrap (AResult x) = x
    let toAsyncR (AResult (x:AR<'a,'b>)): AR<'a,'b> = x
    let private amap f (AResult x) = AResult (f x)
    let zero x = Asyncresult.zero x |> AResult
    let map (fn: 'a -> 'b) (x:M<'a,'e>): M<'b,'e> = amap (Asyncresult.map fn) x
    let bind (fn: 'a -> M<'b,'e>) (x:M<'a,'e>): M<'b,'e> = amap (fn >> toAsyncR |> Asyncresult.bind) x
    let apply (fn:M<'a -> 'b,_>) (x:M<'a,_>): M<'b,_> = bind (map >> apply x) fn
    let ok x = Asyncresult.ok x |> AResult
    let error x = Asyncresult.error x |> AResult
    let fromResult x = Asyncresult.fromResult x |> AResult
    let fromOption e x = Asyncresult.fromOption e x |> AResult
    let okAsync x = Asyncresult.okAsync x |> AResult
    let errorAsync x = Asyncresult.errorAsync x |> AResult
    let okIO x = Asyncresult.okIO x |> AResult
    let endpoint (x:AResult<'a,'b>): AResult<unit,'b> = amap Asyncresult.endpoint x
    let asyncEndpoint x = Asyncresult.asyncEndpoint (unwrap x)
    let sequential (xs:M<'a,'b> list):M<'a list,'b> = 
        Asyncresult.sequential (List.map unwrap xs)
        |> AResult
    let mapAsync f x = amap f x
type AResultComprehension() = 
    member x.Bind(a,fn) = AResult.bind fn a 
    member x.Return(a) = AResult.ok a 
    member x.Zero() = AResult.zero ()
    member x.ReturnFrom(a) = a
let aresult = new AResultComprehension()
type Bar<'a> = Bar of 'a with
    static member map f (Bar x) = Bar (f x)
    static member bind (f:'a -> 'b Bar) (Bar (x:'a)): 'b Bar = 
        match Bar (f x) with
        |Bar y -> y 
module Operators = 
    module Internals = 
        open FSharpPlus
        //open FSharpPlus.Control
        type Map' =
            inherit FSharpPlus.Internals.Default1
            static member Map ((x: Foo<_>,f), _mthd: Map') = Foo.map f x
            static member Map ((x: Bar<_>,f), _mthd: Map') = Bar.map f x
            static member Map ((x: AResult<_,_>,f), _mthd: Map') = AResult.map f x
            static member Map ((x: Result<_,_>,f), _mthd: Map') = Result.map f x
            static member Map ((x: option<_>,f), _mthd: Map') = Option.map f x
            static member Map ((x: Async<_>,f), _mthd: Map') = Async.map f x
            static member Map ((x: IO<_>,f), _mthd: Map') = IO.map f x

            static member inline Invoke (mapping: 'T->'U) (source: '``Functor<'T>``) : '``Functor<'U>`` = 
                let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Map : (_*_)*_ -> _) (source, mapping), mthd)
                call (Unchecked.defaultof<Map'>, source, Unchecked.defaultof<'``Functor<'U>``>)  


        type Bind' =
            static member (>>=) (source: Bar<'T>, f:'T -> Bar<'U>) = Bar.bind f source
            static member (>>=) (source: Foo<'T>, f:'T -> Foo<'U>) = Foo.bind f source
            static member (>>=) (source: AResult<'T,'a>, f:'T -> AResult<'U,'a>) = AResult.bind f source
            static member (>>=) (source: IO<'T>, f:'T -> IO<'U>) = IO.bind f source
            static member (>>=) (source: Result<'T,'E>, f:'T -> Result<'U,'E>) = Result.bind f source
            static member (>>=) (source: option<'T>, f:'T -> option<'U>) = Option.bind f source
            static member (>>=) (source: Async<'T>, f:'T -> Async<'U>) = Async.bind f source

            static member inline Invoke (source: '``Monad<'T>``) (binder: 'T -> '``Monad<'U>``) : '``Monad<'U>`` =
                let inline call (_mthd: 'M, input: 'I, _output: 'R, f) = ((^M or ^I or ^R) : (static member (>>=) : _*_ -> _) input, f)
                call (Unchecked.defaultof<Bind'>, source, Unchecked.defaultof<'``Monad<'U>``>, binder)
            //CTRL+C; CTRL+V :thumbup:
        open System.Runtime.InteropServices
        type Apply' =
            static member Apply (f: list<_> , x: list<'T> ,
                                    [<Optional>]_output: list<'U> , 
                                    [<Optional>]_mthd: Apply') = List.apply f x
            static member Apply (f: Result<_,_> , x: Result<'T,_> ,
                                    [<Optional>]_output: Result<_,_> , 
                                    [<Optional>]_mthd: Apply') = Result.apply f x
             static member Apply (f: AResult<_,_> , x: AResult<'T,_> ,
                                    [<Optional>]_output: AResult<'U,_> , 
                                    [<Optional>]_mthd: Apply') = AResult.apply f x  
            static member Apply (f: Option<_> , x: Option<'T> ,
                                    [<Optional>]_output: Option<'U> , 
                                    [<Optional>]_mthd: Apply') = Option.apply f x
            static member Apply (f: IO<_> , x: IO<'T> ,
                                    [<Optional>]_output: IO<'U> , 
                                    [<Optional>]_mthd: Apply') = IO.ap f x
            static member Apply (f: Async<_> , x: Async<'T> ,
                                    [<Optional>]_output: Async<'U> , 
                                    [<Optional>]_mthd: Apply') = Async.apply f x

            static member inline Invoke (f: '``Applicative<'T -> 'U>``) (x: '``Applicative<'T>``) : '``Applicative<'U>`` =
                let inline call (mthd : ^M, input1: ^I1, input2: ^I2, output: ^R) =
                    ((^M or ^I1 or ^I2 or ^R) : (static member Apply : _*_*_*_ -> _) input1, input2, output, mthd)
                call(Unchecked.defaultof<Apply'>, f, x, Unchecked.defaultof<'``Applicative<'U>``>)
  
    open Internals

    let inline map (f:'T -> 'U) (x:'``Functor<'T>``): '``Functor<'U>`` = Map'.Invoke f x
    let inline ( <!> ) (f:'T -> 'U) (x:'``Functor<'T>``): '``Functor<'U>`` = Map'.Invoke f x
    let inline ( <<| ) (f:'T -> 'U) (x:'``Functor<'T>``): '``Functor<'U>`` = Map'.Invoke f x
    let inline ( |>> ) (x:'``Functor<'T>``) (f:'T -> 'U): '``Functor<'U>`` = Map'.Invoke f x

    let inline (<*>) (f: '``Applicative<'T -> 'U>``) (x: '``Applicative<'T>``) : '``Applicative<'U>`` = Apply'.Invoke f x : '``Applicative<'U>``
    let inline ( *>) (x: '``Applicative<'T>``) (y: '``Applicative<'U>``) : '``Applicative<'U>`` = 
        ((fun (_: 'T) (k: 'U) -> k) <!> x : '``Applicative<'U->'U>``) <*> y
    let inline (<* ) (y: '``Applicative<'U>``) (x: '``Applicative<'T>``) : '``Applicative<'U>`` = ((fun (_: 'T) (k: 'U) -> k) <!>  x : '``Applicative<'U->'U>``) <*> y
 

    let inline bind  (f:'T -> '``Monad<'U>``) (x:'``Monad<'T>``): '``Monad<'U>`` = Bind'.Invoke x f
    let inline (=<<) (f:'T -> '``Monad<'U>``) (x:'``Monad<'T>``): '``Monad<'U>`` = Bind'.Invoke x f
    let inline (>>=) (x:'``Monad<'T>``) (f:'T -> '``Monad<'U>``): '``Monad<'U>`` = Bind'.Invoke x f
    let inline join (x: '``Monad<Monad<'T>>``) : '``Monad<'T>`` = Bind'.Invoke x id


    let baz (x:int Foo):int Foo = x >>= (fun x -> Foo <| x + 1)
    let boo (x:int Bar):int Bar = (fun x -> Bar <| x + 1) =<< x
    let goo (x:int option): int option = (fun x -> Some <| x + 1) =<< x