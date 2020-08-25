module Domain
open FSharpPlus
open DomainTypes
open Errors

let guessResult (command:Commands.GuessResultCommand): Result<Events.DomainEvent list,Error> =
    match command.answer < List.length command.test.all_variants with
    |false -> Domain TestVariantIsNotExists |> Result.Error
    |true ->
        match command.answer = command.test.right_variant.variant with
        |true  -> [Events.TestSolved { test = command.test; answer = command.answer}]
        |false -> [Events.TestFailed { test = command.test; answer = command.answer}]
        |> Result.Ok
    
let newTest (command:Commands.NewTestCommand) generator test: Result<Events.DomainEvent list,Error> =
    [ 
        Events.NewQuizStarted {
            variants_count = command.variants_count
            generator = generator   
        };
        Events.TestSended {
            test = test 
        };
    ] |> Ok

let nextTest (command:Commands.NextTestCommand) test: Result<Events.DomainEvent list,Error> =
    [ Events.TestSended { test = test }] |> Ok

let showTest (command:Commands.ShowTestCommand): Result<Events.TestShowedEvent,Error> =
    let variants = command.test.all_variants
    let format_variant (variant: AnswerVariant) =
        sprintf "%A. %A\n" variant.variant variant.artwork.author

    List.map format_variant variants
    |> List.reduce (+)
    |> fun msg -> Ok {test=command.test;message=msg}

//let attemptToGuess (test:Test) (variant:Variant): TestResult = //what's going on here? O_o
//    List.filter (fun x -> x.variant = variant) test.all_variants
//    |> List.head
//    |> fun x -> x = test.right_variant 
//    |> function
//        |true -> Success
//        |false -> Fail test.right_variant
//let testBuilder (storage_name:Filename) (count:int IO): unit IO -> Test IO =
    //let file: = 