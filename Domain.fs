module Domain
open FSharpPlus
open DomainTypes
open Errors

let guessResult (command:Commands.GuessResultCommand): Result<Events.DomainEvent list,Error> =
    match command.answer < List.length command.test.all_variants with
    |false -> Domain TestVariantIsNotExists |> Error
    |true ->
        match command.answer = command.test.right_variant.variant with
        |true -> Ok [Events.TestSolved {
                sid = command.sub_id
                test = command.test
                answer = command.answer
            }]
        |false -> Ok [Events.TestFailed {
            sid = command.sub_id
            test = command.test
            answer = command.answer
            }]

let newTest (command:Commands.NewTestCommand) generator test: Result<Events.DomainEvent list,Error> =
    [ Events.NewQuizStarted {
        sid = command.sub_id
        variants_count = command.variants_count
        generator = generator   
    };Events.TestSended {
        sid = command.sub_id
        test = test 
    }] |> Ok

let nextTest (command:Commands.NextTestCommand) test: Result<Events.DomainEvent list,Error> =
    [ Events.TestSended {
            sid = command.sub_id
            test = test 
    }] |> Ok

//let attemptToGuess (test:Test) (variant:Variant): TestResult = //what's going on here? O_o
//    List.filter (fun x -> x.variant = variant) test.all_variants
//    |> List.head
//    |> fun x -> x = test.right_variant 
//    |> function
//        |true -> Success
//        |false -> Fail test.right_variant
//let testBuilder (storage_name:Filename) (count:int IO): unit IO -> Test IO =
    //let file: = 