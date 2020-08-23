module DomainTypes
type Image = string
type Variant = int
type VariantsCount = int
type Filename = string
type Artwork =
    {
        image : Image
        author : string
        about : string
    }
type AnswerVariant =
    {
        artwork : Artwork
        variant : Variant
    }
    static member sort (variants: AnswerVariant list): AnswerVariant list =
        List.sortBy (fun x -> x.variant) variants
type TestResult =
    |Success 
    |Fail of AnswerVariant
type Test =
    {
        right_variant : AnswerVariant
        all_variants : AnswerVariant list
    }
open Infrastructure
open Errors
type TestGenerator = TestGenerator of (unit -> AResult<Test,Error>)
type SubscriptionId = SubscriptionId of int