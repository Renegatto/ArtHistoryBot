module Errors
module EventHubErrors =
    type NotFoundError = NotFoundError of sid:int * details:string option
    type EventHubError =
        |NotFound of NotFoundError
        |NoNotProcessedFound
        |IndexNotFound of index:int * details:string option
module CommandHubErrors =
    type NotFoundError = NotFoundError of sid:int * details:string option
    type CommandHubError =
        |NotFound of NotFoundError
        |NoNotProcessedFound
        |IndexNotFound of index:int * details:string option

type DatabaseError = DatabaseError of string
type StorageError = StorageError of string
type NotEnoughArtworksForTestError = {expected : int; got : int}
type DomainError =
    |TestVariantIsNotExists
    |TestGeneratorNotFounded of sid:int
    |NotEnoughArtworksForTest of NotEnoughArtworksForTestError
type SubscriptionError =
    |NoSubscriptionFound of int

type Error = 
    |Database of DatabaseError
    |Storage of StorageError
    |Domain of DomainError
    |Subscription of SubscriptionError
    |EventHub of EventHubErrors.EventHubError
    |CommandHub of CommandHubErrors.CommandHubError