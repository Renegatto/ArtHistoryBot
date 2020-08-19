module Errors
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