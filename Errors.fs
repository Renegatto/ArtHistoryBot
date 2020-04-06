module Errors
type DatabaseError = DatabaseError of string
type StorageError = StorageError of string
type DomainError =
    |TestVariantIsNotExists
    |TestGeneratorNotFounded of sid:int
type SubscriptionError =
    |NoSubscriptionFound of int
type Error = 
    |Database of DatabaseError
    |Storage of StorageError
    |Domain of DomainError
    |Subscription of SubscriptionError