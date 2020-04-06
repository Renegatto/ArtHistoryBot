module Errors
type DatabaseError = DatabaseError of string
type StorageError = StorageError of string
type DomainError = DomainError of string
type SubscriptionError =
    |NoSubscriptionFound of int
type Error = 
    |Database of DatabaseError
    |Storage of StorageError
    |Domain of DomainError
    |Subscription of SubscriptionError