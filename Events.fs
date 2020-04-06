module Events

type DomainEvent =
    |NextTestSended
    |TestSolved
    |TestFailed
    |NewTestSended

type ExternalEvent =
    |UserSentMessage
    |UserEditMessage

