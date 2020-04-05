module Queries
open Commands

type Query<'a> = Query of 'a

type QueryHandler<'a> = QueryHandler of (Query<'a> -> Result<'a,Error>)