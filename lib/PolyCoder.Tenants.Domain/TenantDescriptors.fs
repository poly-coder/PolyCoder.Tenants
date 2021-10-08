[<RequireQualifiedAccess>]
module PolyCoder.Tenants.Domain.TenantDescriptors

type Data = {
  title: string
  description: string
}

type Event =
  | WasUpdated of data: Data

type Command =
  | Update of data: Data

module Validate =
  open AccidentalFish.FSharp.Validation

  [<Literal>]
  let MinTitleLength = 5

  [<Literal>]
  let MaxTitleLength = 80

  [<Literal>]
  let MaxDescriptionLength = 1000

  let data strings =
    createValidatorFor<Data>() {
      validate (fun d -> d.title) [
        Validators.isNotEmptyOrWhiteSpace strings
        Validators.isNotShorterThan MinTitleLength strings
        Validators.isNotLongerThan MaxTitleLength strings
      ]

      validate (fun d -> d.description) [
        Validators.isNotLongerThan MaxDescriptionLength strings
      ]
    }

  let command strings = function
    | Update d -> d |> data strings

module Aggregate =
  open AccidentalFish.FSharp.Validation

  type State = {
    title: string
    description: string
  }

  let initialState : State = {
    title = ""
    description = ""
  }

  let apply state event : State =
    match event with
    | WasUpdated data ->
      { state with title = data.title
                   description = data.description }

  let fold state = Seq.fold apply state

  let internal mustUpdate (state: State) (data: Data) =
    data.title <> state.title && data.title <> state.title

  let handle strings (state: State) (command: Command) =
    match Validate.command strings command, command with
    | Errors es, _ ->
      Result.Error es

    | Ok, Update data when data |> mustUpdate state ->
      Result.Ok [ WasUpdated { title = data.title; description = data.description } ]

    | Ok, Update _ ->
      Result.Ok []

module ListView =
  type Item = { title: string }

  let empty : Item = { title = "" }

  type ViewCommand =
    | UpdateItem of (Item -> Item)

  let project (event: Event) =
    match event with
    | WasUpdated data ->
      UpdateItem (fun item -> { item with title = data.title })

module DetailsView =
  type Details = {
    title: string
    description: string
  }

  let empty : Details = {
    title = ""
    description = ""
  }

  type ViewCommand =
    | UpdateDetails of (Details -> Details)

  let project (event: Event) =
    match event with
    | WasUpdated data ->
      UpdateDetails (fun item -> { item with title = data.title
                                             description = data.description})

module PrintUpdates =
  type ProcessCommand =
    | LogMessage of string

  let handle event =
    match event with
    | WasUpdated data ->
      LogMessage (sprintf "Tenant %s was updated" data.title)
