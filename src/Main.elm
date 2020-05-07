port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import ListType exposing (ListType)
import MultiInput



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port itemsUpdate : List String -> Cmd msg


port itemsSaved : (Encode.Value -> msg) -> Sub msg



-- MODEL


type alias Flags =
    { items : List String
    , list_type : String
    }


type alias Model =
    { currentItems : List String
    , savedItems : List String
    , updateAllowed : Bool
    , list_type : ListType
    , state : MultiInput.State
    }


newState : ListType -> MultiInput.State
newState list_type =
    MultiInput.init <| ListType.tagId list_type


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        list_type =
            ListType.fromString flags.list_type
    in
    ( { currentItems = flags.items
      , savedItems = flags.items
      , updateAllowed = True
      , list_type = list_type
      , state = newState list_type
      }
    , Cmd.none
    )


defaultSeparators : List String
defaultSeparators =
    [ "\n", "\t", " ", "," ]



-- UPDATE


type Msg
    = MultiInputMsg MultiInput.Msg
    | Saved Encode.Value



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MultiInputMsg multiInputMsg ->
            if model.updateAllowed then
                let
                    ( nextState, nextItems, nextCmd ) =
                        MultiInput.update { separators = defaultSeparators } multiInputMsg model.state model.currentItems

                    acceptedItems =
                        List.filter (ListType.validator model.list_type) nextItems

                    shouldSave =
                        (nextState.nextItem == "")
                            && (nextItems /= model.currentItems)
                            && (nextItems == acceptedItems)
                            && (acceptedItems /= model.savedItems)

                    updatedList =
                        if shouldSave then
                            itemsUpdate acceptedItems

                        else
                            Cmd.none

                    savedItems =
                        if shouldSave then
                            acceptedItems

                        else
                            model.savedItems
                in
                ( { model
                    | currentItems = nextItems
                    , savedItems = savedItems
                    , state = nextState
                    , updateAllowed = updatedList == Cmd.none
                  }
                , Cmd.batch
                    [ Cmd.map MultiInputMsg nextCmd
                    , updatedList
                    ]
                )

            else
                ( model, Cmd.none )

        Saved _ ->
            ( { model | updateAllowed = True }
            , Cmd.none
            )



-- SUBSCRIPTIONS
-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ MultiInput.subscriptions model.state
            |> Sub.map MultiInputMsg
        , itemsSaved Saved
        ]



-- VIEW


view : Model -> Html Msg
view model =
    MultiInput.view
        { placeholder = ListType.placeholder model.list_type
        , toOuterMsg = MultiInputMsg
        , isValid = ListType.validator model.list_type
        }
        []
        model.currentItems
        model.state
