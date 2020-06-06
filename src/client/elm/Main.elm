port module Main exposing (Model, Msg(..), init, main, receiveMessage, sendMessage, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Error, field)
import Json.Encode as Encode
import List.Extra
import Time exposing (Posix)
import Url
import Url.Parser
import Url.Parser.Query as Query



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


port sendMessage : String -> Cmd msg


port receiveMessage : (String -> msg) -> Sub msg



-- MODEL


type TimerStatus
    = Running
    | Done
    | Paused
    | NotStarted


type Direction
    = Up
    | Down

type MessageType
    = CREATE_SESSION
    | ACTION


type alias UpdatedModel =
    { persons : List String
    , timerDuration : Int
    , elapsedTime : Int
    , timerStatus : String
    }


type alias Model =
    { persons : List String
    , newPersonInput : String
    , joinSessionInput : String
    , timerDuration : Int
    , elapsedTime : Int
    , timerStatus : TimerStatus
    , sessionId : Maybe String
    , location : String
    , error : Maybe String
    }


init : String -> ( Model, Cmd Msg )
init location =
    let
        sessionId =
            getSessionIdFromLocation location
    in
    ( { persons = []
      , newPersonInput = ""
      , joinSessionInput = ""
      , timerDuration = 10000
      , elapsedTime = 0
      , timerStatus = NotStarted
      , sessionId = sessionId
      , location = location
      , error = Nothing
      }
    , maybeJoinSessionCommand sessionId
    )


sessionIdParser =
    Query.string "s"


getSessionIdFromLocation : String -> Maybe String
getSessionIdFromLocation location =
    case Url.fromString location of
        Just url ->
            { url | path = "" }
                |> Url.Parser.parse (Url.Parser.query sessionIdParser)
                |> Maybe.withDefault Nothing

        Nothing ->
            Nothing



-- UPDATE


type Msg
    = AddPerson
    | RemovePerson String
    | NewPersonInput String
    | JoinSessionInput String
    | StartTimer
    | CreateSession
    | JoinSession
    | Tick Time.Posix
    | Pause
    | Resume
    | MovePerson String Direction
    | ReceiveMessage String
    | DismissError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPerson ->
            let
                newModel =
                    { model | persons = model.newPersonInput :: model.persons, newPersonInput = "" }
            in
            ( newModel, updateSessionCommand newModel )

        RemovePerson personToRemove ->
            let
                newModel =
                    { model | persons = List.filter (\person -> person /= personToRemove) model.persons }
            in
            ( newModel, updateSessionCommand newModel )

        MovePerson personToMove direction ->
            let
                newModel =
                    { model | persons = movePerson model.persons personToMove direction }
            in
            ( newModel, updateSessionCommand newModel )

        NewPersonInput value ->
            ( { model | newPersonInput = value }, Cmd.none )

        JoinSessionInput value ->
            ( { model | joinSessionInput = value }, Cmd.none )

        StartTimer ->
            let
                newModel =
                    { model | timerStatus = Running }
            in
            ( newModel, updateSessionCommand newModel )

        DismissError ->
            ( { model | error = Nothing }, Cmd.none )

        Tick currentTime ->
            if model.elapsedTime < model.timerDuration && model.timerStatus == Running then
                ( { model | elapsedTime = model.elapsedTime + 1000 }, Cmd.none )

            else if model.elapsedTime >= model.timerDuration && model.timerStatus == Running then
                ( { model
                    | timerStatus = Done
                    , elapsedTime = 0
                    , persons = rotate model.persons
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Pause ->
            let
                newModel =
                    { model | timerStatus = Paused }
            in
            ( newModel, updateSessionCommand newModel )

        Resume ->
            let
                newModel =
                    { model | timerStatus = Running }
            in
            ( newModel, updateSessionCommand newModel )

        CreateSession ->
            ( model, createSessionCommand )

        JoinSession ->
            ( { model | sessionId = Just model.joinSessionInput }, joinSessionCommand model.joinSessionInput )

        ReceiveMessage message ->
            let
                modelWithError =
                    ( { model | error = Just ("Could not parse message: " ++ message) }, Cmd.none )
            in
            case decodeMessageType message of
                Ok "action" ->
                    case Decode.decodeString (updatedModelDecoder model) message of
                        Ok newModel ->
                            ( newModel, Cmd.none )

                        Err _ ->
                            modelWithError

                Ok "created-session" ->
                    case Decode.decodeString (createdSessionDecoder model) message of
                        Ok newModel ->
                            ( newModel , Cmd.none )

                        Err _ ->
                            modelWithError

                _ ->
                    modelWithError


createSessionCommand : Cmd Msg
createSessionCommand =
    sendMessage
        (Encode.encode 0
            (Encode.object [ ( "type", Encode.string "create-session" ) ])
        )


updateSessionCommand : Model -> Cmd Msg
updateSessionCommand model =
    case model.sessionId of
        Just session ->
            sendMessage (encodeUpdateSessionMessage ACTION (Just session) model.persons model.timerDuration model.elapsedTime model.timerStatus)

        Nothing ->
            Cmd.none


joinSessionCommand : String -> Cmd Msg
joinSessionCommand sessionIdToJoin =
    sendMessage
        (Encode.encode 0
            (Encode.object
                [ ( "type", Encode.string "join" )
                , ( "sessionId", Encode.string sessionIdToJoin )
                ]
            )
        )


maybeJoinSessionCommand : Maybe String -> Cmd Msg
maybeJoinSessionCommand sessionId =
    case sessionId of
        Just id ->
            joinSessionCommand id

        Nothing ->
            Cmd.none


encodeUpdateSessionMessage : MessageType -> Maybe String -> List String -> Int -> Int -> TimerStatus -> String
encodeUpdateSessionMessage messageType sessionId persons timerDuration elapsedTime timerStatus =
    Encode.encode 0
        (Encode.object
            [ ( "sessionId", (Encode.string (Maybe.withDefault "" sessionId )))
            , ( "persons", Encode.list Encode.string persons )
            , ( "type", Encode.string (messageTypeToString messageType) )
            , ( "timerDuration", Encode.int timerDuration )
            , ( "elapsedTime", Encode.int elapsedTime )
            , ( "timerStatus", Encode.string (timerStatusToString timerStatus) )
            ]
        )


updatedModelDecoder : Model -> Decode.Decoder Model
updatedModelDecoder model =
    Decode.map4
        (\persons timerDuration elapsedTime timerStatus ->
            { model | persons = persons, timerDuration = timerDuration, elapsedTime = elapsedTime, timerStatus = stringToTimerStatus timerStatus }
        )
        (Decode.field "persons" (Decode.list Decode.string))
        (Decode.field "timerDuration" Decode.int)
        (Decode.field "elapsedTime" Decode.int)
        (Decode.field "timerStatus" Decode.string)


createdSessionDecoder : Model -> Decode.Decoder Model
createdSessionDecoder model =
    Decode.map
        (\sessionId -> { model | sessionId = sessionId })
        (Decode.field "sessionId" (Decode.maybe Decode.string))


decodeMessageType : String -> Result Error String
decodeMessageType message =
    Decode.decodeString (Decode.field "type" Decode.string) message


timerStatusToString : TimerStatus -> String
timerStatusToString timerStatus =
    case timerStatus of
        Running ->
            "RUNNING"

        Done ->
            "DONE"

        Paused ->
            "PAUSED"

        NotStarted ->
            "NOT_STARTED"


stringToTimerStatus : String -> TimerStatus
stringToTimerStatus str =
    case str of
        "RUNNING" ->
            Running

        "DONE" ->
            Done

        "PAUSED" ->
            Paused

        _ ->
            NotStarted

messageTypeToString : MessageType -> String
messageTypeToString messageType =
    case messageType of
        CREATE_SESSION -> "create-session"
        ACTION -> "action"

getShareUrl : String -> String -> String
getShareUrl location sessionId =
    location ++ "?s=" ++ sessionId

rotate : List String -> List String
rotate persons =
    case persons of
        [] ->
            []

        [ _ ] ->
            persons

        first :: rest ->
            rest ++ [ first ]


movePerson : List String -> String -> Direction -> List String
movePerson persons person direction =
    let
        index =
            case List.Extra.elemIndex person persons of
                Just number ->
                    number

                Nothing ->
                    -2

        friendIndex =
            case direction of
                Up ->
                    index - 1

                Down ->
                    index + 1
    in
    List.Extra.swapAt index friendIndex persons



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every 1000 Tick, receiveMessage ReceiveMessage ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ errorMessage model.error
        , viewInput "text" "Coder Name" model.newPersonInput NewPersonInput
        , button [ onClick AddPerson ] [ text "Add Coder" ]
        , addedPersons model.persons
        , button [ onClick CreateSession ] [ text "Create new session" ]
        , timer model
        , viewInput "text" "Session id" model.joinSessionInput JoinSessionInput
        , button [ onClick JoinSession ] [ text "Join Session" ]
        , invite model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


addedPersons : List String -> Html Msg
addedPersons persons =
    div []
        (List.map
            (\person ->
                ul []
                    [ li []
                        [ text person
                        , button [ onClick (MovePerson person Up) ] [ text "^" ]
                        , button [ onClick (MovePerson person Down) ] [ text "v" ]
                        , button [ onClick (RemovePerson person) ] [ text "x" ]
                        ]
                    ]
            )
            persons
        )


timer : Model -> Html Msg
timer model =
    case model.timerStatus of
        Running ->
            div []
                [ text (String.fromInt (model.timerDuration - model.elapsedTime))
                , button [ onClick Pause ] [ text "Pause timer" ]
                ]

        Paused ->
            div []
                [ text (String.fromInt (model.timerDuration - model.elapsedTime))
                , button [ onClick Resume ] [ text "Resume" ]
                ]

        Done ->
            div []
                [ text (getNextPerson model.persons ++ "s turn!")
                , button [ onClick StartTimer ] [ text "Continue Coding!" ]
                ]

        NotStarted ->
            if List.isEmpty model.persons then
                div []
                    [ text "Add coders!" ]

            else
                div []
                    [ text "Ready?"
                    , button [ onClick StartTimer ] [ text "Start Coding!" ]
                    ]


errorMessage : Maybe String -> Html Msg
errorMessage error =
    case error of
        Just s ->
            div [ style "display" "flex" ]
                [ div [ style "color" "red" ] [ text s ]
                , div [ style "cursor" "pointer", onClick DismissError ] [ text " x" ]
                ]

        Nothing ->
            div [] []

invite : Model -> Html Msg
invite model =
    case model.sessionId of
        Just s ->
            div []
            [ text ("Invite friends: " ++ (getShareUrl model.location s)) ]

        Nothing ->
            div [] []


getNextPerson : List String -> String
getNextPerson persons =
    case List.head persons of
        Just person ->
            person

        Nothing ->
            "Nobody"
