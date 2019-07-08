module Main exposing (main)

import Browser
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onBlur, onClick, onFocus, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, bool, field, int, list, map2, map3, maybe, string)
import Json.Encode as Encode



-- type RemoteData suc
--     = Initial
--     | Loading
--     | Success suc
--     | Error String
-- RemoteData (Maybe (List Food))


type alias Food =
    { id : String
    , title : String
    , bought : Bool
    , active : Bool
    , changed : Bool
    , order : Int
    }


type alias Model =
    { error : Maybe String
    , groceries : Maybe (List Food)
    , newText : String
    , newItemLoading : Bool
    }


type alias FSDocuments =
    { documents : List FSDocument }


type alias FSDocument =
    { name : String
    , fields : FSDocumentFields
    }


type alias FSDocumentFields =
    { title : String
    , bought : Bool
    , order : String
    }


type Msg
    = ClearFoods
    | ToggleFoodStatus Food
    | FocusFood String
    | SetFoodText String String
    | SetNewText String
    | ReceiveFoods (Result Http.Error (Maybe (List FSDocument)))
    | ReceiveFood (Result Http.Error FSDocument)
    | ReceiveFoodUpdate (Result Http.Error FSDocument)
    | ReceiveFoodDelete (Result Http.Error ())
    | RequestNewFood
    | RequestPatchFood Food
    | RequestDeleteFood String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


base : String
base =
    "https://firestore.googleapis.com/v1/"


key : String
key =
    "?key=AIzaSyDbgLlOX3xa7dexgm0uEe_tqyWBsGf0eDc"


firestoreUrl : String -> String
firestoreUrl path =
    base ++ "projects/zucchini-246013/databases/(default)/" ++ path ++ key


getFoods : Cmd Msg
getFoods =
    Http.get
        { url = firestoreUrl "documents/foods"
        , expect = Http.expectJson ReceiveFoods documentsDecoder
        }


postFood : Food -> Cmd Msg
postFood food =
    Http.post
        { url = firestoreUrl "documents/foods"
        , body = Http.jsonBody (encodeFood food)
        , expect = Http.expectJson ReceiveFood documentDecoder
        }


patchFood : Food -> Cmd Msg
patchFood food =
    Http.request
        { method = "Patch"
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , url = base ++ food.id ++ key
        , body = Http.jsonBody (encodeFood food)
        , expect = Http.expectJson ReceiveFoodUpdate documentDecoder
        }


deleteFood : String -> Cmd Msg
deleteFood foodId =
    Http.request
        { method = "Delete"
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , url = base ++ foodId ++ key
        , body = Http.emptyBody
        , expect = Http.expectWhatever ReceiveFoodDelete
        }


encodeFood : Food -> Encode.Value
encodeFood food =
    Encode.object
        [ ( "fields"
          , Encode.object
                [ ( "title", Encode.object [ ( "stringValue", Encode.string food.title ) ] )
                , ( "bought", Encode.object [ ( "booleanValue", Encode.bool food.bought ) ] )
                , ( "order", Encode.object [ ( "integerValue", Encode.int food.order ) ] )
                ]
          )
        ]


documentsDecoder : Decoder (Maybe (List FSDocument))
documentsDecoder =
    maybe (field "documents" (list documentDecoder))


documentDecoder : Decoder FSDocument
documentDecoder =
    map2 FSDocument
        (field "name" string)
        (field "fields" fieldsDecoder)


fieldsDecoder : Decoder FSDocumentFields
fieldsDecoder =
    map3 FSDocumentFields
        (field "title" (field "stringValue" string))
        (field "bought" (field "booleanValue" bool))
        (field "order" (field "integerValue" string))


fsDocumentToFood : FSDocument -> Food
fsDocumentToFood doc =
    { id = doc.name
    , title = doc.fields.title
    , bought = doc.fields.bought
    , active = False
    , changed = False
    , order = Maybe.withDefault 0 (String.toInt doc.fields.order)
    }


fsDocumentsToFoods : List FSDocument -> List Food
fsDocumentsToFoods =
    List.map fsDocumentToFood


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


find : Food -> List Food -> Maybe Food
find food foods =
    let
        filtered =
            List.filter (\f -> f.id == food.id) foods
    in
    case List.isEmpty filtered of
        True ->
            Nothing

        False ->
            List.head filtered


extractTextFromError : Http.Error -> String
extractTextFromError err =
    case err of
        Http.BadUrl s ->
            s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus code ->
            "Status: " ++ String.fromInt code

        Http.BadBody s ->
            s


init : () -> ( Model, Cmd Msg )
init _ =
    ( { groceries = Nothing
      , error = Nothing
      , newText = ""
      , newItemLoading = False
      }
    , getFoods
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNewText t ->
            ( { model | newText = t }, Cmd.none )

        ClearFoods ->
            init ()

        SetFoodText foodId text ->
            ( { model
                | groceries =
                    Maybe.map
                        (List.map
                            (\fd ->
                                if fd.id == foodId then
                                    { fd | title = text, changed = True }

                                else
                                    fd
                            )
                        )
                        model.groceries
              }
            , Cmd.none
            )

        FocusFood foodId ->
            ( { model
                | groceries =
                    Maybe.map
                        (List.map
                            (\fd ->
                                if fd.id == foodId then
                                    { fd | active = True }

                                else
                                    { fd | active = False }
                            )
                        )
                        model.groceries
              }
            , Cmd.none
            )

        ToggleFoodStatus food ->
            let
                newFood =
                    { food | bought = not food.bought }
            in
            ( { model
                | groceries =
                    Maybe.map
                        (List.map
                            (\fd ->
                                if fd.id == food.id then
                                    newFood

                                else
                                    fd
                            )
                        )
                        model.groceries
              }
            , patchFood newFood
            )

        ReceiveFoods result ->
            case result of
                Ok groceries ->
                    case groceries of
                        Just items ->
                            ( { model | groceries = Just (fsDocumentsToFoods items) }, Cmd.none )

                        Nothing ->
                            ( { model | groceries = Nothing }, Cmd.none )

                Err err ->
                    ( { model | error = Just (extractTextFromError err) }, Cmd.none )

        ReceiveFood result ->
            case result of
                Ok foodDoc ->
                    let
                        food =
                            fsDocumentToFood foodDoc
                    in
                    case model.groceries of
                        Just items ->
                            ( { model
                                | groceries = Just (food :: items)
                                , newText = ""
                                , newItemLoading = False
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model
                                | groceries = Just [ food ]
                                , newText = ""
                                , newItemLoading = False
                              }
                            , Cmd.none
                            )

                Err err ->
                    ( { model
                        | error = Just (extractTextFromError err)
                        , newItemLoading = False
                      }
                    , Cmd.none
                    )

        ReceiveFoodDelete result ->
            case result of
                Ok _ ->
                    ( model, getFoods )

                Err _ ->
                    ( model, Cmd.none )

        ReceiveFoodUpdate result ->
            case result of
                Ok foodDoc ->
                    let
                        newFood =
                            fsDocumentToFood foodDoc
                    in
                    ( { model
                        | groceries =
                            Maybe.map
                                (List.map
                                    (\fd ->
                                        if fd.id == newFood.id then
                                            newFood

                                        else
                                            fd
                                    )
                                )
                                model.groceries
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | error = Just (extractTextFromError err) }, Cmd.none )

        RequestNewFood ->
            let
                order =
                    case model.groceries of
                        Just items ->
                            List.length items

                        Nothing ->
                            0
            in
            if String.isEmpty model.newText then
                ( model, Cmd.none )

            else
                ( { model | newItemLoading = True }, postFood (Food "" model.newText False False False order) )

        RequestPatchFood food ->
            if food.changed then
                ( model, patchFood food )

            else
                ( model, Cmd.none )

        RequestDeleteFood foodId ->
            ( model, deleteFood foodId )


navbar : Model -> Html Msg
navbar model =
    nav [ class "navbar is-primary is-fixed-top", attribute "role" "navigation", attribute "aria-label" "main navigation" ]
        [ div [ class "navbar-brand" ]
            [ div [ class "navbar-item" ] [ text "Zucchini \u{1F957}" ]
            ]
        ]


foodElem : Food -> Html Msg
foodElem food =
    div
        [ css
            [ margin (px 10)
            , Css.height (px 30)
            , transition
                [ Css.Transitions.borderColor 250
                , Css.Transitions.transform3 250 0 easeInOut
                ]
            , borderBottom3
                (px 1)
                solid
                (case food.active of
                    True ->
                        hsl 171 1 0.41

                    False ->
                        hex "d6d6d6"
                )
            , displayFlex
            ]
        ]
        [ button
            [ class "button is-small"
            , onClick (ToggleFoodStatus food)
            ]
            [ text
                (case food.bought of
                    True ->
                        "❌"

                    False ->
                        "✔️"
                )
            ]
        , input
            [ type_ "text"
            , value food.title
            , onFocus (FocusFood food.id)
            , onInput (SetFoodText food.id)
            , onBlur (RequestPatchFood food)
            , Html.Styled.Attributes.disabled food.bought
            , placeholder "Food..."
            , id food.id
            , css
                [ textTransform capitalize
                , borderStyle none
                , Css.height (pct 100)
                , Css.width (pct 100)
                , fontSize (px 16)
                , outline none
                ]
            ]
            []
        , button
            [ class "button is-small"
            , onClick (RequestDeleteFood food.id)
            ]
            [ text "🗑️"
            ]
        ]


notify : String -> Bool -> Html Msg
notify message isDanger =
    article
        [ classList [ ( "is-danger", isDanger ) ]
        , class "message"
        , css [ margin (px 10) ]
        ]
        [ div [ class "message-body" ]
            [ text message
            ]
        ]


addForm : Model -> Html Msg
addForm model =
    Html.Styled.form [ class "field has-addons", onSubmit RequestNewFood ]
        [ div [ class "control", css [ flex (num 1) ] ]
            [ input
                [ type_ "text"
                , placeholder "New..."
                , value model.newText
                , onInput SetNewText
                , class "input is-large"
                , Html.Styled.Attributes.disabled model.newItemLoading
                , css
                    [ Css.width (pct 100)
                    , borderBottom3 (px 1) solid (hsl 171 1 0.41)
                    , borderRadius (px 0)
                    ]
                ]
                []
            ]
        , div [ class "control" ]
            [ button
                [ classList [ ( "is-loading", model.newItemLoading ) ]
                , class "button is-large is-primary"
                , css [ borderRadius (px 0) ]
                , Html.Styled.Attributes.disabled (String.isEmpty model.newText)
                ]
                [ text "Add" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ navbar model
        , main_ []
            (case model.error of
                Just errortext ->
                    [ notify errortext True ]

                Nothing ->
                    [ addForm model
                    , div [ class "container" ]
                        (case model.groceries of
                            Just groceries ->
                                groceries
                                    |> List.sortBy .order
                                    |> List.reverse
                                    |> List.map foodElem

                            Nothing ->
                                [ notify "There are no items in your basket yet. Try adding some by tapping the \"Add\" button above" False
                                ]
                        )
                    ]
            )
        ]
