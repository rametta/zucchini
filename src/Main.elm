module Main exposing (main)

import Browser
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onBlur, onClick, onFocus, onInput)
import Http
import Json.Decode exposing (Decoder, bool, field, list, map2, string)
import Json.Encode as Encode



-- type RemoteData suc
--     = Initial
--     | Fetching
--     | Success suc
--     | Error String
-- RemoteData (Maybe (List Food))


type alias Food =
    { id : String
    , title : String
    , bought : Bool
    , active : Bool
    }


type alias Model =
    { error : Maybe String
    , groceries : Maybe (List Food)
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
    }


type Msg
    = ClearFoods
    | ToggleFoodStatus Food
    | FocusFood String
    | SetFoodText String String
    | ReceiveFoods (Result Http.Error (List FSDocument))
    | ReceiveFood (Result Http.Error FSDocument)
    | ReceiveFoodUpdate (Result Http.Error FSDocument)
    | RequestNewFood
    | RequestPatchFood Food


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


postFood : Cmd Msg
postFood =
    Http.post
        { url = firestoreUrl "documents/foods"
        , body = Http.jsonBody (encodeFood (Food "" "" False False))
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


encodeFood : Food -> Encode.Value
encodeFood food =
    Encode.object
        [ ( "fields"
          , Encode.object
                [ ( "title", Encode.object [ ( "stringValue", Encode.string food.title ) ] )
                , ( "bought", Encode.object [ ( "booleanValue", Encode.bool food.bought ) ] )
                ]
          )
        ]


documentsDecoder : Decoder (List FSDocument)
documentsDecoder =
    field "documents" (list documentDecoder)


documentDecoder : Decoder FSDocument
documentDecoder =
    map2 FSDocument
        (field "name" string)
        (field "fields" fieldsDecoder)


fieldsDecoder : Decoder FSDocumentFields
fieldsDecoder =
    map2 FSDocumentFields
        (field "title" (field "stringValue" string))
        (field "bought" (field "booleanValue" bool))


fsDocumentToFood : FSDocument -> Food
fsDocumentToFood doc =
    { id = doc.name
    , title = doc.fields.title
    , bought = doc.fields.bought
    , active = False
    }


fsDocumentsToFoods : List FSDocument -> List Food
fsDocumentsToFoods =
    List.map fsDocumentToFood


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


createFood : Food
createFood =
    Food "" "" False False


init : () -> ( Model, Cmd Msg )
init _ =
    ( { groceries = Nothing
      , error = Nothing
      }
    , getFoods
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearFoods ->
            ( { groceries = Nothing, error = Nothing }, Cmd.none )

        SetFoodText foodId text ->
            ( { model
                | groceries =
                    Maybe.map
                        (List.map
                            (\fd ->
                                if fd.id == foodId then
                                    { fd | title = text }

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
                    ( { model | groceries = Just (fsDocumentsToFoods groceries) }, Cmd.none )

                Err errText ->
                    ( { model | error = Just "Could not get saved food..." }, Cmd.none )

        ReceiveFood result ->
            case result of
                Ok foodDoc ->
                    let
                        food =
                            fsDocumentToFood foodDoc
                    in
                    case model.groceries of
                        Just items ->
                            ( { model | groceries = Just (food :: items) }, Cmd.none )

                        Nothing ->
                            ( { model | groceries = Just [ food ] }, Cmd.none )

                Err _ ->
                    ( { model | error = Just "Could not create new food..." }, Cmd.none )

        ReceiveFoodUpdate result ->
            case result of
                Ok foodDoc ->
                    ( model, Cmd.none )

                Err _ ->
                    ( { model | error = Just "Could not update existing food..." }, Cmd.none )

        RequestNewFood ->
            ( model, postFood )

        -- TODO: only patch food if it has changed
        RequestPatchFood food ->
            ( model, patchFood food )


navbar : Model -> Html Msg
navbar model =
    nav [ class "navbar is-primary is-fixed-top", attribute "role" "navigation", attribute "aria-label" "main navigation" ]
        [ div [ class "navbar-brand", css [ Css.width (pct 100) ] ]
            [ div [ class "navbar-item" ] [ text "Zucchini \u{1F957}" ]
            , div [ css [ marginLeft auto, marginRight (px 10), marginTop (px 8) ] ]
                [ button
                    [ type_ "button"
                    , class "button"
                    , onClick ClearFoods
                    , Html.Styled.Attributes.disabled
                        (case model.error of
                            Just _ ->
                                True

                            Nothing ->
                                False
                        )
                    ]
                    [ text "♻️" ]
                ]
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
                        hsl 171 100 0.41

                    False ->
                        hex "d6d6d6"
                )
            , displayFlex
            ]
        ]
        [ input
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
        ]


addButton : Html Msg
addButton =
    button
        [ css
            [ position fixed
            , bottom (px 10)
            , right (px 10)
            , boxShadow4 (px 0) (px 10) (px 20) (rgba 0 0 0 0.19)
            ]
        , class "button is-primary"
        , onClick RequestNewFood
        ]
        [ text "Add" ]


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
                    [ div [ class "container" ]
                        (case model.groceries of
                            Just groceries ->
                                List.map foodElem groceries

                            Nothing ->
                                [ notify "There are no items in your basket yet. Try adding some by tapping the \"Add\" button in the bottom right" False
                                ]
                        )
                    , addButton
                    ]
            )
        ]
