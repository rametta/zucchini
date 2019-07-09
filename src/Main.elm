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


type Status
    = Loading
    | Complete


type alias Food =
    { id : String
    , title : String
    , bought : Bool
    , active : Bool
    , changed : Bool
    , order : Int
    }


type alias Model =
    { initialLoad : Status
    , error : Maybe String
    , groceries : List Food
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
    = ToggleFoodStatus Food
    | FocusFood Food
    | SetFoodText Food String
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


key : String
key =
    "?key=AIzaSyDbgLlOX3xa7dexgm0uEe_tqyWBsGf0eDc"


firestoreUrl : String -> String
firestoreUrl path =
    "/api/v1/projects/zucchini-246013/databases/(default)/" ++ path ++ key


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
        , url = "/api/v1/" ++ food.id ++ key
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
        , url = "/api/v1/" ++ foodId ++ key
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


find : Food -> List Food -> Maybe Food
find food =
    List.head << List.filter (\f -> f.id == food.id)


sortFoods : List Food -> List Food
sortFoods =
    List.reverse << List.sortBy .order


replaceFood : Food -> List Food -> List Food
replaceFood food =
    List.map
        (\fd ->
            if fd.id == food.id then
                food

            else
                fd
        )


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { initialLoad = Loading
      , groceries = []
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

        SetFoodText food text ->
            let
                newFood =
                    { food | title = text, changed = True }
            in
            ( { model | groceries = replaceFood newFood model.groceries }, Cmd.none )

        FocusFood food ->
            ( { model
                | groceries =
                    List.map
                        (\fd ->
                            if fd.id == food.id then
                                { fd | active = True }

                            else
                                { fd | active = False }
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
            ( { model | groceries = replaceFood newFood model.groceries }, patchFood newFood )

        ReceiveFoods result ->
            case result of
                Ok groceries ->
                    case groceries of
                        Just items ->
                            ( { model
                                | groceries = items |> fsDocumentsToFoods |> sortFoods
                                , initialLoad = Complete
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | groceries = [], initialLoad = Complete }, Cmd.none )

                Err err ->
                    ( { model | error = Just (extractTextFromError err), initialLoad = Complete }, Cmd.none )

        ReceiveFood result ->
            case result of
                Ok foodDoc ->
                    let
                        food =
                            fsDocumentToFood foodDoc
                    in
                    ( { model
                        | groceries = sortFoods (food :: model.groceries)
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
                    ( model, Cmd.none )

                Err err ->
                    ( { model | error = Just (extractTextFromError err) }, getFoods )

        ReceiveFoodUpdate result ->
            case result of
                Ok foodDoc ->
                    let
                        newFood =
                            fsDocumentToFood foodDoc
                    in
                    ( { model | groceries = replaceFood newFood model.groceries }, Cmd.none )

                Err err ->
                    ( { model | error = Just (extractTextFromError err) }, Cmd.none )

        RequestNewFood ->
            if String.isEmpty model.newText then
                ( model, Cmd.none )

            else
                ( { model | newItemLoading = True }
                , postFood
                    { id = ""
                    , title = model.newText
                    , bought = False
                    , active = False
                    , changed = False
                    , order = List.length model.groceries + 1
                    }
                )

        RequestPatchFood food ->
            if food.changed then
                ( model, patchFood food )

            else
                ( model, Cmd.none )

        RequestDeleteFood foodId ->
            let
                newFoods =
                    List.filter (\food -> not (food.id == foodId)) model.groceries
            in
            ( { model | groceries = newFoods }, deleteFood foodId )


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
            [ transition
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
                        hex "ededed"
                )
            , displayFlex
            , alignItems center
            ]
        ]
        [ button
            [ class "button"
            , css [ borderRadius (pct 50), margin (px 5) ]
            , onClick (ToggleFoodStatus food)
            , attribute "aria-label" "toggle"
            ]
            [ case food.bought of
                True ->
                    span [ css [ color (hex "00d1b2") ], class "icon" ] [ i [ class "fas fa-check fa-lg" ] [] ]

                False ->
                    span [ class "icon" ] []
            ]
        , case food.bought of
            True ->
                div
                    [ css
                        [ padding (px 10)
                        , flex (num 1)
                        , Css.color (hex "b8b8b8")
                        , textDecoration lineThrough
                        , lineHeight (num 1)
                        , textTransform capitalize
                        ]
                    ]
                    [ text food.title ]

            False ->
                input
                    [ type_ "text"
                    , value food.title
                    , onFocus (FocusFood food)
                    , onInput (SetFoodText food)
                    , onBlur (RequestPatchFood food)
                    , Html.Styled.Attributes.disabled food.bought
                    , attribute "aria-label" "Food"
                    , placeholder "Food..."
                    , id food.id
                    , css
                        [ textTransform capitalize
                        , borderStyle none
                        , Css.height (pct 100)
                        , Css.width (pct 100)
                        , fontSize (px 16)
                        , outline none
                        , padding (px 10)
                        ]
                    ]
                    []
        , button
            [ class "button"
            , attribute "aria-label" "delete"
            , onClick (RequestDeleteFood food.id)
            , css [ borderRadius (pct 50), margin (px 5) ]
            ]
            [ span [ css [ color (hex "cccccc") ], class "icon" ] [ i [ class "fas fa-trash fa-lg" ] [] ]
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
    Html.Styled.form
        [ css
            [ position fixed
            , bottom (px 0)
            , Css.width (pct 100)
            , backgroundColor (hex "fff")
            ]
        , class "field has-addons"
        , onSubmit RequestNewFood
        ]
        [ div [ class "control", css [ flex (num 1) ] ]
            [ input
                [ type_ "text"
                , placeholder "New..."
                , value model.newText
                , onInput SetNewText
                , class "input is-large"
                , attribute "aria-label" "New"
                , Html.Styled.Attributes.disabled model.newItemLoading
                , css
                    [ Css.width (pct 100)
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
                , Html.Styled.Attributes.disabled (String.isEmpty model.newText || model.newItemLoading)
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
            [ case model.error of
                Just errortext ->
                    notify errortext True

                Nothing ->
                    text ""
            , div [ class "container" ]
                (case model.initialLoad of
                    Loading ->
                        []

                    Complete ->
                        case List.isEmpty model.groceries of
                            False ->
                                List.map foodElem model.groceries

                            True ->
                                [ notify "There are no items in your basket. Try adding some by tapping the \"Add\" button below." False
                                ]
                )
            , addForm model
            ]
        ]
