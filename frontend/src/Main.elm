module Main exposing (main)

import Browser
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onBlur, onClick, onFocus, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E


type Status
    = Loading
    | Complete


type alias Food =
    { id : String
    , title : String
    , done : Bool
    , date : Int
    , active : Bool
    , changed : Bool
    }


type alias Model =
    { initialLoad : Status
    , error : Maybe String
    , groceries : List Food
    , newText : String
    , newItemLoading : Bool
    }


type Msg
    = ToggleFoodStatus Food
    | FocusFood Food
    | SetFoodText Food String
    | SetNewText String
    | GetFoodsRequest
    | PostFoodRequest
    | PutFoodRequest Food
    | DeleteFoodRequest String
    | GetFoodsResponse (Result Http.Error (Maybe (List Food)))
    | PostFoodResponse (Result Http.Error Food)
    | PutFoodResponse (Result Http.Error Food)
    | DeleteFoodResponse (Result Http.Error ())


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


getFoods : Cmd Msg
getFoods =
    Http.get
        { url = "/api/food"
        , expect = Http.expectJson GetFoodsResponse (D.maybe (D.list foodDecoder))
        }


postFood : Food -> Cmd Msg
postFood food =
    Http.post
        { url = "/api/food"
        , body = Http.jsonBody (encodeFood food)
        , expect = Http.expectJson PostFoodResponse foodDecoder
        }


putFood : Food -> Cmd Msg
putFood food =
    Http.request
        { method = "PUT"
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , url = "/api/food"
        , body = Http.jsonBody (encodeFood food)
        , expect = Http.expectJson PutFoodResponse foodDecoder
        }


deleteFood : String -> Cmd Msg
deleteFood foodId =
    Http.request
        { method = "DELETE"
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , url = "/api/food/" ++ foodId
        , body = Http.emptyBody
        , expect = Http.expectWhatever DeleteFoodResponse
        }


encodeFood : Food -> E.Value
encodeFood food =
    E.object
        [ ( "_id", E.string food.id )
        , ( "title", E.string food.title )
        , ( "done", E.bool food.done )
        , ( "date", E.int food.date )
        ]


foodDecoder : D.Decoder Food
foodDecoder =
    D.map4 (\id title done date -> Food id title done date False False)
        (D.field "_id" D.string)
        (D.field "title" D.string)
        (D.field "done" D.bool)
        (D.field "date" D.int)


find : Food -> List Food -> Maybe Food
find food =
    List.filter (\f -> f.id == food.id) >> List.head


sortFoods : List Food -> List Food
sortFoods =
    List.sortBy .date >> List.reverse


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
                    { food | done = not food.done }
            in
            ( { model | groceries = replaceFood newFood model.groceries }, putFood newFood )

        GetFoodsRequest ->
            ( { model | initialLoad = Loading, error = Nothing }, getFoods )

        GetFoodsResponse res ->
            case res of
                Ok groceries ->
                    case groceries of
                        Just items ->
                            ( { model
                                | groceries = items |> sortFoods
                                , initialLoad = Complete
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | groceries = [], initialLoad = Complete }, Cmd.none )

                Err err ->
                    ( { model | error = Just (extractTextFromError err), initialLoad = Complete }, Cmd.none )

        PostFoodResponse res ->
            case res of
                Ok food ->
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

        DeleteFoodResponse res ->
            case res of
                Ok _ ->
                    ( model, Cmd.none )

                Err err ->
                    ( { model | error = Just (extractTextFromError err) }, getFoods )

        PutFoodResponse result ->
            case result of
                Ok food ->
                    ( { model | groceries = replaceFood food model.groceries }, Cmd.none )

                Err err ->
                    ( { model | error = Just (extractTextFromError err) }, Cmd.none )

        PostFoodRequest ->
            if String.isEmpty model.newText then
                ( model, Cmd.none )

            else
                ( { model | newItemLoading = True }
                , postFood
                    { id = ""
                    , title = model.newText
                    , done = False
                    , active = False
                    , changed = False
                    , date = List.length model.groceries + 1
                    }
                )

        PutFoodRequest food ->
            if food.changed then
                ( model, putFood food )

            else
                ( model, Cmd.none )

        DeleteFoodRequest foodId ->
            ( { model
                | groceries =
                    List.filter (\food -> not (food.id == foodId)) model.groceries
              }
            , deleteFood foodId
            )


navbar : Model -> Html Msg
navbar model =
    nav [ class "navbar is-primary is-fixed-top", attribute "role" "navigation", attribute "aria-label" "main navigation" ]
        [ div [ class "navbar-brand", css [ displayFlex, alignItems center, justifyContent spaceBetween ] ]
            [ div [ class "navbar-item" ] [ text "Zucchini \u{1F957}" ]
            , button
                [ class "button is-primary"
                , css [ marginRight (px 5) ]
                , onClick GetFoodsRequest
                , Html.Styled.Attributes.disabled (model.initialLoad == Loading)
                ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-sync-alt" ] []
                    ]
                ]
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
            [ case food.done of
                True ->
                    span [ css [ color (hex "00d1b2") ], class "icon" ] [ i [ class "fas fa-check fa-lg" ] [] ]

                False ->
                    span [ class "icon" ] []
            ]
        , case food.done of
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
                    , onBlur (PutFoodRequest food)
                    , Html.Styled.Attributes.disabled food.done
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
            , onClick (DeleteFoodRequest food.id)
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
        , onSubmit PostFoodRequest
        ]
        [ div [ class "control", css [ flex (num 1) ] ]
            [ input
                [ type_ "text"
                , placeholder "New..."
                , value model.newText
                , onInput SetNewText
                , class "input is-large is-fullwidth"
                , attribute "aria-label" "New"
                , Html.Styled.Attributes.autofocus True
                , Html.Styled.Attributes.disabled model.newItemLoading
                , css
                    [ borderRadius (px 0)
                    ]
                ]
                []
            ]
        , div [ class "control" ]
            [ button
                [ classList [ ( "is-loading", model.newItemLoading ) ]
                , class "button is-large is-primary"
                , css [ borderRadius (px 0) ]
                , Html.Styled.Attributes.disabled (String.isEmpty model.newText || model.newItemLoading || (model.initialLoad == Loading))
                ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-plus" ] []
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ navbar model
        , main_ [ css [ paddingBottom (px 54) ] ]
            [ case model.error of
                Just errortext ->
                    notify errortext True

                Nothing ->
                    text ""
            , case model.initialLoad of
                Loading ->
                    Html.Styled.progress
                        [ class "progress is-primary"
                        , Html.Styled.Attributes.max "100"
                        , css [ borderRadius (px 0) ]
                        ]
                        []

                Complete ->
                    case List.isEmpty model.groceries of
                        False ->
                            div [] (List.map foodElem model.groceries)

                        True ->
                            notify "There are no items in your basket. Try adding some by tapping the \"Add\" button below." False
            , addForm model
            ]
        ]
