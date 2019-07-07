module Main exposing (main)

import Browser
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onFocus, onInput)


type alias Food =
    { id : String
    , title : String
    , bought : Bool
    , active : Bool
    }


type alias Model =
    { groceries : Maybe (List Food) }


type Msg
    = CreateFood
    | ClearFoods
    | ToggleFoodStatus String
    | FocusFood String
    | SetFoodText String String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


createFood : Food
createFood =
    Food "" "" False False


init : () -> ( Model, Cmd Msg )
init _ =
    ( { groceries =
            Just
                [ Food "a" "Cheese" False False
                , Food "b" "Bread" False True
                , Food "c" "coffee" True False
                , Food "d" "white wine" True False
                , Food "e" "jam" False False
                ]
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateFood ->
            case model.groceries of
                Just items ->
                    ( { model | groceries = Just (createFood :: items) }, Cmd.none )

                Nothing ->
                    ( { model | groceries = Just [ createFood ] }, Cmd.none )

        ClearFoods ->
            ( { groceries = Nothing }, Cmd.none )

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

        ToggleFoodStatus foodId ->
            ( { model
                | groceries =
                    Maybe.map
                        (List.map
                            (\fd ->
                                if fd.id == foodId then
                                    { fd | bought = not fd.bought }

                                else
                                    fd
                            )
                        )
                        model.groceries
              }
            , Cmd.none
            )


navbar : Html Msg
navbar =
    nav [ class "navbar is-primary is-fixed-top", attribute "role" "navigation", attribute "aria-label" "main navigation" ]
        [ div [ class "navbar-brand", css [ Css.width (pct 100) ] ]
            [ div [ class "navbar-item" ] [ text "Zucchini \u{1F957}" ]
            , div [ css [ marginLeft auto, marginRight (px 10), marginTop (px 8) ] ]
                [ button [ type_ "button", class "button", onClick ClearFoods ] [ text "♻️" ]
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
            , onClick (ToggleFoodStatus food.id)
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
        , onClick CreateFood
        ]
        [ text "Add" ]


view : Model -> Html Msg
view model =
    div
        []
        [ navbar
        , main_ []
            [ div
                [ class "container" ]
                (case model.groceries of
                    Just groceries ->
                        List.map foodElem groceries

                    Nothing ->
                        [ article [ class "message", css [ margin (px 10) ] ]
                            [ div [ class "message-body" ]
                                [ text "There are no items in your basket yet. Try adding some by tapping the \"Add\" button in the bottom right"
                                ]
                            ]
                        ]
                )
            , addButton
            ]
        ]
