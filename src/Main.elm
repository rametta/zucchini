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
    Browser.sandbox
        { init = init
        , view = view >> toUnstyled
        , update = update
        }


createFood : Food
createFood =
    Food "" "" False False


init : Model
init =
    { groceries =
        Just
            [ Food "a" "Cheese" False False
            , Food "b" "Bread" False True
            , Food "c" "coffee" True False
            , Food "d" "white wine" True False
            , Food "e" "jam" False False
            ]
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        CreateFood ->
            case model.groceries of
                Just items ->
                    { model | groceries = Just (createFood :: items) }

                Nothing ->
                    { model | groceries = Just [ createFood ] }

        ClearFoods ->
            { groceries = Nothing }

        SetFoodText foodId text ->
            { model
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

        FocusFood foodId ->
            { model
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

        ToggleFoodStatus foodId ->
            { model
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


navbar : Html Msg
navbar =
    nav [ class "navbar is-primary is-fixed-top", attribute "role" "navigation", attribute "aria-label" "main navigation" ]
        [ div [ class "navbar-brand" ]
            [ span [ class "navbar-item" ] [ text "Groceries \u{1F957}" ]
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
                        []
                )
            , div [ class "container" ]
                [ button
                    [ css [ margin (px 10) ]
                    , class "button is-primary"
                    , onClick CreateFood
                    ]
                    [ text "New" ]
                ]
            ]
        ]
