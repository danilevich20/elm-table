module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    { paymentsData : List (List Data)
    , displayMode : DisplayMode
    }

type DisplayMode
    = Table
    | Card

type Content
    = Text String
    | Date String
    | Number Int

type Alignment
    = Left
    | Right

type Hierarchy
    = Title
    | Subtitle
    | Hidden

type alias Data =
    { content : Content
    , name : String
    , hierarchy : Maybe Hierarchy
    }

initialModel : Model
initialModel = 
    { paymentsData = paymentsData
    , displayMode = Table
    }

paymentsData : List (List Data)
paymentsData =
    List.repeat
        10
        <| rowData

rowData : List Data
rowData =
    [ Data (Text "Charge") "Type" Nothing 
    , Data (Text "Success") "Status" Nothing
    , Data (Date "Jan 01, 2020") "Date" (Just Title)
    , Data (Number 72) "Amount" (Just Subtitle)
    , Data (Text "4242") "Source" Nothing
    , Data (Text "Campaigns") "Products" Nothing
    ]

headerNames : List String
headerNames =
    [ "Type" 
    , "Status"
    , "Date"
    , "Amount"
    , "Source"
    , "Products"
    ]

type Msg
    = NoOp
    | ToggledDisplayMode

update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ToggledDisplayMode ->
            case model.displayMode of
                Table ->
                    { model 
                        | displayMode = Card
                    }

                Card ->
                    { model 
                        | displayMode = Table
                    }


view : Model -> Html Msg
view model =
   div [ class "container" ]
        [ h1 [ class "pageTitle" ] [ text "Payments" ]
        , button
            [ onClick ToggledDisplayMode ]
            [ text "Toggle Display Mode" ]
        , viewTable model.displayMode model.paymentsData
        ]

viewTable : DisplayMode -> List (List Data) -> Html Msg
viewTable displayMode data =
    case displayMode of
        Table ->
            table 
                []
                ([ tr  
                    []
                    (List.map viewHeader headerNames)
                ] ++
                (List.map viewRow data))

        Card ->
            viewCards data


-- Table

viewHeader : String -> Html Msg
viewHeader header =
    th
        []
        [ text header
        ]

viewRow : List Data -> Html Msg
viewRow data =
    tr
        []
        (List.map viewCell data)

viewCell : Data -> Html Msg
viewCell data =
    case data.content of
        Text str ->
            baseCell Left str

        Date str ->
            baseCell Left str

        Number int ->
            baseCell Right (String.fromInt int)


baseCell : Alignment -> String -> Html Msg
baseCell alignment str =
    case alignment of
        Left ->
            td
                [ class "cell string-cell" ]
                [ text str ]

        Right ->
            td
                [ class "cell number-cell" ]
                [ text str]

-- Card
viewCards : List (List Data) -> Html Msg
viewCards cards =
    div
        [ class "cards-list" ]
        (List.map viewCard cards)

sortCardData : List Data -> List Data -> List Data -> List Data -> List Data
sortCardData data titleList subtitleList defaultList =
    if List.isEmpty data then
        List.concat [ titleList, subtitleList, defaultList ]
    else
        case List.head data of
            Just d ->
                case d.hierarchy of
                    Just Title ->
                        sortCardData (List.drop 1 data) (d :: titleList) subtitleList defaultList

                    Just Subtitle ->
                        sortCardData (List.drop 1 data) titleList (d :: subtitleList) defaultList

                    Just Hidden ->
                        sortCardData (List.drop 1 data) titleList subtitleList defaultList

                    Nothing ->
                        sortCardData (List.drop 1 data) titleList subtitleList (d :: defaultList)

            Nothing ->
                List.concat [ titleList, subtitleList, defaultList ]

        

viewCard : List Data -> Html Msg
viewCard data =
    let
        sortedData =
            sortCardData data [] [] []

        _ =
            Debug.log "Sorted Data" sortedData
    in
    
    div
        [ class "card" ]
        (List.map viewSortCard sortedData)
        -- (List.map 
        --     (\d -> 
        --         d
        --             |> sortCardData
        --             |> viewSortCard) 
        --     data)
        -- (List.map
        --     (\d -> viewSortCard d)
        --     data
        -- )

viewSortCard : Data -> Html Msg
viewSortCard data =
    case data.hierarchy of
        Just Title ->
            viewCardTitle data.content

        Just Subtitle ->
            viewCardSubtitle data.content

        Just Hidden ->
            text ""
    
        _ ->
            viewCardDefault data.name data.content

viewCardTitle : Content -> Html Msg
viewCardTitle content =
    case content of
        Number int ->
            h2
                []
                [ text (String.fromInt int) ]

        Text str ->
            h2
                []
                [ text str ]

        Date str ->
            h2
                []
                [ text str ]

viewCardSubtitle : Content -> Html Msg
viewCardSubtitle content =
    case content of
        Number int ->
            h3
                []
                [ text (String.fromInt int) ]

        Text str ->
            h3
                []
                [ text str ]

        Date str ->
            h3
                []
                [ text str ]

viewCardDefault : String -> Content -> Html Msg
viewCardDefault name content =
    div
        [ class "default-card"]
        [ small 
            []
            [ text name ]
        , p
            []
            [ text  
                (case content of
                    Number int ->
                        String.fromInt int

                    Text str ->
                        str
                    
                    Date str ->
                        str)
            ]
        ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
