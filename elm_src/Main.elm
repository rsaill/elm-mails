module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, bool, field, int, list, map2, map6, string)
import Server
import Time



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Mail =
    { subject : String
    , from : String
    , to : String
    , date : String
    , udate : Int
    , webmail : String
    }


type alias Content =
    { errors : List String
    , mails : List Mail
    }


type alias Model =
    { logedIn : Bool
    , mdp : String
    , content : Content
    , loading : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { logedIn = False
      , mdp = ""
      , content = { errors = [], mails = [] }
      , loading = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotMails (Result Http.Error Content)
    | Refresh
    | SetMdp String
    | LogIn
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMails result ->
            case result of
                Ok content ->
                    ( { model | loading = False, content = content }, Cmd.none )

                Err err ->
                    ( { model | loading = False, content = { errors = [ "Http Error: " ++ error_to_string err ], mails = [] } }, Cmd.none )

        SetMdp mdp ->
            ( { model | mdp = mdp }, Cmd.none )

        Tick _ ->
            ( { model | loading = True }, getMails model.mdp )

        Refresh ->
            ( { model | loading = True }, getMails model.mdp )

        LogIn ->
            ( { model | logedIn = True, loading = True }, getMails model.mdp )


error_to_string : Http.Error -> String
error_to_string err =
    case err of
        Http.BadUrl s ->
            "Bad Url (" ++ s ++ ")"

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus s ->
            "Bad Status (" ++ String.fromInt s ++ ")"

        Http.BadBody s ->
            "Bad Body (" ++ s ++ ")"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 900000 Tick -- 15 minutes


-- VIEW


view : Model -> Browser.Document Msg
view model =
    if not model.logedIn then
        { title = "Restricted Area"
        , body =
            [ div [ class "w3-display-middle w3-card-4", style "width" "50%" ]
                [ div [ class "w3-container w3-green" ] [ h1 [] [ text "Restricted Area" ] ]
                , form [ class "w3-container", onSubmit LogIn ]
                    [ p [] [ input [ type_ "password", class "w3-input w3-border", onInput SetMdp ] [] ]
                    , p [] [ input [ type_ "submit", class "w3-input w3-border", value "Log In" ] [] ]
                    ]
                ]
            ]
        }

    else
        let
            errors =
                List.map view_error model.content.errors
        in
        let
            legend =
                tr [] [ th [] [ text "Subject" ], th [] [ text "From" ], th [] [ text "To" ], th [] [ text "Date" ], th [] [ text "Webmail" ] ]
        in
        let
            loading =
                if model.loading then
                    text "Refreshing..."

                else
                    button [ onClick Refresh, class "w3-button w3-blue" ] [ text "Refresh" ]
        in
        let
            mails =
                div [ class "w3-container" ] [ h1 [] [ text "Emails" ], p [] [ loading ], table [ class "w3-table-all" ] (legend :: List.map view_mail model.content.mails) ]
        in
        let
            size =
                List.length model.content.mails
        in
        let
            title =
                if size == 0 then
                    "No new mails"

                else if size == 1 then
                    "1 new mail"

                else
                    String.fromInt size ++ " new mails"
        in
        { title = title, body = errors ++ [ mails ] }


view_mail : Mail -> Html Msg
view_mail mail =
    let
        subject =
            td [] [ text mail.subject ]
    in
    let
        from =
            td [] [ text mail.from ]
    in
    let
        to =
            td [] [ text mail.to ]
    in
    let
        date =
            td [] [ text mail.date ]
    in
    let
        webmail =
            td [] [ a [ href mail.webmail ] [ text "GO" ] ]
    in
    tr [] [ subject, from, to, date, webmail ]


view_error : String -> Html Msg
view_error err =
    div [ class "w3-panel w3-pale-red" ] [ p [] [ text err ] ]



-- HTTP request


getMails : String -> Cmd Msg
getMails mdp =
    Http.get
        { url = "https://" ++ Server.address ++ "/imap.php?p=" ++ mdp
        , expect = Http.expectJson GotMails mainDecoder
        }



-- JSON decoder


mailDecoder : Decoder Mail
mailDecoder =
    map6 Mail (field "subject" string) (field "from" string) (field "to" string) (field "date" string) (field "udate" int) (field "webmail" string)


cmp : Mail -> Mail -> Order
cmp m1 m2 =
    case compare m1.udate m2.udate of
        EQ ->
            EQ

        LT ->
            GT

        GT ->
            LT


mainDecoder : Decoder Content
mainDecoder =
    map2 (\errors mails -> { errors = errors, mails = List.sortWith cmp mails }) (field "errors" (list string)) (field "mails" (list mailDecoder))
