module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href, type_, value, target, src, autoplay)
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


type State =
    NotLoggedIn
    | Loading
    | NewMails
    | NothingNew


type alias Model =
    { state : State
    , mdp : String
    , blur : Bool
    , content : Content
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = NotLoggedIn
      , mdp = ""
      , blur = False
      , content = { errors = [], mails = [] }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotMails (Result Http.Error Content)
    | Refresh
    | LogOut
    | SetMdp String
    | LogIn
    | Tick Time.Posix
    | Show
    | Hide


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMails result ->
            case result of
                Ok content ->
                    let state = if List.length content.mails > List.length model.content.mails then NewMails else NothingNew in
                    ( { model | state = state, content = content }, Cmd.none )

                Err err ->
                    ( { model | state = NothingNew, content = { errors = [ "Http Error: " ++ error_to_string err ], mails = [] } }, Cmd.none )

        SetMdp mdp ->
            ( { model | mdp = mdp }, Cmd.none )

        Tick _ ->
            ( { model | state = Loading }, getMails model.mdp )

        Refresh ->
            ( { model | state = Loading }, getMails model.mdp )

        LogIn ->
            ( { model | state = Loading }, getMails model.mdp )

        LogOut ->
            init ()

        Show ->
            ( { model | blur = False }, Cmd.none )

        Hide ->
            ( { model | blur = True }, Cmd.none )


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
    if model.state == NotLoggedIn then
        { title = "Restricted Area"
        , body =
            [ div [ class "w3-display-middle w3-card-4 login" ]
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
            buttons =
                if model.state == Loading then
                    [ text "Refreshing..." ]

                else
                    [ button [ onClick Refresh, class "w3-button w3-blue" ] [ text "Refresh" ],
                    if model.blur then button [ onClick Show, class "w3-button w3-yellow" ] [ text "Show" ]
                    else button [ onClick Hide, class "w3-button w3-yellow" ] [ text "Hide" ],
                      button [ onClick LogOut, class "w3-button w3-red" ] [ text "Log Out" ] ]
        in
        let
            mails =
                div [ class "w3-container" ] [ h1 [] [ text "Emails" ], p [] buttons, div [class "w3-responsive"] [ table [ class "w3-table-all" ] (legend :: List.map (view_mail model.blur) model.content.mails) ] ]
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
        let body =
                if model.state == NewMails then
                    (audio [src "./bell.wav",autoplay True] []) :: (errors ++ [ mails ])
                else
                    errors ++ [ mails ]
        in
        { title = title, body = body }


view_mail : Bool -> Mail -> Html Msg
view_mail blur mail =
    let attr = if blur then [class "blur"] else [] in
    let
        subject =
            td attr [ text mail.subject ]
    in
    let
        from =
            td attr [ text mail.from ]
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
            td [] [ a [ href mail.webmail, target "_blank" ] [ text "GO" ] ]
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
