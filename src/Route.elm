module Route exposing
    ( Route(..)
    , decode
    , encode
    )

import AppUrl
import Id exposing (Id)
import MatchPage exposing (MatchId)
import Url exposing (Url)
import Url.Builder


type Route
    = HomePageRoute
    | InMatchRoute (Id MatchId)


decode : Url -> Route
decode url =
    let
        url2 =
            AppUrl.fromUrl url
    in
    case url2.path of
        [ "match", matchId ] ->
            case String.toInt matchId of
                Just matchId2 ->
                    InMatchRoute (Id.fromInt matchId2)

                Nothing ->
                    HomePageRoute

        _ ->
            HomePageRoute


encode : Route -> String
encode route =
    let
        ( path, query ) =
            case route of
                HomePageRoute ->
                    ( [], [] )

                InMatchRoute id ->
                    ( [ "match", Id.toString id ], [] )
    in
    Url.Builder.absolute path query
