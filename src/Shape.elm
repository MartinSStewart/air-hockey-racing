module Shape exposing (Layer, LayerId, PathSegment, RenderableShape, Shape, codec, go, imp, one, shapeToMesh_, surprise, three, two)

import CubicSpline2d
import Effect.WebGL as WebGL exposing (Mesh)
import FontRender exposing (FontVertex)
import Geometry
import Geometry.Interop.LinearAlgebra.Point2d as Point2d
import Id exposing (Id)
import Length exposing (Meters)
import List.Nonempty
import Match exposing (WorldCoordinate)
import Math.Vector3 exposing (Vec3)
import Point2d exposing (Point2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity)
import SeqDict exposing (SeqDict)
import Serialize exposing (Codec)
import Vector2d exposing (Vector2d)


type alias Shape =
    { layers : SeqDict (Id LayerId) Layer }


type alias RenderableShape =
    { layers : List { color : Vec3, mesh : Mesh FontVertex } }


type LayerId
    = LayerId Never


type alias Layer =
    { paths : List (List PathSegment)
    , red : Int
    , green : Int
    , blue : Int
    }


type alias PathSegment =
    { position : Point2d Meters WorldCoordinate
    , handlePrevious : Vector2d Meters WorldCoordinate
    , handleNext : Vector2d Meters WorldCoordinate
    }


stringToRenderable : String -> RenderableShape
stringToRenderable text =
    case Serialize.decodeFromString codec text of
        Ok { layers } ->
            { layers =
                SeqDict.values layers
                    |> List.map
                        (\layer ->
                            { mesh =
                                List.map
                                    (\path ->
                                        pathToQuadraticSplines path
                                            |> List.concat
                                            |> List.map
                                                (\spline ->
                                                    { position = QuadraticSpline2d.startPoint spline
                                                    , controlPoint = QuadraticSpline2d.secondControlPoint spline
                                                    }
                                                )
                                    )
                                    layer.paths
                                    |> shapeToMesh_
                            , color =
                                Math.Vector3.vec3
                                    (toFloat layer.red / 255)
                                    (toFloat layer.green / 255)
                                    (toFloat layer.blue / 255)
                            }
                        )
            }

        Err _ ->
            { layers = [] }


shapeToMesh_ :
    List (List { position : Point2d units coordinates, controlPoint : Point2d units coordinates })
    -> Mesh FontVertex
shapeToMesh_ paths =
    List.concatMap
        (\path ->
            (case List.map (.position >> Point2d.toVec2) path of
                first :: second :: rest ->
                    List.foldl
                        (\point state ->
                            { first = state.first
                            , previous = point
                            , triangles =
                                ( { position = point, s = 0.2, t = 0.2 }
                                , { position = state.previous, s = 0.2, t = 0.2 }
                                , { position = state.first, s = 0.2, t = 0.2 }
                                )
                                    :: state.triangles
                            }
                        )
                        { first = first, previous = second, triangles = [] }
                        rest
                        |> .triangles

                _ ->
                    []
            )
                ++ (case path of
                        first :: rest ->
                            List.foldl
                                (\point state ->
                                    { previous = point
                                    , triangles =
                                        ( { position = Point2d.toVec2 point.position
                                          , s = 0
                                          , t = 0
                                          }
                                        , { position = Point2d.toVec2 state.previous.position
                                          , s = 0
                                          , t = 1
                                          }
                                        , { position = Point2d.toVec2 state.previous.controlPoint
                                          , s = 1
                                          , t = 0
                                          }
                                        )
                                            :: state.triangles
                                    }
                                )
                                { previous = first, triangles = [] }
                                (rest ++ [ first ])
                                |> .triangles

                        [] ->
                            []
                   )
        )
        paths
        |> WebGL.triangles


pathToQuadraticSplines : List PathSegment -> List (List (QuadraticSpline2d Meters WorldCoordinate))
pathToQuadraticSplines path =
    case path of
        first :: rest ->
            List.foldl
                (\segment state ->
                    { index = state.index + 1
                    , previousPoint = segment
                    , curves =
                        state.curves
                            ++ [ Geometry.cubicSplineToQuadratic
                                    (Length.meters 1)
                                    (CubicSpline2d.fromControlPoints
                                        state.previousPoint.position
                                        (Point2d.translateBy
                                            state.previousPoint.handleNext
                                            state.previousPoint.position
                                        )
                                        (Point2d.translateBy
                                            segment.handlePrevious
                                            segment.position
                                        )
                                        segment.position
                                    )
                                    |> List.Nonempty.toList
                               ]
                    }
                )
                { index = 1, previousPoint = first, curves = [] }
                (rest ++ [ first ])
                |> .curves

        [] ->
            []


codec : Codec e Shape
codec =
    Serialize.record Shape
        |> Serialize.field .layers (dictCodec idCodec layerCodec)
        |> Serialize.finishRecord


idCodec : Codec e (Id idType)
idCodec =
    Serialize.int |> Serialize.map Id.fromInt Id.toInt


dictCodec : Codec e k -> Codec e v -> Codec e (SeqDict k v)
dictCodec keyCodec valueCodec =
    Serialize.list (Serialize.tuple keyCodec valueCodec)
        |> Serialize.map SeqDict.fromList SeqDict.toList


layerCodec : Codec e Layer
layerCodec =
    Serialize.record (\a b c d -> Layer a b c d)
        |> Serialize.field .paths (Serialize.list pathSegmentCodec |> Serialize.map List.singleton (\a -> Debug.todo ""))
        |> Serialize.field .red Serialize.byte
        |> Serialize.field .green Serialize.byte
        |> Serialize.field .blue Serialize.byte
        |> Serialize.finishRecord


pathSegmentCodec : Codec e PathSegment
pathSegmentCodec =
    Serialize.record PathSegment
        |> Serialize.field .position point2d
        |> Serialize.field .handlePrevious vector2d
        |> Serialize.field .handleNext vector2d
        |> Serialize.finishRecord


point2d : Codec e (Point2d units coordinates)
point2d =
    Serialize.record Point2d.xy
        |> Serialize.field Point2d.xCoordinate quantity
        |> Serialize.field Point2d.yCoordinate quantity
        |> Serialize.finishRecord


vector2d : Codec e (Vector2d units coordinates)
vector2d =
    Serialize.record Vector2d.xy
        |> Serialize.field Vector2d.xComponent quantity
        |> Serialize.field Vector2d.yComponent quantity
        |> Serialize.finishRecord


quantity : Codec e (Quantity Float units)
quantity =
    Serialize.float |> Serialize.map Quantity.unsafe Quantity.unwrap


surprise : RenderableShape
surprise =
    "AQAAAARAAAAAAAAAAAAAAAdAMp2JvDOy_EBVD7AV4jEAP9vNQ_nKXMA_9trSk6SzgMAT-LyUn2Q4wDliGzJf3ADAUsaCmlt8yMA6HneKfopnQE80-MdIAthASqOiT3JYxUBRhKXE8xbowDM2OdxWd1lAW6jSGXck1EABusOp-ClwwEHZlzD2pvzAN9QM-jAy_kATjx6XbF3Av_1WreMijKBAaQAAAAAAAMA1CMhd9s7FwEclrPIm_cwAAAAAAAAAAEBcj0ad0a3fv-dvazbjzwBAeo0hSCWAHEBpMKtQynbAP-L8DUCrhADAWx-B09Yteb_i_A1Aq4QAQFsfgdPWLXlAaN6Q1ER2ukB6aEIyPF30QF4clc7x_1y_yfFya3LYAMBeHJXO8f9cP8nxcmty2ADAOKoVgEVr60BpFIwDVWVgv8K_iPi2PABAXJgtWb4edD_PZMXbVGsAwE8s7kEWXmpQUFAAAAAAAAAAAAAAAAdAQ9RCEa11yEBVD7AV4jEAP-aLYmpDEgBAA8lAHxaEIMAhBDGnlAncwDzmBOECB-TAPYAr7H1WGMAuCiaugb02QE3jgSXLMmRARAxMKAJGdEBV6YnfPc3IP79LZCV6GwBAW_jCyJufskA8vlq1J__cwENgLYE0JnzAMbmzuTo0_kATjx6XbF3Av_1WreMijKBAaQAAAAAAAD_oVahlO2BdwEclrPIm_cwAAAAAAAAAAEBYtv8G0E3fAAAAAAAAAABAeQwq1DKdsEBpMKtQynbAP_hVqGU7YADAWXmsSfoo4b_4VahlO2AAQFl5rEn6KOFAaQAAAAAAAEB5DCrUMp2wQFkYVahlO2C_-FWoZTthAMBZGFWoZTtgP_hVqGU7YQC_6FWoZTtgXUBpMKtQynbAAAAAAAAAAABAWFWoZTtgXAAAAAAAAAAAwFGcdeEuICX___9ACAAAAAAAAAAAAARAaOLCPyHGS0BcTbdfjcTSwDiTSUjk4sA_0tYQUGXaAEA4k0lI5OLAv9LWEFBl2gBAbcEpJ_tjakBU4Jjdf1lvv-FEOZ8IBwBANceC3PXETD_hRDmfCAcAwDXHgtz1xExAaPpZRBAIGEBKWmjk66oUQDiTSUjk4qg_0tYQUGXbAMA4k0lI5OKov9LWEFBl2wBAZHxWdX_IJ0BUjA3mcMYbP9X5vbMhgADANsKpC9BnpL_V-b2zIYAAQDbCqQvQZ6T_MmRAEAAAAAAAAAAAAARAaP3ds-x3vUBgnpnbIrhLwD607aQ-AFA_qp4nmf-QAEA-tO2kPgBQv6qeJ5n_kABAbeuymDbvoEBvNu3JpBoWv7P2nbN_sADAR_O2Tc0GUL_RnNKNM-YAQFKCgmkwPgxAaS2Re5cPWkB2ERFPdiyTQDyhAaXmdSi_529rNuPQAMA8oQGl5nUoP-dvazbj0ABAZBUrf58D9UBvqZwOAB0qAAAAAAAAAABAUoDYhraeGAAAAAAAAAAAwEjSa0yejKD_MmQ"
        |> stringToRenderable


imp : RenderableShape
imp =
    "AQAAAAVAAAAAAAAAAAAAAA1AMp2JvDOy_EBVD7AV4jEAP9vNQ_nKXMA_9trSk6SzgMAT-LyUn2Q4wDliGzJf3ADAUsaCmlt8yMA6HneKfopnQE80-MdIAthASqOiT3JYxUBRhKXE8xbowDM2OdxWd1lAW6jSGXck1EABusOp-ClwwEHZlzD2pvzAN9QM-jAy_kATjx6XbF3Av_1WreMijKBAaQAAAAAAAMA1CMhd9s7FwEclrPIm_cwAAAAAAAAAAEBcj0ad0a3fv-dvazbjzwBAeo0hSCWAHEBpMKtQynbAP-L8DUCrhADAWx-B09Yteb_i_A1Aq4QAQESBu69En7JAeT-wBCc8SUBzfqMOB8zFQCbw7xTOkkDAOdWSAZrd8EA_ifrtxSPgQEVFPhF5DFhAdgJiJkYWWkCAKkeQs3UiQFgFXl7ESXy_53iLHBugAMAwIt-jUwBwPUAAAAAAAABAdDizjaH7ZEB5UQmPgKfdQD7ONpTkRtBANgECalnpcMBAgMHPw28QQCd4ixwbo6BAaN6Q1ER2ukB6aEIyPF30QEQ_3rIE7AC_7fTntvhaAMBCanPT0sMwP8nxcmty2ABAVBu2MlpP0EB5Z1jsEVvSQD0PzZHc1yBAH-rC5do6gMBBQSVSkppSQDb0jlNcY-BAQWY0Q35I6kCAPXqn3jadQDAQGM2i6mw_0TZD4WmYAMBUuftgnxMEP89LZCV6QADAGRs2tK5QAEBz73taQn6zwECAwc_DbxRAR4gwzi5gyMAn1m1IjBJAwDn6FKEb7IDAOKoVgEVr60BpFIwDVWVgv-g_ANWZ7ABAQ5lL6Hf40D_PZMXbVGsAwE8s7kEWXmpQKFAAAAAAAAAAAAAAAA1AQ9RCEa11yEBVD7AV4jEAP-aLYmpDEgBAA8lAHxaEIMAhBDGnlAncwDzmBOECB-TAPYAr7H1WGMAuCiaugb02QE3jgSXLMmRARAxMKAJGdEBV6YnfPc3IP79LZCV6GwBAW_jCyJufskA8vlq1J__cwENgLYE0JnzAMbmzuTo0_kATjx6XbF3Av_1WreMijKBAaQAAAAAAAD_oVahlO2BdwEclrPIm_cwAAAAAAAAAAEBYtv8G0E3fAAAAAAAAAABAeQwq1DKdsEBpMKtQynbAP_hVqGU7YADAWXmsSfoo4b_4VahlO2AAQFAfpNzHJalAdzA4BIF5y0Bzp3nrmfzcQDKhSAS18FDAQ5GfgLcLYEA8mumKQa9wQDvfJTFg0lBAdsadi0QZ0kB-eK9zEEh9QE_VFkSFfGDAQgsRVzU_eD_50WXSHmcAwFtI7oPgIXRAc3TGM6pbtEB3uWy6AivNQDTiJKmtCXDAJWSJdZ6GYMAz3VsRyg8wQB6Pn8yZUkBAaQAAAAAAAEB5DCrUMp2wQEtE83dXeOy_4BIdzhd6AMBKbQTd7nqiP-SBEEJVmABAVobFPN8z6EB3tZP-O25hQDNdDF2WzWhAIZpoVRS6wMAzXQxdls1owCGaaFUUusBAOgvhgjaQzEB-y9CkWdxyP9oA7pcEcwDAW8Smv8THpMBQHvJ5tjOmwEKnnpeYHmBAOFZhucJAKEB0BYS1C9GMwDp-j4TLpOhAPZlw2un3oMAyYrE8avv0wEOqw1IE8BC_6FWoZTtgXUBpMKtQynbAAAAAAAAAAABATxDodWIF-AAAAAAAAAAAwFGcdeEuICXIZP9ACAAAAAAAAAAAAARAVStxnQRF3UBhhS2H2aatQCawmzi6bHBAITWR2X-GYMAmsJs4umxwwCE1kdl_hmBAaT98ZiUUZUBQY-Ff4RlKwFXA5jC-6cY_6ObODwGsAEBVwOYwvunGv-jmzg8BrABAc_JJ7E2oekBhQq1TCgMIQCaInnGm-wDAIsA6ZOZggMAmiJ5xpvsAQCLAOmTmYIBAaRcCcYENCEBZQ3KraJEIQFKyiWkV-yi_7eHEEgIAgMBSsolpFfsoP-3hxBICAIAoChRAEAAAAAAAAAAAAAdAYkrL5Qs_50BtYgxI49fFQBZpUw2BgLDAIlYsrfWX4EA267fZcv3gwCVkiXWehmBAY37aBtwqrEBwbs69rRxoQE3MCJqvFrzAQxnD39_TgMBBUWMWEp2gQChy5j1HdSBAUv-_yPHm4kBxKj-i2EA-QDRfv927jEwAAAAAAAAAAMBAC2cYNuTgAAAAAAAAAABATdky0sv8EEBvRiJ-B5HZwCuBQwTwY9C_4EyZfi-kAEBXWo2GzQ6PwCCAwc_DbwBAU-CtqL9FGkBrEMGo8qtXP-BMmX4vpABAQ4IUgwdq0D_gTJl-L6MAwDJWLK31l-BAXag89HVz9EBmtePZO4p5wDCa1fiNVOQ_4EyZfi-kAEAwmtX4jVTkv-BMmX4vpABAYqYSc83kR0BqJgw52Zbxv7oUKMnlyADAKKcOjttAwD-6FCjJ5cgAQCinDo7bQMAoChRAFAAAAAAAAAAAAAdAb3QT1jjcAkBtdZtne0QjwBZpUw2BgLDAIlYsrfWX4MA267fZcv3gwCVkiXWehmBAbkAFtGfxPUBweJZM-NKWwE3MCJqvFrzAQxnD39_TgEBBUWMWEp2gQChy5j1HdSBAdB9_62WUPEBxNAcyI_ZuwDRfv927jEwAAAAAAAAAAEBAC2cYNuTgAAAAAAAAAABAdSRJg0iOckBvWbGcnv43QCuBQwTwY9C_4EyZfi-kAMBXWo2GzQ6PwCCAwc_DbwBAc-dEc3I8rkBrJFDHihe1v-BMmX4vpABAQ4IUgwdq0L_gTJl-L6MAwDJWLK31l-BAcXVgoISw-EBmyXL30vbXQDCa1fiNVOQ_4EyZfi-kAMAwmtX4jVTkv-BMmX4vpABAbxjNR3Y3okBqOZtYcQNPP7oUKMnlyADAKKcOjttAwL-6FCjJ5cgAQCinDo7bQMAoChQ"
        |> stringToRenderable


one : RenderableShape
one =
    "AQAAAAE_8AAAAAAAAAAAAAsAAAAAAAAAAAAAAAAAAAAAwFZPST0k9JTAQTEMxDMQzAAAAAAAAAAAQFkAAAAAAABAFP9T_U_1QEB4mzZs2bNnAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADAWO8zvM7zPEB4mzZs2bNnAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADAWO8zvM7zPEB_0vtL7S-2vSAAAAAAAADATN8TfE3xQEBlp06dOnTrQD9-_fv37_BAR58-fPnz6ECELVq1atWsAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAZKtWrVq1a0CELVq1atWsAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAYxgwYMGDBkAE_1P9T_VAAAAAAAAAAABAWQAAAAAAAEBVp06dOnTqwD_CLwi8IvBAcJF4ReEXhcBE_1P9T_VAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAcBN8TfE3xMBjB2QdkHZCAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADAWO8zvM7zPMBhY3GNxjcZAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADAWO8zvM7zPMBGT0k9JPSUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD_AGQ"
        |> stringToRenderable


two : RenderableShape
two =
    "AQAAAAEAAAAAAAAAAAAAAAnAbDKaymsprUBGF9BfQX0GAAAAAAAAAADAWQAAAAAAAAAAAAAAAAAAQFkAAAAAAABAYZZTWU1lNkB6KaymsprLAAAAAAAAAADAWGsprKaymAAAAAAAAAAAQFhrKaymspzAZ_uI7iO4kEB7eI7iO4jvQHLlNZTWU1pAVhfQX0F9CAAAAAAAAAAAQFWC-gvoL6DAZ7EdxHcR3ECBziO4juI8wAKaymsprMDAUEdxHcR3EEBIaymsprKcQEq-gvoL6DBAWd9BfQX0GkCFE1lNZTWWAAAAAAAAAAAAAAAAAAAAAEB0DuI7iO4kQEFxHcR3EdDAQy-gvoL6EEBJlNZTWU1mQHi1lNZTWU5AYbuI7iO4j0B29xHcR3EeAAAAAAAAAABAdLZTWU1lNkBDxHcR3EdxAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAcy-gvoL6DMBWrKaymspsAAAAAAAAAABAWQAAAAAAAMBlzWU1lNZUwBvoL6C-gwDAbA1lNZTWVMBXQX0F9BfRAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADIAsg"
        |> stringToRenderable


three : RenderableShape
three =
    "AQAAAAE_8AAAAAAAAAAAAAnActKaymsprcAb6C-gvoMAQIOfQX0F9BjAboX0F9BfQkA3QX0F9BfcQEdBfQX0F8_AbYF9BfQX0kBZlNZTWU1lwDvoL6C-gwDAQprKaymspkBmF9BfQX0HwFWC-gvoL6JAWd9BfQX0GkBWF9BfQX0IwDKaymsprKbARO4juI7iPEAymsprKaymQETuI7iO4jzAZfKaymsprkBt8R3EdxHcQHN6C-gvoL7AIprKaymsoMAb6C-gvoL4QDTuI7iO4ijAZxxHcR3EeEB1OI7iO4juQBvoL6C-gxDAR0F9BfQXzEBy5TWU1lNYQCvoL6C-gtBAVA7iO4juJEB87KaymsptQBKaymsprJjARhfQX0F9CMASmsprKayYQEYX0F9BfQjAZfKaymsprUCA3EdxHcR3QGJQX0F9BfVAN0F9BfQX4MA-O4juI7iQQEq-gvoL6CDAbYF9BfQX0ECEtlNZTWU2QDKaymsprKDAQprKaymskEB4RfQX0F9CQFtTWU1lNZhAZDQX0F9BfUByKymsprKcQGKaymsprKdAZ4voL6C-gkBe0F9BfQX0wFasprKaymj_ZgE"
        |> stringToRenderable


go : RenderableShape
go =
    "AQAAAAJAAAAAAAAAAAAAAAdAZANcDXA1wUBvKwCsArALwBT_U_1P9UDAdf9-_fv38AAAAAAAAAAAQHfNcDXA1wRAgT2l9pfaYEBwE3xN8TfFQAT_U_1P9QBAehldZXWV1kAU_1P9T_WAwHdPdD3Q90RAdg2hfu0MzcBXQvqKoFowQFBWpKGmA4S_yiqnTLNqAEAU_1P9T_UAQFOvXr169exAdnlH5R-UfwAAAAAAAAAAwAvNUcF-oADAQwaqQWpvocBiC2wtsLbDwBT_U_1P9UBAdnlH5R-UfkB7OyDsg7IQwGX7S-0vtL7ABP9T_U_1QEBmo0aNGjRqQBT_U_1P9WBAdnlH5R-Ufj9q66P5skAAQGGWipTfYnxAEyliqXBFUMAGXqqBGVsAwD4tlQPW40pAdgvHcs26XMBXOTHzh4qyP_fyoickPwBAM8ytjRe2WMBWT0k9JPSYQAT_U_1P9UAC-gI_8AAAAAAAAAAAAAnAQl9pfaX2mEB4mzZs2bNnQDT_U_1P9UBARi2wtsLbEMBk_1P9T_VAwALly5cuXIDAcOV1ldZXWkBmT0k9JPSUQBT_U_1P9UBAWucjnI5yOsAU_1P9T_VAwFrnI5yOcjrAUGd5neZ3mkBLjx48ePHkwFT_U_1P9UBAMl9pfaX2mEAE_1P9T_UAQD9-_fv37-DAXN8TfE3xOEBhD3Q90PdEQFo_KPyj8pDABP9T_U_1QMAE_1P9T_VAQEJfaX2l9pjAWZcuXLly5kBsixYsWLFjwB9-_fv37-DAP379-_fv4EBGT0k9JPSUwBT_U_1P9UBAVvdD3Q90PkBp6yusrrK7wE4vCLwi8IxAH379-_fv4MAffv379-_gwGWnTp06dOrAYGd5neZ3msBW90PdD3Q-QFefPnz58-jAFP9T_U_1QMBXnz58-fPoQBT_U_1P9UDAeUMxDMQzEUBl-0vtL7S9wAT_U_1P9QDAazsg7IOyDUAE_1P9T_UAQGs7IOyDsg1AFxjcY3GNwECAE3xN8TfFwG4vCLwi8IxAXEflH5R-UMAvfv379-_gwFEgQIECBAgC-gI"
        |> stringToRenderable
