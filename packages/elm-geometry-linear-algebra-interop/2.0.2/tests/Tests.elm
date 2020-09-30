module Tests exposing (..)

import Angle
import Axis3d
import Direction2d
import Direction3d
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.Interop.LinearAlgebra.Direction2d as Direction2d
import Geometry.Interop.LinearAlgebra.Direction3d as Direction3d
import Geometry.Interop.LinearAlgebra.Frame3d as Frame3d
import Geometry.Interop.LinearAlgebra.Point2d as Point2d
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Geometry.Interop.LinearAlgebra.Vector2d as Vector2d
import Geometry.Interop.LinearAlgebra.Vector3d as Vector3d
import Math.Matrix4
import Math.Vector3
import Point3d
import Test exposing (Test)
import Vector3d


vector2dConversionRoundTrips : Test
vector2dConversionRoundTrips =
    Test.fuzz Fuzz.vector2d
        "Vector2d conversion round-trips"
        (\vector ->
            vector
                |> Vector2d.toVec2
                |> Vector2d.fromVec2
                |> Expect.vector2d vector
        )


vector3dConversionRoundTrips : Test
vector3dConversionRoundTrips =
    Test.fuzz Fuzz.vector3d
        "Vector3d conversion round-trips"
        (\vector ->
            vector
                |> Vector3d.toVec3
                |> Vector3d.fromVec3
                |> Expect.vector3d vector
        )


direction2dConversionRoundTrips : Test
direction2dConversionRoundTrips =
    Test.fuzz Fuzz.direction2d
        "Direction2d conversion round-trips"
        (\direction ->
            direction
                |> Direction2d.toVec2
                |> Vector2d.fromVec2
                |> Expect.vector2d (Direction2d.toVector direction)
        )


direction3dConversionRoundTrips : Test
direction3dConversionRoundTrips =
    Test.fuzz Fuzz.direction3d
        "Direction3d conversion round-trips"
        (\direction ->
            direction
                |> Direction3d.toVec3
                |> Vector3d.fromVec3
                |> Expect.vector3d (Direction3d.toVector direction)
        )


point2dConversionRoundTrips : Test
point2dConversionRoundTrips =
    Test.fuzz Fuzz.point2d
        "Point2d conversion round-trips"
        (\point ->
            point
                |> Point2d.toVec2
                |> Point2d.fromVec2
                |> Expect.point2d point
        )


point3dConversionRoundTrips : Test
point3dConversionRoundTrips =
    Test.fuzz Fuzz.point3d
        "Point3d conversion round-trips"
        (\point ->
            point
                |> Point3d.toVec3
                |> Point3d.fromVec3
                |> Expect.point3d point
        )


point3dPlaceInIsTransform : Test
point3dPlaceInIsTransform =
    Test.fuzz2 Fuzz.point3d
        Fuzz.frame3d
        "Point3d.placeIn is equivalent to transform with Frame3d.toMat4"
        (\point frame ->
            point
                |> Point3d.toVec3
                |> Math.Matrix4.transform (Frame3d.toMat4 frame)
                |> Point3d.fromVec3
                |> Expect.point3d (Point3d.placeIn frame point)
        )


vector3dPlaceInIsTransform : Test
vector3dPlaceInIsTransform =
    let
        transformVec mat vec =
            Math.Vector3.sub
                (Math.Matrix4.transform mat vec)
                (Math.Matrix4.transform mat (Math.Vector3.vec3 0 0 0))
    in
    Test.fuzz2 Fuzz.vector3d
        Fuzz.frame3d
        "Vector3d.placeIn is equivalent to transform with Frame3d.toMat4"
        (\vector frame ->
            vector
                |> Vector3d.toVec3
                |> transformVec (Frame3d.toMat4 frame)
                |> Vector3d.fromVec3
                |> Expect.vector3d (Vector3d.placeIn frame vector)
        )


point3dPlaceInIsTransformBy : Test
point3dPlaceInIsTransformBy =
    Test.fuzz2 Fuzz.point3d
        Fuzz.frame3d
        "Point3d.placeIn is equivalent to transformBy Frame3d.toMat4"
        (\point frame ->
            point
                |> Point3d.transformBy (Frame3d.toMat4 frame)
                |> Expect.point3d (Point3d.placeIn frame point)
        )


vector3dPlaceInIsTransformBy : Test
vector3dPlaceInIsTransformBy =
    Test.fuzz2 Fuzz.vector3d
        Fuzz.frame3d
        "Vector3d.placeIn is equivalent to transformBy Frame3d.toMat4"
        (\vector frame ->
            vector
                |> Vector3d.transformBy (Frame3d.toMat4 frame)
                |> Expect.vector3d (Vector3d.placeIn frame vector)
        )


point3dRelativeToIsTransformByInverse : Test
point3dRelativeToIsTransformByInverse =
    Test.fuzz2 Fuzz.point3d
        Fuzz.frame3d
        "Point3d.relativeTo is equivalent to transformBy inverse of Frame3d.toMat4"
        (\point frame ->
            point
                |> Point3d.transformBy (Math.Matrix4.inverseOrthonormal (Frame3d.toMat4 frame))
                |> Expect.point3d (Point3d.relativeTo frame point)
        )


vector3dRelativeToIsTransformByInverse : Test
vector3dRelativeToIsTransformByInverse =
    Test.fuzz2 Fuzz.vector3d
        Fuzz.frame3d
        "Vector3d.relativeTo is equivalent to transformBy inverse of Frame3d.toMat4"
        (\vector frame ->
            vector
                |> Vector3d.transformBy (Math.Matrix4.inverseOrthonormal (Frame3d.toMat4 frame))
                |> Expect.vector3d (Vector3d.relativeTo frame vector)
        )


point3dRotationMatchesMatrix : Test
point3dRotationMatchesMatrix =
    Test.fuzz3 Fuzz.point3d
        Fuzz.direction3d
        Fuzz.angle
        "Point3d rotation matches matrix version"
        (\point direction angle ->
            let
                axis =
                    Axis3d.through Point3d.origin direction

                rotationMatrix =
                    Math.Matrix4.makeRotate (Angle.inRadians angle) (Direction3d.toVec3 direction)
            in
            Point3d.rotateAround axis angle point
                |> Expect.point3d (Point3d.transformBy rotationMatrix point)
        )


point3dTranslationMatchesMatrix : Test
point3dTranslationMatchesMatrix =
    Test.fuzz2 Fuzz.point3d
        Fuzz.vector3d
        "Point3d translation matches matrix version"
        (\point vector ->
            let
                translationMatrix =
                    Math.Matrix4.makeTranslate (Vector3d.toVec3 vector)
            in
            Point3d.translateBy vector point
                |> Expect.point3d (Point3d.transformBy translationMatrix point)
        )
