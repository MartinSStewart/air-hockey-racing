module Internal.BroadPhase exposing (addContacts)

{-| This is very naive implementation of BroadPhase,
that checks if the bounding spheres of each two bodies overlap
-}

import Internal.Body exposing (Body)
import Internal.Contact exposing (ContactGroup)
import Internal.NarrowPhase as NarrowPhase
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Internal.World exposing (World)


addContacts : World data -> World data
addContacts world =
    { world
        | contactGroups =
            case world.bodies of
                body :: restBodies ->
                    addContactsHelp body restBodies restBodies []

                [] ->
                    []
    }


addContactsHelp : Body data -> List (Body data) -> List (Body data) -> List (ContactGroup data) -> List (ContactGroup data)
addContactsHelp body1 currentBodies restBodies result =
    case restBodies of
        body2 :: newRestBodies ->
            addContactsHelp
                body1
                currentBodies
                newRestBodies
                (if bodiesMayContact body1 body2 then
                    case NarrowPhase.getContacts body1 body2 of
                        [] ->
                            result

                        contacts ->
                            { body1 = body1
                            , body2 = body2
                            , contacts = contacts
                            }
                                :: result

                 else
                    result
                )

        [] ->
            case currentBodies of
                newBody1 :: newRestBodies ->
                    addContactsHelp
                        newBody1
                        newRestBodies
                        newRestBodies
                        result

                [] ->
                    result


bodiesMayContact : Body data -> Body data -> Bool
bodiesMayContact body1 body2 =
    not (body1.mass == 0 && body2.mass == 0)
        && (body1.boundingSphereRadius + body2.boundingSphereRadius)
        - Vec3.distance (Transform3d.originPoint body1.transform3d) (Transform3d.originPoint body2.transform3d)
        > 0
