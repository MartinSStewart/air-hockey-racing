module KeyboardExtra exposing (..)

import Keyboard


type alias Config a =
    { a
        | currentKeys : List Keyboard.Key
        , previousKeys : List Keyboard.Key
    }


keyPressed : Config a -> Keyboard.Key -> Bool
keyPressed config key =
    List.any ((==) key) config.currentKeys && not (List.any ((==) key) config.previousKeys)


keyReleased : Config a -> Keyboard.Key -> Bool
keyReleased config key =
    List.any ((==) key) config.previousKeys && not (List.any ((==) key) config.currentKeys)


keyDown : Config a -> Keyboard.Key -> Bool
keyDown config key =
    List.any ((==) key) config.currentKeys


pressedUndo : Config a -> Bool
pressedUndo config =
    keyPressed config (Keyboard.Character "Z")
        && not (keyDown config Keyboard.Shift)
        && (keyDown config Keyboard.Control || keyDown config Keyboard.Meta)


pressedRedo : Config a -> Bool
pressedRedo config =
    (keyPressed config (Keyboard.Character "Z")
        && keyDown config Keyboard.Shift
        && (keyDown config Keyboard.Control || keyDown config Keyboard.Meta)
    )
        || (keyPressed config (Keyboard.Character "Y")
                && (keyDown config Keyboard.Control || keyDown config Keyboard.Meta)
           )
