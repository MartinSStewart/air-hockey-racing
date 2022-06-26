module FontRender exposing (FontVertex, drawLayer, fragmentShaderFont, vertexShaderFont)

import Effect.WebGL as WebGL exposing (Mesh, Shader)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector4 as Vec4 exposing (Vec4)
import WebGL.Settings
import WebGL.Settings.StencilTest


type alias FontVertex =
    { position : Vec2
    , s : Float
    , t : Float
    }


drawLayer : Vec4 -> WebGL.Mesh FontVertex -> Mat4 -> List WebGL.Entity
drawLayer color feature viewMatrix =
    [ WebGL.entityWith
        [ WebGL.Settings.StencilTest.test
            { ref = 1
            , mask = 0xFF
            , test = WebGL.Settings.StencilTest.always
            , fail = WebGL.Settings.StencilTest.invert
            , zfail = WebGL.Settings.StencilTest.invert
            , zpass = WebGL.Settings.StencilTest.invert
            , writeMask = 0xFF
            }
        , WebGL.Settings.colorMask False False False False
        ]
        vertexShaderFont
        fragmentShaderFont
        feature
        { color = Vec4.vec4 0 0 0 1, viewMatrix = viewMatrix }
    , WebGL.entityWith
        [ WebGL.Settings.StencilTest.test
            { ref = 0
            , mask = 0xFF
            , test = WebGL.Settings.StencilTest.less
            , fail = WebGL.Settings.StencilTest.replace
            , zfail = WebGL.Settings.StencilTest.replace
            , zpass = WebGL.Settings.StencilTest.replace
            , writeMask = 0xFF
            }
        ]
        vertexShader
        fragmentShader
        viewportSquare
        { color = color, viewMatrix = Math.Matrix4.identity }
    ]


viewportSquare : Mesh { position : Vec2 }
viewportSquare =
    WebGL.triangleFan
        [ { position = Vec2.vec2 -1 -1 }
        , { position = Vec2.vec2 1 -1 }
        , { position = Vec2.vec2 1 1 }
        , { position = Vec2.vec2 -1 1 }
        ]


vertexShaderFont : Shader FontVertex { a | viewMatrix : Mat4 } { sVarying : Float, tVarying : Float }
vertexShaderFont =
    [glsl|

attribute vec2 position;
attribute float s;
attribute float t;
uniform mat4 viewMatrix;
varying float sVarying;
varying float tVarying;

void main () {
  sVarying = s;
  tVarying = t;
  gl_Position = viewMatrix * vec4(position, 0.0, 1.0);
}

|]


fragmentShaderFont : Shader {} { b | color : Vec4 } { sVarying : Float, tVarying : Float }
fragmentShaderFont =
    [glsl|
        precision mediump float;
        uniform vec4 color;
        varying float sVarying;
        varying float tVarying;
        
        void main () {
            if (pow(sVarying / 2.0 + tVarying, 2.0) >= tVarying) {
                discard;
            }
            gl_FragColor = color;
        }
    |]


vertexShader : Shader { position : Vec2 } { a | viewMatrix : Mat4 } {}
vertexShader =
    [glsl|

attribute vec2 position;
uniform mat4 viewMatrix;

void main () {
  gl_Position = viewMatrix * vec4(position, 0.0, 1.0);
}

|]


fragmentShader : Shader {} { b | color : Vec4 } {}
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec4 color;
        
        void main () {
            gl_FragColor = color;
        }
    |]
