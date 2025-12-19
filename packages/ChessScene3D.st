"Package: ChessScene3D"
"Version: 1.0"
"Author: ZigSmalltalk"
"Description: Full Featured OpenGL Chess Demo with PBR rendering"
"Classes: GLConstants, OBJLoader, ChessScene3D"
"Dependencies: external-structure.st, src/image/ffi.st"

"ChessScene3D - Full Featured OpenGL Demo for Zig Smalltalk"
"Port of the Dolphin Smalltalk ChessScene3D demo"
"Features: PBR shaders, shadow mapping, texture loading, OBJ models"

"OpenGL Constants"
Object subclass: #GLConstants
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!GLConstants class methodsFor!

GL_DEPTH_TEST ^2929!
GL_CULL_FACE ^2884!
GL_BLEND ^3042!
GL_SRC_ALPHA ^770!
GL_ONE_MINUS_SRC_ALPHA ^771!
GL_COLOR_BUFFER_BIT ^16384!
GL_DEPTH_BUFFER_BIT ^256!
GL_TRIANGLES ^4!
GL_UNSIGNED_INT ^5125!
GL_FLOAT ^5126!
GL_FALSE ^0!
GL_TRUE ^1!
GL_ARRAY_BUFFER ^34962!
GL_ELEMENT_ARRAY_BUFFER ^34963!
GL_STATIC_DRAW ^35044!
GL_VERTEX_SHADER ^35633!
GL_FRAGMENT_SHADER ^35632!
GL_COMPILE_STATUS ^35713!
GL_LINK_STATUS ^35714!
GL_INFO_LOG_LENGTH ^35716!
GL_TEXTURE_2D ^3553!
GL_TEXTURE0 ^33984!
GL_RGB ^6407!
GL_RGBA ^6408!
GL_UNSIGNED_BYTE ^5121!
GL_TEXTURE_MIN_FILTER ^10241!
GL_TEXTURE_MAG_FILTER ^10240!
GL_TEXTURE_WRAP_S ^10242!
GL_TEXTURE_WRAP_T ^10243!
GL_LINEAR ^9729!
GL_LINEAR_MIPMAP_LINEAR ^9987!
GL_REPEAT ^10497!
GL_CLAMP_TO_EDGE ^33071!
GL_DEPTH_COMPONENT ^6402!
GL_FRAMEBUFFER ^36160!
GL_DEPTH_ATTACHMENT ^36096!
GL_NONE ^0!
GL_FRAMEBUFFER_COMPLETE ^36053!

"GLFW Constants"
GLFW_CONTEXT_VERSION_MAJOR ^139266!
GLFW_CONTEXT_VERSION_MINOR ^139267!
GLFW_OPENGL_PROFILE ^139272!
GLFW_OPENGL_CORE_PROFILE ^204801!
GLFW_OPENGL_COMPAT_PROFILE ^204802!
GLFW_CURSOR ^208897!
GLFW_CURSOR_DISABLED ^212995!
GLFW_KEY_ESCAPE ^256!
GLFW_KEY_W ^87!
GLFW_KEY_S ^83!
GLFW_KEY_A ^65!
GLFW_KEY_D ^68!
GLFW_KEY_Q ^81!
GLFW_KEY_E ^69!
GLFW_KEY_UP ^265!
GLFW_KEY_DOWN ^264!
GLFW_KEY_LEFT ^263!
GLFW_KEY_RIGHT ^262!
GLFW_PRESS ^1! !

"OBJLoader - loads OBJ files using VM primitive"
Object subclass: #OBJLoader
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!OBJLoader class methodsFor!

load: filePath
    "Load an OBJ file, returns array: {vertexData. indexData. vertexCount. indexCount}"
    <primitive: 798>
    ^self primitiveFailed! !

"ChessScene3D class"
Object subclass: #ChessScene3D
    instanceVariableNames: 'width height window programId shadowProgramId gradientProgramId VAO VBO EBO vertexData indexData vertexCount indexCount albedoTexture normalTexture metallicTexture roughnessTexture aoTexture shadowFBO shadowMap lightSpaceMatrix x y z yaw pitch lastMouseX lastMouseY firstMouse running frameCount assetsPath'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ChessScene3D methodsFor!

initialize
    width := 1280.
    height := 720.
    running := true.
    frameCount := 0.
    "Camera position - same as Dolphin original"
    x := 0.0.
    y := 30.0.
    z := 50.0.
    "Camera angles - same as Dolphin original"
    yaw := -90.0.
    pitch := -30.0.
    "Mouse state"
    firstMouse := true.
    lastMouseX := 0.0.
    lastMouseY := 0.0.
    "Assets path"
    assetsPath := 'C:/programming/ZigSmalltalk/OpenGLDolphin/Models/'.
    ^self!

initializeGLFW
    | status |
    Transcript show: '1. Calling glfwInit...'; cr.
    status := 'GLFW' ffiCall: #glfwInit with: {}.
    Transcript show: '2. glfwInit returned: ', status printString; cr.
    status = 0 ifTrue: [
        Transcript show: 'GLFW initialization failed!'; cr.
        ^self
    ].
    Transcript show: '3. Setting window hints...'; cr.
    'GLFW' ffiCall: #glfwDefaultWindowHints with: {}.
    'GLFW' ffiCall: #glfwWindowHint with: { 139266. 3 }.
    'GLFW' ffiCall: #glfwWindowHint with: { 139267. 3 }.
    'GLFW' ffiCall: #glfwWindowHint with: { 135181. 8 }.  "GLFW_SAMPLES = 4x MSAA"
    Transcript show: '4. Creating window...'; cr.
    window := 'GLFW' ffiCall: #glfwCreateWindow with: { width. height. 'Chess Scene 3D - Cinematic Lighting'. nil. nil }.
    Transcript show: '5. Window: ', window printString; cr.
    (window isNil or: [window = 0]) ifTrue: [
        | errorCode |
        errorCode := 'GLFW' ffiCall: #glfwGetError with: { nil }.
        Transcript show: 'Failed to create window! GLFW error: ', errorCode printString; cr.
        Transcript show: 'Codes: 65537=NOT_INIT, 65540=INVALID_VALUE, 65544=PLATFORM_ERROR'; cr.
        'GLFW' ffiCall: #glfwTerminate with: {}.
        ^self
    ].
    Transcript show: '6. Making context current...'; cr.
    'GLFW' ffiCall: #glfwMakeContextCurrent with: { window }.
    'GLFW' ffiCall: #glfwSwapInterval with: { 1 }.
    Transcript show: '7. Setting input mode (capturing mouse)...'; cr.
    'GLFW' ffiCall: #glfwSetInputMode with: { window. 208897. 212995 }.
    Transcript show: '8. Initializing GL dynamic loader...'; cr.
    GL initialize.
    Transcript show: '9. Setting viewport...'; cr.
    GL glViewport: 0 y: 0 width: width height: height.
    GL glEnable: 2929.  "GL_DEPTH_TEST"
    GL glEnable: 32925.  "GL_MULTISAMPLE"
    GL glDisable: 2884.  "GL_CULL_FACE - disabled for debugging"
    Transcript show: '10. GLFW initialized successfully!'; cr!

vertexShaderSource
    ^'#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTexCoord;

out vec3 WorldPos;
out vec3 Normal;
out vec2 TexCoord;
out vec4 FragPosLightSpace;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
uniform mat4 lightSpaceMatrix;

void main()
{
    WorldPos = vec3(model * vec4(aPos, 1.0));
    Normal = mat3(transpose(inverse(model))) * aNormal;
    TexCoord = aTexCoord;
    FragPosLightSpace = lightSpaceMatrix * vec4(WorldPos, 1.0);
    gl_Position = projection * view * vec4(WorldPos, 1.0);
}'!

fragmentShaderSource
    ^'#version 330 core
out vec4 FragColor;

in vec3 WorldPos;
in vec3 Normal;
in vec2 TexCoord;
in vec4 FragPosLightSpace;

uniform sampler2D albedoMap;
uniform sampler2D normalMap;
uniform sampler2D metallicMap;
uniform sampler2D roughnessMap;
uniform sampler2D aoMap;
uniform sampler2D shadowMapTex;

uniform vec3 lightPositions[4];
uniform vec3 lightColors[4];
uniform int numLights;
uniform vec3 camPos;

const float PI = 3.14159265359;

float ShadowCalculation(vec4 fragPosLightSpace, vec3 normal, vec3 lightDir)
{
    vec3 projCoords = fragPosLightSpace.xyz / fragPosLightSpace.w;
    projCoords = projCoords * 0.5 + 0.5;

    if(projCoords.z > 1.0)
        return 0.0;

    float currentDepth = projCoords.z;
    float bias = max(0.002 * (1.0 - dot(normal, lightDir)), 0.0005);

    // 7x7 PCF for softer shadows
    float shadow = 0.0;
    vec2 texelSize = 1.0 / textureSize(shadowMapTex, 0);
    for(int x = -3; x <= 3; ++x) {
        for(int y = -3; y <= 3; ++y) {
            float pcfDepth = texture(shadowMapTex, projCoords.xy + vec2(x, y) * texelSize).r;
            shadow += currentDepth - bias > pcfDepth ? 1.0 : 0.0;
        }
    }
    shadow /= 49.0;

    return shadow;
}

vec3 getNormalFromMap()
{
    vec3 tangentNormal = texture(normalMap, TexCoord).xyz * 2.0 - 1.0;
    vec3 Q1 = dFdx(WorldPos);
    vec3 Q2 = dFdy(WorldPos);
    vec2 st1 = dFdx(TexCoord);
    vec2 st2 = dFdy(TexCoord);
    vec3 N = normalize(Normal);
    vec3 T = normalize(Q1*st2.t - Q2*st1.t);
    vec3 B = -normalize(cross(N, T));
    mat3 TBN = mat3(T, B, N);
    return normalize(TBN * tangentNormal);
}

float DistributionGGX(vec3 N, vec3 H, float r) {
    float a = r*r; float a2 = a*a;
    float NdotH = max(dot(N,H), 0.0);
    float d = (NdotH*NdotH*(a2-1.0)+1.0);
    return a2 / (PI*d*d);
}

float GeometrySmith(vec3 N, vec3 V, vec3 L, float r) {
    float k = (r+1.0)*(r+1.0)/8.0;
    float NdotV = max(dot(N,V), 0.0);
    float NdotL = max(dot(N,L), 0.0);
    float g1 = NdotV / (NdotV*(1.0-k)+k);
    float g2 = NdotL / (NdotL*(1.0-k)+k);
    return g1*g2;
}

vec3 fresnelSchlick(float cosTheta, vec3 F0) {
    return F0 + (1.0-F0)*pow(clamp(1.0-cosTheta, 0.0, 1.0), 5.0);
}

void main()
{
    vec3 albedo = pow(texture(albedoMap, TexCoord).rgb, vec3(2.2));
    float metallic = texture(metallicMap, TexCoord).r;
    float roughness = clamp(texture(roughnessMap, TexCoord).r, 0.05, 1.0);
    float ao = texture(aoMap, TexCoord).r;

    vec3 N = getNormalFromMap();
    vec3 V = normalize(camPos - WorldPos);
    vec3 F0 = mix(vec3(0.04), albedo, metallic);

    vec3 lightDir = normalize(lightPositions[0] - WorldPos);
    float shadow = ShadowCalculation(FragPosLightSpace, N, lightDir);

    vec3 Lo = vec3(0.0);
    for(int i = 0; i < numLights; ++i) {
        vec3 L = normalize(lightPositions[i] - WorldPos);
        vec3 H = normalize(V + L);
        float dist = length(lightPositions[i] - WorldPos);
        vec3 radiance = lightColors[i] / (dist*dist);

        float NDF = DistributionGGX(N, H, roughness);
        float G = GeometrySmith(N, V, L, roughness);
        vec3 F = fresnelSchlick(max(dot(H,V),0.0), F0);

        vec3 spec = NDF*G*F / (4.0*max(dot(N,V),0.0)*max(dot(N,L),0.0)+0.0001);
        vec3 kD = (1.0-F) * (1.0-metallic);

        float NdotL = max(dot(N,L), 0.0);

        // Apply deep shadow to key light (92% darkening - chiaroscuro style)
        float shadowFactor = (i == 0) ? (1.0 - shadow * 0.92) : 1.0;
        Lo += (kD*albedo/PI + spec) * radiance * NdotL * shadowFactor;
    }

    // Dark moody ambient - film noir style (reduced for more contrast)
    vec3 skyColor = vec3(0.3, 0.4, 0.6) * 0.04;    // Muted cool sky
    vec3 groundColor = vec3(0.4, 0.3, 0.2) * 0.02; // Dark warm ground
    float hemiFactor = N.y * 0.5 + 0.5;
    vec3 ambient = mix(groundColor, skyColor, hemiFactor) * albedo * ao;

    // Enhanced directional rim lighting from back light position
    vec3 rimLightDir = normalize(vec3(-5.0, 10.0, -20.0) - WorldPos);
    float rimDot = 1.0 - max(dot(V, N), 0.0);
    float backFacing = max(dot(N, -rimLightDir), 0.0);
    float rimIntensity = smoothstep(0.4, 1.0, rimDot) * (0.5 + 0.5 * backFacing);
    vec3 rimColor = vec3(0.6, 0.7, 0.9) * rimIntensity * 0.25 * (1.0 - metallic * 0.5);

    vec3 color = ambient + Lo + rimColor;

    // ACES tonemapping with adjusted exposure for drama
    color = color * 0.55;  // Slightly darker exposure
    vec3 a = vec3(2.51);
    vec3 b = vec3(0.03);
    vec3 c = vec3(2.43);
    vec3 d = vec3(0.59);
    vec3 e = vec3(0.14);
    color = clamp((color*(a*color+b))/(color*(c*color+d)+e), 0.0, 1.0);

    // Cinematic color grading - warm highlights, cool shadows
    vec3 warmTint = vec3(1.05, 1.0, 0.95);
    vec3 coolTint = vec3(0.95, 0.98, 1.05);
    float luminance = dot(color, vec3(0.299, 0.587, 0.114));
    color = mix(color * coolTint, color * warmTint, luminance);

    // Vignette effect - darkens edges for cinematic look
    vec2 uv = gl_FragCoord.xy / vec2(1200.0, 900.0);  // Assuming window size
    vec2 vignetteUV = uv * (1.0 - uv);
    float vignette = vignetteUV.x * vignetteUV.y * 15.0;
    vignette = pow(vignette, 0.25);
    color = color * vignette;

    // Subtle contrast boost
    color = (color - 0.5) * 1.08 + 0.5;
    color = clamp(color, 0.0, 1.0);

    color = pow(color, vec3(1.0/2.2));

    FragColor = vec4(color, 1.0);
}'!

shadowVertexShaderSource
    ^'#version 330 core
layout (location = 0) in vec3 aPos;

uniform mat4 lightSpaceMatrix;
uniform mat4 model;

void main()
{
    gl_Position = lightSpaceMatrix * model * vec4(aPos, 1.0);
}'!

shadowFragmentShaderSource
    ^'#version 330 core
void main()
{
}'!

gradientVertexShaderSource
    ^'#version 330 core
out vec2 vUV;
void main()
{
    // Fullscreen triangle using gl_VertexID
    vec2 pos = vec2((gl_VertexID << 1) & 2, gl_VertexID & 2);
    vUV = pos * 0.5;
    gl_Position = vec4(pos * 2.0 - 1.0, 0.999, 1.0);
}'!

gradientFragmentShaderSource
    ^'#version 330 core
in vec2 vUV;
out vec4 FragColor;
void main()
{
    // Vertical gradient from dark blue-gray at bottom to slightly lighter at top
    vec3 bottomColor = vec3(0.08, 0.09, 0.12);  // Dark blue-gray
    vec3 topColor = vec3(0.15, 0.17, 0.22);     // Lighter blue-gray
    vec3 color = mix(bottomColor, topColor, vUV.y);
    FragColor = vec4(color, 1.0);
}'!

compileShader: source type: shaderType
    | shader |
    shader := GL createShader: shaderType.
    GL shaderSource: shader source: source.
    GL compileShader: shader.
    ^shader!

compileShaderProgram: vertexSource fragment: fragmentSource
    | vertexShader fragmentShader prog |
    vertexShader := self compileShader: vertexSource type: 35633 "GL_VERTEX_SHADER".
    fragmentShader := self compileShader: fragmentSource type: 35632 "GL_FRAGMENT_SHADER".
    prog := GL createProgram.
    GL attachShader: prog shader: vertexShader.
    GL attachShader: prog shader: fragmentShader.
    GL linkProgram: prog.
    GL detachShader: prog shader: vertexShader.
    GL detachShader: prog shader: fragmentShader.
    GL deleteShader: vertexShader.
    GL deleteShader: fragmentShader.
    ^prog!

initializeShaders
    Transcript show: 'Compiling shaders...'; cr.
    programId := self compileShaderProgram: self vertexShaderSource fragment: self fragmentShaderSource.
    shadowProgramId := self compileShaderProgram: self shadowVertexShaderSource fragment: self shadowFragmentShaderSource.
    gradientProgramId := self compileShaderProgram: self gradientVertexShaderSource fragment: self gradientFragmentShaderSource.
    Transcript show: 'Shaders compiled. Main: ', programId printString, ' Shadow: ', shadowProgramId printString, ' Gradient: ', gradientProgramId printString; cr!

loadChessModel
    "Load the chess model from OBJ file"
    | objPath result |
    Transcript show: 'Loading chess OBJ model...'; cr.

    objPath := assetsPath, 'chess_obj/Realistic Wooden Chess-Obj/Realistic Wooden Chess.obj'.
    Transcript show: '  Path: ', objPath; cr.

    result := OBJLoader load: objPath.
    result isNil ifTrue: [
        Transcript show: '  OBJ load failed, falling back to generated geometry'; cr.
        ^self loadChessboardScene
    ].

    "Result is {vertexData. indexData. vertexCount. indexCount}"
    vertexData := result at: 1.
    indexData := result at: 2.
    vertexCount := result at: 3.
    indexCount := result at: 4.

    Transcript show: '  OBJ loaded: ', vertexCount printString, ' vertices, ', indexCount printString, ' indices'; cr.
    ^self!

loadChessboardScene
    "Create a chessboard with simple piece geometry"
    | boardSize squareSize pieceVertices pieceIndices boardVertices boardIndices totalVerts totalInds vOffset iOffset |
    Transcript show: 'Generating chessboard geometry...'; cr.

    boardSize := 8.
    squareSize := 3.0.  "Size of each square"

    "Board: 8x8 squares as a single flat plane with proper UV mapping"
    "Each square is 2 triangles = 4 vertices, 6 indices"
    "64 squares = 256 vertices, 384 indices"

    "Pieces: 16 per side = 32 total, each piece is a cube (24 verts, 36 indices)"
    "32 pieces = 768 vertices, 1152 indices"

    "Total: 256 + 768 = 1024 vertices, 384 + 1152 = 1536 indices"
    totalVerts := 1024.
    totalInds := 1536.

    vertexData := ByteArray new: totalVerts * 32.  "8 floats per vertex"
    indexData := ByteArray new: totalInds * 4.

    vOffset := 0.
    iOffset := 0.

    "Generate board squares"
    0 to: 7 do: [:row |
        0 to: 7 do: [:col |
            | baseX baseZ u0 v0 u1 v1 baseIdx |
            baseX := (col - 4) * squareSize.
            baseZ := (row - 4) * squareSize.

            "UV coordinates for texture tiling"
            u0 := col / 8.0.
            v0 := row / 8.0.
            u1 := (col + 1) / 8.0.
            v1 := (row + 1) / 8.0.

            baseIdx := vOffset // 32.

            "4 vertices for this square (y=0.1, slightly raised)"
            self setVertex: baseIdx     x: baseX            y: 0.1 z: baseZ            nx: 0.0 ny: 1.0 nz: 0.0 u: u0 v: v0.
            self setVertex: baseIdx + 1 x: baseX + squareSize y: 0.1 z: baseZ            nx: 0.0 ny: 1.0 nz: 0.0 u: u1 v: v0.
            self setVertex: baseIdx + 2 x: baseX + squareSize y: 0.1 z: baseZ + squareSize nx: 0.0 ny: 1.0 nz: 0.0 u: u1 v: v1.
            self setVertex: baseIdx + 3 x: baseX            y: 0.1 z: baseZ + squareSize nx: 0.0 ny: 1.0 nz: 0.0 u: u0 v: v1.

            "2 triangles - counter-clockwise winding for upward-facing board"
            self setIndex: iOffset // 4     value: baseIdx.
            self setIndex: iOffset // 4 + 1 value: baseIdx + 3.
            self setIndex: iOffset // 4 + 2 value: baseIdx + 2.
            self setIndex: iOffset // 4 + 3 value: baseIdx + 2.
            self setIndex: iOffset // 4 + 4 value: baseIdx + 1.
            self setIndex: iOffset // 4 + 5 value: baseIdx.

            vOffset := vOffset + (4 * 32).
            iOffset := iOffset + (6 * 4).
        ].
    ].

    "Add pieces (simple cubes)"
    "Back row white (row 0): Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook"
    #(0 1 2 3 4 5 6 7) do: [:col |
        self addPieceAt: col row: 0 height: 2.5 vOffset: vOffset iOffset: iOffset squareSize: squareSize.
        vOffset := vOffset + (24 * 32).
        iOffset := iOffset + (36 * 4).
    ].

    "Front row white (row 1): Pawns"
    0 to: 7 do: [:col |
        self addPieceAt: col row: 1 height: 1.5 vOffset: vOffset iOffset: iOffset squareSize: squareSize.
        vOffset := vOffset + (24 * 32).
        iOffset := iOffset + (36 * 4).
    ].

    "Front row black (row 6): Pawns"
    0 to: 7 do: [:col |
        self addPieceAt: col row: 6 height: 1.5 vOffset: vOffset iOffset: iOffset squareSize: squareSize.
        vOffset := vOffset + (24 * 32).
        iOffset := iOffset + (36 * 4).
    ].

    "Back row black (row 7)"
    #(0 1 2 3 4 5 6 7) do: [:col |
        self addPieceAt: col row: 7 height: 2.5 vOffset: vOffset iOffset: iOffset squareSize: squareSize.
        vOffset := vOffset + (24 * 32).
        iOffset := iOffset + (36 * 4).
    ].

    vertexCount := totalVerts.
    indexCount := totalInds.

    "Debug: show sample vertex data - first vertex should be at (-12.0, 0.1, -12.0)"
    Transcript show: 'Final vOffset: ', vOffset printString, ', iOffset: ', iOffset printString; cr.
    Transcript show: 'First 12 bytes (xyz): '; cr.
    0 to: 11 do: [:i | Transcript show: (vertexData at: i + 1) printString, ' '].
    Transcript cr.
    Transcript show: 'Sample index 0-5: ', (indexData uint32At: 0) printString, ', ', (indexData uint32At: 4) printString, ', ', (indexData uint32At: 8) printString, ', ', (indexData uint32At: 12) printString, ', ', (indexData uint32At: 16) printString, ', ', (indexData uint32At: 20) printString; cr.
    Transcript show: 'Chessboard scene created: ', totalVerts printString, ' vertices, ', totalInds printString, ' indices'; cr!

addPieceAt: col row: row height: pieceHeight vOffset: vOff iOffset: iOff squareSize: sqSize
    "Add a simple cube piece at the given board position"
    | cx cz w h baseIdx baseVert |
    cx := (col - 4) * sqSize + (sqSize / 2.0).
    cz := (row - 4) * sqSize + (sqSize / 2.0).
    w := sqSize * 0.3.  "Piece width"
    h := pieceHeight.

    baseVert := vOff // 32.
    baseIdx := iOff // 4.

    "Front face"
    self setVertexAt: vOff + 0   x: cx - w y: 0.0 z: cz + w nx: 0.0 ny: 0.0 nz: 1.0 u: 0.0 v: 0.0.
    self setVertexAt: vOff + 32  x: cx + w y: 0.0 z: cz + w nx: 0.0 ny: 0.0 nz: 1.0 u: 0.3 v: 0.0.
    self setVertexAt: vOff + 64  x: cx + w y: h   z: cz + w nx: 0.0 ny: 0.0 nz: 1.0 u: 0.3 v: 0.3.
    self setVertexAt: vOff + 96  x: cx - w y: h   z: cz + w nx: 0.0 ny: 0.0 nz: 1.0 u: 0.0 v: 0.3.

    "Back face"
    self setVertexAt: vOff + 128 x: cx + w y: 0.0 z: cz - w nx: 0.0 ny: 0.0 nz: -1.0 u: 0.0 v: 0.0.
    self setVertexAt: vOff + 160 x: cx - w y: 0.0 z: cz - w nx: 0.0 ny: 0.0 nz: -1.0 u: 0.3 v: 0.0.
    self setVertexAt: vOff + 192 x: cx - w y: h   z: cz - w nx: 0.0 ny: 0.0 nz: -1.0 u: 0.3 v: 0.3.
    self setVertexAt: vOff + 224 x: cx + w y: h   z: cz - w nx: 0.0 ny: 0.0 nz: -1.0 u: 0.0 v: 0.3.

    "Top face"
    self setVertexAt: vOff + 256 x: cx - w y: h z: cz + w nx: 0.0 ny: 1.0 nz: 0.0 u: 0.0 v: 0.0.
    self setVertexAt: vOff + 288 x: cx + w y: h z: cz + w nx: 0.0 ny: 1.0 nz: 0.0 u: 0.3 v: 0.0.
    self setVertexAt: vOff + 320 x: cx + w y: h z: cz - w nx: 0.0 ny: 1.0 nz: 0.0 u: 0.3 v: 0.3.
    self setVertexAt: vOff + 352 x: cx - w y: h z: cz - w nx: 0.0 ny: 1.0 nz: 0.0 u: 0.0 v: 0.3.

    "Bottom face"
    self setVertexAt: vOff + 384 x: cx - w y: 0.0 z: cz - w nx: 0.0 ny: -1.0 nz: 0.0 u: 0.0 v: 0.0.
    self setVertexAt: vOff + 416 x: cx + w y: 0.0 z: cz - w nx: 0.0 ny: -1.0 nz: 0.0 u: 0.3 v: 0.0.
    self setVertexAt: vOff + 448 x: cx + w y: 0.0 z: cz + w nx: 0.0 ny: -1.0 nz: 0.0 u: 0.3 v: 0.3.
    self setVertexAt: vOff + 480 x: cx - w y: 0.0 z: cz + w nx: 0.0 ny: -1.0 nz: 0.0 u: 0.0 v: 0.3.

    "Right face"
    self setVertexAt: vOff + 512 x: cx + w y: 0.0 z: cz + w nx: 1.0 ny: 0.0 nz: 0.0 u: 0.0 v: 0.0.
    self setVertexAt: vOff + 544 x: cx + w y: 0.0 z: cz - w nx: 1.0 ny: 0.0 nz: 0.0 u: 0.3 v: 0.0.
    self setVertexAt: vOff + 576 x: cx + w y: h   z: cz - w nx: 1.0 ny: 0.0 nz: 0.0 u: 0.3 v: 0.3.
    self setVertexAt: vOff + 608 x: cx + w y: h   z: cz + w nx: 1.0 ny: 0.0 nz: 0.0 u: 0.0 v: 0.3.

    "Left face"
    self setVertexAt: vOff + 640 x: cx - w y: 0.0 z: cz - w nx: -1.0 ny: 0.0 nz: 0.0 u: 0.0 v: 0.0.
    self setVertexAt: vOff + 672 x: cx - w y: 0.0 z: cz + w nx: -1.0 ny: 0.0 nz: 0.0 u: 0.3 v: 0.0.
    self setVertexAt: vOff + 704 x: cx - w y: h   z: cz + w nx: -1.0 ny: 0.0 nz: 0.0 u: 0.3 v: 0.3.
    self setVertexAt: vOff + 736 x: cx - w y: h   z: cz - w nx: -1.0 ny: 0.0 nz: 0.0 u: 0.0 v: 0.3.

    "Indices for 6 faces - counter-clockwise winding"
    0 to: 5 do: [:face |
        | fv fi |
        fv := baseVert + (face * 4).
        fi := baseIdx + (face * 6).
        self setIndex: fi     value: fv.
        self setIndex: fi + 1 value: fv + 2.
        self setIndex: fi + 2 value: fv + 1.
        self setIndex: fi + 3 value: fv.
        self setIndex: fi + 4 value: fv + 3.
        self setIndex: fi + 5 value: fv + 2.
    ]!

setVertexAt: byteOffset x: vx y: vy z: vz nx: nx ny: ny nz: nz u: u v: v
    "Set vertex data at a specific byte offset"
    vertexData float32At: byteOffset put: vx.
    vertexData float32At: byteOffset + 4 put: vy.
    vertexData float32At: byteOffset + 8 put: vz.
    vertexData float32At: byteOffset + 12 put: nx.
    vertexData float32At: byteOffset + 16 put: ny.
    vertexData float32At: byteOffset + 20 put: nz.
    vertexData float32At: byteOffset + 24 put: u.
    vertexData float32At: byteOffset + 28 put: v!

loadCubeModel
    "Fallback: Use a simple cube as placeholder model"
    Transcript show: 'Loading placeholder cube model...'; cr.

    "Cube: 24 vertices (8 floats each: x,y,z, nx,ny,nz, u,v)"
    vertexData := ByteArray new: 768.  "24 * 8 * 4 bytes"
    indexData := ByteArray new: 144.   "36 indices * 4 bytes"

    "Front face (z=1)"
    self setVertex: 0  x: -1.0 y: -1.0 z: 1.0 nx: 0.0 ny: 0.0 nz: 1.0 u: 0.0 v: 0.0.
    self setVertex: 1  x:  1.0 y: -1.0 z: 1.0 nx: 0.0 ny: 0.0 nz: 1.0 u: 1.0 v: 0.0.
    self setVertex: 2  x:  1.0 y:  1.0 z: 1.0 nx: 0.0 ny: 0.0 nz: 1.0 u: 1.0 v: 1.0.
    self setVertex: 3  x: -1.0 y:  1.0 z: 1.0 nx: 0.0 ny: 0.0 nz: 1.0 u: 0.0 v: 1.0.

    "Back face (z=-1)"
    self setVertex: 4  x:  1.0 y: -1.0 z: -1.0 nx: 0.0 ny: 0.0 nz: -1.0 u: 0.0 v: 0.0.
    self setVertex: 5  x: -1.0 y: -1.0 z: -1.0 nx: 0.0 ny: 0.0 nz: -1.0 u: 1.0 v: 0.0.
    self setVertex: 6  x: -1.0 y:  1.0 z: -1.0 nx: 0.0 ny: 0.0 nz: -1.0 u: 1.0 v: 1.0.
    self setVertex: 7  x:  1.0 y:  1.0 z: -1.0 nx: 0.0 ny: 0.0 nz: -1.0 u: 0.0 v: 1.0.

    "Top face (y=1)"
    self setVertex: 8  x: -1.0 y: 1.0 z:  1.0 nx: 0.0 ny: 1.0 nz: 0.0 u: 0.0 v: 0.0.
    self setVertex: 9  x:  1.0 y: 1.0 z:  1.0 nx: 0.0 ny: 1.0 nz: 0.0 u: 1.0 v: 0.0.
    self setVertex: 10 x:  1.0 y: 1.0 z: -1.0 nx: 0.0 ny: 1.0 nz: 0.0 u: 1.0 v: 1.0.
    self setVertex: 11 x: -1.0 y: 1.0 z: -1.0 nx: 0.0 ny: 1.0 nz: 0.0 u: 0.0 v: 1.0.

    "Bottom face (y=-1)"
    self setVertex: 12 x: -1.0 y: -1.0 z: -1.0 nx: 0.0 ny: -1.0 nz: 0.0 u: 0.0 v: 0.0.
    self setVertex: 13 x:  1.0 y: -1.0 z: -1.0 nx: 0.0 ny: -1.0 nz: 0.0 u: 1.0 v: 0.0.
    self setVertex: 14 x:  1.0 y: -1.0 z:  1.0 nx: 0.0 ny: -1.0 nz: 0.0 u: 1.0 v: 1.0.
    self setVertex: 15 x: -1.0 y: -1.0 z:  1.0 nx: 0.0 ny: -1.0 nz: 0.0 u: 0.0 v: 1.0.

    "Right face (x=1)"
    self setVertex: 16 x: 1.0 y: -1.0 z:  1.0 nx: 1.0 ny: 0.0 nz: 0.0 u: 0.0 v: 0.0.
    self setVertex: 17 x: 1.0 y: -1.0 z: -1.0 nx: 1.0 ny: 0.0 nz: 0.0 u: 1.0 v: 0.0.
    self setVertex: 18 x: 1.0 y:  1.0 z: -1.0 nx: 1.0 ny: 0.0 nz: 0.0 u: 1.0 v: 1.0.
    self setVertex: 19 x: 1.0 y:  1.0 z:  1.0 nx: 1.0 ny: 0.0 nz: 0.0 u: 0.0 v: 1.0.

    "Left face (x=-1)"
    self setVertex: 20 x: -1.0 y: -1.0 z: -1.0 nx: -1.0 ny: 0.0 nz: 0.0 u: 0.0 v: 0.0.
    self setVertex: 21 x: -1.0 y: -1.0 z:  1.0 nx: -1.0 ny: 0.0 nz: 0.0 u: 1.0 v: 0.0.
    self setVertex: 22 x: -1.0 y:  1.0 z:  1.0 nx: -1.0 ny: 0.0 nz: 0.0 u: 1.0 v: 1.0.
    self setVertex: 23 x: -1.0 y:  1.0 z: -1.0 nx: -1.0 ny: 0.0 nz: 0.0 u: 0.0 v: 1.0.

    "Indices for 6 faces (2 triangles each)"
    self setIndex: 0 value: 0. self setIndex: 1 value: 1. self setIndex: 2 value: 2.
    self setIndex: 3 value: 2. self setIndex: 4 value: 3. self setIndex: 5 value: 0.
    self setIndex: 6 value: 4. self setIndex: 7 value: 5. self setIndex: 8 value: 6.
    self setIndex: 9 value: 6. self setIndex: 10 value: 7. self setIndex: 11 value: 4.
    self setIndex: 12 value: 8. self setIndex: 13 value: 9. self setIndex: 14 value: 10.
    self setIndex: 15 value: 10. self setIndex: 16 value: 11. self setIndex: 17 value: 8.
    self setIndex: 18 value: 12. self setIndex: 19 value: 13. self setIndex: 20 value: 14.
    self setIndex: 21 value: 14. self setIndex: 22 value: 15. self setIndex: 23 value: 12.
    self setIndex: 24 value: 16. self setIndex: 25 value: 17. self setIndex: 26 value: 18.
    self setIndex: 27 value: 18. self setIndex: 28 value: 19. self setIndex: 29 value: 16.
    self setIndex: 30 value: 20. self setIndex: 31 value: 21. self setIndex: 32 value: 22.
    self setIndex: 33 value: 22. self setIndex: 34 value: 23. self setIndex: 35 value: 20.

    vertexCount := 24.
    indexCount := 36.
    Transcript show: 'Cube model loaded.'; cr!

setVertex: idx x: vx y: vy z: vz nx: nx ny: ny nz: nz u: u v: v
    | offset |
    offset := idx * 32.  "8 floats * 4 bytes"
    vertexData float32At: offset put: vx.
    vertexData float32At: offset + 4 put: vy.
    vertexData float32At: offset + 8 put: vz.
    vertexData float32At: offset + 12 put: nx.
    vertexData float32At: offset + 16 put: ny.
    vertexData float32At: offset + 20 put: nz.
    vertexData float32At: offset + 24 put: u.
    vertexData float32At: offset + 28 put: v!

setIndex: idx value: val
    indexData uint32At: idx * 4 put: val!

initializeBuffers
    | stride |
    Transcript show: 'Initializing buffers...'; cr.
    stride := 8 * 4.  "8 floats * 4 bytes"
    "Generate VAO"
    VAO := GL genVertexArray.
    GL bindVertexArray: VAO.
    "Generate and fill VBO"
    VBO := GL genBuffer.
    GL bindBuffer: 34962 "GL_ARRAY_BUFFER" buffer: VBO.
    GL bufferData: 34962 "GL_ARRAY_BUFFER" size: (vertexData basicSize) data: vertexData usage: 35044 "GL_STATIC_DRAW".
    "Generate and fill EBO"
    EBO := GL genBuffer.
    GL bindBuffer: 34963 "GL_ELEMENT_ARRAY_BUFFER" buffer: EBO.
    GL bufferData: 34963 "GL_ELEMENT_ARRAY_BUFFER" size: (indexData basicSize) data: indexData usage: 35044 "GL_STATIC_DRAW".
    "Position attribute (location 0)"
    GL vertexAttribPointer: 0 size: 3 type: 5126 "GL_FLOAT" normalized: 0 "GL_FALSE" stride: stride pointer: 0.
    GL enableVertexAttribArray: 0.
    "Normal attribute (location 1)"
    GL vertexAttribPointer: 1 size: 3 type: 5126 "GL_FLOAT" normalized: 0 "GL_FALSE" stride: stride pointer: 12.
    GL enableVertexAttribArray: 1.
    "UV attribute (location 2)"
    GL vertexAttribPointer: 2 size: 2 type: 5126 "GL_FLOAT" normalized: 0 "GL_FALSE" stride: stride pointer: 24.
    GL enableVertexAttribArray: 2.
    GL bindVertexArray: 0.
    Transcript show: 'Buffers initialized'; cr!

loadTextureFromFile: filename
    "Load a texture using Raylib's LoadImage and upload to OpenGL"
    | image imgData imgWidth imgHeight imgFormat texIdArr texId glFormat |
    Transcript show: 'Loading texture: ', filename; cr.

    "Load image using Raylib"
    image := 'Raylib' ffiCall: #LoadImage with: { filename }.
    (image isNil or: [image basicSize < 20]) ifTrue: [
        Transcript show: 'Failed to load image: ', filename; cr.
        ^0
    ].

    "Extract Image struct fields (64-bit layout)"
    "Image { void* data, int width, int height, int mipmaps, int format }"
    "Read 64-bit pointer as two 32-bit values (little-endian)"
    imgData := (image uint32At: 0) + ((image uint32At: 4) * 4294967296).
    imgWidth := image int32At: 8.        "width at offset 8"
    imgHeight := image int32At: 12.      "height at offset 12"
    imgFormat := image int32At: 20.      "format at offset 20"

    Transcript show: '  Image: ', imgWidth printString, 'x', imgHeight printString, ' format:', imgFormat printString, ' data:', imgData printString; cr.

    (imgWidth <= 0 or: [imgData = 0]) ifTrue: [
        Transcript show: '  Invalid image dimensions or null data'; cr.
        ^0
    ].

    "Raylib pixel formats:
     1 = GRAYSCALE
     2 = GRAY_ALPHA
     3 = R5G6B5
     4 = R8G8B8 (RGB)
     5 = R5G5B5A1
     6 = R4G4B4A4
     7 = R8G8B8A8 (RGBA)"
    glFormat := imgFormat = 7 ifTrue: [6408 "GL_RGBA"]
        ifFalse: [imgFormat = 1 ifTrue: [6403 "GL_RED"] ifFalse: [6407 "GL_RGB"]].

    "Generate OpenGL texture"
    texIdArr := ByteArray new: 4.
    GL glGenTextures: 1 textures: texIdArr.
    texId := texIdArr uint32At: 0.
    Transcript show: '  Generated texture ID: ', texId printString; cr.

    GL glBindTexture: 3553 texture: texId.

    "Set texture parameters"
    GL glTexParameteri: 3553 pname: 10242 param: 10497.
    GL glTexParameteri: 3553 pname: 10243 param: 10497.
    GL glTexParameteri: 3553 pname: 10241 param: 9987.
    GL glTexParameteri: 3553 pname: 10240 param: 9729.

    "Upload texture data"
    Transcript show: '  Uploading to OpenGL (format: ', glFormat printString, ')...'; cr.
    GL glTexImage2D: 3553 level: 0 internalformat: glFormat width: imgWidth height: imgHeight border: 0 format: glFormat type: 5121 pixels: imgData.

    GL generateMipmap: 3553.

    "Enable 16x anisotropic filtering for sharp textures at angles"
    GL glTexParameteri: 3553 pname: 34046 param: 16.  "GL_TEXTURE_MAX_ANISOTROPY_EXT"

    "Unload Raylib image"
    'Raylib' ffiCall: #UnloadImage with: { image }.

    Transcript show: '  Texture loaded: ', texId printString; cr.
    ^texId!

loadTextures
    | basePath pbrPath |
    Transcript show: 'Loading PBR textures (8K)...'; cr.

    basePath := assetsPath, 'chess_obj/Realistic Wooden Chess-Obj/'.
    pbrPath := assetsPath, 'chess_pbr_metalness/PBR (Metalness)/'.

    "Load full 8K textures"
    albedoTexture := self loadTextureFromFile: basePath, 'Chess_Color_8k.jpg'.
    normalTexture := self loadTextureFromFile: pbrPath, 'Chess_normal_8k.jpg'.
    metallicTexture := self loadTextureFromFile: pbrPath, 'Chess_metalness_8k.jpg'.
    roughnessTexture := self loadTextureFromFile: pbrPath, 'Chess_roughness_8k.jpg'.
    aoTexture := self loadTextureFromFile: pbrPath, 'Chess_ao_8k.jpg'.

    Transcript show: 'Textures loaded.'; cr!

initializeShadowMap
    | shadowWidth shadowHeight fboIdArr texIdArr fboStatus |
    Transcript show: 'Initializing shadow map...'; cr.

    shadowWidth := 4096.
    shadowHeight := 4096.

    "Generate framebuffer"
    fboIdArr := ByteArray new: 4.
    GL genFramebuffers: 1 framebuffers: fboIdArr.
    shadowFBO := fboIdArr uint32At: 0.

    "Generate depth texture"
    texIdArr := ByteArray new: 4.
    GL glGenTextures: 1 textures: texIdArr.
    shadowMap := texIdArr uint32At: 0.

    GL glBindTexture: 3553 texture: shadowMap.

    "Create depth texture"
    GL glTexImage2D: 3553 level: 0 internalformat: 6402 width: shadowWidth height: shadowHeight border: 0 format: 6402 type: 5126 pixels: 0.

    GL glTexParameteri: 3553 pname: 10241 param: 9729.
    GL glTexParameteri: 3553 pname: 10240 param: 9729.
    GL glTexParameteri: 3553 pname: 10242 param: 33071.
    GL glTexParameteri: 3553 pname: 10243 param: 33071.

    "Attach to framebuffer"
    GL bindFramebuffer: 36160 framebuffer: shadowFBO.
    GL framebufferTexture2D: 36160 attachment: 36096 textarget: 3553 texture: shadowMap level: 0.
    GL drawBuffer: 0.
    GL readBuffer: 0.

    "Check framebuffer status - skip check, just assume it worked"
    Transcript show: '  Shadow FBO: ', shadowFBO printString, ', shadowMap: ', shadowMap printString; cr.

    GL bindFramebuffer: 36160 framebuffer: 0.

    "Initialize light space matrix"
    lightSpaceMatrix := self computeLightSpaceMatrix.

    Transcript show: 'Shadow map initialized (', shadowWidth printString, 'x', shadowHeight printString, ')'; cr!

computeLightSpaceMatrix
    "Compute proper light space matrix for shadow mapping"
    "Light position matches key light at (15, 12, 15) looking at board center"
    | lightPos lightView lightProj |

    "Light position - matches cinematic key light position"
    lightPos := { 15.0. 12.0. 15.0 }.

    "Create lookAt matrix from light position to scene center"
    lightView := self lookAtMatrix: lightPos target: { 0.0. 0.0. 0.0 } up: { 0.0. 1.0. 0.0 }.

    "Create orthographic projection for directional light shadow"
    lightProj := self orthographicMatrix: -30.0 right: 30.0 bottom: -30.0 top: 30.0 near: 0.1 far: 60.0.

    "Return lightProj * lightView"
    ^self multiplyMatrix: lightProj by: lightView!

orthographicMatrix: left right: right bottom: bottom top: top near: near far: far
    "Create orthographic projection matrix (column-major for OpenGL)"
    | m |
    m := ByteArray new: 64.
    1 to: 16 do: [:i | m float32At: (i - 1) * 4 put: 0.0 ].

    "Column 0"
    m float32At: 0 put: (2.0 / (right - left)).
    "Column 1"
    m float32At: 20 put: (2.0 / (top - bottom)).
    "Column 2"
    m float32At: 40 put: (-2.0 / (far - near)).
    "Column 3"
    m float32At: 48 put: ((right + left) negated / (right - left)).
    m float32At: 52 put: ((top + bottom) negated / (top - bottom)).
    m float32At: 56 put: ((far + near) negated / (far - near)).
    m float32At: 60 put: 1.0.
    ^m!

identityMatrix
    | m |
    m := ByteArray new: 64.  "16 floats * 4 bytes"
    "Initialize all to 0.0"
    m float32At: 0 put: 0.0. m float32At: 4 put: 0.0. m float32At: 8 put: 0.0. m float32At: 12 put: 0.0.
    m float32At: 16 put: 0.0. m float32At: 20 put: 0.0. m float32At: 24 put: 0.0. m float32At: 28 put: 0.0.
    m float32At: 32 put: 0.0. m float32At: 36 put: 0.0. m float32At: 40 put: 0.0. m float32At: 44 put: 0.0.
    m float32At: 48 put: 0.0. m float32At: 52 put: 0.0. m float32At: 56 put: 0.0. m float32At: 60 put: 0.0.
    "Set diagonal to 1.0"
    m float32At: 0 put: 1.0.
    m float32At: 20 put: 1.0.
    m float32At: 40 put: 1.0.
    m float32At: 60 put: 1.0.
    ^m!

perspectiveMatrix: fov aspect: aspect near: near far: far
    | m tanHalfFov |
    m := ByteArray new: 64.
    1 to: 16 do: [:i | m float32At: (i - 1) * 4 put: 0.0 ].
    tanHalfFov := 0.41421356.
    m float32At: 0 put: (1.0 / (aspect * tanHalfFov)).
    m float32At: 20 put: (1.0 / tanHalfFov).
    m float32At: 40 put: ((far + near) negated / (far - near)).
    m float32At: 44 put: -1.0.
    m float32At: 56 put: ((2.0 * far * near) negated / (far - near)).
    m float32At: 60 put: 0.0.
    ^m!

lookAtMatrix: eye target: center up: upVec
    | f s u len result |
    f := { (center at: 1) - (eye at: 1). (center at: 2) - (eye at: 2). (center at: 3) - (eye at: 3) }.
    len := (((f at: 1) * (f at: 1)) + ((f at: 2) * (f at: 2)) + ((f at: 3) * (f at: 3))) sqrt.
    len > 0.0001 ifTrue: [
        f := { (f at: 1) / len. (f at: 2) / len. (f at: 3) / len }
    ].
    s := { ((f at: 2) * (upVec at: 3)) - ((f at: 3) * (upVec at: 2)).
           ((f at: 3) * (upVec at: 1)) - ((f at: 1) * (upVec at: 3)).
           ((f at: 1) * (upVec at: 2)) - ((f at: 2) * (upVec at: 1)) }.
    len := (((s at: 1) * (s at: 1)) + ((s at: 2) * (s at: 2)) + ((s at: 3) * (s at: 3))) sqrt.
    len > 0.0001 ifTrue: [
        s := { (s at: 1) / len. (s at: 2) / len. (s at: 3) / len }
    ].
    u := { ((s at: 2) * (f at: 3)) - ((s at: 3) * (f at: 2)).
           ((s at: 3) * (f at: 1)) - ((s at: 1) * (f at: 3)).
           ((s at: 1) * (f at: 2)) - ((s at: 2) * (f at: 1)) }.
    result := ByteArray new: 64.
    result float32At: 0 put: (s at: 1).
    result float32At: 4 put: (u at: 1).
    result float32At: 8 put: (f at: 1) negated.
    result float32At: 12 put: 0.0.
    result float32At: 16 put: (s at: 2).
    result float32At: 20 put: (u at: 2).
    result float32At: 24 put: (f at: 2) negated.
    result float32At: 28 put: 0.0.
    result float32At: 32 put: (s at: 3).
    result float32At: 36 put: (u at: 3).
    result float32At: 40 put: (f at: 3) negated.
    result float32At: 44 put: 0.0.
    result float32At: 48 put: (((s at: 1) * (eye at: 1)) + ((s at: 2) * (eye at: 2)) + ((s at: 3) * (eye at: 3))) negated.
    result float32At: 52 put: (((u at: 1) * (eye at: 1)) + ((u at: 2) * (eye at: 2)) + ((u at: 3) * (eye at: 3))) negated.
    result float32At: 56 put: (((f at: 1) * (eye at: 1)) + ((f at: 2) * (eye at: 2)) + ((f at: 3) * (eye at: 3))).  "NOT negated - matches viewMatrix"
    result float32At: 60 put: 1.0.
    ^result!

multiplyMatrix: a by: b
    "Matrix multiply for column-major 4x4 matrices"
    | result col row k sum aVal bVal |
    result := ByteArray new: 64.
    col := 0.
    [col < 4] whileTrue: [
        row := 0.
        [row < 4] whileTrue: [
            sum := 0.0.
            k := 0.
            [k < 4] whileTrue: [
                aVal := a float32At: (k * 4 + row) * 4.
                bVal := b float32At: (col * 4 + k) * 4.
                sum := sum + (aVal * bVal).
                k := k + 1.
            ].
            result float32At: (col * 4 + row) * 4 put: sum.
            row := row + 1.
        ].
        col := col + 1.
    ].
    ^result!

handleInput
    "Handle keyboard input for movement - matches Dolphin original"
    | speed yawRad dirX dirZ |
    speed := 0.5.

    "Check for escape"
    ('GLFW' ffiCall: #glfwGetKey with: { window. 256 }) = 1 ifTrue: [
        running := false.
        ^self
    ].

    "Calculate forward direction in XZ plane based on yaw"
    yawRad := yaw * 0.01745329252.
    dirX := yawRad cos.
    dirZ := yawRad sin.

    "W - move forward"
    ('GLFW' ffiCall: #glfwGetKey with: { window. 87 }) = 1 ifTrue: [
        x := x + (dirX * speed).
        z := z + (dirZ * speed).
    ].

    "S - move backward"
    ('GLFW' ffiCall: #glfwGetKey with: { window. 83 }) = 1 ifTrue: [
        x := x - (dirX * speed).
        z := z - (dirZ * speed).
    ].

    "A - strafe left (perpendicular to forward)"
    ('GLFW' ffiCall: #glfwGetKey with: { window. 65 }) = 1 ifTrue: [
        x := x + (dirZ * speed).
        z := z - (dirX * speed).
    ].

    "D - strafe right"
    ('GLFW' ffiCall: #glfwGetKey with: { window. 68 }) = 1 ifTrue: [
        x := x - (dirZ * speed).
        z := z + (dirX * speed).
    ].

    "Q - move down"
    ('GLFW' ffiCall: #glfwGetKey with: { window. 81 }) = 1 ifTrue: [
        y := y - speed.
    ].

    "E - move up"
    ('GLFW' ffiCall: #glfwGetKey with: { window. 69 }) = 1 ifTrue: [
        y := y + speed.
    ]!

updateMouseLook
    "Poll cursor position and update yaw/pitch"
    | xposArr yposArr xpos ypos xoffset yoffset sensitivity |

    "Get cursor position - GLFW returns two doubles"
    xposArr := ByteArray new: 8.
    yposArr := ByteArray new: 8.
    'GLFW' ffiCall: #glfwGetCursorPos with: { window. xposArr. yposArr }.

    "Read as float64"
    xpos := xposArr float64At: 0.
    ypos := yposArr float64At: 0.

    firstMouse ifTrue: [
        lastMouseX := xpos.
        lastMouseY := ypos.
        firstMouse := false.
        ^self
    ].

    xoffset := xpos - lastMouseX.
    yoffset := lastMouseY - ypos.  "Reversed since y increases downward"
    lastMouseX := xpos.
    lastMouseY := ypos.

    sensitivity := 0.1.
    yaw := yaw + (xoffset * sensitivity).
    pitch := pitch + (yoffset * sensitivity).

    "Clamp pitch"
    pitch < -89.0 ifTrue: [ pitch := -89.0 ].
    pitch > 89.0 ifTrue: [ pitch := 89.0 ]!

viewMatrix
    "FPS-style camera - matches Dolphin original exactly"
    | yawRad pitchRad dirX dirY dirZ rightX rightY rightZ upX upY upZ len result |

    "Convert to radians"
    yawRad := yaw * 0.01745329252.
    pitchRad := pitch * 0.01745329252.

    "Calculate forward direction from yaw and pitch"
    dirX := yawRad cos * pitchRad cos.
    dirY := pitchRad sin.
    dirZ := yawRad sin * pitchRad cos.

    "Normalize forward"
    len := ((dirX * dirX) + (dirY * dirY) + (dirZ * dirZ)) sqrt.
    len < 0.0001 ifTrue: [ len := 1.0 ].
    dirX := dirX / len. dirY := dirY / len. dirZ := dirZ / len.

    "Right vector = cross(forward, world_up) simplified since world_up is (0,1,0)"
    rightX := dirZ negated. rightY := 0.0. rightZ := dirX.
    len := ((rightX * rightX) + (rightY * rightY) + (rightZ * rightZ)) sqrt.
    len < 0.0001 ifTrue: [ len := 1.0 ].
    rightX := rightX / len. rightY := rightY / len. rightZ := rightZ / len.

    "Up vector = cross(right, forward)"
    upX := (rightY * dirZ) - (rightZ * dirY).
    upY := (rightZ * dirX) - (rightX * dirZ).
    upZ := (rightX * dirY) - (rightY * dirX).
    len := ((upX * upX) + (upY * upY) + (upZ * upZ)) sqrt.
    len < 0.0001 ifTrue: [ len := 1.0 ].
    upX := upX / len. upY := upY / len. upZ := upZ / len.

    "Build view matrix (column-major for OpenGL)"
    result := ByteArray new: 64.

    "Column 0"
    result float32At: 0 put: rightX.
    result float32At: 4 put: upX.
    result float32At: 8 put: dirX negated.
    result float32At: 12 put: 0.0.

    "Column 1"
    result float32At: 16 put: rightY.
    result float32At: 20 put: upY.
    result float32At: 24 put: dirY negated.
    result float32At: 28 put: 0.0.

    "Column 2"
    result float32At: 32 put: rightZ.
    result float32At: 36 put: upZ.
    result float32At: 40 put: dirZ negated.
    result float32At: 44 put: 0.0.

    "Column 3 - translation (dot products)"
    result float32At: 48 put: ((rightX * x) + (rightY * y) + (rightZ * z)) negated.
    result float32At: 52 put: ((upX * x) + (upY * y) + (upZ * z)) negated.
    result float32At: 56 put: (dirX * x) + (dirY * y) + (dirZ * z).  "NOT negated - matches Dolphin"
    result float32At: 60 put: 1.0.

    ^result!

renderShadowPass
    | modelMatrix loc |
    "Bind shadow framebuffer"
    GL glViewport: 0 y: 0 width: 4096 height: 4096.
    GL bindFramebuffer: 36160 framebuffer: shadowFBO.
    GL glEnable: 2929.  "GL_DEPTH_TEST"
    GL glEnable: 32925.  "GL_MULTISAMPLE"
    GL glDepthMask: 1.   "Enable depth writes"

    "Use shadow shader"
    GL useProgram: shadowProgramId.

    "Set lightSpaceMatrix uniform"
    loc := GL getUniformLocation: shadowProgramId name: 'lightSpaceMatrix'.
    GL uniformMatrix4fv: loc count: 1 transpose: 0 value: lightSpaceMatrix.

    "Set model matrix (identity)"
    modelMatrix := self identityMatrix.
    loc := GL getUniformLocation: shadowProgramId name: 'model'.
    GL uniformMatrix4fv: loc count: 1 transpose: 0 value: modelMatrix.

    "Draw scene"
    GL bindVertexArray: VAO.
    GL drawElements: 4 count: indexCount type: 5125 indices: 0.
    GL bindVertexArray: 0.

    "Unbind"
    GL bindFramebuffer: 36160 framebuffer: 0.
    GL glViewport: 0 y: 0 width: width height: height!

setUniforms
    | modelMatrix projMatrix loc shadowTexLoc |
    modelMatrix := self identityMatrix.
    projMatrix := self perspectiveMatrix: 45.0 aspect: (width asFloat / height asFloat) near: 0.1 far: 1000.0.

    loc := GL getUniformLocation: programId name: 'model'.
    GL uniformMatrix4fv: loc count: 1 transpose: 0 value: modelMatrix.
    loc := GL getUniformLocation: programId name: 'view'.
    GL uniformMatrix4fv: loc count: 1 transpose: 0 value: self viewMatrix.
    loc := GL getUniformLocation: programId name: 'projection'.
    GL uniformMatrix4fv: loc count: 1 transpose: 0 value: projMatrix.
    loc := GL getUniformLocation: programId name: 'lightSpaceMatrix'.
    GL uniformMatrix4fv: loc count: 1 transpose: 0 value: lightSpaceMatrix.
    loc := GL getUniformLocation: programId name: 'camPos'.
    GL uniform3f: loc x: x y: y z: z.

    "Bind PBR textures"
    GL activeTexture: 33984.
    GL glBindTexture: 3553 texture: albedoTexture.
    GL uniform1i: (GL getUniformLocation: programId name: 'albedoMap') value: 0.

    GL activeTexture: 33985.
    GL glBindTexture: 3553 texture: normalTexture.
    GL uniform1i: (GL getUniformLocation: programId name: 'normalMap') value: 1.

    GL activeTexture: 33986.
    GL glBindTexture: 3553 texture: metallicTexture.
    GL uniform1i: (GL getUniformLocation: programId name: 'metallicMap') value: 2.

    GL activeTexture: 33987.
    GL glBindTexture: 3553 texture: roughnessTexture.
    GL uniform1i: (GL getUniformLocation: programId name: 'roughnessMap') value: 3.

    GL activeTexture: 33988.
    GL glBindTexture: 3553 texture: aoTexture.
    GL uniform1i: (GL getUniformLocation: programId name: 'aoMap') value: 4.

    "Bind shadow map"
    (shadowMap isNil not and: [shadowMap > 0]) ifTrue: [
        | shadowTexLoc |
        GL activeTexture: 33989.
        GL glBindTexture: 3553 texture: shadowMap.
        shadowTexLoc := GL getUniformLocation: programId name: 'shadowMapTex'.
        GL uniform1i: shadowTexLoc value: 5.
    ].

    "Set number of lights"
    loc := GL getUniformLocation: programId name: 'numLights'.
    GL uniform1i: loc value: 4.

    "Cinematic 3-point lighting setup (film noir / chiaroscuro style)"
    "Key Light - warm dramatic side light at 45 degrees (shadow caster)"
    loc := GL getUniformLocation: programId name: 'lightPositions[0]'.
    GL uniform3f: loc x: 15.0 y: 12.0 z: 15.0.
    "Fill Light - cool soft light opposite to key (lifts shadows slightly)"
    loc := GL getUniformLocation: programId name: 'lightPositions[1]'.
    GL uniform3f: loc x: -18.0 y: 8.0 z: -10.0.
    "Rim/Back Light - strong backlight for edge separation (cinematic look)"
    loc := GL getUniformLocation: programId name: 'lightPositions[2]'.
    GL uniform3f: loc x: -5.0 y: 10.0 z: -20.0.
    "Accent Light - subtle overhead highlight"
    loc := GL getUniformLocation: programId name: 'lightPositions[3]'.
    GL uniform3f: loc x: 0.0 y: 20.0 z: 0.0.

    "Light colors - cinematic warm/cool contrast"
    "Key: Warm orange-yellow (like candlelight/tungsten) - high intensity"
    loc := GL getUniformLocation: programId name: 'lightColors[0]'.
    GL uniform3f: loc x: 14000.0 y: 10000.0 z: 6000.0.
    "Fill: Cool blue (like reflected sky/window) - low intensity"
    loc := GL getUniformLocation: programId name: 'lightColors[1]'.
    GL uniform3f: loc x: 1500.0 y: 2000.0 z: 3000.0.
    "Rim: Bright cool white - strong for edge definition"
    loc := GL getUniformLocation: programId name: 'lightColors[2]'.
    GL uniform3f: loc x: 6000.0 y: 6500.0 z: 7000.0.
    "Accent: Neutral warm - subtle top highlight"
    loc := GL getUniformLocation: programId name: 'lightColors[3]'.
    GL uniform3f: loc x: 2000.0 y: 1800.0 z: 1500.0!

render
    "Shadow pass"
    (shadowFBO isNil not and: [shadowFBO > 0]) ifTrue: [ self renderShadowPass ].

    "Main pass"
    GL glViewport: 0 y: 0 width: width height: height.
    GL glClearColor: 0.02 g: 0.02 b: 0.03 a: 1.0.  "Near-black cinematic background"
    GL glClear: 16640.  "GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT"


    GL glEnable: 2929.  "GL_DEPTH_TEST"
    GL glEnable: 32925.  "GL_MULTISAMPLE"
    GL useProgram: programId.
    self setUniforms.
    GL bindVertexArray: VAO.
    GL drawElements: 4 count: indexCount type: 5125 indices: 0.
    GL bindVertexArray: 0.
    GL useProgram: 0.
    'GLFW' ffiCall: #glfwSwapBuffers with: { window }.
    frameCount := frameCount + 1!

mainLoop
    | shouldClose |
    Transcript show: 'Starting main loop...'; cr.
    Transcript show: 'Vertex count: ', vertexCount printString, ', Index count: ', indexCount printString; cr.
    Transcript show: 'VAO: ', VAO printString, ', VBO: ', VBO printString, ', EBO: ', EBO printString; cr.
    Transcript show: ''; cr.
    Transcript show: 'Controls:'; cr.
    Transcript show: '  Mouse  - Look around'; cr.
    Transcript show: '  W/S    - Move forward/back'; cr.
    Transcript show: '  A/D    - Strafe left/right'; cr.
    Transcript show: '  Q/E    - Move down/up'; cr.
    Transcript show: '  ESC    - Exit'; cr.
    [
        shouldClose := 'GLFW' ffiCall: #glfwWindowShouldClose with: { window }.
        running and: [ shouldClose = 0 ]
    ] whileTrue: [
        'GLFW' ffiCall: #glfwPollEvents with: {}.
        self handleInput.
        self updateMouseLook.
        self render.
    ].
    Transcript show: 'Main loop ended after ', frameCount printString, ' frames'; cr!

cleanup
    Transcript show: 'Cleaning up...'; cr.
    (VAO isNil not) ifTrue: [ GL deleteVertexArray: VAO ].
    (VBO isNil not) ifTrue: [ GL deleteBuffer: VBO ].
    (EBO isNil not) ifTrue: [ GL deleteBuffer: EBO ].
    (programId isNil not) ifTrue: [ GL deleteProgram: programId ].
    (shadowProgramId isNil not) ifTrue: [ GL deleteProgram: shadowProgramId ].
    (gradientProgramId isNil not) ifTrue: [ GL deleteProgram: gradientProgramId ].
    (albedoTexture isNil not and: [ albedoTexture > 0 ]) ifTrue: [ self deleteTexture: albedoTexture ].
    (normalTexture isNil not and: [ normalTexture > 0 ]) ifTrue: [ self deleteTexture: normalTexture ].
    (metallicTexture isNil not and: [ metallicTexture > 0 ]) ifTrue: [ self deleteTexture: metallicTexture ].
    (roughnessTexture isNil not and: [ roughnessTexture > 0 ]) ifTrue: [ self deleteTexture: roughnessTexture ].
    (aoTexture isNil not and: [ aoTexture > 0 ]) ifTrue: [ self deleteTexture: aoTexture ].
    (shadowFBO isNil not and: [ shadowFBO > 0 ]) ifTrue: [ GL deleteFramebuffer: shadowFBO ].
    (shadowMap isNil not and: [ shadowMap > 0 ]) ifTrue: [ self deleteTexture: shadowMap ].
    'GLFW' ffiCall: #glfwTerminate with: {}.
    Transcript show: 'Cleanup complete'; cr!

deleteTexture: texId
    | arr |
    arr := ByteArray new: 4.
    arr uint32At: 0 put: texId.
    GL glDeleteTextures: 1 textures: arr!

run
    Transcript show: '=== Chess Scene 3D - Cinematic Lighting ==='; cr.
    self initialize.
    self initializeGLFW.
    self initializeShaders.
    self loadChessModel.
    self initializeBuffers.
    self loadTextures.
    self initializeShadowMap.
    self mainLoop.
    self cleanup.
    Transcript show: '=== Demo finished ==='; cr! !

!ChessScene3D class methodsFor!

demo
    ^self new run! !

Transcript show: ''; cr.
Transcript show: 'Chess Scene 3D (Full PBR + Shadows) loaded.'; cr.
Transcript show: 'Run with: ChessScene3D demo'; cr.
Transcript show: ''; cr.
Transcript show: 'Controls:'; cr.
Transcript show: '  Mouse  - Look around'; cr.
Transcript show: '  W/S    - Move forward/back'; cr.
Transcript show: '  A/D    - Strafe left/right'; cr.
Transcript show: '  Q/E    - Move down/up'; cr.
Transcript show: '  ESC    - Exit'; cr.
