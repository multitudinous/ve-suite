#ifndef VERT_FRAG_H
#define VERT_FRAG_H

////////////////////////////////////////////////////////////////////////////////
char base_vertex[] =
    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "vec3 eyePos = vec3( gl_ModelViewMatrix * gl_Vertex ); \n"
        "vec3 lightPos = gl_LightSource[ 1 ].position.xyz; \n"
        "vec3 normal = vec3( gl_NormalMatrix * gl_Normal ); \n"

        "vec3 N = normalize( normal ); \n"
        "vec3 L = normalize( lightPos ); \n"
        "float NDotL = max( 0.0, dot( N, L ) ); \n"

        "vec3 TotalAmbient = gl_LightSource[ 1 ].ambient.rgb * \n"
                            "gl_FrontMaterial.ambient.rgb; \n"
        "vec3 TotalDiffuse = gl_LightSource[ 1 ].diffuse.rgb * \n"
                            "gl_FrontMaterial.diffuse.rgb*NDotL; \n"
        "vec3 TotalSpecular = gl_LightSource[ 1 ].specular.rgb * \n"
                             "gl_FrontMaterial.specular.rgb * \n"
                             "pow( NDotL, gl_FrontMaterial.shininess ); \n"

        "vec3 color = TotalAmbient + TotalDiffuse + TotalSpecular; \n"
        "color *= gl_FrontMaterial.emission.rgb; \n"

        "gl_FrontColor = vec4( color, 1.0 ); \n"
    "} \n";

char base_fragment[] =
    "void main() \n"
    "{ \n"
        "gl_FragColor = gl_Color; \n"
    "} \n";

////////////////////////////////////////////////////////////////////////////////

char xray_vertex[] =
    "varying vec3 eyePos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "eyePos = vec3( gl_ModelViewMatrix * gl_Vertex ); \n"
        "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"
    "} \n";

char xray_fragment[] =
    "varying vec3 eyePos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "vec3 N = normalize( normal ); \n"
        "vec3 V = normalize( eyePos ); \n"

        "float opac = dot( N, V ); \n"
        "opac = abs( opac ); \n"
        "opac = 1.0 - pow( opac, 0.8 ); \n"

        "gl_FragColor = opac * vec4( 0.0, 1.0, 0.0, 1.0 ); \n"
    "} \n";

////////////////////////////////////////////////////////////////////////////////

char options_vertex[] =
    //"uniform bvec4 options; \n"

    "varying vec4 eyePos; \n"
    "varying vec3 lightPos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "eyePos = gl_ModelViewMatrix * gl_Vertex; \n"
        "lightPos = gl_LightSource[ 1 ].position.xyz; \n"
        "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"

        //"if( options ) \n"
        //"{ \n"
            "gl_TexCoord[ 0 ].s = dot( eyePos, gl_EyePlaneS[ 0 ] ); \n"
            "gl_TexCoord[ 0 ].t = dot( eyePos, gl_EyePlaneT[ 0 ] ); \n"
            "gl_TexCoord[ 0 ].p = dot( eyePos, gl_EyePlaneR[ 0 ] ); \n"
            "gl_TexCoord[ 0 ].q = dot( eyePos, gl_EyePlaneQ[ 0 ] ); \n"
        //"} \n"

        //"if( options ) \n"
        //"{ \n"
            "gl_TexCoord[ 1 ].st = gl_MultiTexCoord0.xy; \n"
        //"} \n"
    "} \n";

char options_fragment[] =
    "uniform bvec4 options; \n"
    "uniform sampler2DShadow shadowMap; \n"
    "uniform samplerCube envMap; \n"
    "uniform sampler2D baseMap; \n"

    "varying vec4 eyePos; \n"
    "varying vec3 lightPos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "vec3 N = normalize( normal ); \n"
        "vec3 L = normalize( lightPos ); \n"
        "float NDotL = max( dot( N, L ), 0.0 ); \n"

        "vec3 V = normalize( eyePos.xyz ); \n"
        "vec3 R = reflect( V, N ); \n"
        "float RDotL = max( dot( R, L ), 0.0 ); \n"

        "vec3 TotalAmbient = gl_LightSource[ 1 ].ambient.rgb * \n"
                            "gl_FrontMaterial.ambient.rgb; \n"
        "vec3 TotalDiffuse = gl_LightSource[ 1 ].diffuse.rgb * \n"
                            "gl_FrontMaterial.diffuse.rgb * NDotL; \n"
        "vec3 TotalSpecular = gl_LightSource[ 1 ].specular.rgb * \n"
                             "gl_FrontMaterial.specular.rgb * \n"
                             "pow( RDotL, gl_FrontMaterial.shininess ); \n"

        "vec3 ambientDiffuse = TotalAmbient + TotalDiffuse; \n"
        "if( options.y ) \n"
        "{ \n"
            "vec3 texture = vec3( texture2D( baseMap, gl_TexCoord[ 1 ].st ) ); \n"
            "ambientDiffuse *= texture; \n"
        "} \n"

        "vec3 base = ambientDiffuse + TotalSpecular; \n"
        "base *= gl_FrontMaterial.emission.rgb; \n"

        "if( options.z ) \n"
        "{ \n"
            "vec3 reflection = textureCube( envMap, R ).rgb; \n"

            "base = mix( base, reflection, 0.05 ); \n"
        "} \n"

        "if( options.w ) \n"
        "{ \n"
            "const float kTransparency = 0.3; \n"

            "vec3 shadowUV = gl_TexCoord[ 0 ].stp / gl_TexCoord[ 0 ].q; \n"
            "float mapScale = 1.0 / 4096.0; \n"

            "vec4 shadowColor = shadow2D( shadowMap, shadowUV ); \n"

            "for( int i = 1; i < 4; ++i ) \n"
            "{ \n"
                "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3(  i * mapScale,  i * mapScale, 0 ) ); \n"
                "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3(  i * mapScale, -i * mapScale, 0 ) ); \n"
                "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3( -i * mapScale,  i * mapScale, 0 ) ); \n"
                "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3( -i * mapScale, -i * mapScale, 0 ) ); \n"
                "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3(  i * mapScale,  0,            0 ) ); \n"
                "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3( -i * mapScale,  0,            0 ) ); \n"
                "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3(  0,             i * mapScale, 0 ) ); \n"
                "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3(  0,            -i * mapScale, 0 ) ); \n"
            "} \n"

            "shadowColor = shadowColor / 25.0; \n"

            "shadowColor += kTransparency; \n"
            "shadowColor = clamp( shadowColor, 0.0, 1.0 ); \n"

            "if( shadowUV.x >= 0.0 && \n"
                "shadowUV.y >= 0.0 && \n"
                "shadowUV.x <= 1.0 && \n"
                "shadowUV.y <= 1.0 ) \n"
            "{ \n"
                "gl_FragColor = vec4( base, 1.0 ) * shadowColor; \n"
            "} \n"

            "else \n"
            "{ \n"
                "gl_FragColor = vec4( base, 1.0 ); \n"
            "} \n"
        "} \n"
        "else \n"
        "{ \n"
            "gl_FragColor = vec4( base, 1.0 ); \n"
        "} \n"
    "} \n";

////////////////////////////////////////////////////////////////////////////////

#endif // VERT_FRAG_H
