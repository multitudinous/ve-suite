#ifndef SHADERS_H
#define SHADERS_H

/*----------------------------------------------------------------------------*/
char vertex_shader[] =
    "varying vec4 eyePos; \n"
    "varying vec3 lightPos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "eyePos = gl_ModelViewMatrix * gl_Vertex; \n"
        "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
        "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"

        "gl_TexCoord[ 0 ].s = dot( eyePos, gl_EyePlaneS[ 0 ] ); \n"
        "gl_TexCoord[ 0 ].t = dot( eyePos, gl_EyePlaneT[ 0 ] ); \n"
        "gl_TexCoord[ 0 ].p = dot( eyePos, gl_EyePlaneR[ 0 ] ); \n"
        "gl_TexCoord[ 0 ].q = dot( eyePos, gl_EyePlaneQ[ 0 ] ); \n"

        "gl_TexCoord[ 1 ].st = gl_MultiTexCoord0.xy; \n"
    "} \n";
/*----------------------------------------------------------------------------*/
char fragment_shader[] =
    "uniform sampler2D projectionMap; \n"

    "varying vec4 eyePos; \n"
    "varying vec3 lightPos; \n"
    "varying vec3 normal; \n"

    "vec4 getProjectionMap() \n"
    "{ \n"
        "vec4 color = texture2DProj( shadowMap, gl_TexCoord[ 0 ] ); \n"

        "return color; \n"
    "} \n";

    "void main() \n"
    "{ \n"
        "vec3 N = normalize( normal ); \n"
        "vec3 L = normalize( lightPos ); \n"
        "float NDotL = max( dot( N, L ), 0.0 ); \n"

        "vec3 V = normalize( eyePos.xyz ); \n"
        "vec3 R = reflect( V, N ); \n"
        "float RDotL = max( dot( R, L ), 0.0 ); \n"

        "vec3 totalAmbient = gl_LightSource[ 0 ].ambient.rgb * \n"
                            "gl_FrontMaterial.ambient.rgb; \n"
        "vec3 totalDiffuse = gl_LightSource[ 0 ].diffuse.rgb * \n"
                            "gl_FrontMaterial.diffuse.rgb * NDotL; \n"
        "vec3 totalSpecular = gl_LightSource[ 0 ].specular.rgb * \n"
                             "gl_FrontMaterial.specular.rgb * \n"
                             "pow( RDotL, gl_FrontMaterial.shininess ); \n"

        "vec3 color = totalAmbient + totalDiffuse + totalSpecular; \n"

        "gl_FragColor = vec4( color, 1.0 ) * getProjectionMap(); \n"
    "} \n";
/*----------------------------------------------------------------------------*/

#endif //SHADERS_H
