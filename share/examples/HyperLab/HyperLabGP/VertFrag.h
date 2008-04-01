/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef VERT_FRAG_H
#define VERT_FRAG_H

////////////////////////////////////////////////////////////////////////////////
char lights_vertex[] =
    "void main() \n"
    "{ \n"
        //Billboard the quads
        "float coronaSize = 20.0; \n"
        "vec3 pos = coronaSize * ( gl_Vertex.x * vec3( gl_ModelViewMatrix[ 0 ][ 0 ], \n"
                                                      "gl_ModelViewMatrix[ 1 ][ 0 ], \n"
                                                      "gl_ModelViewMatrix[ 2 ][ 0 ] ) +  \n"
                                  "gl_Vertex.y * vec3( gl_ModelViewMatrix[ 0 ][ 1 ],  \n"
                                                      "gl_ModelViewMatrix[ 1 ][ 1 ],  \n"
                                                      "gl_ModelViewMatrix[ 2 ][ 1 ] ) ); \n"

        "pos.x += 0.0; \n"
        "pos.y += 0.0; \n"
        "pos.z += 100.0; \n"

        "gl_Position = gl_ModelViewProjectionMatrix * vec4( pos, 1.0 ); \n"
        //"gl_Position = ftransform(); \n"

        "gl_TexCoord[ 0 ].xy = 0.5 * gl_Vertex.xy + 0.5; \n"
    "} \n";

char lights_fragment[] =
    "uniform sampler2D baseMap; \n"

    "float saturate( float inValue ) \n"
    "{ \n"
        "return clamp( inValue, 0.0, 1.0 ); \n"
    "} \n"

    "void main() \n"
    "{ \n"
        "vec4 corona = texture2D( baseMap, gl_TexCoord[ 0 ].xy ); \n"

        //"vec3 vdir = vec3( view_direction.x, view_direction.y, view_direction.z ); \n"
        //"vec3 cdir = vec3( coronaDir.x, coronaDir.y, coronaDir.z ); \n"

        //Glow depending on cosine dot product with corona directon
        //"float glow = saturate( dot( vdir, cdir ) ); \n"

        //"float strength = 1.0; \n"
        //"float coronaExp = 1.0; \n"
        //"gl_FragColor = strength * pow( glow, coronaExp ) * corona * lightColor; \n"

        "gl_FragColor = vec4( 1.0, 0.0, 0.0, 1.0 ); \n"
    "} \n";
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

char options_base_map_fragment[] =
    "uniform sampler2D baseMap; \n"
    "vec3 getBaseMapValue() \n"
    "{ \n"
    "   return vec3( texture2D( baseMap, gl_TexCoord[ 1 ].st ) ); \n"
    "} \n";

char options_base_map_fragment_off[] =
    "vec3 getBaseMapValue() \n"
    "{ \n"
    "   return vec3( 1.0, 1.0, 1.0 ); \n"
    "} \n";

char options_env_map_fragment[] =
    "uniform samplerCube envMap; \n"
    "void getReflectionValue( inout vec3 base, inout vec3 R ) \n"
    "{ \n"
        "vec3 reflection = textureCube( envMap, R ).rgb; \n"
        "base = mix( base, reflection, 0.05 ); \n"
    "} \n";

char options_env_map_fragment_off[] =
    "void getReflectionValue( inout vec3 base, inout vec3 R ) \n"
    "{ \n"
    "  base = base * vec3(1.0, 1.0, 1.0); \n"
    "  R = R * vec3(1.0, 1.0, 1.0); \n"
    "} \n";

char options_shadow_map_fragment[] =
    "uniform sampler2DShadow shadowMap; \n"
    "vec4 getShadowMapValue() \n"
    "{ \n"
        "const float kTransparency = 0.4; \n"

        "vec3 shadowUV = gl_TexCoord[ 0 ].stp / gl_TexCoord[ 0 ].q; \n"
        "vec4 shadowColor = vec4( 1.0, 1.0, 1.0, 1.0 ); \n"

        "if( shadowUV.x >= 0.0 && \n"
            "shadowUV.y >= 0.0 && \n"
            "shadowUV.x <= 1.0 && \n"
            "shadowUV.y <= 1.0 ) \n"
        "{ \n"
            "float mapScale = 1.0 / 4096.0; \n"

            "shadowColor = shadow2D( shadowMap, shadowUV ); \n"

            //1AA on shadowMap
            "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3(  mapScale,  mapScale, 0.0 ) ); \n"
            "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3(  mapScale, -mapScale, 0.0 ) ); \n"
            "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3( -mapScale,  mapScale, 0.0 ) ); \n"
            "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3( -mapScale, -mapScale, 0.0 ) ); \n"
            "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3(  mapScale,  0.0,      0.0 ) ); \n"
            "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3( -mapScale,  0.0,      0.0 ) ); \n"
            "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3(  0.0,       mapScale, 0.0 ) ); \n"
            "shadowColor += shadow2D( shadowMap, shadowUV.xyz + vec3(  0.0,      -mapScale, 0.0 ) ); \n"

            "shadowColor = shadowColor / 18.0; \n"

            "shadowColor += kTransparency; \n"
            "shadowColor = clamp( shadowColor, 0.0, 1.0 ); \n"
        "} \n"
        "return shadowColor; \n"
    "} \n";

char options_shadow_map_fragment_off[] =
    "vec4 getShadowMapValue() \n"
    "{ \n"
        "return vec4( 1.0, 1.0, 1.0, 1.0 ); \n"
    "} \n";

char options_fragment[] =
    "varying vec4 eyePos; \n"
    "varying vec3 lightPos; \n"
    "varying vec3 normal; \n"

    "vec3 getBaseMapValue(); \n"
    "void getReflectionValue( inout vec3 base, inout vec3 R ); \n"
    "vec4 getShadowMapValue(); \n"

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

        "ambientDiffuse *= getBaseMapValue(); \n"

        "vec3 base = ambientDiffuse + TotalSpecular; \n"
        "base *= gl_FrontMaterial.emission.rgb; \n"

        "getReflectionValue( base, R ); \n"

        "gl_FragColor = vec4( base, 1.0 ) * getShadowMapValue(); \n"
    "} \n";

////////////////////////////////////////////////////////////////////////////////

#endif // VERT_FRAG_H
