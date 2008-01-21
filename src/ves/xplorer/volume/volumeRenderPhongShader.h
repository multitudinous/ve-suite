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
#ifndef VR_PHONG_SHADER_SOURCE_H
#define VR_PHONG_SHADER_SOURCE_H
//the shader inline source
static const char* vrPhongVertSource =
    {
        " \n"
        "//#version 110\n"
        "//varying vec3  ViewVec;\n"
        "varying vec4 eyePos;\n"
        "//#version 110\n"
        "uniform vec3 stepSize;\n"
        "uniform vec3 datacenter;\n"
        "//varying vec3  lightVec;\n"
        "//varying vec3 nextTexCoord[3];\n"
        "//varying vec3 previousTexCoord[3];\n"

        " \n"
        "void main() \n"
        "{ \n"
        "gl_Position=ftransform(); \n"
        "gl_ClipVertex = gl_ModelViewMatrix*gl_Vertex;\n"
        " \n"
        "eyePos = gl_ClipVertex;\n"
        "gl_TexCoord[0] = gl_MultiTexCoord0;\n"
        "gl_TexCoord[4] = gl_TexCoord[0];\n"
        "gl_TexCoord[5] = gl_TexCoord[0];\n"
        "gl_TexCoord[6] = gl_TexCoord[0];\n"

        "gl_TexCoord[4].x = gl_TexCoord[0].x + stepSize.x;\n"
        "gl_TexCoord[5].y = gl_TexCoord[0].y + stepSize.y;\n"
        "gl_TexCoord[6].z = gl_TexCoord[0].z + stepSize.z;\n"
        "\n"
        "gl_TexCoord[1] = gl_TexCoord[0];\n"
        "gl_TexCoord[2] = gl_TexCoord[0];\n"
        "gl_TexCoord[3] = gl_TexCoord[0];\n"

        "gl_TexCoord[1].x = gl_TexCoord[0].x - stepSize.x;\n"
        "gl_TexCoord[2].y = gl_TexCoord[0].y - stepSize.y;\n"
        "gl_TexCoord[3].z = gl_TexCoord[0].z - stepSize.z;\n"

        "//Set the alphas of the extra texture coords as the back slice tcoord\n"
        "//transform view position and direction into object space\n"
        "vec4 cameraPosition = vec4(0,0,0,1);\n"
        "cameraPosition = gl_ModelViewMatrixInverse*cameraPosition;\n"
        "vec4 viewDirection = vec4(0.0,0.0,-1.0,1.0);\n"
        "//viewDirection = normalize(gl_ModelViewMatrixInverse*viewDirection) ;\n"
        "viewDirection = normalize(vec4(datacenter,1) - cameraPosition) ;\n"
        "//Compute the position of back texture coord\n"
        "vec4 eyeToVertex = normalize(gl_Vertex - cameraPosition);\n"
        "vec4 backTextureCoord = gl_TexCoord[0] - eyeToVertex*(gl_MultiTexCoord1.x/dot(viewDirection,eyeToVertex));\n"
        "gl_TexCoord[1].q = backTextureCoord.x;\n"
        "gl_TexCoord[2].q = backTextureCoord.y;\n"
        "gl_TexCoord[3].q = backTextureCoord.z;\n"

        "vec3 lightVector = normalize(gl_LightSource[0].position - gl_Vertex).xyz;\n"

        "gl_TexCoord[4].q = lightVector.x;\n"
        "gl_TexCoord[5].q = lightVector.y;\n"
        "gl_TexCoord[6].q = lightVector.z;\n"
        "} \n"
    };

static const char* vrPhongFragSource =
    {
        "//#version 110\n"
        //a volume rendering shader which applies a 2D transfer function
        //and phong shading
        "//Phong shading\n"
        "vec3 phongShading(vec3 normalVector, vec3 viewVector, vec3 lightVector,vec3 materialDiffuse)\n"
        "{\n"
        "//material properties\n"
        " vec3 ka = vec3(.1*materialDiffuse.r,.1*materialDiffuse.g,.1*materialDiffuse.b);\n"
        "//vec3 kd = vec3(.6,.6,.6);\n"
        " vec3 ks = vec3(.6,.6,.6);\n"
        " float shininess = 15.0;\n"
        "//light properties\n"
        " vec3 lightColor = vec3(1.,1.,1.);\n"
        " vec3 ambientLight = vec3(.3,.3,.3);\n"
        "//halfway vector\n"
        " vec3 halfVector = normalize(lightVector - viewVector);\n"
        " vec3 ambient = ka*ambientLight;\n"
        " //Compute the diffuse term\n"
        " float diffuseLight = max(dot(lightVector,normalVector),0.0);\n"
        " vec3 diffuse = materialDiffuse*lightColor*diffuseLight;\n"
        " //Compute the specular term\n"

        " float specularLight = (diffuseLight <= 0.0)?0.0:pow(max(dot(halfVector, normalVector),0.0),shininess);\n"
        " vec3 specular = ks*lightColor*specularLight; \n"
        "return ambient + diffuse + specular;\n"
        "}\n"

        "varying vec4 eyePos;\n"
        "uniform bool fastUpdate;"
        "uniform sampler3D volumeData;\n"
        "uniform sampler2D transferFunction;\n"
        "uniform sampler2D jitter2D;\n"
        "uniform vec3 viewRay;\n"
        "uniform vec2 jitterSize;\n"
        "uniform float alphaRatio;\n"

        "void main(void)\n"
        "{\n"
        "//dependent texture look up in transfer function \n"
        "float frontScalar = texture3D(volumeData,gl_TexCoord[0].xyz).a;\n"
        "//vec3 jitter = viewRay*texture2D(jitter2D,gl_FragCoord.xy/jitterSize.xy).x;\n"
        "//float frontScalar = texture3D(volumeData,gl_TexCoord[0].xyz+jitter).a;\n"

        "vec3 backCoord = vec3(gl_TexCoord[1].q,gl_TexCoord[2].q,gl_TexCoord[3].q);\n"
        "//float backScalar = texture3D(volumeData,backCoord.xyz+jitter).a;\n"
        "float backScalar = texture3D(volumeData,backCoord.xyz).a;\n"
        "gl_FragColor = (fastUpdate==true)?texture2D(transferFunction,vec2(frontScalar)):texture2D(transferFunction,vec2(frontScalar,backScalar));\n"
        "if(gl_FragColor.a > .1)\n"
        "{\n"
        "vec3 forwardDiff;\n"
        "vec3 backwardDiff;\n"
        "forwardDiff.x = texture3D(volumeData,gl_TexCoord[4].xyz).a;\n"
        "forwardDiff.y = texture3D(volumeData,gl_TexCoord[5].xyz).a;\n"
        "forwardDiff.z = texture3D(volumeData,gl_TexCoord[6].xyz).a;\n"
        "backwardDiff.x = texture3D(volumeData,gl_TexCoord[1].xyz).a;\n"
        "backwardDiff.y = texture3D(volumeData,gl_TexCoord[2].xyz).a;\n"
        "backwardDiff.z = texture3D(volumeData,gl_TexCoord[3].xyz).a;\n"
        "\n"
        "vec3 surfaceNormal = normalize(backwardDiff-forwardDiff  );\n"
        "vec3 lightVector = vec3(gl_TexCoord[4].q,gl_TexCoord[5].q,gl_TexCoord[6].q);\n"
        "lightVector = normalize(lightVector - gl_TexCoord[0].xyz);\n"
        "vec3 viewVector = normalize(eyePos.xyz - gl_TexCoord[0].xyz); \n"
        "gl_FragColor.rgb+= phongShading(surfaceNormal,viewVector,lightVector,gl_FragColor.rgb);\n"
        "}\n"
        "//gl_FragColor.a = 1.0-pow(1.0-gl_FragColor.a,alphaRatio);\n"
        "}\n"
    };
#endif
