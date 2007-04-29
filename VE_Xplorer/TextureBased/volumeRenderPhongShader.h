/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date: 2006-07-08 22:57:46 -0500 (Sat, 08 Jul 2006) $
 * Version:       $Rev: 4907 $
 * Author:        $Author: mccdo $
 * Id:            $Id: cfdScalarShaderManager.cxx 4907 2006-07-09 03:57:46Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VR_PHONG_SHADER_SOURCE_H
#define VR_PHONG_SHADER_SOURCE_H
//the shader inline source
static const char* vrPhongVertSource = {
   " \n"
   "//varying vec3  ViewVec;\n"
   "varying vec4 eyePos;\n"
   "uniform vec3 stepSize;\n"
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
     
      "//gl_TexCoord[0].s=dot(eyePos,gl_EyePlaneS[0]); \n"
      "//gl_TexCoord[0].t=dot(eyePos,gl_EyePlaneT[0]); \n"
      "//gl_TexCoord[0].p=dot(eyePos,gl_EyePlaneR[0]); \n"
      "//gl_TexCoord[0].q=dot(eyePos,gl_EyePlaneQ[0]); \n"
      "//gl_TexCoord[0] *= gl_TextureMatrix[0];\n"
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
      "gl_TexCoord[1].q = gl_MultiTexCoord1.x;\n"
      "gl_TexCoord[2].q = gl_MultiTexCoord1.y;\n"
      "gl_TexCoord[3].q = gl_MultiTexCoord1.z;\n"

      "vec3 lightVector = normalize(gl_ModelViewMatrix*gl_LightSource[0].position).xyz;\n"
      
      "gl_TexCoord[4].q = lightVector.x;\n"
      "gl_TexCoord[5].q = lightVector.y;\n"
      "gl_TexCoord[6].q = lightVector.z;\n"
   "} \n"
};

static const char* vrPhongFragSource = {
   //a volume rendering shader which applies a 2D transfer function
   //and phong shading
   "//varying vec3 halfVector;\n"
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
      "//float frontScalar = texture3D(volumeData,gl_TexCoord[0].xyz).a;\n"
      "vec3 jitter = viewRay*texture2D(jitter2D,gl_FragCoord.xy/jitterSize.xy).x;\n"
      "float frontScalar = texture3D(volumeData,gl_TexCoord[0].xyz+jitter).a;\n"

      "vec3 backCoord = vec3(gl_TexCoord[1].q,gl_TexCoord[2].q,gl_TexCoord[3].q);\n"
      "float backScalar = texture3D(volumeData,backCoord.xyz+jitter).a;\n"
      "vec3 forwardDiff;\n"
      "vec3 backwardDiff;\n"
      "forwardDiff.x = texture3D(volumeData,gl_TexCoord[4].xyz).a;\n"
      "forwardDiff.y = texture3D(volumeData,gl_TexCoord[5].xyz).a;\n"
      "forwardDiff.z = texture3D(volumeData,gl_TexCoord[6].xyz).a;\n"
      "backwardDiff.x = texture3D(volumeData,gl_TexCoord[1].xyz).a;\n"
      "backwardDiff.y = texture3D(volumeData,gl_TexCoord[2].xyz).a;\n"
      "backwardDiff.z = texture3D(volumeData,gl_TexCoord[3].xyz).a;\n"
      "\n"
      "vec3 normal = normalize(backwardDiff-forwardDiff  );\n"
      "vec4 textureColor = (fastUpdate==true)?texture2D(transferFunction,vec2(frontScalar)):texture2D(transferFunction,vec2(frontScalar,backScalar));\n"
      "//vec4 textureColor = (fastUpdate==true)?vec4(gl_TexCoord[0].xyz,.2):vec4(backCoord,.2);\n"
      "float l = length(normal);\n"
      "bool computeShade = (l> .01)?true:false;\n"
      "if(computeShade){\n"
      "   vec3 lightVector = vec3(gl_TexCoord[4].q,gl_TexCoord[5].q,gl_TexCoord[6].q);\n"
      "   vec3 L=normalize(lightVector); \n"
      "   float NDotL=max(dot(normal,L),0.0); \n"

      "   vec3 V=normalize(eyePos.xyz); \n"
      "   vec3 R=reflect(V,normal); \n"
      "   float RDotL=max(dot(R,L),0.0); \n"

         "//vec3 TotalAmbient=gl_LightSource[0].ambient.rgb*textureColor.rgb; \n"
         "vec3 TotalDiffuse=gl_LightSource[0].diffuse.rgb*textureColor.rgb*NDotL; \n"
         "vec3 TotalSpecular=gl_LightSource[0].specular.rgb*pow(RDotL,50.0); \n"
         "gl_FragColor =   vec4(TotalDiffuse+TotalSpecular,textureColor.a);\n"
      "}else\n"
      "{\n"
      "   gl_FragColor = textureColor;\n"
      "}\n"
      "gl_FragColor.a = 1.0-pow(1.0-gl_FragColor.a,alphaRatio);\n"
   "}\n"
};
#endif
