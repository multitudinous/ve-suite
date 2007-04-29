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
 * Date modified: $Date: 2007-04-05 15:23:00 -0500 (Thu, 05 Apr 2007) $
 * Version:       $Rev: 7270 $
 * Author:        $Author: biv $
 * Id:            $Id: cfdScalarShaderManager.cxx 7270 2007-04-05 20:23:00Z biv $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VR_BASIC_SHADER_SOURCE_H
#define VR_BASIC_SHADER_SOURCE_H
static const char* vrBasicVertSource = {
   "//#version 110\n"
   "void main() \n"
   "{ \n"
   " \n"
      "//gl_TexCoord[0].s=dot(gl_ClipVertex,gl_EyePlaneS[0]); \n"
      "//gl_TexCoord[0].t=dot(gl_ClipVertex,gl_EyePlaneT[0]); \n"
      "//gl_TexCoord[0].p=dot(gl_ClipVertex,gl_EyePlaneR[0]); \n"
      "//gl_TexCoord[0].q=dot(gl_ClipVertex,gl_EyePlaneQ[0]); \n"
      "//gl_TexCoord[0] *= gl_TextureMatrix[0];\n"
      "gl_TexCoord[0] = gl_MultiTexCoord0;\n"
      "gl_TexCoord[1] = gl_MultiTexCoord1;\n"
       "gl_ClipVertex = gl_ModelViewMatrix*gl_Vertex;\n"
       "gl_Position=ftransform(); \n"
      
   "} \n"
};
static const char* vrBasicFragSource = {
   //a volume rendering shader which applies a 2D transfer function
   "uniform sampler3D volumeData;\n"
   "uniform sampler2D transferFunction;\n"
   "uniform sampler2D jitter2D;\n"
   "uniform vec3 viewRay;\n"
   "uniform vec2 jitterSize;\n"
   "uniform bool fastUpdate;\n"
   "uniform float alphaRatio;\n"
   "void main(void)\n"
   "{\n"
      "//dependent texture look up in transfer function\n"
      "//vec3 jitter = viewRay*texture2D(jitter2D,gl_FragCoord.xy/jitterSize.xy).x;\n"
      "//jitter = normalize(jitter);\n"
      "//float sfront = texture3D(volumeData,gl_TexCoord[0].xyz+jitter).a;\n"
      "//float sback = texture3D(volumeData,gl_TexCoord[1].xyz+jitter).a;\n"
      "float sfront = texture3D(volumeData,gl_TexCoord[0].xyz).a;\n"
      "float sback = texture3D(volumeData,gl_TexCoord[1].xyz).a;\n"
      "//PreIntegration\n"
      "gl_FragColor = (fastUpdate==true)?texture2D(transferFunction,vec2(sback)):texture2D(transferFunction,vec2(sfront,sback)); \n"
      "//gl_FragColor = (fastUpdate==true)?vec4(gl_TexCoord[1].xyz,.2):vec4(gl_TexCoord[0].xyz,.2); \n"

      "//set the opacity to .2 for all fragments\n"
      "gl_FragColor.a = 1.0-pow(1.0-gl_FragColor.a,alphaRatio);\n"
   "}\n"
};
#endif// VR_BASIC_VERT_SOURCE_H
