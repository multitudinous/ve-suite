/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VR_BASIC_SHADER_SOURCE_H
#define VR_BASIC_SHADER_SOURCE_H
static const char* vrBasicVertSource = {
   "#version 110\n"
   "uniform vec3 datacenter;\n"
   "//varying vec3 sliceNormal;\n"
   "//varying vec3 sliceNormal;\n"
   "//uniform mat4 inverseModelViewMatrix;\n"
   "varying vec4 viewDirection;\n"
   "void main() \n"
   "{ \n"
   " \n"
      "gl_TexCoord[0] = gl_MultiTexCoord0;\n"
      "//transform view position and direction into object space\n"
      "vec4 cameraPosition = vec4(0,0,0,1);\n"
      "cameraPosition = gl_ModelViewMatrixInverse*cameraPosition;\n"
      "//vec4 viewDirection = vec4(0.0,0.0,-1.0,1.0);\n"
      "//viewDirection = normalize(gl_ModelViewMatrixInverse*viewDirection) ;\n"
      "viewDirection = normalize(vec4(datacenter,1) - cameraPosition) ;\n"
      "//Compute the position of back texture coord\n"
      "vec4 eyeToVertex = normalize(gl_Vertex - cameraPosition);\n"
      "gl_TexCoord[1]= gl_TexCoord[0] - eyeToVertex*(gl_MultiTexCoord1.x/dot(viewDirection,eyeToVertex));\n"
      "gl_ClipVertex = gl_ModelViewMatrix*gl_Vertex;\n"
      "///The texture space slice delta\n"
      "gl_TexCoord[3] = gl_MultiTexCoord1;\n"
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
   "//varying vec3 sliceNormal;\n"
   "varying vec4 viewDirection;\n"
   "void main(void)\n"
   "{\n"
      "//dependent texture look up in transfer function\n"
      "//vec3 jitter = vec3(texture2D(jitter2D,gl_FragCoord.xy/jitterSize.xy).a);\n"
      "//float jitter = texture2D(jitter2D,gl_FragCoord.xy/128.0).a;\n"
      "//jitter = normalize(jitter);\n"
      "//float sfront = texture3D(volumeData, gl_TexCoord[0].xyz + jitter).a;\n"
      "//float sback = texture3D(volumeData, gl_TexCoord[1].xyz + jitter).a;\n"
      "float sfront = texture3D(volumeData,gl_TexCoord[0].xyz).a;\n"
      "float sback = texture3D(volumeData,gl_TexCoord[1].xyz).a;\n"
      "//PreIntegration\n"
      "gl_FragColor = (fastUpdate==true)?texture2D(transferFunction,vec2(sback)):texture2D(transferFunction,vec2(sfront,sback)); \n"
      "//gl_FragColor = vec4(jitter*.1,.2); \n"
      "//gl_FragColor = vec4(gl_FragCoord.xy,0,.2); \n"
      "//gl_FragColor = (fastUpdate==true)?vec4(gl_TexCoord[1].xyz,.2):vec4(gl_TexCoord[0].xyz,.2); \n"
      "//gl_FragColor = (fastUpdate==true)?vec4(gl_TexCoord[1].xyz,.2):vec4(sliceNormal.xyz,.2); \n"
      "//set the opacity to .2 for all fragments\n"
      "//gl_FragColor.a = 1.0-pow(1.0-gl_FragColor.a,alphaRatio);\n"
   "}\n"
};
#endif// VR_BASIC_VERT_SOURCE_H
