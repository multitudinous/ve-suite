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
   "void main() \n"
   "{ \n"
       "gl_Position=ftransform(); \n"
       "gl_ClipVertex = gl_ModelViewMatrix*gl_Vertex;\n"
   " \n"
      "gl_TexCoord[0].s=dot(gl_ClipVertex,gl_EyePlaneS[0]); \n"
      "gl_TexCoord[0].t=dot(gl_ClipVertex,gl_EyePlaneT[0]); \n"
      "gl_TexCoord[0].p=dot(gl_ClipVertex,gl_EyePlaneR[0]); \n"
      "gl_TexCoord[0].q=dot(gl_ClipVertex,gl_EyePlaneQ[0]); \n"
      "gl_TexCoord[0] *= gl_TextureMatrix[0];\n"
      
   "} \n"
};
static const char* vrBasicFragSource = {
   //a volume rendering shader which applies a 1D transfer function
   "uniform sampler3D volumeData;\n"
   "uniform sampler1D transferFunction;\n"
   "void main(void)\n"
   "{\n"
      "//dependent texture look up in transfer function\n"
      "vec2 scalar = texture3D(volumeData,gl_TexCoord[0].xyz).ga;\n"
      "gl_FragColor = /*(scalar.x==0.0)?*/texture1D(transferFunction,scalar.y);//:vec4(0,0,0,0);\n"

      "//set the opacity to .2 for all fragments\n"
      "gl_FragColor.a *= gl_Color.a;\n"
   "}\n"
};
#endif// VR_BASIC_VERT_SOURCE_H
