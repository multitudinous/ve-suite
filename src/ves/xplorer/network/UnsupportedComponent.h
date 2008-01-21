/*************** <auto-copyright.rb BEGIN do not edit this line> *************
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
 *************** <auto-copyright.rb END do not edit this line> **************/
#ifndef GETVESUITE_UnsupportedComponent_H
#define GETVESUITE_UnsupportedComponent_H

#include <string>

std::string GetVESuite_UnsupportedComponent( void )
{
    std::string osgData;
    osgData.append( "MatrixTransform {  DataVariance STATIC  name \"Scene Root\"  nodeMask 0xff  cullingActive TRUE  StateSet {    DataVariance STATIC    rendering_hint DEFAULT_BIN    renderBinMode INHERIT    GL_LIGHTING ON  }  referenceFrame RELATIVE  Matrix {    1 0 0 0    0 1 0 0    0 0 1 0    0 0 0 1  }  num_children 1  Group {    DataVariance STATIC    name \"Box01\"    nodeMask 0xff    cullingActive TRUE    num_children 1    Geode {      DataVariance STATIC      nodeMask 0xff      cullingActive TRUE      num_drawables 1      Geometry {        DataVariance DYNAMIC        StateSet {          DataVariance STATIC          rendering_hint DEFAULT_BIN          renderBinMode INHERIT          GL_CULL_FACE ON          GL_LIGHTING ON          0xba1 ON          Material {            DataVariance STATIC            ColorMode OFF            ambientColor 1 0 0 1            diffuseColor 1 0 0 1            specularColor 0 0 0 1            emissionColor 0 0 0 1            shininess 0          }        }        useDisplayList TRUE        useVertexBufferObjects FALSE        PrimitiveSets 1        {          DrawArrays TRIANGLES 0 36        }        VertexArray Vec3Array 36        {          -6 -6 0          -6 6 0          6 6 0          6 6 0          6 -6 0          -6 -6 0          -6 -6 12          6 -6 12          6 6 12          6 6 12          -6 6 12          -6 -6 12          -6 -6 0          6 -6 0          6 -6 12          6 -6 12          -6 -6 12          -6 -6 0          6 -6 0          6 6 0          6 6 12          6 6 12          6 -6 12          6 -6 0          6 6 0          -6 6 0          -6 6 12          -6 6 12          6 6 12          6 6 0          -6 6 0          -6 -6 0          -6 -6 12          -6 -6 12          -6 6 12          -6 6 0        }        NormalBinding PER_VERTEX        NormalArray Vec3Array 36        {          0 0 -1          0 0 -1          0 0 -1          0 0 -1          0 0 -1          0 0 -1          0 0 1          0 0 1          0 0 1          0 0 1          0 0 1          0 0 1          0 -1 0          0 -1 0          0 -1 0          0 -1 0          0 -1 0          0 -1 0          1 0 0          1 0 0          1 0 0          1 0 0          1 0 0          1 0 0          0 1 0          0 1 0          0 1 0          0 1 0          0 1 0          0 1 0          -1 0 0          -1 0 0          -1 0 0          -1 0 0          -1 -0 -0          -1 0 0        }      }    }  }}" );
    return osgData;
}
#endif

