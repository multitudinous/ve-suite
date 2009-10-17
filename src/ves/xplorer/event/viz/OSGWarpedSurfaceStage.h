/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#pragma once
//This class wraps the OSG Vertex Shader based rendering
//the Testure2D data is used as away to transfer data into the shader
//It acts as a raw array instead of 2D data

#ifdef WIN32
#include <windows.h>
#endif

#include <osgDB/ReadFile>
#include <osg/Geometry>
#include <osg/PositionAttitudeTransform>
//#include "VTKStage.h"
#include <vtkPointData.h>
#include <math.h>

#include <osg/Geometry>
#include <osg/io_utils>
#include <osg/Math>

class vtkPolyData;

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class Geode;
}
}
}

namespace ves
{
namespace xplorer
{
namespace event
{
namespace viz
{
class OSGWarpedSurfaceStage
{
public:
    OSGWarpedSurfaceStage(void);
    ~OSGWarpedSurfaceStage(void);

    //create a osgNode
    //polydata is supposed to be triangle strips
    //displacement is the vetice displacemnet vector for each of the point data in polydata
    //colorScalar is the scalar for the point data in polydata used to color the result
    ves::xplorer::scenegraph::Geode* createMesh(vtkPolyData* polydata, std::string displacement, std::string colorScalar);

private:

    //an m x n texture is used to transfer data
    int tm; 
    int tn;

    //two utility functions used to determine tm and tn, since texture dimension needs to be 2^n
    int mylog2(unsigned x);
    int mypow2(unsigned x);
        
    void createMeshData( osg::Geometry* geom, vtkPolyData* polydata, 
                        std::string displacement, std::string colorScalar);
};    
}
}
}
}
