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
 * Date modified: $Date: 2009-10-15 22:09:35 -0500 (Thu, 15 Oct 2009) $
 * Version:       $Rev: 13580 $
 * Author:        $Author: mccdo $
 * Id:            $Id: OSGWarpedSurfaceStage.h 13580 2009-10-16 03:09:35Z mccdo $
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

#include <osg/Geometry>
#include <osg/ShapeDrawable>
#include <osg/BlendFunc>
#include <osg/Depth>
#include <osg/Point>
#include <osg/PointSprite>
#include <osg/AlphaFunc>

#include <vtkPointData.h>
#include <vtkCellArray.h>
#include <vtkPoints.h>

#include <string>
#include <deque>
#include <vector>

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

class OSGStreamlineStage
{
public:
    ///Constructor
    OSGStreamlineStage();
    ///Destructor
    ~OSGStreamlineStage();

    ///create a Group of Stream Lines
    ves::xplorer::scenegraph::Geode* createInstanced(vtkPolyData* polyData, 
        int mult, const char* scalarName, const std::string& activeVector );

private:
    struct Point
    {
        double x[ 3 ];
        vtkIdType vertId;
    };
    
    ///Test if a particular line is backwards
    bool IsStreamlineBackwards( vtkIdType cellId, vtkPolyData* polydata );

    ///Process the streamline to get a list of points
    void ProcessStreamLines( vtkPolyData* polydata );

    ///two utility functions used to determine tm and tn, since texture dimension needs to be 2^n
    int mylog2(unsigned x);
    int mypow2(unsigned x);

    ///Configure a Geometry to draw a single point, but use the draw instanced PrimitiveSet to draw the point multiple times.
    void createSLPoint( osg::Geometry& geom, int nInstances, const osg::Vec3 position, const osg::Vec4 color );

    ///create the position array based on the passed in VTK points
    //float* createPositionArray( int numPoints , int mult, vtkPoints* points, const vtkIdType* pts, int &tm, int &tn);
    float* createPositionArray( int numPoints , int mult, std::deque< Point > pointList, int &tm, int &tn);

    ///create strealines
    void createStreamLines(vtkPolyData* polyData, ves::xplorer::scenegraph::Geode* geode, int mult, const char* scalarName);
    
    ///create the coloring scalar array
    float* createScalarArray( vtkIdType numPoints , int mult, vtkPointData* pointdata, std::deque< Point > pointList, int &tm, int &tn, const char* scalarName);

    ///The map of points to create a streamline line segment    
    std::vector< std::deque< Point > > m_streamlineList;
    
    std::string m_activeVector;

};
