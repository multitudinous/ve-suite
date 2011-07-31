/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

#include <ves/xplorer/scenegraph/VTKParticleTextureCreator.h>

class vtkPolyData;

namespace ves
{
namespace xplorer
{
class DataSet;
namespace scenegraph
{
class Geode;
}
}
}

class OSGParticleStage
{
public:
    ///Constructor
    OSGParticleStage();
    ///Destructor
    ~OSGParticleStage();

    ///create a Group of Stream Lines
    ves::xplorer::scenegraph::Geode* createInstanced(const std::vector< ves::xplorer::DataSet* >& transData, 
        const std::string& activeScalar, const std::string& activeVector );

    void SetParticleDiameter( int pDiameter );

private:
    ///Process the streamline to get a list of points
    void ProcessStreamLines( vtkPolyData* polydata );


    ///Configure a Geometry to draw a single point, but use the draw instanced PrimitiveSet to draw the point multiple times.
    void createSLPoint( osg::Geometry& geom, int nInstances );

    ///create the position array based on the passed in VTK points
    //float* createPositionArray( int numPoints , int mult, vtkPoints* points, const vtkIdType* pts, int &tm, int &tn);
    //float* createPositionArray( int numPoints , int mult, std::deque< Point > pointList, int &tm, int &tn);

    ///create strealines
    void createStreamLines( ves::xplorer::scenegraph::Geode* geode );
    
    ///create the coloring scalar array
    //float* createScalarArray( vtkIdType numPoints , int mult, vtkPointData* pointdata, std::deque< Point > pointList, int &tm, int &tn, const std::string& scalarName);

    ///The map of points to create a streamline line segment    
    std::vector< std::deque< ves::xplorer::scenegraph::VTKParticleTextureCreator::Point > > m_streamlineList;
    ///The raw collection of points
    std::vector< std::vector< std::pair< vtkIdType, double* > > >  m_pointCollection;
    ///The raw data for the respective points
    std::vector< std::vector< std::pair< std::string, std::vector< double > > > >  m_dataCollection;
    ///Streamline ordered raw data for the respective lines
    std::vector< std::vector< std::pair< std::string, std::vector< double > > > >  m_lineDataCollection;
    ///Container holding all of the datasets for these particles
    std::vector< ves::xplorer::DataSet* > m_transientDataSet;
    ///The active vector for the particles
    std::string m_activeVector;
    ///The active scalar for these particles
    std::string m_activeScalar;
    ///The bounding box for the particle set
    double m_bb[6];
    ///The desired particle diameter
    float m_particleDiameter;

};
