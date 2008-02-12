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
#include <ves/xplorer/event/data/DataSetAxis.h>

#include <ves/open/xml/Command.h>

#include <ves/xplorer/Debug.h>

#include <osg/Node>
#include <osg/Geode>
#include <osg/Group>
#include <osg/Geometry>
#include <osg/Array>
#include <osg/LineWidth>
#include <osgText/Font>
#include <osgText/Text>

#include <fstream>
#include <sstream>
#include <iomanip>
#include <string>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
DataSetAxis::DataSetAxis( void )
{
    vprDEBUG( vesDBG, 2 ) << "constructing cfdScalarBarActor"
    << std::endl << vprDEBUG_FLUSH;
    bbox[ 0 ] = bbox[ 2 ] = bbox[ 4 ] = 0.0f;
    bbox[ 1 ] = bbox[ 3 ] = bbox[ 5 ] = 1.0f;

    xAxisLabel = "X Axis";
    yAxisLabel = "Y Axis";
    zAxisLabel = "Z Axis";
    axisGroup = new ves::xplorer::scenegraph::Group();
}
////////////////////////////////////////////////////////////////////////////////
DataSetAxis::~DataSetAxis()
{
    vprDEBUG( vesDBG, 2 ) << "deconstructing cfdScalarBarActor"
    << std::endl << vprDEBUG_FLUSH;

    // may note need to delete anything
    vprDEBUG( vesDBG, 2 ) << "   finished deconstructing cfdScalarBarActor"
    << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void DataSetAxis::SetBoundingBox( double* inBBox )
{
    for( size_t i = 0; i < 6; ++i )
    {
        bbox[ i ] = inBBox[ i ];
    }
}
////////////////////////////////////////////////////////////////////////////////
void DataSetAxis::SetAxisLabels( std::string xAxis,
                                 std::string yAxis,
                                 std::string zAxis )
{
    xAxisLabel = xAxis;
    yAxisLabel = yAxis;
    zAxisLabel = zAxis;
}
////////////////////////////////////////////////////////////////////////////////
void DataSetAxis::CreateAxis( void )
{
    //Now add the labels
    osg::Group* tempGroup = axisGroup.get() ;
    size_t numChildren = tempGroup->getNumChildren();
    for( size_t i = 0; i < numChildren; ++i )
    {
        //we always remove 0 because once we start removing there are no longer
        // n number of children but 1 less so always remove 0
        tempGroup->removeChild( 0, 1 );
    }

    tempGroup->addChild( CreateAxisLabels( xAxisLabel, bbox[ 1 ], bbox[ 2 ], bbox[ 4 ] ).get() );
    tempGroup->addChild( CreateAxisLabels( yAxisLabel, bbox[ 0 ], bbox[ 3 ], bbox[ 4 ] ).get() );
    tempGroup->addChild( CreateAxisLabels( zAxisLabel, bbox[ 0 ], bbox[ 2 ], bbox[ 5 ] ).get() );

    //Now add the lines
    tempGroup->addChild( CreateAxisLines().get() );
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::Group* DataSetAxis::GetAxis( void )
{
    return axisGroup.get();
}
//////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr< osg::Geode > DataSetAxis::CreateAxisLines( void )
{
    // create LINES
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    // create Geometry object to store all the vetices and lines primtive.
    osg::ref_ptr< osg::Geometry > linesGeom = new osg::Geometry();

    // this time we'll prealloacte the vertex array to the size we
    // need and then simple set them as array elements, 8 points
    // makes 4 line segments.
    osg::Vec3Array* vertices = new osg::Vec3Array( 6 );
    ( *vertices )[0].set( bbox[ 0 ], bbox[ 2 ], bbox[ 4 ] );
    ( *vertices )[1].set( bbox[ 1 ], bbox[ 2 ], bbox[ 4 ] );
    ( *vertices )[2].set( bbox[ 0 ], bbox[ 2 ], bbox[ 4 ] );
    ( *vertices )[3].set( bbox[ 0 ], bbox[ 3 ], bbox[ 4 ] );
    ( *vertices )[4].set( bbox[ 0 ], bbox[ 2 ], bbox[ 4 ] );
    ( *vertices )[5].set( bbox[ 0 ], bbox[ 2 ], bbox[ 5 ] );

    // pass the created vertex array to the points geometry object.
    linesGeom->setVertexArray( vertices );

    // set the colors as before, plus using the aobve
    osg::Vec4Array* colors = new osg::Vec4Array;
    colors->push_back( osg::Vec4( 1.0f, 1.0f, 0.0f, 1.0f ) );
    linesGeom->setColorArray( colors );
    linesGeom->setColorBinding( osg::Geometry::BIND_OVERALL );

    // set the normal in the same way color.
    osg::Vec3Array* normals = new osg::Vec3Array;
    normals->push_back( osg::Vec3( 0.0f, -1.0f, 0.0f ) );
    linesGeom->setNormalArray( normals );
    linesGeom->setNormalBinding( osg::Geometry::BIND_OVERALL );

    // This time we simply use primitive, and hardwire the number of coords to use
    // since we know up front,
    linesGeom->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 6 ) );

    //Set the line width
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth;
    lineWidth->setWidth( 5.0f );
    stateset->setAttributeAndModes( lineWidth.get(), osg::StateAttribute::ON );
    geode->setStateSet( stateset.get() );

    // add the points geomtry to the geode.
    geode->addDrawable( linesGeom.get() );
    return geode;
}
//////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr< osg::Geode > DataSetAxis::CreateAxisLabels( std::string textIn, double x, double y, double z )
{
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    float characterSizeFont = 0.05f;
    osgText::Text* text1 = new osgText::Text;
    text1->setFont( "fonts/times.ttf" );
    text1->setCharacterSize( characterSizeFont );
    osg::Vec3 pos1( x, y, z );
    text1->setPosition( pos1 );
    osg::Vec4 color( 1.0f, 1.0f, 0.0f, 1.0f );
    text1->setColor( color );
    text1->setAxisAlignment( osgText::Text::SCREEN );
    text1->setText( textIn );
    text1->setLayout( osgText::Text::LEFT_TO_RIGHT );
    geode->addDrawable( text1 );
    return geode;
}
