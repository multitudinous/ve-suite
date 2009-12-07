/*************** <auto-copyright.rb BEGIN do not edit this line> *************
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
 *************** <auto-copyright.rb END do not edit this line> **************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/vtkActorToStreamLine.h>
#include <ves/xplorer/util/readWriteVtkThings.h>

// --- VTK Includes --- //
#include <vtkDataSet.h>
#include <vtkPolyData.h>
#include <vtkProperty.h>
#include <vtkPointData.h>
#include <vtkCellData.h>
#include <vtkMapper.h>
#include <vtkPolyDataMapper.h>

// --- OSG Includes --- //
#include <osg/Vec3>
#include <osg/BlendFunc>
#include <osg/Depth>

// --- C/C++ Libraries --- //
#include <iostream>
#include <cstring>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
osg::Geode* ves::xplorer::scenegraph::vtkActorToStreamLine( vtkActor *actor, osg::Geode* geode, int verbose )
{
    //Make actor current
    actor->GetMapper()->Update();

    //This could possibly be any type of DataSet, vtkActorToOSG assumes polyData
    if( strcmp( actor->GetMapper()->GetInput()->GetClassName(), "vtkPolyData" ) )
    {
        std::cerr << "ERROR! Actor must use a vtkPolyDataMapper." << std::endl;
        std::cerr << "If you are using a vtkDataSetMapper, use vtkGeometryFilter instead." << std::endl;
        return NULL;
    }

    //If geode doesn't exist, then create a new one
    if( !geode )
    {
        geode = new osg::Geode();
        //std::cout << " creating a new geode in vtkactortoosg" << std::endl;
    }

    //Get poly data
    vtkPolyData* polyData = dynamic_cast< vtkPolyData* >( actor->GetMapper()->GetInput() );

    if( verbose )
    {
        std::cout << polyData->GetNumberOfPolys() << std::endl;
        std::cout << polyData->GetPolys()->GetNumberOfCells() << std::endl;
        std::cout << polyData->GetNumberOfPolys() << std::endl;
        std::cout << polyData->GetNumberOfCells() << std::endl;
        std::cout << polyData->GetNumberOfPoints() << std::endl;
    }

    //Get primitive arrays
    osg::ref_ptr< osg::Geometry > lines;

    //Create new Geometry for the Geode
    lines = ves::xplorer::scenegraph::ProcessPrimitive( actor, polyData->GetLines(), osg::PrimitiveSet::LINE_STRIP, verbose );

    //Remove old gsets and delete them
    while( geode->getNumDrawables() )
    {
        geode->removeDrawable( static_cast< unsigned int >( 0 ) );
    }

    if( lines.valid() )
    {
        geode->addDrawable( lines.get() );
    }

    //geode->setCullingActive( false );

    return geode;
}
////////////////////////////////////////////////////////////////////////////////
osg::Geometry* ves::xplorer::scenegraph::ProcessPrimitive( vtkActor *actor, vtkCellArray *primArray, int primType, int verbose )
{
    if( verbose )
    {
        int numPts = primArray->GetNumberOfCells();
        std::cerr << " " << numPts << " prim type " << primType;
        std::cerr << "..." << std::endl;
        std::cerr.flush();
    }

    //Get polyData from vtkActor
    vtkPolyData *polyData = ( vtkPolyData* )actor->GetMapper()->GetInput();

    int numPrimitives = primArray->GetNumberOfCells();
    if( numPrimitives == 0 )
    {
        if( verbose )
        {
            std::cout << " no cells" << std::endl;
        }

        return NULL;
    }

    osg::Geometry* geometry = new osg::Geometry();
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array();
    osg::ref_ptr< osg::Vec3Array > normals = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > texcoord0 = new osg::Vec3Array();

    double opacity = actor->GetProperty()->GetOpacity();
    vtkUnsignedCharArray *colorArray = actor->GetMapper()->MapScalars( opacity );

    //Copy data from vtk prim array to osg Geometry
    int prim = 0, totpts = 0;
    vtkIdType npts, *pts;

    //Go through cells (primitives)
    for( primArray->InitTraversal(); primArray->GetNextCell( npts, pts ); ++prim )
    {
        bool isFirstPoint = true;
        int count = 0;
        double leftOver = 0;
        double delta = 0.1;

        for( int i = 0; i < npts - 1; ++i )
        {
            double aVertex[ 3 ], bVertex[ 3 ];
            polyData->GetPoint( pts[ i ], aVertex );
            polyData->GetPoint( pts[ i + 1 ], bVertex );

            osg::Vec3d pointA( aVertex[ 0 ], aVertex[ 1 ], aVertex[ 2 ] );
            osg::Vec3d pointB( bVertex[ 0 ], bVertex[ 1 ], bVertex[ 2 ] );

            osg::Vec3d BminusA = pointB - pointA;

            if( isFirstPoint )
            {
                //Calculate the translated position
                for( int k = 0; k < 3; ++k )
                {
                    texcoord0->push_back( pointA );
                }

                //A unit equilateral triangle at the translated position
                vertices->push_back( osg::Vec3d( 0.0,  2.0, 0 ) + pointA );
                vertices->push_back( osg::Vec3d( -1.73205, -1.0, 0 ) + pointA );
                vertices->push_back( osg::Vec3d( 1.73205, -1.0, 0 ) + pointA );

                unsigned char *aColor = colorArray->GetPointer( 4 * pts[ i ] );
                colors->push_back( osg::Vec4d( aColor[ 0 ] / 255.0f,
                                               aColor[ 1 ] / 255.0f,
                                               aColor[ 2 ] / 255.0f,
                                               aColor[ 3 ] / 255.0f ) );

                count += 3;
                isFirstPoint = false;
            }

            double lineSegmentLength = BminusA.length();
            double totalDistance = lineSegmentLength + leftOver;

            if( totalDistance >= delta )
            {
                int numSteps = static_cast< int >( totalDistance / delta );
                double ds = delta / lineSegmentLength;
                double firstPos = ds * ( ( delta - leftOver ) / delta );

                leftOver = totalDistance - ( numSteps * delta );

                double j = firstPos;
                while( j <= 1 )
                {
                    //Calculate the translated position
                    osg::Vec3d temp = pointA + ( BminusA * j );
                    for( int k = 0; k < 3; ++k )
                    {
                        texcoord0->push_back( temp );
                    }

                    //A unit equilateral triangle at the translated position
                    vertices->push_back( osg::Vec3d( 0.0,  2.0, j ) + temp );
                    vertices->push_back( osg::Vec3d( -1.73205, -1.0, j ) + temp );
                    vertices->push_back( osg::Vec3d( 1.73205, -1.0, j ) + temp );

                    unsigned char *aColor = colorArray->GetPointer( 4 * pts[ i ] );
                    colors->push_back( osg::Vec4d( aColor[ 0 ] / 255.0f,
                                                   aColor[ 1 ] / 255.0f,
                                                   aColor[ 2 ] / 255.0f,
                                                   aColor[ 3 ] / 255.0f ) );

                    count += 3;
                    j += ds;
                }
            }
            else
            {
                leftOver = totalDistance;
            }
        }

        geometry->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::TRIANGLES, totpts, count ) );
        totpts += count;
    }

    geometry->setVertexArray( vertices.get() );

    geometry->setColorArray( colors.get() );
    geometry->setColorBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    normals->push_back( osg::Vec3d( 0.0f, 0.0f, 1.0f ) );
    geometry->setNormalArray( normals.get() );
    geometry->setNormalBinding( osg::Geometry::BIND_OVERALL );

    geometry->setTexCoordArray( 0, texcoord0.get() );

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    stateset->setRenderBinDetails( 10, "DepthSortedBin" );
    stateset->setMode( GL_BLEND, osg::StateAttribute::ON );

    osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
    bf->setFunction( GL_ONE, GL_ONE_MINUS_SRC_ALPHA );
    //bf->setFunction( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    stateset->setAttribute( bf.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Depth > depth = new osg::Depth();
    depth->setWriteMask( false );
    stateset->setAttribute( depth.get(), osg::StateAttribute::ON );

    stateset->setMode( GL_CULL_FACE, osg::StateAttribute::OFF );
    stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

    stateset->setAttribute( GetShader(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > parSize = new osg::Uniform( "particleSize", static_cast< float >( 0.4 ) );
    stateset->addUniform( parSize.get() );

    osg::ref_ptr< osg::Uniform > parExp = new osg::Uniform( "particleExp", static_cast< float >( 0.04 ) );
    stateset->addUniform( parExp.get() );

    geometry->setStateSet( stateset.get() );

    return geometry;
}
////////////////////////////////////////////////////////////////////////////////
osg::Program* ves::xplorer::scenegraph::GetShader()
{
    char vertexPass[] =
        "uniform float particleSpeed; \n"
        "uniform float particleLength; \n"
        "uniform float particleSize; \n"

        "void main() \n"
        "{ \n"
            //Subtract the translated position from the triangle vertex
            "vec2 temp = gl_Vertex.xy - gl_MultiTexCoord0.xy; \n"

            //Billboard the triangles using the original position
            "vec3 pos = particleSize * \n"
                "( temp.x * vec3( gl_ModelViewMatrix[ 0 ][ 0 ], \n"
                                 "gl_ModelViewMatrix[ 1 ][ 0 ], \n"
                                 "gl_ModelViewMatrix[ 2 ][ 0 ] ) +  \n"
                  "temp.y * vec3( gl_ModelViewMatrix[ 0 ][ 1 ],  \n"
                                 "gl_ModelViewMatrix[ 1 ][ 1 ],  \n"
                                 "gl_ModelViewMatrix[ 2 ][ 1 ] ) ); \n"

            //Move the triangle vertex back to its translated position
            "pos += gl_MultiTexCoord0.xyz; \n"

            //Set the variables
            "gl_Position = gl_ModelViewProjectionMatrix * vec4( pos, 1.0 ); \n"
            "gl_TexCoord[ 0 ].xy = temp; \n"
            "gl_FrontColor = gl_Color; \n"
        "} \n";

    char fragmentPass[] =
        "uniform float particleExp; \n"

        "void main() \n"
        "{ \n"
            "vec4 totalColor = ( 1.0 - pow( dot( gl_TexCoord[ 0 ].xy, gl_TexCoord[ 0 ].xy ), particleExp ) ) * gl_Color; \n"

            "gl_FragColor = totalColor; \n"
        "} \n";

    osg::Program* program = new osg::Program();
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader( osg::Shader::VERTEX, vertexPass );
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader( osg::Shader::FRAGMENT, fragmentPass );

    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );

    return program;
}
////////////////////////////////////////////////////////////////////////////////
