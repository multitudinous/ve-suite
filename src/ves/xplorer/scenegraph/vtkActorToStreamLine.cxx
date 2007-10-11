/*************** <auto-copyright.pl BEGIN do not edit this line> *************
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
 * Date modified: $Date: 2007-06-15 11:06:13 -0500 (Fri, 15 Jun 2007) $
 * Version:       $Rev: 8206 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> **************/

#include <VE_Xplorer/SceneGraph/vtkActorToStreamLine.h>
#include <VE_Xplorer/Utilities/readWriteVtkThings.h>

#include <vtkDataSet.h>
#include <vtkPolyData.h>
#include <vtkProperty.h>
#include <vtkPointData.h>
#include <vtkCellData.h>
#include <vtkMapper.h>
#include <vtkPolyDataMapper.h>

#include <osg/Vec3>
#include <osg/BlendFunc>
#include <osg/Depth>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace VE_SceneGraph;

typedef double vtkReal;

////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr< osg::Geode > VE_SceneGraph::vtkActorToStreamLine( vtkActor *actor, osg::ref_ptr< osg::Geode > geode, int verbose )
{
    // make actor current
    actor->GetMapper()->Update();

    // this could possibly be any type of DataSet, vtkActorToOSG assumes polyData
    if( strcmp( actor->GetMapper()->GetInput()->GetClassName(), "vtkPolyData" ) )
    {
        std::cerr << "ERROR! Actor must use a vtkPolyDataMapper." << std::endl;
        std::cerr << "If you are using a vtkDataSetMapper, use vtkGeometryFilter instead." << std::endl;
        return NULL;
    }

    //If geode doesn't exist, then create a new one
    if( !geode.valid() )
    {
        geode = new osg::Geode();
        //std::cout << " creating a new geode in vtkactortoosg" << std::endl;
    }

    // get poly data
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
    lines = VE_SceneGraph::ProcessPrimitive( actor, polyData->GetLines(), osg::PrimitiveSet::LINE_STRIP, verbose );

    //Remove old gsets and delete them
    while( geode->getNumDrawables() )
    {
        geode->removeDrawable( static_cast< unsigned int >( 0 ) );
    }

    if( lines.valid() )
    {
        geode->addDrawable( lines.get() );
    }

    return geode;
}
////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr< osg::Geometry > VE_SceneGraph::ProcessPrimitive( vtkActor *actor, vtkCellArray *primArray, int primType, int verbose )
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

    osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array();
    osg::ref_ptr< osg::Vec3Array > normals = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > texcoord0 = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > texcoord1 = new osg::Vec3Array();

    //Get number of indices in the vtk prim array.
    //Each vtkCell has the length (not counted), followed by the indices.
    int primArraySize = primArray->GetNumberOfConnectivityEntries();
    int numIndices = primArraySize - numPrimitives;

    vtkReal opacity = actor->GetProperty()->GetOpacity();
    vtkUnsignedCharArray *colorArray = actor->GetMapper()->MapScalars( opacity );

    //Copy data from vtk prim array to osg Geometry
    int prim = 0, totpts = 0, transparentFlag = 0;
    int i, npts, *pts;

    //Go through cells (primitives)
    for( primArray->InitTraversal(); primArray->GetNextCell( npts, pts ); ++prim )
    {
        //Go through points in cell (verts)
        for( i = 0; i < npts; ++i )
        {
            vtkReal* aVertex = polyData->GetPoint( pts[i] );
            vtkReal* bVertex = polyData->GetPoint( pts[i] );

            osg::Vec3 pointA( aVertex[0], aVertex[1], aVertex[2] );
            osg::Vec3 pointB( bVertex[0], bVertex[1], bVertex[2] );

            osg::Vec3 BminusA( pointB.x() - pointA.x(), 
                               pointB.y() - pointA.y(),
                               pointB.z() - pointA.z() );

            //for( float t = 0; t < 1.0; )
            //{                           
                vertices->push_back( osg::Vec3( -1.0,  1.0, 0 ) );
                vertices->push_back( osg::Vec3( -1.0, -1.0, 0 ) );
                vertices->push_back( osg::Vec3(  1.0, -1.0, 0 ) );
                vertices->push_back( osg::Vec3(  1.0,  1.0, 0 ) );

                for( int b = 0; b < 4; ++b )
                {
                    texcoord0->push_back( pointA );
                    texcoord1->push_back( BminusA );
                }

                unsigned char *aColor = colorArray->GetPointer( 4 * pts[i] );

                colors->push_back( osg::Vec4( aColor[0] / 255.0f,
                                              aColor[1] / 255.0f,
	                                          aColor[2] / 255.0f,
                                              aColor[3] / 255.0f ) );
                if( aColor[3] / 255.0f < 1 )
                {
                    transparentFlag = 1;
                }

                //t += 0.5;
        }

        geometry->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, totpts, npts * 4 ) );
        totpts += npts * 4;
    }

    geometry->setVertexArray( vertices.get() );

    geometry->setColorArray( colors.get() );
    geometry->setColorBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    normals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );
    geometry->setNormalArray( normals.get() );
    geometry->setNormalBinding( osg::Geometry::BIND_OVERALL );

    geometry->setTexCoordArray( 0, texcoord0.get() );
    geometry->setTexCoordArray( 1, texcoord1.get() );

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    stateset->setRenderingHint( osg::StateSet::TRANSPARENT_BIN );
    stateset->setMode( GL_BLEND, osg::StateAttribute::ON );

    osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
    bf->setSourceRGB( GL_ONE );
    bf->setSourceAlpha( GL_ONE );
    bf->setDestinationRGB( GL_ONE );
    bf->setDestinationAlpha( GL_ONE );
    stateset->setAttribute( bf.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Depth > depth = new osg::Depth();
    depth->setWriteMask( false );
    stateset->setAttribute( depth.get(), osg::StateAttribute::ON );

    stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

    stateset->setAttribute( GetShader().get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > parSize = new osg::Uniform( "particleSize", static_cast< float >( 0.2 ) );
    stateset->addUniform( parSize.get() );

    osg::ref_ptr< osg::Uniform > parExp = new osg::Uniform( "particleExp", static_cast< float >( 0.1 ) );
    stateset->addUniform( parExp.get() );

    geometry->setStateSet( stateset.get() );

    return geometry;
}
////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr< osg::Program > VE_SceneGraph::GetShader()
{
    char vertexPass[] =
        "uniform float particleSpeed; \n"
        "uniform float particleLength; \n"
        "uniform float particleSize; \n"

        "varying vec2 texCoord; \n"
        "varying vec4 color; \n"

        "void main() \n"
        "{ \n"
            //Billboard the quads
            "vec3 pos = particleSize * \n"
                      "( gl_Vertex.x * vec3( gl_ModelViewMatrix[0][0], \n"
                                            "gl_ModelViewMatrix[1][0], \n"
                                            "gl_ModelViewMatrix[2][0] ) +  \n"
                        "gl_Vertex.y * vec3( gl_ModelViewMatrix[0][1],  \n"
                                            "gl_ModelViewMatrix[1][1],  \n"
                                            "gl_ModelViewMatrix[2][1] ) ); \n"

            //Move the quads along the line segment
            "float t = gl_Vertex.z; \n"
            "pos.x += gl_MultiTexCoord0.x + t * gl_MultiTexCoord1.x; \n"
            "pos.y += gl_MultiTexCoord0.y + t * gl_MultiTexCoord1.y; \n"
            "pos.z += gl_MultiTexCoord0.z + t * gl_MultiTexCoord1.z; \n"

            //Set the variables
            "gl_Position = gl_ModelViewProjectionMatrix * vec4( pos, 1.0 ); \n"
            "texCoord = gl_Vertex.xy; \n"
            "color = gl_Color; \n"
        "} \n";

    char fragmentPass[] =
        "uniform float particleExp; \n"

        "varying vec2 texCoord; \n"
        "varying vec4 color; \n"

        "void main() \n"
        "{ \n"
            "vec4 totalColor = ( 1.0 - pow( dot( texCoord, texCoord ), particleExp ) ) * color; \n"

            "gl_FragColor = totalColor; \n"
        "} \n";

    osg::ref_ptr< osg::Program > program = new osg::Program();
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader( osg::Shader::VERTEX, vertexPass );
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader( osg::Shader::FRAGMENT, fragmentPass );

    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );

    return program;
}
////////////////////////////////////////////////////////////////////////////////
