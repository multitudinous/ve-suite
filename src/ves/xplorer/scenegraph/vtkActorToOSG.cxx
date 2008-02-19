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
//C++ source - fIVE|Analyse - Copyright (C) 2002-2003 Michael Gronager, UNI-C
//Distributed under the terms of the GNU Library General Public License (LGPL)
//as published by the Free Software Foundation.

// this is a workaround for compiling VTK with stlport
// stlport is required for compiling OSG on MSVS60 - (try .NET?)
#ifdef _OSG
//#define _INC_STRSTREAM
//#include <strstream.h>
// workaround end

#include <ves/xplorer/scenegraph/vtkActorToOSG.h>
#include <ves/xplorer/util/readWriteVtkThings.h>
#ifdef VTK44
#define VTK4
typedef double vtkReal;
#else
typedef float vtkReal;
#endif

#ifdef VTK4
#include <vtkDataSet.h>
#include <vtkPolyData.h>
#include <vtkProperty.h>
#include <vtkPointData.h>
#include <vtkCellData.h>
#include <vtkMapper.h>
#include <vtkPolyDataMapper.h>
#endif

#include <osg/Vec3>
#include <osg/LineWidth>
#include <iostream>

using namespace ves::xplorer::scenegraph;

osg::ref_ptr< osg::Geode > ves::xplorer::scenegraph::vtkActorToOSG( vtkActor *actor, osg::ref_ptr< osg::Geode > geode, int verbose )
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

    // if geode doesn't exist, then create a new one
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
    // get primitive arrays
    osg::ref_ptr< osg::Geometry > points, lines, polys, strips;

    // create new Geometry for the Geode
    points = ves::xplorer::scenegraph::processPrimitive( actor, polyData->GetVerts(), osg::PrimitiveSet::POINTS, verbose );
    lines = ves::xplorer::scenegraph::processPrimitive( actor, polyData->GetLines(), osg::PrimitiveSet::LINE_STRIP, verbose );
    polys = ves::xplorer::scenegraph::processPrimitive( actor, polyData->GetPolys(), osg::PrimitiveSet::POLYGON, verbose );
    strips = ves::xplorer::scenegraph::processPrimitive( actor, polyData->GetStrips(), osg::PrimitiveSet::TRIANGLE_STRIP, verbose );

    // remove old gsets and delete them
    while( geode->getNumDrawables() ) geode->removeDrawable(( unsigned int )0 );//removeDrawable(0);

    if( points.valid() ) geode->addDrawable( points.get() );
    if( lines.valid() ) geode->addDrawable( lines.get() );
    if( polys.valid() ) geode->addDrawable( polys.get() );
    if( strips.valid() ) geode->addDrawable( strips.get() );

    return geode;
}

osg::ref_ptr< osg::Geometry > ves::xplorer::scenegraph::processPrimitive( vtkActor *actor, vtkCellArray *primArray, int primType, int verbose )
{

    if( verbose )
    {
        int numPts = primArray->GetNumberOfCells();
        std::cerr << " " << numPts << " prim type " << primType;
        std::cerr << "..." << std::endl;
        std::cerr.flush();
    }

    // get polyData from vtkActor
    vtkPolyData *polyData = ( vtkPolyData * ) actor->GetMapper()->GetInput();

    int numPrimitives = primArray->GetNumberOfCells();
    if( numPrimitives == 0 )
    {
        if( verbose )
        {
            std::cout << " no cells" << std::endl;
        }
        return NULL;
    }
    //Initialize the Geometry
    osg::ref_ptr< osg::Geometry > geom = new osg::Geometry;

    // get number of indices in the vtk prim array. Each vtkCell has the length
    // (not counted), followed by the indices.
    int primArraySize = primArray->GetNumberOfConnectivityEntries();
    int numIndices = primArraySize - numPrimitives;

    // allocate as many verts as there are indices in vtk prim array
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array;

    // check to see if there are normals
    int normalPerVertex = 0;
    int normalPerCell = 0;
#ifdef VTK4
    vtkDataArray* normals = polyData->GetPointData()->GetNormals();
#else
    vtkNormals* normals = polyData->GetPointData()->GetNormals();
#endif
    if( actor->GetProperty()->GetInterpolation() == VTK_FLAT )
        normals = NULL;
    if( normals != NULL )
        normalPerVertex = 1;
    else
    {
        normals = polyData->GetCellData()->GetNormals();
        if( normals != NULL )
            normalPerCell = 1;
    }

    osg::ref_ptr< osg::Vec3Array > norms = new osg::Vec3Array;

    // check to see if there is color information
    int colorPerVertex = 0;
    int colorPerCell = 0;
#ifdef VTK4
    vtkReal opacity = actor->GetProperty()->GetOpacity();
    vtkUnsignedCharArray *colorArray = actor->GetMapper()->MapScalars( opacity );
#else
    vtkScalars *colorArray = actor->GetMapper()->GetColors();
#endif
    if( actor->GetMapper()->GetScalarVisibility() && colorArray != NULL )
    {
        int scalarMode = actor->GetMapper()->GetScalarMode();
        if( scalarMode == VTK_SCALAR_MODE_USE_CELL_DATA ||
                !polyData->GetPointData()->GetScalars() ) // there is no point data
            colorPerCell = 1;
        else
            colorPerVertex = 1;
    }

    osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array;

    // check to see if there are texture coordinates
#ifdef VTK4
    vtkDataArray* texCoords = polyData->GetPointData()->GetTCoords();
#else
    vtkTCoords* texCoords = polyData->GetPointData()->GetTCoords();
#endif
    osg::ref_ptr< osg::Vec2Array > tcoords = new osg::Vec2Array;

    // copy data from vtk prim array to osg Geometry
    int prim = 0, vert = 0;
    int i, npts, totpts = 0, *pts, transparentFlag = 0;
    ;
    // go through cells (primitives)
    for( primArray->InitTraversal(); primArray->GetNextCell( npts, pts ); prim++ )
    {
        geom->addPrimitiveSet( new osg::DrawArrays( primType, totpts, npts ) );
        totpts += npts;
        if( colorPerCell )
        {
#ifdef VTK4
            unsigned char *aColor = colorArray->GetPointer( 4 * prim );
#else
            unsigned char *aColor = colorArray->GetColor( prim );
#endif
            colors->push_back( osg::Vec4( aColor[0] / 255.0f, aColor[1] / 255.0f,
                                          aColor[2] / 255.0f, aColor[3] / 255.0f ) );
            if( aColor[3] / 255.0f < 1 )
                transparentFlag = 1;
        }

        if( normalPerCell )
        {
            vtkReal* aNormal = normals->GetTuple( prim );
            norms->push_back( osg::Vec3( aNormal[0], aNormal[1], aNormal[2] ) );
        }

        // go through points in cell (verts)
        for( i = 0; i < npts; i++ )
        {
            vtkReal* aVertex = polyData->GetPoint( pts[i] );
            vertices->push_back( osg::Vec3( aVertex[0], aVertex[1], aVertex[2] ) );
            if( normalPerVertex )
            {
                vtkReal* aNormal = normals->GetTuple( pts[i] );
                norms->push_back( osg::Vec3( aNormal[0], aNormal[1], aNormal[2] ) );
            }
            if( colorPerVertex )
            {
#ifdef VTK4
                unsigned char *aColor = colorArray->GetPointer( 4 * pts[i] );
#else
                unsigned char *aColor = colorArray->GetColor( pts[i] );
#endif
                colors->push_back( osg::Vec4( aColor[0] / 255.0f, aColor[1] / 255.0f,
                                              aColor[2] / 255.0f, aColor[3] / 255.0f ) );
                if( aColor[3] / 255.0f < 1 )
                    transparentFlag = 1;
            }
            if( texCoords != NULL )
            {
                vtkReal* aTCoord = texCoords->GetTuple( pts[i] );
                tcoords->push_back( osg::Vec2( aTCoord[0], aTCoord[1] ) );
            }
            vert++;
        }
    }

    // add attribute arrays to gset
    geom->setVertexArray( vertices.get() );
    geom->setColorArray( colors.get() );
    if( normals ) geom->setNormalArray( norms.get() );

    if( normalPerVertex )
        geom->setNormalBinding( osg::Geometry::BIND_PER_VERTEX );
    if( normalPerCell )
        geom->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    if( colorPerVertex )
        geom->setColorBinding( osg::Geometry::BIND_PER_VERTEX );
    else if( colorPerCell )
        geom->setColorBinding( osg::Geometry::BIND_PER_PRIMITIVE );
    else
    {
        // use overall color (get from Actor)
        vtkReal* actorColor = actor->GetProperty()->GetColor();
        vtkReal opacity = actor->GetProperty()->GetOpacity();

        colors->push_back( osg::Vec4( actorColor[0], actorColor[1], actorColor[2], opacity ) );
        geom->setColorBinding( osg::Geometry::BIND_OVERALL );
    }

    if( texCoords != NULL )
        geom->setTexCoordArray( 0, tcoords.get() );


    // create a geostate for this geoset
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet;

    // if not opaque
    if( actor->GetProperty()->GetOpacity() < 1.0 || transparentFlag )
    {
        stateset->setRenderBinDetails( 10, "DepthSortedBin "); 
        stateset->setMode( GL_BLEND, osg::StateAttribute::ON );
        stateset->setMode( GL_CULL_FACE, osg::StateAttribute::OFF );
    }

    // wireframe and line strips
    if (( actor->GetProperty()->GetRepresentation() == VTK_WIREFRAME ) ||
            ( osg::PrimitiveSet::LINE_STRIP == primType ) )
    {
        osg::ref_ptr<osg::LineWidth> lineWidth = new osg::LineWidth;
        lineWidth->setWidth( actor->GetProperty()->GetLineWidth() );
        stateset->setAttributeAndModes( lineWidth.get(), osg::StateAttribute::ON );
    }

    // backface culling
    if( !actor->GetProperty()->GetBackfaceCulling() )
        stateset->setMode( GL_CULL_FACE, osg::StateAttribute::OFF );

    // lighting
    if( normals != NULL )
        stateset->setMode( GL_LIGHTING, osg::StateAttribute::ON );
    else
        stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

    // if it is lines, turn off lighting.
    if( primType == osg::PrimitiveSet::LINE_STRIP )
        stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

    geom->setStateSet( stateset.get() );
    return geom;
}

#endif
