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
//=========================================================================
//#
//#  vtkActorToPF.cxx                                  #####
//#                                                   ##   ##
//#  Authors: Paul Rajlich                            ##
//#  Email: prajlich@ncsa.uiuc.edu                    ##   ##
//#  Date created:                                     #####
//#
//#  Description: function that translates any vtkActor to a pfGeode!
//#    Note, this is a complete rewrite of the original vtkActorToPF()
//#    function written by Paul Rajlich, Rob Stein, and Randy Heiland.
//#
//=========================================================================
//# Copyright (C) 1998-2000 Board of Trustees of the University of Illinois
//#
//# This software, both binary and source, is copyrighted by The
//# Board of Trustees of the University of Illinois.  Ownership
//# remains with the Univerity. For more information, contact:
//#
//#     Visualization and Virtual Environments
//#     National Center for Supercomputing Applications
//#     University of Illinois
//#     405 North Mathews Ave.
//#     Urbana, IL 61801
//#     baker@ncsa.uiuc.edu
//=========================================================================
#ifdef _PERFORMER
#include <vtkActorToPF.h>

#include <vtkDataSet.h>
#include <vtkPolyData.h>
#include <vtkPointData.h>
#include <vtkCellData.h>
#include <vtkImageData.h>
#include <vtkProperty.h>
#include <vtkTexture.h>

#include <Performer/pr/pfTexture.h>
#include <iostream>
#include <cstring>

using namespace std;
using namespace ves::xplorer::scenegraph;

pfGeode* ves::xplorer::scenegraph::vtkActorToPF( vtkActor *actor, pfGeode *geode, int verbose )
{
    // performance instrumentation
    float beforeTime = 0.0f;
    if( verbose )
    {
        cerr.precision( 5 );
        cerr << "-- vtkActorToPF --" << endl;
        cerr << "updating vtkActor...";
        cerr.flush();
        beforeTime = pfGetTime();
    }

    // make actor current
    actor->GetMapper()->Update();

    if( verbose )
    {
        float elapsedTime = pfGetTime() - beforeTime;
        cerr << "done (" << elapsedTime << "s)" << endl;
        cerr << "translating...";
        beforeTime = pfGetTime();
    }

    // if geode doesn't exist, then create a new one
    if( geode == NULL )
        geode = new pfGeode;

    // create new gsets for this geode
    pfGeoSet *gsets[4];
    vtkActorToGeoSets( actor, gsets, verbose );

    // remove old gsets and delete them
    int numGSets = geode->getNumGSets();
    int i;
    for( i = 0; i < numGSets; i++ )
    {
        pfGeoSet *g = geode->getGSet( 0 );  // children shift after a removal
        geode->removeGSet( g );
        pfDelete( g );
    }

    // put in new gsets
    for( i = 0; i < 4; i++ )
        if( gsets[i] != NULL )
            geode->addGSet( gsets[i] );

    // performance instrumentation
    if( verbose )
    {
        float elapsedTime = pfGetTime() - beforeTime;
        cerr << "done (" << elapsedTime << "s)" << endl;
    }

    return geode;
}

void ves::xplorer::scenegraph::vtkActorToGeoSets( vtkActor *actor, pfGeoSet *gsets[],
                                                  int verbose )
{
    // this could possibly be any type of DataSet, vtkActorToPF assumes polyData
    if( strcmp( actor->GetMapper()->GetInput()->GetClassName(), "vtkPolyData" ) )
    {
        cerr << "ERROR! Actor must use a vtkPolyDataMapper." << endl;
        cerr << "If you are using a vtkDataSetMapper, use vtkGeometryFilter instead." << endl;
        gsets[0] = NULL;
        gsets[1] = NULL;
        gsets[2] = NULL;
        gsets[3] = NULL;
        return;
    }

    // get poly data
    vtkPolyData *polyData = ( vtkPolyData * ) actor->GetMapper()->GetInput();

    // If verbose, check for normals
    // Note: vtk doesn't neccesarily give you normals - USE vtkPolyDataNormals
    if( verbose )
    {
        vtkDataArray *normals = polyData->GetPointData()->GetNormals();
        if( normals == NULL )
            normals = polyData->GetCellData()->GetNormals();
        if( normals == NULL )
            cerr << "no normals...";
    }

    // get primitive arrays
    vtkCellArray *points, *lines, *polys, *strips;
    points = polyData->GetVerts();
    lines = polyData->GetLines();
    polys = polyData->GetPolys();
    strips = polyData->GetStrips();

    if( verbose )
    {
        int numPts = points->GetNumberOfCells();
        int numLines = lines->GetNumberOfCells();
        int numPolys = polys->GetNumberOfCells();
        int numStrips = strips->GetNumberOfCells();
        //cerr << "pts = "     << numPts   << " lines = "  << numLines
        //     << " polys = "  << numPolys << " strips = " << numStrips << "...";
        if( numPts > 0 )
            cerr << " " << numPts << " points";
        if( numLines > 0 )
            cerr << " " << numLines << " lines";
        if( numPolys > 0 )
            cerr << " " << numPolys << " polys";
        if( numStrips > 0 )
            cerr << " " << numStrips << " strips";
        cerr << "...";
        cerr.flush();
    }

    // create new GeoSets for the geode
    gsets[0] = processPrimitive( actor, points, PFGS_POINTS, verbose );
    gsets[1] = processPrimitive( actor, lines, PFGS_LINESTRIPS, verbose );
    gsets[2] = processPrimitive( actor, polys, PFGS_POLYS, verbose );
    gsets[3] = processPrimitive( actor, strips, PFGS_TRISTRIPS, verbose );
}

pfGeoSet* ves::xplorer::scenegraph::processPrimitive( vtkActor *actor, vtkCellArray *primArray,
                                                      int primType, int verbose )
{
    // get polyData from vtkActor
    vtkPolyData *polyData = ( vtkPolyData * ) actor->GetMapper()->GetInput();

    int numPrimitives = primArray->GetNumberOfCells();
    if( numPrimitives == 0 )
        return NULL;

    //Initialize the gset
    pfGeoSet *gset = new pfGeoSet;
    gset->setPrimType( primType );
    // get number of indices in the vtk prim array. Each vtkCell has the length
    // (not counted), followed by the indices.
    int primArraySize = primArray->GetNumberOfConnectivityEntries();
    int numIndices = primArraySize - numPrimitives;

    // get shared memory arena
    void *pfArena = pfGetSharedArena();

    // set number of primitives and allocate lengths array
    gset->setNumPrims( numPrimitives );
    int *lengths = ( int * )pfMalloc( numPrimitives * sizeof( int ), pfArena );
    if( !lengths )
        return 0;
    gset->setPrimLengths( lengths );

    // allocate as many verts as there are indices in vtk prim array
    pfVec3 *verts;
    verts = ( pfVec3 * ) pfMalloc( numIndices * sizeof( pfVec3 ), pfArena );
    if( !verts )
    {
        pfDelete( lengths );
        return 0;
    }
    // check to see if there are normals
    int normalPerVertex = 0;
    int normalPerCell = 0;
    vtkDataArray *normals = polyData->GetPointData()->GetNormals();
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

    pfVec3 *norms = NULL;
    if( normalPerVertex )
    {
        norms = ( pfVec3 * ) pfMalloc( numIndices * sizeof( pfVec3 ), pfArena );
        if( !norms )
        {
            pfDelete( lengths );
            pfDelete( verts );
            return 0;
        }
    }

    if( normalPerCell )
    {
        norms = ( pfVec3 * ) pfMalloc( numPrimitives * sizeof( pfVec3 ), pfArena );
        if( !norms )
        {
            pfDelete( lengths );
            pfDelete( verts );
            return 0;
        }
    }

    // check to see if there is color information
    int colorPerVertex = 0;
    int colorPerCell = 0;
    double opacity = actor->GetProperty()->GetOpacity();
    vtkUnsignedCharArray *colorArray = actor->GetMapper()->MapScalars( opacity );
    if( actor->GetMapper()->GetScalarVisibility() && colorArray != NULL )
    {
        int scalarMode = actor->GetMapper()->GetScalarMode();
        if( scalarMode == VTK_SCALAR_MODE_USE_CELL_DATA || !polyData->GetPointData()->GetScalars() )    // there is no point data
            colorPerCell = 1;
        else
            colorPerVertex = 1;
    }

    pfVec4 *colors = NULL;
    if( colorPerVertex )
    {
        colors = ( pfVec4 * ) pfMalloc( numIndices * sizeof( pfVec4 ), pfArena );
        if( !colors )
        {
            pfDelete( lengths );
            pfDelete( verts );
            pfDelete( norms );
            return 0;
        }
    }

    if( colorPerCell )
    {
        colors = ( pfVec4 * ) pfMalloc( numPrimitives * sizeof( pfVec4 ), pfArena );
        if( !colors )
        {
            pfDelete( lengths );
            pfDelete( verts );
            pfDelete( norms );
            return 0;
        }
    }

    // check to see if there are texture coordinates
    vtkDataArray *texCoords = polyData->GetPointData()->GetTCoords();
    pfVec2 *tcoords = NULL;
    if( texCoords != NULL )
    {
        tcoords = ( pfVec2 * ) pfMalloc( numIndices * sizeof( pfVec2 ), pfArena );
        if( !tcoords )
        {
            pfDelete( lengths );
            pfDelete( verts );
            pfDelete( norms );
            pfDelete( colors );
            return 0;
        }
    }

    // copy data from vtk prim array to performer geoset
    int prim = 0, vert = 0;
    int i, npts, *pts, transparentFlag = 0;
    // go through cells (primitives)
    for( primArray->InitTraversal(); primArray->GetNextCell( npts, pts ); prim++ )
    {
        lengths[prim] = npts;
        if( colorPerCell )
        {
            unsigned char *aColor = colorArray->GetPointer( 4 * prim );
            colors[prim].set( aColor[0] / 255.0f, aColor[1] / 255.0f,
                              aColor[2] / 255.0f, aColor[3] / 255.0f );
            if( aColor[3] / 255.0f < 1 )
                transparentFlag = 1;
        }

        if( normalPerCell )
        {
            double *aNormal = normals->GetTuple( prim );
            norms[prim].set( ( float )aNormal[0], ( float )aNormal[1], ( float )aNormal[2] );
        }
        // go through points in cell (verts)
        for( i = 0; i < npts; i++ )
        {
            double *aVertex = polyData->GetPoint( pts[i] );
            verts[vert].set( ( float )aVertex[0], ( float )aVertex[1], ( float )aVertex[2] );
            if( normalPerVertex )
            {
                double *aNormal = normals->GetTuple( pts[i] );
                norms[vert].set( ( float )aNormal[0], ( float )aNormal[1], ( float )aNormal[2] );
            }

            if( colorPerVertex )
            {
                unsigned char *aColor = colorArray->GetPointer( 4 * pts[i] );
                colors[vert].set( aColor[0] / 255.0f, aColor[1] / 255.0f,
                                  aColor[2] / 255.0f, aColor[3] / 255.0f );
                if( aColor[3] / 255.0f < 1 )
                    transparentFlag = 1;
            }

            if( texCoords != NULL )
            {
                double *aTCoord = texCoords->GetTuple( pts[i] );
                tcoords[vert].set( ( float )aTCoord[0], ( float )aTCoord[1] );
            }
            vert++;
        }
    }

    // add attribute arrays to gset
    gset->setAttr( PFGS_COORD3, PFGS_PER_VERTEX, verts, NULL );
    if( normalPerVertex )
        gset->setAttr( PFGS_NORMAL3, PFGS_PER_VERTEX, norms, NULL );

    if( normalPerCell )
        gset->setAttr( PFGS_NORMAL3, PFGS_PER_PRIM, norms, NULL );

    if( colorPerVertex )
        gset->setAttr( PFGS_COLOR4, PFGS_PER_VERTEX, colors, NULL );
    else if( colorPerCell )
        gset->setAttr( PFGS_COLOR4, PFGS_PER_PRIM, colors, NULL );
    else
    {
        // use overall color (get from Actor)
        double *actorColor = actor->GetProperty()->GetColor();
        double opacity = actor->GetProperty()->GetOpacity();

        pfVec4 *color = ( pfVec4 * ) pfMalloc( sizeof( pfVec4 ), pfArena );
        color->set( ( float )actorColor[0], ( float )actorColor[1], ( float )actorColor[2],
                    ( float )opacity );
        gset->setAttr( PFGS_COLOR4, PFGS_OVERALL, color, NULL );
    }

    if( texCoords != NULL )
        gset->setAttr( PFGS_TEXCOORD2, PFGS_PER_VERTEX, tcoords, NULL );

    // create a geostate for this geoset
    pfGeoState *gstate = new pfGeoState;

    // check if a texture needs to be translated
    if( texCoords != NULL )
        updateTexture( actor, gset, gstate, verbose );

    // if not opaque
    if( actor->GetProperty()->GetOpacity() < 1.0 || transparentFlag )
    {
        gset->setDrawBin( PFSORT_TRANSP_BIN );  // draw last
        gstate->setMode( PFSTATE_CULLFACE, PFCF_OFF );  // want to see backside thru
        gstate->setMode( PFSTATE_TRANSPARENCY, PFTR_BLEND_ALPHA );  //| PFTR_NO_OCCLUDE
    }

    // wireframe
    if( actor->GetProperty()->GetRepresentation() == VTK_WIREFRAME )
    {
        gset->setLineWidth( actor->GetProperty()->GetLineWidth() );
        gstate->setMode( PFSTATE_ENWIREFRAME, PF_ON );
    }

    // points - NOTE: you can modify properties of points through lpState
    if( actor->GetProperty()->GetRepresentation() == VTK_POINTS )
    {
        gset->setPrimType( PFGS_POINTS );
        gset->setPntSize( actor->GetProperty()->GetPointSize() );
        pfLPointState *lpState = new pfLPointState;
        gstate->setMode( PFSTATE_ENLPOINTSTATE, PF_ON );
        gstate->setAttr( PFSTATE_LPOINTSTATE, lpState );
    }

    // backface culling
    if( !actor->GetProperty()->GetBackfaceCulling() )
        gstate->setMode( PFSTATE_CULLFACE, PFCF_OFF );

    // lighting
    if( normals != NULL )
        gstate->setMode( PFSTATE_ENLIGHTING, PF_ON );
    else
        gstate->setMode( PFSTATE_ENLIGHTING, PF_OFF );

    // if it is lines, turn off lighting.
    if( primType == PFGS_LINESTRIPS )
        gstate->setMode( PFSTATE_ENLIGHTING, PF_OFF );

    gset->setGState( gstate );
    return gset;
}

// texturing - note: since we always create new geosets, we need to
//   retranslate texture as well. Maybe there is a better way (have
//   separate function from vtkActorToPF for texture.
void ves::xplorer::scenegraph::updateTexture( vtkActor *actor, pfGeoSet *gset,
                                              pfGeoState *gstate, int verbose )
{
    // no texture!
    if( !actor->GetTexture() )
        return;

    // update texture
    actor->GetTexture()->GetInput()->Update();

    pfTexture *texture = new pfTexture;
    pfTexEnv *texEnv = new pfTexEnv;

    // get image data
    vtkImageData *img = actor->GetTexture()->GetInput();
    int *dims = img->GetDimensions();
    vtkDataArray *scalars = img->GetPointData()->GetScalars();
    int bytesPerPixel = scalars->GetNumberOfComponents();

    // determine tex dimensions
    int xsize, ysize;
    if( dims[0] == 1 )
    {
        xsize = dims[1];
        ysize = dims[2];
    }
    else
    {
        xsize = dims[0];
        if( dims[1] == 1 )
            ysize = dims[2];
        else
        {
            ysize = dims[1];
            if( dims[2] != 1 )
                cerr << "3D texture maps currently are not supported!" << endl;
        }
    }

    // if verbose, print tex information
    if( verbose )
    {
        cerr << "tex = " << xsize << "x" << ysize << "_" << bytesPerPixel << "...";
        cerr.flush();
    }

    // copy data to performer texture
    uint *texels = ( uint * ) pfMalloc( xsize * ysize * bytesPerPixel, pfGetSharedArena() );
    unsigned char *texelData = ( unsigned char * )texels;
    unsigned char *dataPtr = NULL;
    dataPtr = ( unsigned char * )scalars->GetVoidPointer( 0 );

    memcpy( texelData, dataPtr, xsize * ysize * bytesPerPixel );
    texture->setImage( texels, bytesPerPixel, xsize, ysize, 0 );

    // set image format for texture (lum, lum-alpha, rgb, rgba)
    if( bytesPerPixel == 1 )
        texture->setFormat( PFTEX_IMAGE_FORMAT, PFTEX_LUMINANCE );
    if( bytesPerPixel == 2 )
        texture->setFormat( PFTEX_IMAGE_FORMAT, PFTEX_LUMINANCE_ALPHA );
    if( bytesPerPixel == 3 )
        texture->setFormat( PFTEX_IMAGE_FORMAT, PFTEX_RGB );
    if( bytesPerPixel == 4 )
        texture->setFormat( PFTEX_IMAGE_FORMAT, PFTEX_RGBA );

    // for textures with alpha
    if( bytesPerPixel == 2 || bytesPerPixel == 4 )
    {
        gset->setDrawBin( PFSORT_TRANSP_BIN );  // draw last
        gstate->setMode( PFSTATE_CULLFACE, PFCF_OFF );  // want to see backside thru
        gstate->setMode( PFSTATE_TRANSPARENCY, PFTR_BLEND_ALPHA );  //| PFTR_NO_OCCLUDE);
    }

    // set geostate attributes
    gstate->setMode( PFSTATE_ENTEXTURE, PF_ON );
    gstate->setAttr( PFSTATE_TEXTURE, texture );
    gstate->setAttr( PFSTATE_TEXENV, texEnv );
}

#endif
