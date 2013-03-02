/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: PluginBase.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VEIntStovemod.h"

#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/Geode.h>
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>

#include <latticefx/utils/vtk/readWriteVtkThings.h>

#include <ves/xplorer/event/viz/cfdCuttingPlane.h>

#include <ves/open/xml/model/Model.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <fstream>
#include <string>
#include <sstream>
#include <string>
#include <map>

#include <vtkDataSet.h>
#include <vtkXMLFileReadTester.h>
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkXMLStructuredGridReader.h>
#include <vtkXMLRectilinearGridReader.h>
#include <vtkPolyDataReader.h>
#include <vtkDataSetReader.h>
#include <vtkStructuredGrid.h>
#include <vtkRectilinearGrid.h>
#include <vtkUnstructuredGrid.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkPolyData.h>
#include <vtkXMLDataSetWriter.h>
#include <vtkUnstructuredGridWriter.h>
#include <vtkStructuredGridWriter.h>
#include <vtkRectilinearGridWriter.h>
#include <vtkPolyDataWriter.h>
#include <vtkUnstructuredGrid.h>
#include <vtkCutter.h>
#include <vtkStripper.h>
#include <vtkPolyDataNormals.h>
#include <vtkTriangleFilter.h>
#include <vtkPolyDataMapper.h>
#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkProperty.h>
#include <vtkPointData.h>
#include <vtkGlyph3D.h>
#include <vtkMaskPoints.h>

using namespace std;
using namespace ves::xplorer::scenegraph;
using namespace ves::xplorer;
///////////////////////////////////////////////////////////////////////////////
VEIntStovemod::VEIntStovemod( void ) 
    : 
    PluginBase(),
    m_stoveData( 0 ),
    m_contourPlane( 0 ),
    m_vectorPlane( 0 ),
    
    setcount( 0 ), 
    _outFileName( 0 ),
    _dataSet( 0 ),
    color( 1 ),
    transFlag( 1 ),
    
    baffleOne( 0 ),
    baffleTwo( 0 ),
    baffleThree( 0 ),
    baffleFour( 0 ),
    baffleFive( 0 ),
    baffleSix( 0 ),
    baffleSeven( 0 )
{
    mObjectName = "IntStoves";

    showVectors = false;
    showContour = false;

    stlColor[0] = 0.0; stlColor[1] = 0.0; stlColor[2] = 1.0;
    mEventHandlerMap["BAFFLE_UPDATE"]=this;
    mEventHandlerMap["SHOW_VECTORS"]=this;
    mEventHandlerMap["SHOW_CONTOUR"]=this;
}
////////////////////////////////////////////////////////////////////////////////
VEIntStovemod::~VEIntStovemod( void )
{
    if ( baffleOne )
    {
        delete baffleOne;
        baffleOne = 0;
    }
    if ( baffleTwo )
    {
        delete baffleTwo;
        baffleTwo = 0;
    }
    if ( baffleThree )
    {
        delete baffleThree;
        baffleThree = 0;
    }
    if ( baffleFour )
    {
        delete baffleFour;
        baffleFour = 0;
    }
    if ( baffleFive )
    {
        delete baffleFive;
        baffleFive = 0;
    }
    if ( baffleSix )
    {
        delete baffleSix;
        baffleSix = 0;
    }
    if ( baffleSeven )
    {
        delete baffleSeven;
        baffleSeven = 0;
    }
    
    if( m_stoveData )
    {
        m_stoveData->Delete();
        m_stoveData = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void VEIntStovemod::InitializeNode( osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );

    baffleOne = new ves::xplorer::scenegraph::CADEntity(
        "baffle_new2.stl", mDCS.get() );
    baffleOne->GetDCS()->ToggleDisplay( false );

    baffleTwo = new ves::xplorer::scenegraph::CADEntity(
        "baffle_new2.stl", mDCS.get() );
    baffleTwo->GetDCS()->ToggleDisplay( false );

    baffleThree = new ves::xplorer::scenegraph::CADEntity(
        "baffle_new2.stl", mDCS.get() );
    baffleThree->GetDCS()->ToggleDisplay( false );

    baffleFour = new ves::xplorer::scenegraph::CADEntity(
        "baffle_new2.stl", mDCS.get() );
    baffleFour->GetDCS()->ToggleDisplay( false );

    baffleFive = new ves::xplorer::scenegraph::CADEntity(
        "baffle_new2.stl", mDCS.get() );
    baffleFive->GetDCS()->ToggleDisplay( false );

    baffleSix = new ves::xplorer::scenegraph::CADEntity(
        "baffle_new2.stl", mDCS.get() );
    baffleSix->GetDCS()->ToggleDisplay( false );

    baffleSeven = new ves::xplorer::scenegraph::CADEntity( 
        "baffle_new2.stl", mDCS.get() );
    baffleSeven->GetDCS()->ToggleDisplay( false );
}
///////////////////////////////////////////////////////////////////////////////
void VEIntStovemod::PreFrameUpdate( void )
{
    //LoadStoveDataSet();
    //CreateContourPlane();
}
///////////////////////////////////////////////////////////////////////////////
void VEIntStovemod::CreateCustomVizFeature( int  )
{
    LoadStoveDataSet();
    if( showContour )
    {
        CreateContourPlane();
    }
    if( showVectors )
    {
        CreateVectorPlane();
    }
}
///////////////////////////////////////////////////////////////////////////////
void VEIntStovemod::SetCurrentCommand(ves::open::xml::CommandPtr command)
{ 
    if( !command )
    {
        return;
    }

    if( command->GetCommandName() == "BAFFLE_UPDATE" )
    {
        command->GetDataValuePair("baffleParams")->GetData(baffleParams);

        baffleNum = baffleParams.at(0);
        startXPos = baffleParams.at(1);
        startYPos = baffleParams.at(2);
        direction = baffleParams.at(3);
        length = baffleParams.at(4);
        depth = baffleParams.at(5);

        trans[0] = startXPos/2;
        trans[1] = startYPos/2;
        trans[2] = 1;

        scale[0] = 1;
        scale[1] = length*5;
        scale[2] = -(depth)*5/6;

        rotate[1] = 0;
        rotate[2] = 0;

        if( direction == 0 )
        {
            rotate[0] = 90;
        }
        else if( direction == 1 )
        {
            rotate[0] = 180;
        }
        else if( direction == 2 )
        {
            rotate[0] = 270;
        }
        else if( direction == 3 )
        {
            rotate[0] = 0;
        }

        if( baffleNum == 0 )
        {
            baffleOne->GetDCS()->ToggleDisplay(true);
            baffleOne->GetDCS()->SetTranslationArray(trans);
            baffleOne->GetDCS()->SetScaleArray(scale);
            baffleOne->GetDCS()->SetRotationArray(rotate);
        }
        else if( baffleNum == 1 )
        {
            baffleTwo->GetDCS()->ToggleDisplay(true);
            baffleTwo->GetDCS()->SetTranslationArray(trans);
            baffleTwo->GetDCS()->SetScaleArray(scale);   
            baffleTwo->GetDCS()->SetRotationArray(rotate);
        }
        else if( baffleNum == 2 )
        {
            baffleThree->GetDCS()->ToggleDisplay(true);
            baffleThree->GetDCS()->SetTranslationArray(trans);
            baffleThree->GetDCS()->SetScaleArray(scale);  
            baffleThree->GetDCS()->SetRotationArray(rotate);
        }
        else if( baffleNum == 3 )
        {
            baffleFour->GetDCS()->ToggleDisplay(true);
            baffleFour->GetDCS()->SetTranslationArray(trans);
            baffleFour->GetDCS()->SetScaleArray(scale);    
            baffleFour->GetDCS()->SetRotationArray(rotate);
        }
        else if( baffleNum == 4 )
        {
            baffleFive->GetDCS()->ToggleDisplay(true);
            baffleFive->GetDCS()->SetTranslationArray(trans);
            baffleFive->GetDCS()->SetScaleArray(scale);   
            baffleFive->GetDCS()->SetRotationArray(rotate);
        }
        else if( baffleNum == 5 )
        {
            baffleSix->GetDCS()->ToggleDisplay(true);
            baffleSix->GetDCS()->SetTranslationArray(trans);
            baffleSix->GetDCS()->SetScaleArray(scale);   
            baffleSix->GetDCS()->SetRotationArray(rotate);
        }
        else if( baffleNum == 6 )
        {
            baffleSeven->GetDCS()->ToggleDisplay(true);
            baffleSeven->GetDCS()->SetTranslationArray(trans);
            baffleSeven->GetDCS()->SetScaleArray(scale);   
            baffleSeven->GetDCS()->SetRotationArray(rotate);
        }
    }
    else if( command->GetCommandName() == "SHOW_VECTORS" )
    {
        command->GetDataValuePair("vectors")->GetData(vectors);

        if( vectors == 1 )
        {
            showVectors = true;
            CreateVectorPlane();
        }
        else
        {
            showVectors = false;
            mDCS->removeChild( m_vectorPlane.get() );
        }
    }
    else if( command->GetCommandName() == "SHOW_CONTOUR" )
    {
        command->GetDataValuePair("contour")->GetData(contour);

        if( contour == 1 )
        {
            showContour = true;
            CreateContourPlane();
        }
        else
        {
            showContour = false;
            mDCS->removeChild( m_contourPlane.get() );
        }
    }
}
///////////////////////////////////////////////////////////////////////////////
void VEIntStovemod::LoadStoveDataSet()
{
    ///Cleanup
    if( m_stoveData )
    {
        m_stoveData->Delete();
        m_stoveData = 0;
    }
    ///Determine when data needs loaded
    ///Load data
    std::string tempStr;
#ifdef WIN32
    tempStr = "C:\\TSVEG\\stoves\\star.vtu";
#else
    tempStr = "star.vtu";
#endif
    m_stoveData = static_cast< vtkDataSet* >( lfx::vtk_utils::readVtkThing( tempStr ) );
}
///////////////////////////////////////////////////////////////////////////////
void VEIntStovemod::CreateContourPlane()
{
    if( !m_stoveData )
    {
        return;
    }

    ///Remove previous contour plane from graph
    if( m_contourPlane.valid() )
    {
        mDCS->removeChild( m_contourPlane.get() );
    }

    double bounds[6];
    m_stoveData->GetBounds(bounds);
    m_stoveData->GetPointData()->SetActiveScalars( "Temperature" );
    cfdCuttingPlane* cuttingPlane = new cfdCuttingPlane( bounds, 2, 10 );

    // insure that we are using correct bounds for the given data set...
    cuttingPlane->SetBounds( m_stoveData->GetBounds() );
    cuttingPlane->Advance( 28 );
    vtkCutter* cutter = vtkCutter::New();
    cutter->SetCutFunction( cuttingPlane->GetPlane() );
    cutter->SetInput( m_stoveData );
    
    vtkTriangleFilter* tris = vtkTriangleFilter::New();
    tris->SetInputConnection( cutter->GetOutputPort() );
    tris->Update();
    tris->GetOutput()->ReleaseDataFlagOn();
    
    vtkStripper* strip = vtkStripper::New();
    strip->SetInputConnection( tris->GetOutputPort() );
    strip->Update();
    strip->GetOutput()->ReleaseDataFlagOn(); 
    
    vtkPolyDataNormals* normals = vtkPolyDataNormals::New();
    normals->SetInputConnection( strip->GetOutputPort() );
    normals->SetFeatureAngle( 130.0f );
    //normals->GetOutput()->ReleaseDataFlagOn(); 
    normals->ComputePointNormalsOn();
    //normals->ComputeCellNormalsOn();
    normals->FlipNormalsOn();
    normals->Update();

    vtkPolyDataMapper* mapper = vtkPolyDataMapper::New();
    mapper->SetInputConnection( normals->GetOutputPort() );
    mapper->ImmediateModeRenderingOn();    
    mapper->SetScalarRange( 472.0f, 979.0f );
    
    vtkLookupTable* lut = vtkLookupTable::New();
    lut->SetNumberOfColors( 256 );            //default is 256
    lut->SetHueRange( 2.0f/3.0f, 0.0f );      //a blue-to-red scale
    lut->SetTableRange( 472.0f, 979.0f );
    lut->Build();
    mapper->SetLookupTable( lut );
    
    vtkActor* temp = vtkActor::New();
    temp->SetMapper( mapper );
    temp->GetProperty()->SetSpecularPower( 20.0f );
    
    try
    {
        osg::ref_ptr< ves::xplorer::scenegraph::Geode > tempGeode = 
            new ves::xplorer::scenegraph::Geode();
        tempGeode->TranslateToGeode( temp );
        m_contourPlane = tempGeode.get();
    }
    catch( std::bad_alloc )
    {
        ;
    }
    
    lut->Delete();
    mapper->Delete();
    temp->Delete();
    delete cuttingPlane;
    cutter->Delete();
    tris->Delete();
    strip->Delete();
    normals->Delete();
    
    ///Now add the contour plane
    if( m_contourPlane.valid() )
    {
        mDCS->addChild( m_contourPlane.get() );
    }
}
///////////////////////////////////////////////////////////////////////////////
void VEIntStovemod::CreateVectorPlane()
{
    if( !m_stoveData )
    {
        return;
    }

    ///Remove previous vector plane from graph
    if( m_vectorPlane.valid() )
    {
        mDCS->removeChild( m_vectorPlane.get() );
    }

    double bounds[6];
    m_stoveData->GetBounds(bounds);
    m_stoveData->GetPointData()->SetActiveScalars( "Velocity_Magnitude" );
    m_stoveData->GetPointData()->SetActiveVectors( "Velocity" );

    cfdCuttingPlane* cuttingPlane = new cfdCuttingPlane( bounds, 2, 10 );
    cuttingPlane->SetBounds( m_stoveData->GetBounds() );
    cuttingPlane->Advance( 29 );

    vtkCutter* cutter = vtkCutter::New();
    cutter->SetCutFunction( cuttingPlane->GetPlane() );
    cutter->SetInput( m_stoveData );

    vtkMaskPoints* ptmask = vtkMaskPoints::New();
    ptmask->SetInputConnection( cutter->GetOutputPort() );
    ptmask->SetOnRatio( 1 );

    //ves::xplorer::DataSet* dataset = new ves::xplorer::DataSet();

    vtkGlyph3D* glyph = vtkGlyph3D::New();
    glyph->SetInputConnection( ptmask->GetOutputPort() );
    glyph->SetSource( ves::xplorer::ModelHandler::instance()->GetArrow() );
    glyph->SetVectorModeToUseVector();
    glyph->SetScaleFactor( 2 );
    glyph->SetScaleModeToScaleByVector();
    glyph->SetColorModeToColorByScalar();
    glyph->ClampingOn();
    glyph->SetRange( m_stoveData->GetScalarRange() );
    glyph->Update();

    vtkPolyDataMapper* mapper = vtkPolyDataMapper::New();
    mapper->SetInputConnection( glyph->GetOutputPort() );
    mapper->ImmediateModeRenderingOn();    
    mapper->SetScalarRange( m_stoveData->GetScalarRange() );
    
    vtkLookupTable* lut = vtkLookupTable::New();
    lut->SetNumberOfColors( 256 );            //default is 256
    lut->SetHueRange( 2.0f/3.0f, 0.0f );      //a blue-to-red scale
    lut->SetTableRange( m_stoveData->GetScalarRange() );
    lut->Build();
    mapper->SetLookupTable( lut );

    vtkActor* temp = vtkActor::New();
    temp->SetMapper( mapper );
    temp->GetProperty()->SetSpecularPower( 20.0f );

    try
    {
        osg::ref_ptr< ves::xplorer::scenegraph::Geode > tempGeode = 
            new ves::xplorer::scenegraph::Geode();
        tempGeode->TranslateToGeode( temp );
        m_vectorPlane = tempGeode.get();
    }
    catch( std::bad_alloc )
    {
        ;
    }

    delete cuttingPlane;
    cutter->Delete();
    ptmask->Delete();
    mapper->Delete();
    lut->Delete();
    temp->Delete();

    ///Now add the vector plane
    if( m_vectorPlane.valid() )
    {
        mDCS->addChild( m_vectorPlane.get() );
    }
}
