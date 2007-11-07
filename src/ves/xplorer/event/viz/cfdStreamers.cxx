/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/xplorer/event/viz/cfdStreamers.h>
#include <ves/xplorer/cfdDataSet.h>
#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/cfdCommandArray.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkPointSet.h>
#include <vtkRungeKutta45.h>
#include <vtkStreamLine.h>
#include <vtkStreamTracer.h>
#include <vtkTubeFilter.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkPolyDataWriter.h>
#include <vtkConeSource.h>
#include <vtkStreamPoints.h>
#include <vtkGlyph3D.h>
#include <vtkAppendPolyData.h>
#include <vtkStreamTracer.h>
#include <vtkStripper.h>
#include <vtkTriangleFilter.h>
#include <vtkPolyDataNormals.h>

#include <ves/xplorer/cfdDebug.h>

using namespace VE_Xplorer;
using namespace ves::xplorer::scenegraph;

//////////////////////////////////////////////////////////////////////////////////
cfdStreamers::cfdStreamers()
:
integrationDirection( 0 ),
propagationTime( -1 ),
integrationStepLength( -1 ),
lineDiameter( 1.0f ),
arrowDiameter( 1 ),
streamArrows( 0 ),
//pointSource( 0 ),

xValue( 4 ),
yValue( 4 ),
zValue( 4 ),
seedPoints( 0 ),
points( 0 ),
xMinBB( 0 ),
yMinBB( 0 ),
zMinBB( 0 ),
xMaxBB( 1 ),
yMaxBB( 1 ),
zMaxBB( 1 )
{
    streamTracer = vtkStreamTracer::New();
    integ = vtkRungeKutta45::New();
    tubeFilter = vtkTubeFilter::New();
    mapper = vtkPolyDataMapper::New();
}
//////////////////////////////////////////////////////////////////////////////////
cfdStreamers::~cfdStreamers()
{
    streamTracer->Delete();
    integ->Delete();
    tubeFilter->Delete();
    mapper->Delete();
}
//////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::Update()
{
    if( seedPoints == NULL )
    {
        vprDEBUG( vesDBG, 0 ) << "|\tcfdStreamers::Update, No Cursor Type Selected" 
                              << std::endl << vprDEBUG_FLUSH;

        return;
    }

    vprDEBUG( vesDBG, 0 ) << "|   cfdStreamers::Update, origin = "
                          << origin[ 0 ] << " : " 
                          << origin[ 1 ] << " : " 
                          << origin[ 2 ] << std::endl 
                          << " Prop Time : " << propagationTime 
                          << " Integration Step Length : " << integrationStepLength 
                          << " Integration Direction : " << integrationDirection
                          << std::endl << vprDEBUG_FLUSH;

    tubeFilter->SetRadius( lineDiameter );
    tubeFilter->SetNumberOfSides( 3 );
    tubeFilter->SidesShareVerticesOn();

    if( propagationTime == -1 )
    {
        propagationTime = 10.0f * GetActiveDataSet()->GetMaxTime();
    }

    if( integrationStepLength == -1 )
    {
        integrationStepLength = 0.050f;
    }

    streamTracer->SetInput( (vtkDataSet*)GetActiveDataSet()->GetDataSet() );
    //overall length of streamline
    streamTracer->SetMaximumPropagation( propagationTime );

    // typically < 1
    streamTracer->SetMaximumIntegrationStep( integrationStepLength );

    // Stream Points Section
    vtkStreamPoints* streamPoints = 0;
    vtkConeSource* cone = 0;
    vtkGlyph3D* cones = 0;
    vtkAppendPolyData* append = 0;
    vtkPolyDataNormals* normals = 0;

    if( streamArrows )
    {
        streamPoints = vtkStreamPoints::New();
        streamPoints->SetInput( static_cast< vtkDataSet* >( GetActiveDataSet()->GetDataSet() ) );
        streamPoints->SetSource( seedPoints );
        //streamPoints->SetTimeIncrement( stepLength * 500 );
        streamPoints->SetMaximumPropagationTime( propagationTime );
        streamPoints->SetIntegrationStepLength( integrationStepLength );    
        streamPoints->SetIntegrator( integ );
        streamPoints->SpeedScalarsOff();
    }

    if( integrationDirection == 0 )
    {
        streamTracer->SetIntegrationDirectionToBoth();
        if( streamArrows )
        {
            streamPoints->SetIntegrationDirectionToIntegrateBothDirections();
        }
    }
    else if( integrationDirection == 1 )
    {
        streamTracer->SetIntegrationDirectionToForward();
        if( streamArrows )
        {
            streamPoints->SetIntegrationDirectionToForward();
        }
    }
    else if( integrationDirection == 2 )
    {
        streamTracer->SetIntegrationDirectionToBackward();
        if( streamArrows )
        {
            streamPoints->SetIntegrationDirectionToBackward();
        }
    }

    streamTracer->SetSource( seedPoints );
    streamTracer->SetIntegrator( integ );
    // Good Test code to see if you are actually getting streamlines
    /*vtkPolyDataWriter *writer = vtkPolyDataWriter::New();
    writer->SetInput( ( vtkPolyData * ) stream->GetOutput() );
    writer->SetFileName( "teststreamers.vtk" );
    writer->Write();*/

    if( streamArrows )
    {
        cone = vtkConeSource::New();
        cone->SetResolution( 5 );

        cones = vtkGlyph3D::New();
        cones->SetInput( streamPoints->GetOutput() );
        cones->SetSource( cone->GetOutput() );
        cones->SetScaleFactor( arrowDiameter );
        //cones->SetScaleModeToScaleByVector();
        cones->SetScaleModeToDataScalingOff();
        cones->SetVectorModeToUseVector();
        //cones->GetOutput()->ReleaseDataFlagOn();

        append = vtkAppendPolyData::New();
        append->AddInput( streamTracer->GetOutput() );
        append->AddInput( cones->GetOutput() );
        append->Update();

        normals = vtkPolyDataNormals::New();
        normals->SetInput( append->GetOutput() );
        normals->SplittingOff();
        normals->ConsistencyOn();
        normals->AutoOrientNormalsOn();
        normals->ComputePointNormalsOn();
        normals->ComputeCellNormalsOff();
        normals->NonManifoldTraversalOff();

        mapper->SetInput( normals->GetOutput() );
    }
    else
    {
        mapper->SetInput( streamTracer->GetOutput() );
    }

    mapper->SetColorModeToMapScalars();
    mapper->SetScalarRange( GetActiveDataSet()->GetUserRange() );
    mapper->SetLookupTable( GetActiveDataSet()->GetLookupTable() );
    mapper->ImmediateModeRenderingOn();

    vtkActor* temp = vtkActor::New();
    temp->SetMapper( mapper );
    temp->GetProperty()->SetSpecularPower( 20.0f );
    temp->GetProperty()->SetLineWidth( lineDiameter );
    //test to see if there is enough memory, if not, filters are deleted
    try
    {
        osg::ref_ptr< ves::xplorer::scenegraph::Geode > tempGeode = new ves::xplorer::scenegraph::Geode();
        //tempGeode->TranslateToGeode( temp );
        tempGeode->StreamLineToGeode( temp );
        geodes.push_back( tempGeode ); 
        updateFlag = true;
    }
    catch( std::bad_alloc )
    {
        streamTracer->Delete();
        streamTracer = vtkStreamTracer::New();
        mapper->Delete();
        mapper = vtkPolyDataMapper::New();
        vprDEBUG(vesDBG,0) << "|\tMemory allocation failure : cfdStreamers " 
                           << std::endl << vprDEBUG_FLUSH;
    }

    temp->Delete();

    if( streamArrows )
    {
        // Clean Up Now...
        streamPoints->Delete();
        append->Delete();
        cone->Delete();
        cones->Delete();
        normals->Delete();
    }

    vprDEBUG(vesDBG,0) << "|\tcfdStreamers::Update End" << std::endl << vprDEBUG_FLUSH;
}
//////////////////////////////////////////////////////////////////////////////////
vtkPolyData * cfdStreamers::GetStreamersOutput()
{
    // may need to gaurd this somehow
    return ( streamTracer->GetOutput() );
}
//////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::SetIntegrationDirection( int value )
{
    integrationDirection = value;
}
//////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::SetPropagationTime( double value )
{
    propagationTime = value * ( 100.0f * GetActiveDataSet()->GetMaxTime() / 20.0f );
}
//////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::SetIntegrationStepLength( int value )
{
    integrationStepLength = (float)value * ( 0.050f ) / 50.0f;
}
//////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::UpdateCommand()
{
    //Call base method - currently does nothing
    cfdObjects::UpdateCommand();

    //Extract the specific commands from the overall command
    ves::open::xml::DataValuePairWeakPtr activeModelDVP = veCommand->GetDataValuePair( "Sub-Dialog Settings" );
    ves::open::xml::Command* objectCommand = dynamic_cast< ves::open::xml::Command* >( activeModelDVP->GetDataXMLObject() );

    //Extract the integration direction
    activeModelDVP = objectCommand->GetDataValuePair( "Integration Direction" );
    std::string intDirection;
    activeModelDVP->GetData( intDirection );

    if( !intDirection.compare( "backward" ) )         
    {
        vprDEBUG(vesDBG,0) << " BACKWARD_INTEGRATION" 
                           << std::endl << vprDEBUG_FLUSH;

        SetIntegrationDirection( 2 );
    }
    else if( !intDirection.compare( "forward" ) )         
    {
        vprDEBUG(vesDBG,0) << " FORWARD_INTEGRATION"
                           << std::endl << vprDEBUG_FLUSH;

        SetIntegrationDirection( 1 );
    }
    else if( !intDirection.compare( "both directions" ) )         
    {
        vprDEBUG(vesDBG,0) << " TWO_DIRECTION_INTEGRATION" 
                           << std::endl << vprDEBUG_FLUSH;

        SetIntegrationDirection( 0 );
    }

    //Extract the advanced settings from the commands
    activeModelDVP = objectCommand->GetDataValuePair( "Advanced Streamline Settings" );
    objectCommand = dynamic_cast< ves::open::xml::Command* >( activeModelDVP->GetDataXMLObject() );

    /////////////////////
    activeModelDVP = objectCommand->GetDataValuePair( "Use Stream Arrows" );
    unsigned int opacity;
    activeModelDVP->GetData( opacity );
    vprDEBUG(vesDBG,0) << " STREAMLINE_ARROW\t" << opacity 
                       << std::endl << vprDEBUG_FLUSH;
    streamArrows = opacity;

    /////////////////////
    activeModelDVP = objectCommand->GetDataValuePair( "Integration Step Size" );
    double contourLOD;
    activeModelDVP->GetData( contourLOD );
    vprDEBUG(vesDBG,0) << " CHANGE_INT_STEP_LENGTH\t"
                       << contourLOD << std::endl << vprDEBUG_FLUSH;
    SetIntegrationStepLength( static_cast< int >( contourLOD ) );

    /////////////////////
    activeModelDVP = objectCommand->GetDataValuePair( "Propagation Time" );
    double tempPropagationTime = 1.0f;
    activeModelDVP->GetData( tempPropagationTime );
    vprDEBUG(vesDBG,0) << " CHANGE_PROPAGATION_TIME\t" 
    << tempPropagationTime 
    << std::endl << vprDEBUG_FLUSH;
    SetPropagationTime( tempPropagationTime );

    /////////////////////
    activeModelDVP = objectCommand->GetDataValuePair( "Diameter" );
    double streamDiamter = 1.0f;
    activeModelDVP->GetData( streamDiamter );
    vprDEBUG( vesDBG, 0 ) << " STREAMLINE_DIAMETER\t" 
                          << streamDiamter << std::endl << vprDEBUG_FLUSH; 
    // diameter is obtained from gui, -100 < vectorScale < 100
    // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1
    // convert range to -2.5 < x < 2.5, and compute the exponent...
    float range = 2.5f;
    int diameter = static_cast< int >( streamDiamter );
    float localLineDiameter = exp( diameter / ( 100.0 / range ) ) * 1.0f * 0.001f;

    // this is to normalize -100 to 100 on the GUI  to  1-21 for diameters
    // note that multiplying by 0.005 is the same as dividing by 200, or the range
    lineDiameter = ( diameter + 110 ) * 0.005 *  20;

    vprDEBUG(vesDBG,1) << "|\tNew Streamline Diameter : " 
                       << lineDiameter << std::endl << vprDEBUG_FLUSH;
    arrowDiameter = localLineDiameter * 60.0f;
    vprDEBUG(vesDBG,1) << "|\tNew Arrow Diameter : " 
                       << arrowDiameter << std::endl << vprDEBUG_FLUSH;

    /////////////////////
    //activeModelDVP = objectCommand->GetDataValuePair( "Sphere/Arrow/Particle Size" );
    //double sphereArrow = 1.0f;
    //activeModelDVP->GetData( streamDiamter );

    /////////////////////
    activeModelDVP = objectCommand->GetDataValuePair( "Use Last Seed Pt" );
    unsigned int lastSeedPt;
    activeModelDVP->GetData( lastSeedPt );

    ////////////////////
    //Set the number of seed points in each direction and get the %BB info
    //Extract the advanced settings from the commands
    //activeModelDVP = objectCommand->GetDataValuePair( "Seed_Point_Settings" );
    //objectCommand = dynamic_cast< ves::open::xml::Command* >( activeModelDVP->GetDataXMLObject() );
    activeModelDVP = objectCommand->GetDataValuePair( "Max_X_BB" );
    //if 1 is there then they all are there
    if( activeModelDVP )
    {
        activeModelDVP->GetData( xMaxBB );   
        vprDEBUG( vesDBG, 0 ) << "|\txMaxBB : " << xMaxBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Max_Y_BB" );
        activeModelDVP->GetData( yMaxBB );   
        vprDEBUG( vesDBG, 0 ) << "|\tyMaxBB : " << yMaxBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Max_Z_BB" );
        activeModelDVP->GetData( zMaxBB );   
        vprDEBUG( vesDBG, 0 ) << "|\tzMaxBB : " << zMaxBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Min_X_BB" );
        activeModelDVP->GetData( xMinBB );   
        vprDEBUG( vesDBG, 0 ) << "|\txMinBB : " << xMinBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Min_Y_BB" );
        activeModelDVP->GetData( yMinBB );   
        vprDEBUG( vesDBG, 0 ) << "|\tyMinBB : " << yMinBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Min_Z_BB" );
        activeModelDVP->GetData( zMinBB );   
        vprDEBUG( vesDBG, 0 ) << "|\tzMinBB : " << zMinBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Num_X_Points" );
        activeModelDVP->GetData( xValue );   
        vprDEBUG( vesDBG, 0 ) << "|\tX Points : " << xValue << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Num_Y_Points" );
        activeModelDVP->GetData( yValue );   
        vprDEBUG( vesDBG, 0 ) << "|\tY Points : " << yValue << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Num_Z_Points" );
        activeModelDVP->GetData( zValue );
        vprDEBUG( vesDBG, 0 ) << "|\tZ Points : " << zValue << std::endl << vprDEBUG_FLUSH;
    }

    CreateSeedPoints();
}
//////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::CreateSeedPoints()
{
    double bounds[ 6 ];
    GetActiveDataSet()->GetBounds( bounds );

    double xDiff = bounds[ 1 ] - bounds[ 0 ];
    double yDiff = bounds[ 3 ] - bounds[ 2 ];
    double zDiff = bounds[ 5 ] - bounds[ 4 ];

    double xMin = bounds[ 0 ] + ( xDiff * xMinBB );
    double xMax = bounds[ 0 ] + ( xDiff * xMaxBB );
    double yMin = bounds[ 2 ] + ( yDiff * yMinBB );
    double yMax = bounds[ 2 ] + ( yDiff * yMaxBB );
    double zMin = bounds[ 4 ] + ( zDiff * zMinBB );
    double zMax = bounds[ 4 ] + ( zDiff * zMaxBB );

    double xLoc = 0;
    double yLoc = 0;
    double zLoc = 0;
    int number = 0;

    //insert evenly spaced points inside bounding box
    if( points )
    {      
        points->Delete();
    }
    points = vtkPoints::New();

    double deltaX = ( xValue == 1 ) ? 0 : ( xMax - xMin ) / double( xValue - 1 );
    double deltaY = ( yValue == 1 ) ? 0 : ( yMax - yMin ) / double( yValue - 1 );
    double deltaZ = ( zValue == 1 ) ? 0 : ( zMax - zMin ) / double( zValue - 1 );

    for( unsigned int i = 0; i < xValue; ++i )
    {
        xLoc = xMin + (i*deltaX);
        for (unsigned int j = 0; j < yValue; ++j )
        {
            yLoc = yMin + ( j * deltaY );
            for( unsigned int k = 0; k < zValue; ++k )			
            {
                //points added in ptMin + length*iteration/(number of equal segments)
                //where (number of equal segments) = ptValue+1
                zLoc = zMin + ( k * deltaZ );
                //std::cout << xLoc << " " <<  yLoc << " " <<  zLoc << std::endl;
                points->InsertPoint( number, xLoc, yLoc, zLoc ); 
                number = number + 1;
            }
        }
    }

    //create polydata to be glyphed
    if( seedPoints )
    {
        seedPoints->Delete();
    }

    seedPoints = vtkPolyData::New();
    seedPoints->SetPoints( points );
}
//////////////////////////////////////////////////////////////////////////////////
