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
#include <ves/xplorer/event/viz/cfdStreamers.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkPointSet.h>
#include <vtkRungeKutta4.h>
#include <vtkStreamLine.h>
#include <vtkStreamTracer.h>
#include <vtkTubeFilter.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkConeSource.h>
#include <vtkStreamPoints.h>
#include <vtkGlyph3D.h>
#include <vtkAppendPolyData.h>
#include <vtkStreamTracer.h>
#include <vtkStripper.h>
#include <vtkTriangleFilter.h>
#include <vtkPolyDataNormals.h>
#include <vtkCellDataToPointData.h>
#include <vtkMaskPoints.h>
#include <vtkThresholdPoints.h>
#include <vtkCellArray.h>
#include <vtkCleanPolyData.h>
#include <vtkPointData.h>
#include <vtkPolyDataAlgorithm.h>
#include <vtkRibbonFilter.h>

#include <ves/xplorer/Debug.h>

using namespace ves::xplorer;
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
    integ = vtkRungeKutta4::New();
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

    if( seedPoints )
    {
        seedPoints->Delete();
    }
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

    //tubeFilter->SetRadius( lineDiameter );
    //tubeFilter->SetNumberOfSides( 3 );
    //tubeFilter->SidesShareVerticesOn();

    if( propagationTime == -1 )
    {
        propagationTime = 10.0f * GetActiveDataSet()->GetMaxTime();
    }

    if( integrationStepLength == -1 )
    {
        integrationStepLength = 0.050f;
    }

    vtkCellDataToPointData* c2p = vtkCellDataToPointData::New();
    c2p->SetInput(  GetActiveDataSet()->GetDataSet() );
    
    streamTracer->SetInputConnection( c2p->GetOutputPort() );
    //overall length of streamline
    streamTracer->SetMaximumPropagation( propagationTime );
    
    // typically < 1
    streamTracer->SetMaximumIntegrationStep( integrationStepLength );

    if( integrationDirection == 0 )
    {
        streamTracer->SetIntegrationDirectionToBoth();
    }
    else if( integrationDirection == 1 )
    {
        streamTracer->SetIntegrationDirectionToForward();
    }
    else if( integrationDirection == 2 )
    {
        streamTracer->SetIntegrationDirectionToBackward();
    }

    streamTracer->SetSource( seedPoints );
    /*vtkPolyDataAlgorithm* pdFilter = vtkPolyDataAlgorithm::New();
    pdFilter->SetInput( seedPoints );
    streamTracer->SetSourceConnection( pdFilter->GetOutputPort() );
    pdFilter->Delete();*/
    /*vtkCleanPolyData* pdFilter = vtkCleanPolyData::New();
    pdFilter->PointMergingOn();
    pdFilter->SetInput( seedPoints );
    streamTracer->SetSourceConnection( pdFilter->GetOutputPort() );
    pdFilter->Delete();
     */
    streamTracer->SetIntegrator( integ );
    streamTracer->ComputeVorticityOn();
    streamTracer->SetInputArrayToProcess( 0, 0, 0,
       vtkDataObject::FIELD_ASSOCIATION_POINTS, 
       GetActiveDataSet()->GetActiveVectorName().c_str() );

    vtkCleanPolyData* cleanPD = vtkCleanPolyData::New();
    cleanPD->PointMergingOn();
    cleanPD->SetInputConnection( streamTracer->GetOutputPort() );
    
    vtkRibbonFilter* ribbon = 0;
    if( m_streamRibbons )
    {
        ribbon = vtkRibbonFilter::New();
        //ribbon->SetWidthFactor( arrowDiameter * 0.25);
        ribbon->SetWidth( arrowDiameter );
        ribbon->SetInputConnection( cleanPD->GetOutputPort() );
        ribbon->SetInputArrayToProcess( 0, 0, 0,
                                       vtkDataObject::FIELD_ASSOCIATION_POINTS, 
                                       "Vorticity" );
    }
        
    if( streamArrows )
    {
        // Stream Points Section
        vtkConeSource* cone = 0;
        vtkGlyph3D* cones = 0;
        vtkAppendPolyData* append = 0;
        vtkPolyDataNormals* normals = 0;

        /*{
            vtkXMLPolyDataWriter* writer = vtkXMLPolyDataWriter::New();
            writer->SetInput( ( vtkPolyData* ) cleanPD->GetOutput() );
            //writer->SetDataModeToAscii();
            writer->SetFileName( "teststreamersclean.vtk" );
            writer->Write();
            writer->Delete();
        }*/
        
        vtkMaskPoints* ptmask = vtkMaskPoints::New();
        ptmask->RandomModeOff();
        ptmask->SetInputConnection( cleanPD->GetOutputPort() );
        ptmask->SetOnRatio( 2 );

        cone = vtkConeSource::New();
        cone->SetResolution( 5 );

        cones = vtkGlyph3D::New();
        cones->SetInputConnection( 0, ptmask->GetOutputPort() );
        cones->SetInputConnection( 1, cone->GetOutputPort() );
        //cones->SetSource( cone->GetOutput() );
        cones->SetScaleFactor( arrowDiameter );
        //cones->SetScaleModeToScaleByVector();
        cones->SetScaleModeToDataScalingOff();
        //cones->SetColorModeToColorByScalar();
        cones->SetVectorModeToUseVector();
        //cones->GetOutput()->ReleaseDataFlagOn();
        cones->SetInputArrayToProcess( 1, 0, 0,
                                             vtkDataObject::FIELD_ASSOCIATION_POINTS, 
                                             GetActiveDataSet()->GetActiveVectorName().c_str() );
        cones->SetInputArrayToProcess( 0, 0, 0,
                                      vtkDataObject::FIELD_ASSOCIATION_POINTS, 
                                      GetActiveDataSet()->GetActiveScalarName().c_str() );
        cones->SetInputArrayToProcess( 3, 0, 0,
                                      vtkDataObject::FIELD_ASSOCIATION_POINTS, 
                                      GetActiveDataSet()->GetActiveScalarName().c_str() );
        ptmask->Delete();
        
        normals = vtkPolyDataNormals::New();
        normals->SetInputConnection( cones->GetOutputPort() );
        normals->SplittingOff();
        normals->ConsistencyOn();
        normals->AutoOrientNormalsOn();
        normals->ComputePointNormalsOn();
        normals->ComputeCellNormalsOff();
        normals->NonManifoldTraversalOff();
        normals->Update();
        /*{
            vtkXMLPolyDataWriter* writer = vtkXMLPolyDataWriter::New();
            writer->SetInput( ( vtkPolyData* ) normals->GetOutput() );
            //writer->SetDataModeToAscii();
            writer->SetFileName( "teststreamercones.vtk" );
            writer->Write();
            writer->Delete();
        }*/
        normals->GetOutput()->GetPointData()->SetActiveNormals( "Normals" );
        normals->GetOutput()->GetPointData()->SetActiveScalars( GetActiveDataSet()->GetActiveScalarName().c_str() );
        normals->GetOutput()->GetPointData()->SetActiveVectors( GetActiveDataSet()->GetActiveVectorName().c_str() );

        streamTracer->GetOutput()->GetPointData()->SetActiveNormals( "Normals" );
        streamTracer->GetOutput()->GetPointData()->SetActiveScalars( GetActiveDataSet()->GetActiveScalarName().c_str() );
        streamTracer->GetOutput()->GetPointData()->SetActiveVectors( GetActiveDataSet()->GetActiveVectorName().c_str() );

        append = vtkAppendPolyData::New();
        //append->DebugOn();

        if( ribbon )
        {
            append->AddInput( ribbon->GetOutput() );
            ribbon->Delete();
        }
        else
        {
            append->AddInput( cleanPD->GetOutput() );
        }
        cleanPD->Delete();

        /*{
            vtkXMLPolyDataWriter* writer = vtkXMLPolyDataWriter::New();
            writer->SetInput( ( vtkPolyData* ) streamTracer->GetOutput() );
            //writer->SetDataModeToAscii();
            writer->SetFileName( "teststreamersstream.vtk" );
            writer->Write();
            writer->Delete();
        }*/
        append->AddInput( normals->GetOutput() );
        /*{
            vtkXMLPolyDataWriter* writer = vtkXMLPolyDataWriter::New();
            writer->SetInput( ( vtkPolyData* ) append->GetOutput() );
            //writer->SetDataModeToAscii();
            writer->SetFileName( "teststreamersapp.vtk" );
            writer->Write();
            writer->Delete();
        }*/

        vtkPolyDataNormals* overallNormals = vtkPolyDataNormals::New();
        overallNormals->SetInputConnection( append->GetOutputPort() );
        overallNormals->SplittingOff();
        overallNormals->ConsistencyOn();
        overallNormals->AutoOrientNormalsOn();
        overallNormals->ComputePointNormalsOn();
        overallNormals->ComputeCellNormalsOff();
        overallNormals->NonManifoldTraversalOff();
        //overallNormals->Update();
        
        mapper->SetInputConnection( overallNormals->GetOutputPort() );
        
        // Stream Points Section
        cone->Delete();
        cones->Delete();
        append->Delete();
        normals->Delete();
        overallNormals->Delete();
    }
    else
    {
        
        if( ribbon )
        {
            mapper->SetInputConnection( ribbon->GetOutputPort() );
            ribbon->Delete();
        }
        else
        {
            /*{
             vtkXMLPolyDataWriter* writer = vtkXMLPolyDataWriter::New();
             writer->SetInput( cleanPD->GetOutput() );
             //writer->SetDataModeToAscii();
             writer->SetFileName( "streamline_output.vtp" );
             writer->Write();
             writer->Delete();
             }*/
            mapper->SetInputConnection( cleanPD->GetOutputPort() );
        }
        cleanPD->Delete();
    }

    mapper->SetColorModeToMapScalars();
    mapper->SetLookupTable( GetActiveDataSet()->GetLookupTable() );
    mapper->ImmediateModeRenderingOn();
    mapper->SetScalarModeToUsePointFieldData();
    mapper->UseLookupTableScalarRangeOn();
    mapper->SelectColorArray( GetActiveDataSet()->
        GetActiveScalarName().c_str() );

    vtkActor* temp = vtkActor::New();
    temp->SetMapper( mapper );
    temp->GetProperty()->SetSpecularPower( 20.0f );
    temp->GetProperty()->SetLineWidth( lineDiameter );
    //test to see if there is enough memory, if not, filters are deleted
    try
    {
        osg::ref_ptr< ves::xplorer::scenegraph::Geode > tempGeode = 
            new ves::xplorer::scenegraph::Geode();
        tempGeode->TranslateToGeode( temp );
        //tempGeode->StreamLineToGeode( temp );

        geodes.push_back( tempGeode );
        updateFlag = true;
    }
    catch ( std::bad_alloc )
    {
        streamTracer->Delete();
        streamTracer = vtkStreamTracer::New();
        mapper->Delete();
        mapper = vtkPolyDataMapper::New();
        vprDEBUG( vesDBG, 0 ) << "|\tMemory allocation failure : cfdStreamers "
            << std::endl << vprDEBUG_FLUSH;
    }

    temp->Delete();
    c2p->Delete();

    vprDEBUG( vesDBG, 0 ) << "|\tcfdStreamers::Update End" << std::endl << vprDEBUG_FLUSH;
}
//////////////////////////////////////////////////////////////////////////////////
vtkPolyData* cfdStreamers::GetStreamersOutput()
{
    // may need to gaurd this somehow
    return ( streamTracer->GetOutput() );
}
//////////////////////////////////////////////////////////////////////////////////
double cfdStreamers::GetArrowDiameter()
{
    return arrowDiameter;
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
    integrationStepLength = ( float )value * ( 0.050f ) / 50.0f;
}
//////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::UpdateCommand()
{
    //Call base method - currently does nothing
    cfdObjects::UpdateCommand();

    //Extract the specific commands from the overall command
    ves::open::xml::DataValuePairPtr activeModelDVP = veCommand->GetDataValuePair( "Sub-Dialog Settings" );
    ves::open::xml::CommandPtr objectCommand = boost::dynamic_pointer_cast<ves::open::xml::Command>(  activeModelDVP->GetDataXMLObject() );

    //Extract the integration direction
    activeModelDVP = objectCommand->GetDataValuePair( "Integration Direction" );
    std::string intDirection;
    activeModelDVP->GetData( intDirection );
    
    vprDEBUG( vesDBG, 0 ) << "|\tStreamline settings"
        << std::endl << vprDEBUG_FLUSH;

    if( !intDirection.compare( "backward" ) )
    {
        vprDEBUG( vesDBG, 0 ) << "|\t\tBACKWARD_INTEGRATION"
            << std::endl << vprDEBUG_FLUSH;

        SetIntegrationDirection( 2 );
    }
    else if( !intDirection.compare( "forward" ) )
    {
        vprDEBUG( vesDBG, 0 ) << "|\t\tFORWARD_INTEGRATION"
            << std::endl << vprDEBUG_FLUSH;

        SetIntegrationDirection( 1 );
    }
    else if( !intDirection.compare( "both directions" ) )
    {
        vprDEBUG( vesDBG, 0 ) << "|\t\tTWO_DIRECTION_INTEGRATION"
            << std::endl << vprDEBUG_FLUSH;

        SetIntegrationDirection( 0 );
    }

    //Extract the advanced settings from the commands
    activeModelDVP = objectCommand->GetDataValuePair( "Advanced Streamline Settings" );
    objectCommand = boost::dynamic_pointer_cast<ves::open::xml::Command>( activeModelDVP->GetDataXMLObject() );

    /////////////////////
    activeModelDVP = objectCommand->GetDataValuePair( "Use Stream Ribbons" );
    unsigned int streamRibbons;
    activeModelDVP->GetData( streamRibbons );
    vprDEBUG( vesDBG, 0 ) << "|\t\tUse Stream Ribbons\t" << streamRibbons
        << std::endl << vprDEBUG_FLUSH;
    m_streamRibbons = streamRibbons;
    
    /////////////////////
    activeModelDVP = objectCommand->GetDataValuePair( "Use Stream Arrows" );
    unsigned int opacity;
    activeModelDVP->GetData( opacity );
    vprDEBUG( vesDBG, 0 ) << "|\t\tSTREAMLINE_ARROW\t" << opacity
        << std::endl << vprDEBUG_FLUSH;
    streamArrows = opacity;

    /////////////////////
    activeModelDVP = objectCommand->GetDataValuePair( "Integration Step Size" );
    double contourLOD;
    activeModelDVP->GetData( contourLOD );
    vprDEBUG( vesDBG, 0 ) << "|\t\tCHANGE_INT_STEP_LENGTH\t"
        << contourLOD << std::endl << vprDEBUG_FLUSH;
    SetIntegrationStepLength( static_cast< int >( contourLOD ) );

    /////////////////////
    activeModelDVP = objectCommand->GetDataValuePair( "Propagation Time" );
    double tempPropagationTime = 1.0f;
    activeModelDVP->GetData( tempPropagationTime );
    vprDEBUG( vesDBG, 0 ) << "|\t\tCHANGE_PROPAGATION_TIME\t"
        << tempPropagationTime
        << std::endl << vprDEBUG_FLUSH;
    SetPropagationTime( tempPropagationTime );

    /////////////////////
    activeModelDVP = objectCommand->GetDataValuePair( "Diameter" );
    double streamDiamter = 1.0f;
    activeModelDVP->GetData( streamDiamter );
    vprDEBUG( vesDBG, 0 ) << "|\t\tSTREAMLINE_DIAMETER\t"
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

    vprDEBUG( vesDBG, 1 ) << "|\t\tNew Streamline Diameter : "
        << lineDiameter << std::endl << vprDEBUG_FLUSH;

    /////////////////////
    activeModelDVP = 
        objectCommand->GetDataValuePair( "Sphere/Arrow/Particle Size" );
    activeModelDVP->GetData( arrowDiameter );
    arrowDiameter = localLineDiameter * 60.0f * arrowDiameter;
    vprDEBUG( vesDBG, 1 ) << "|\t\tNew Arrow Diameter : "
        << arrowDiameter << std::endl << vprDEBUG_FLUSH;

    /////////////////////
    activeModelDVP = objectCommand->GetDataValuePair( "Use Last Seed Pt" );
    unsigned int lastSeedPt;
    activeModelDVP->GetData( lastSeedPt );

    ////////////////////
    //Set the number of seed points in each direction and get the %BB info
    //Extract the advanced settings from the commands
    activeModelDVP = objectCommand->GetDataValuePair( "Max_X_BB" );
    //if 1 is there then they all are there
    if( activeModelDVP )
    {
        activeModelDVP->GetData( xMaxBB );
        vprDEBUG( vesDBG, 0 ) << "|\t\txMaxBB : " << xMaxBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Max_Y_BB" );
        activeModelDVP->GetData( yMaxBB );
        vprDEBUG( vesDBG, 0 ) << "|\t\tyMaxBB : " << yMaxBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Max_Z_BB" );
        activeModelDVP->GetData( zMaxBB );
        vprDEBUG( vesDBG, 0 ) << "|\t\tzMaxBB : " << zMaxBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Min_X_BB" );
        activeModelDVP->GetData( xMinBB );
        vprDEBUG( vesDBG, 0 ) << "|\t\txMinBB : " << xMinBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Min_Y_BB" );
        activeModelDVP->GetData( yMinBB );
        vprDEBUG( vesDBG, 0 ) << "|\t\tyMinBB : " << yMinBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Min_Z_BB" );
        activeModelDVP->GetData( zMinBB );
        vprDEBUG( vesDBG, 0 ) << "|\t\tzMinBB : " << zMinBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Num_X_Points" );
        activeModelDVP->GetData( xValue );
        vprDEBUG( vesDBG, 0 ) << "|\t\tX Points : " << xValue << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Num_Y_Points" );
        activeModelDVP->GetData( yValue );
        vprDEBUG( vesDBG, 0 ) << "|\t\tY Points : " << yValue << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Num_Z_Points" );
        activeModelDVP->GetData( zValue );
        vprDEBUG( vesDBG, 0 ) << "|\t\tZ Points : " << zValue << std::endl << vprDEBUG_FLUSH;
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
    points = vtkPoints::New();

    double deltaX = ( xValue == 1 ) ? 0 : ( xMax - xMin ) / double( xValue - 1 );
    double deltaY = ( yValue == 1 ) ? 0 : ( yMax - yMin ) / double( yValue - 1 );
    double deltaZ = ( zValue == 1 ) ? 0 : ( zMax - zMin ) / double( zValue - 1 );

    for( unsigned int i = 0; i < xValue; ++i )
    {
        xLoc = xMin + ( i * deltaX );
        for( unsigned int j = 0; j < yValue; ++j )
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
    points->Delete();
}
//////////////////////////////////////////////////////////////////////////////////
