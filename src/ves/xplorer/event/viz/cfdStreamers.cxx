/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/event/viz/OSGStreamlineStage.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <propertystore/PropertySet.h>

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

#include <latticefx/core/vtk/DataSet.h>
#include <latticefx/core/vtk/ChannelDatavtkDataObject.h>
#include <latticefx/core/vtk/VTKStreamlineRenderer.h>
#include <latticefx/core/vtk/VTKStreamlineRTP.h>

#include <ves/xplorer/Debug.h>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

//////////////////////////////////////////////////////////////////////////////////
cfdStreamers::cfdStreamers()
    :
    cfdObjects(),
    streamTracer( vtkStreamTracer::New() ),
    tubeFilter( vtkTubeFilter::New() ),
    mapper( vtkPolyDataMapper::New() ),
    integ( vtkRungeKutta4::New() ),
    seedPoints( 0 ),
    points( 0 ),
    m_integrationDirection( 0 ),
    m_streamArrows( 0 ),
    m_streamRibbons( 0 ),
    m_propagationTime( -1 ),
    m_integrationStepLength( -1 ),
    m_lineDiameter( 1.0f ),
    m_arrowDiameter( 1 ),
    m_particleDiameter( 1.0f ),
    m_xValue( 4 ),
    m_yValue( 4 ),
    m_zValue( 4 ),
    m_xMinBB( 0 ),
    m_yMinBB( 0 ),
    m_zMinBB( 0 ),
    m_xMaxBB( 1 ),
    m_yMaxBB( 1 ),
    m_zMaxBB( 1 )
{
    ;
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
////////////////////////////////////////////////////////////////////////////////
cfdStreamers::cfdStreamers( cfdStreamers const& src )
    :
    cfdObjects( src ),
    streamTracer( vtkStreamTracer::New() ),
    tubeFilter( vtkTubeFilter::New() ),
    mapper( vtkPolyDataMapper::New() ),
    integ( vtkRungeKutta4::New() ),
    seedPoints( 0 ),
    points( 0 ),
    m_integrationDirection( 0 ),
    m_streamArrows( 0 ),
    m_streamRibbons( 0 ),
    m_propagationTime( -1 ),
    m_integrationStepLength( -1 ),
    m_lineDiameter( 1.0f ),
    m_arrowDiameter( 1 ),
    m_particleDiameter( 1.0f ),
    m_xValue( 4 ),
    m_yValue( 4 ),
    m_zValue( 4 ),
    m_xMinBB( 0 ),
    m_yMinBB( 0 ),
    m_zMinBB( 0 ),
    m_xMaxBB( 1 ),
    m_yMaxBB( 1 ),
    m_zMaxBB( 1 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
cfdObjects* cfdStreamers::CreateCopy()
{
    return new cfdStreamers( *this );
}
//////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::Update()
{
	CreateLFXPlane();
    this->updateFlag = true;
    return;

    if( seedPoints == NULL ) 
    {
        vprDEBUG( vesDBG, 0 ) << "|\tcfdStreamers::Update : No Cursor Type Selected"
                              << std::endl << vprDEBUG_FLUSH;

        return;
    }

    vprDEBUG( vesDBG, 0 ) << "|\tcfdStreamers::Update : "
                          << " Prop Time : " << m_propagationTime
                          << " Integration Step Length : " << m_integrationStepLength
                          << " Integration Direction : " << m_integrationDirection
                          << std::endl << vprDEBUG_FLUSH;

    //tubeFilter->SetRadius( lineDiameter );
    //tubeFilter->SetNumberOfSides( 3 );
    //tubeFilter->SidesShareVerticesOn();

    if( m_propagationTime == -1 )
    {
       m_propagationTime = 10.0f * GetActiveDataSet()->GetMaxTime();
    }

    if( m_integrationStepLength == -1 )
    {
        m_integrationStepLength = 0.050f;
    }

    vtkCellDataToPointData* c2p = vtkCellDataToPointData::New();
    c2p->SetInput( GetActiveDataSet()->GetDataSet() );

    streamTracer->SetInputConnection( c2p->GetOutputPort() );
    //overall length of streamline
    streamTracer->SetMaximumPropagation( m_propagationTime );

    // typically < 1
    streamTracer->SetMaximumIntegrationStep( m_integrationStepLength );

    if( m_integrationDirection == 0 )
    {
        streamTracer->SetIntegrationDirectionToBoth();
    }
    else if( m_integrationDirection == 1 )
    {
        streamTracer->SetIntegrationDirectionToForward();
    }
    else if( m_integrationDirection == 2 )
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
    streamTracer->SetComputeVorticity( true );
    streamTracer->SetInputArrayToProcess( 0, 0, 0,
                                          vtkDataObject::FIELD_ASSOCIATION_POINTS,
                                          GetActiveDataSet()->GetActiveVectorName().c_str() );

    vtkCleanPolyData* cleanPD = vtkCleanPolyData::New();
    cleanPD->PointMergingOn();
    cleanPD->SetInputConnection( streamTracer->GetOutputPort() );

    vtkRibbonFilter* ribbon = 0;

	// DON"T NEED TO WORRY ABOUT THIS FOR NOW.. 
    if( m_streamRibbons )
    {
        ribbon = vtkRibbonFilter::New();
        //ribbon->SetWidthFactor( arrowDiameter * 0.25);
        ribbon->SetWidth( m_arrowDiameter );
        ribbon->SetInputConnection( cleanPD->GetOutputPort() );
        ribbon->SetInputArrayToProcess( 0, 0, 0,
                                        vtkDataObject::FIELD_ASSOCIATION_POINTS,
                                        "Vorticity" );
    }


	// DON"T NEED TO WORRY ABOUT THIS FOR NOW
    if( m_streamArrows )
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
        cones->SetScaleFactor( m_arrowDiameter );
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
            ribbon = 0;
        }
        else
        {
            append->AddInput( cleanPD->GetOutput() );
        }

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
		// no need to worry about RIBBON
        if( ribbon )
        {
            mapper->SetInputConnection( ribbon->GetOutputPort() );
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
    }

    mapper->SetColorModeToMapScalars();
    mapper->SetLookupTable( GetActiveDataSet()->GetLookupTable() );
    mapper->ImmediateModeRenderingOn();
    mapper->SetScalarModeToUsePointFieldData();
    mapper->UseLookupTableScalarRangeOn();
    mapper->SelectColorArray( GetActiveDataSet()->
                              GetActiveScalarName().c_str() );

    if( !m_gpuTools )
    {
        vtkActor* temp = vtkActor::New();
        temp->SetMapper( mapper );
        temp->GetProperty()->SetSpecularPower( 20.0f );
        temp->GetProperty()->SetLineWidth( m_lineDiameter );
        //test to see if there is enough memory, if not, filters are deleted
        try
        {
            osg::ref_ptr< ves::xplorer::scenegraph::Geode > tempGeode =
                new ves::xplorer::scenegraph::Geode();
            tempGeode->TranslateToGeode( temp );
            //tempGeode->StreamLineToGeode( temp );

            geodes.push_back( tempGeode.get() );
            updateFlag = true;
        }
        catch( std::bad_alloc )
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
    }
    // When gpu rendering is on
    else
    {
        try
        {
            OSGStreamlineStage* tempStage = new OSGStreamlineStage();
            tempStage->SetParticleDiameter( int( m_particleDiameter ) );
            //This is a multiplier to create extra points using
            //linear interplation to smooth out the animation
            int mult = 10;

            osg::ref_ptr<ves::xplorer::scenegraph::Geode > tempGeode1 =
                tempStage->createInstanced( cleanPD->GetOutput(), mult,
                                            GetActiveDataSet()->GetActiveScalarName().c_str(),
                                            GetActiveDataSet()->GetActiveVectorName().c_str() );
            delete tempStage;

            size_t numdraw = tempGeode1->getNumDrawables();
            for( size_t k = 0; k < numdraw; ++k )
            {
                osg::ref_ptr< osg::Uniform > warpScaleUniform =
                    tempGeode1->getDrawable( k )->
                    getStateSet()->getUniform( "scalarMinMax" );
                double scalarRange[ 2 ] = {0, 0};
                GetActiveDataSet()->GetUserRange( scalarRange );
                osg::Vec2 opacityValVec;
                warpScaleUniform->get( opacityValVec );
                opacityValVec[ 0 ] = scalarRange[ 0 ];
                opacityValVec[ 1 ] = scalarRange[ 1 ];
                warpScaleUniform->set( opacityValVec );
            }
            geodes.push_back( tempGeode1.get() );
            updateFlag = true;
        }
        catch( std::bad_alloc )
        {
            streamTracer->Delete();
            streamTracer = vtkStreamTracer::New();
            mapper->Delete();
            mapper = vtkPolyDataMapper::New();
            vprDEBUG( vesDBG, 0 ) << "|\tMemory allocation failure : cfdStreamers "
                                  << std::endl << vprDEBUG_FLUSH;
        }
        c2p->Delete();
    }
    cleanPD->Delete();
    if( ribbon )
    {
        ribbon->Delete();
    }
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
    return m_arrowDiameter;
}
//////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::SetIntegrationDirection( int value )
{
    m_integrationDirection = value;
}
//////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::SetPropagationTime( double value )
{
    m_propagationTime = value * ( 100.0f * GetActiveDataSet()->GetMaxTime() / 20.0f );
}
//////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::SetIntegrationStepLength( int value )
{
    m_integrationStepLength = ( float )value * ( 0.050f ) / 50.0f;
}
//////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::UpdateCommand()
{
    UpdatePropertySet();
    return;

    //Call base method - currently does nothing
    cfdObjects::UpdateCommand();

    //Extract the specific commands from the overall command
    ves::open::xml::DataValuePairPtr activeModelDVP = veCommand->GetDataValuePair( "Sub-Dialog Settings" );
    ves::open::xml::CommandPtr objectCommand = boost::dynamic_pointer_cast<ves::open::xml::Command>( activeModelDVP->GetDataXMLObject() );

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
    m_streamArrows = opacity;

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
    m_lineDiameter = ( diameter + 110 ) * 0.005 *  20;
    m_particleDiameter = m_lineDiameter;

    vprDEBUG( vesDBG, 1 ) << "|\t\tNew Streamline Diameter : "
                          << m_lineDiameter << std::endl << vprDEBUG_FLUSH;

    /////////////////////
    activeModelDVP =
        objectCommand->GetDataValuePair( "Sphere/Arrow/Particle Size" );
    activeModelDVP->GetData( m_arrowDiameter );
    m_arrowDiameter = localLineDiameter * 60.0f * m_arrowDiameter;
    vprDEBUG( vesDBG, 1 ) << "|\t\tNew Arrow Diameter : "
                          << m_arrowDiameter << std::endl << vprDEBUG_FLUSH;

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
        activeModelDVP->GetData( m_xMaxBB );
        vprDEBUG( vesDBG, 0 ) << "|\t\txMaxBB : " << m_xMaxBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Max_Y_BB" );
        activeModelDVP->GetData( m_yMaxBB );
        vprDEBUG( vesDBG, 0 ) << "|\t\tyMaxBB : " << m_yMaxBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Max_Z_BB" );
        activeModelDVP->GetData( m_zMaxBB );
        vprDEBUG( vesDBG, 0 ) << "|\t\tzMaxBB : " << m_zMaxBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Min_X_BB" );
        activeModelDVP->GetData( m_xMinBB );
        vprDEBUG( vesDBG, 0 ) << "|\t\txMinBB : " << m_xMinBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Min_Y_BB" );
        activeModelDVP->GetData( m_yMinBB );
        vprDEBUG( vesDBG, 0 ) << "|\t\tyMinBB : " << m_yMinBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Min_Z_BB" );
        activeModelDVP->GetData( m_zMinBB );
        vprDEBUG( vesDBG, 0 ) << "|\t\tzMinBB : " << m_zMinBB << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Num_X_Points" );
        activeModelDVP->GetData( m_xValue );
        vprDEBUG( vesDBG, 0 ) << "|\t\tX Points : " << m_xValue << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Num_Y_Points" );
        activeModelDVP->GetData( m_yValue );
        vprDEBUG( vesDBG, 0 ) << "|\t\tY Points : " << m_yValue << std::endl << vprDEBUG_FLUSH;
        activeModelDVP = objectCommand->GetDataValuePair( "Num_Z_Points" );
        activeModelDVP->GetData( m_zValue );
        vprDEBUG( vesDBG, 0 ) << "|\t\tZ Points : " << m_zValue << std::endl << vprDEBUG_FLUSH;
    }

    /////////////////////
    activeModelDVP = objectCommand->GetDataValuePair( "GPU Tools" );
    if( activeModelDVP )
    {
        unsigned int gpuFlag;
        activeModelDVP->GetData( gpuFlag );
        m_gpuTools = gpuFlag;
    }
    else
    {
        m_gpuTools = false;
    }

    //Extract the surface flag
    activeModelDVP = objectCommand->GetDataValuePair( "SURF Tools" );
    bool hasSurface = false;
    if( activeModelDVP )
    {
        hasSurface = true;
        activeModelDVP->GetData( m_surfDataset );
    }
    /////////////////////
    if( !hasSurface )
    {
        CreateSeedPoints();
    }
    else
    {
        CreateArbSurface();
    }

}
//////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::CreateSeedPoints()
{
    double bounds[ 6 ];
    GetActiveDataSet()->GetBounds( bounds );

    double xDiff = bounds[ 1 ] - bounds[ 0 ];
    double yDiff = bounds[ 3 ] - bounds[ 2 ];
    double zDiff = bounds[ 5 ] - bounds[ 4 ];

    double xMin = bounds[ 0 ] + ( xDiff * m_xMinBB );
    double xMax = bounds[ 0 ] + ( xDiff * m_xMaxBB );
    double yMin = bounds[ 2 ] + ( yDiff * m_yMinBB );
    double yMax = bounds[ 2 ] + ( yDiff * m_yMaxBB );
    double zMin = bounds[ 4 ] + ( zDiff * m_zMinBB );
    double zMax = bounds[ 4 ] + ( zDiff * m_zMaxBB );

    double xLoc = 0;
    double yLoc = 0;
    double zLoc = 0;
    int number = 0;

    //insert evenly spaced points inside bounding box
    points = vtkPoints::New();

    double deltaX = ( m_xValue == 1 ) ? 0 : ( xMax - xMin ) / double( m_xValue - 1 );
    double deltaY = ( m_yValue == 1 ) ? 0 : ( yMax - yMin ) / double( m_yValue - 1 );
    double deltaZ = ( m_zValue == 1 ) ? 0 : ( zMax - zMin ) / double( m_zValue - 1 );

    for( unsigned int i = 0; i < m_xValue; ++i )
    {
        xLoc = xMin + ( i * deltaX );
        for( unsigned int j = 0; j < m_yValue; ++j )
        {
            yLoc = yMin + ( j * deltaY );
            for( unsigned int k = 0; k < m_zValue; ++k )
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
void cfdStreamers::CreateArbSurface()
{
    //First reset seed points so that if the polydata call fails we do not
    //pass along bad seed point information
    if( seedPoints )
    {
        seedPoints->Delete();
        seedPoints = 0;
    }

    //Need to set the active datasetname and get the position of the dataset
    Model* activeModel = ModelHandler::instance()->GetActiveModel();
    // set the dataset as the appropriate dastaset type
    // (and the active dataset as well)
    lfx::core::vtk::DataSetPtr surfDataset = activeModel->GetCfdDataSet(
                               activeModel->GetIndexOfDataSet( m_surfDataset ) );
    vtkPolyData* pd = surfDataset->GetPolyData();

    if( !pd )
    {
        std::cerr << "ERROR: Activate a polydata file to use this function"
                  << std::endl;
        return;
    }

    points = pd->GetPoints();

    seedPoints = vtkPolyData::New();
    seedPoints->SetPoints( points );
}
///////////////////////////////////////////////////////////////////////////
void cfdStreamers::UpdatePropertySet()
{
    //Extract the integration direction
    std::string intDirection = boost::any_cast<std::string >( m_propertySet->GetPropertyValue( "IntegrationDirection" ) );

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

    /////////////////////
    m_streamRibbons = boost::any_cast<bool>( m_propertySet->GetPropertyValue( "UseStreamRibbons" ) );
    vprDEBUG( vesDBG, 0 ) << "|\t\tUse Stream Ribbons\t" << m_streamRibbons
                          << std::endl << vprDEBUG_FLUSH;

    /////////////////////
    m_streamArrows = boost::any_cast<bool>( m_propertySet->GetPropertyValue( "UseStreamArrows" ) );
    vprDEBUG( vesDBG, 0 ) << "|\t\tSTREAMLINE_ARROW\t" << m_streamArrows
                          << std::endl << vprDEBUG_FLUSH;

    /////////////////////
    SetIntegrationStepLength( static_cast< int >( boost::any_cast<double>( m_propertySet->GetPropertyValue( "Advanced_IntegrationStepSize" ) ) ) );

    /////////////////////
    SetPropagationTime( boost::any_cast<double>( m_propertySet->GetPropertyValue( "Advanced_PropogationTime" ) ) );

    /////////////////////
    double streamDiamter = boost::any_cast<double>( m_propertySet->GetPropertyValue( "Advanced_Diameter" ) );
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
    m_lineDiameter = ( diameter + 110 ) * 0.005 *  20;
    m_particleDiameter = m_lineDiameter;

    vprDEBUG( vesDBG, 1 ) << "|\t\tNew Streamline Diameter : "
                          << m_lineDiameter << std::endl << vprDEBUG_FLUSH;

    /////////////////////
    m_arrowDiameter = boost::any_cast<double>( m_propertySet->GetPropertyValue( "Advanced_SphereArrowParticleSize" ) );
    m_arrowDiameter = localLineDiameter * 60.0f * m_arrowDiameter;
    vprDEBUG( vesDBG, 1 ) << "|\t\tNew Arrow Diameter : "
                          << m_arrowDiameter << std::endl << vprDEBUG_FLUSH;

    ////////////////////
    //Set the number of seed points in each direction and get the %BB info
    //Extract the advanced settings from the commands
    m_xMaxBB = boost::any_cast<double>( m_propertySet->GetPropertyValue( "SeedPoints_Bounds_XMax" ) );
    m_xMinBB = boost::any_cast<double>( m_propertySet->GetPropertyValue( "SeedPoints_Bounds_XMin" ) );

    m_yMaxBB = boost::any_cast<double>( m_propertySet->GetPropertyValue( "SeedPoints_Bounds_YMax" ) );
    m_yMinBB = boost::any_cast<double>( m_propertySet->GetPropertyValue( "SeedPoints_Bounds_YMin" ) );

    m_zMaxBB = boost::any_cast<double>( m_propertySet->GetPropertyValue( "SeedPoints_Bounds_ZMax" ) );
    m_zMinBB = boost::any_cast<double>( m_propertySet->GetPropertyValue( "SeedPoints_Bounds_ZMin" ) );

    m_xValue = boost::any_cast<int>( m_propertySet->GetPropertyValue( "SeedPoints_NumberOfPointsInX" ) );
    m_yValue = boost::any_cast<int>( m_propertySet->GetPropertyValue( "SeedPoints_NumberOfPointsInY" ) );
    m_zValue = boost::any_cast<int>( m_propertySet->GetPropertyValue( "SeedPoints_NumberOfPointsInZ" ) );

    /////////////////////
    m_gpuTools = boost::any_cast<bool>( m_propertySet->GetPropertyValue( "UseGPUTools" ) );

    //Extract the surface flag
    //activeModelDVP = objectCommand->GetDataValuePair( "SURF Tools" );
    bool hasSurface = false;
    /*if( activeModelDVP )
    {
        hasSurface = true;
    	activeModelDVP->GetData( m_surfDataset );
    }*/
    /////////////////////
    if( !hasSurface )
    {
        CreateSeedPoints();
    }
    else
    {
        CreateArbSurface();
    }
}
///////////////////////////////////////////////////////////////////////////
void cfdStreamers::CreateLFXPlane()
{
    m_dsp = lfx::core::DataSetPtr( new lfx::core::DataSet() );

	if( !GetActiveDataSet().get() )
	{
		vprDEBUG( vesDBG, 1 ) << "|\t\tNo active dataset : " << std::endl << vprDEBUG_FLUSH;
		return;
	}

	// let get the properties we need

	std::string vector = GetActiveDataSet()->GetActiveVectorName();
	std::string scalar = GetActiveDataSet()->GetActiveScalarName();

    lfx::core::vtk::ChannelDatavtkDataObjectPtr dobjPtr( new lfx::core::vtk::ChannelDatavtkDataObject( GetActiveDataSet()->GetDataSet(), "vtkDataObject" ) );
    m_dsp->addChannel( dobjPtr );

	std::vector<double> bounds;
	bounds.resize(6);
	GetActiveDataSet()->GetBounds(&bounds[0]);

	float maxTime = GetActiveDataSet()->GetMaxTime();
	lfx::core::vtk::VTKStreamlineRTPPtr rtp( new lfx::core::vtk::VTKStreamlineRTP() );
	rtp->setMaxTime( maxTime );
	rtp->SetActiveVector( vector );
    rtp->SetActiveScalar( scalar );
	rtp->setDatasetBounds( &bounds[0] );
	rtp->setSeedPtsBox( m_xMinBB, m_xMaxBB, m_yMinBB, m_yMaxBB, m_zMinBB, m_zMaxBB );
	rtp->setSeedPtsCount( m_xValue, m_yValue, m_zValue );
	rtp->setIntegrationDir( m_integrationDirection );
	rtp->setIntegrationStepLen( m_integrationStepLength );
	rtp->setStreamArrows( m_streamArrows );
	rtp->setStreamRibbons( m_streamRibbons );
	rtp->setPropagationTime( m_propagationTime );
	rtp->setLineDiameter( m_lineDiameter );
	rtp->setArrowDiameter( m_arrowDiameter );
	rtp->setParticleDiameter( m_particleDiameter );
    rtp->addInput( "vtkDataObject" );

    m_dsp->addOperation( rtp );

	lfx::core::vtk::VTKStreamlineRendererPtr renderOp( new lfx::core::vtk::VTKStreamlineRenderer() );
    renderOp->SetActiveVector( vector );
    renderOp->SetActiveScalar( scalar );
    renderOp->addInput( "vtkPolyData" );
    renderOp->addInput( "vtkDataObject" );
	renderOp->addInput( "positions" );
	//renderOp->setHardwareMaskOperator( lfx::core::Renderer::HM_OP_OFF );
	//renderOp->setAnimationEnable( false );
    m_dsp->setRenderer( renderOp );
	m_dsp->setDirty();
	bool success = m_dsp->updateAll();
    
    if( !success )
    {
		vprDEBUG( vesDBG, 1 ) << "|\t\tSome sort of problem with lfx : "
                          << std::endl << vprDEBUG_FLUSH;
    }

	Model* activeModel = ModelHandler::instance()->GetActiveModel();
	activeModel->SetVtkRenderSet( "str", m_dsp );
}
///////////////////////////////////////////////////////////////////////////
