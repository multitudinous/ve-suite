/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <ves/xplorer/event/viz/cfdIsosurface.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/Debug.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/event/data/DataSetScalarBar.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <vtkLookupTable.h>
#include <vtkPolyDataMapper.h>
#include <vtkDataSet.h>
#include <vtkContourFilter.h>
#include <vtkPolyDataNormals.h>
#include <vtkActor.h>
#include <vtkProperty.h>
#include <vtkPointData.h>
#include <vtkDataArray.h>
#include <vtkPolyData.h>
#include <vtkCellDataToPointData.h>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

cfdIsosurface::cfdIsosurface( int numsteps )
{
    this->totalId = numsteps;
    this->value = 0.0f;

#ifdef USE_OMP
    this->append = vtkAppendPolyData::New( );
    this->nData = this->GetActiveDataSet()->GetNoOfDataForProcs( );

    for( int i = 0; i < this->nData; i++ )
    {
        this->contour[i] = vtkContourFilter::New( );
        //this->contour[i]->UseScalarTreeOff( );
        this->contour[i]->UseScalarTreeOn();

        this->normals[i] = vtkPolyDataNormals::New( );
        this->normals[i]->SetInput( this->contour[i]->GetOutput( ) );

        this->append->AddInput( this->normals[i]->GetOutput( ) );
    }
#else
    this->normals = vtkPolyDataNormals::New();
#endif


#ifdef USE_OMP
    this->filter->SetInput(( vtkDataSet * )this->append->GetOutput() );
#endif

    this->mapper = vtkPolyDataMapper::New();
}

cfdIsosurface::~cfdIsosurface()
{
#ifdef USE_OMP
    for( int i = 0; this->nData; i++ )
    {
        this->contour[i]->Delete();
        this->normals[i]->Delete();
    }
    this->append->Delete();
#else
    this->normals->Delete();
#endif
    this->mapper->Delete();
}

void cfdIsosurface::Update()
{
    //SetActiveVtkPipeline();
    vprDEBUG( vesDBG, 1 ) << "|\tcfdIsosurface::Update: FileName: "
    << this->GetActiveDataSet()->GetFileName() << std::endl << vprDEBUG_FLUSH;

    vprDEBUG( vesDBG, 1 ) << "|\trequestedValue: " << this->requestedValue
    << std::endl << vprDEBUG_FLUSH;

    // convert the requested value percentage (0-100) to a scalar value
    this->value = convertPercentage( this->requestedValue );

    vprDEBUG( vesDBG, 1 ) << "|\tthis->value: " << this->value
    << std::endl << vprDEBUG_FLUSH;

#ifdef USE_OMP
    int imax = this->nData;
    int i;
# pragma omp parallel for private(i)
    for( i = 0; i < imax; i++ )
    {
        this->contour[i]->SetInput( this->GetActiveDataSet()->GetData( i ) );
        this->contour[i]->SetValue( 0, this->value );
        this->normals[i]->Update();
    }
    this->append->Update( );
#else
    vtkCellDataToPointData* c2p = vtkCellDataToPointData::New(); 
    c2p->SetInput( GetActiveDataSet()->GetDataSet() );
    c2p->Update();
    
    vtkContourFilter* contourFilter = vtkContourFilter::New();
    contourFilter->UseScalarTreeOn();
    contourFilter->SetInputConnection( 0, c2p->GetOutputPort( 0 ) );
    contourFilter->SetValue( 0, this->value );
    contourFilter->ComputeNormalsOff();
    contourFilter->SetInputArrayToProcess( 0, 0, 0,
        vtkDataObject::FIELD_ASSOCIATION_POINTS, 
        GetActiveDataSet()->GetActiveScalarName().c_str() );
    contourFilter->Update();

    vtkAlgorithmOutput* polydata = 
        ApplyGeometryFilterNew( contourFilter->GetOutputPort( 0 ) );

    normals->SetInputConnection( polydata );

    mapper->SetInputConnection( normals->GetOutputPort() );
    mapper->SetScalarModeToUsePointFieldData();
    mapper->UseLookupTableScalarRangeOn();
    mapper->SelectColorArray( colorByScalar.c_str() );
    //this->mapper->SetColorModeToMapScalars();
#endif

    vtkLookupTable* lut = vtkLookupTable::New();
    lut->SetNumberOfColors( 256 );            //default is 256
    lut->SetHueRange( 2.0f / 3.0f, 0.0f );    //a blue-to-red scale
    lut->SetTableRange( minValue, maxValue );
    lut->Build();

    mapper->SetLookupTable( lut );
    mapper->Update();

    vtkActor* temp = vtkActor::New();
    temp->SetMapper( this->mapper );
    temp->GetProperty()->SetSpecularPower( 20.0f );
    geodes.push_back( new ves::xplorer::scenegraph::Geode() );
    geodes.back()->TranslateToGeode( temp );
    temp->Delete();
    lut->Delete();
    c2p->Delete();
    contourFilter->Delete();
    this->updateFlag = true;
}
////////////////////////////////////////////////////////////////////////////////
double cfdIsosurface::GetValue()
{
    return this->value;
}
////////////////////////////////////////////////////////////////////////////////
double cfdIsosurface::convertPercentage( const int percentage )
{
    // set the step-size for isosurface based on the "pretty" range
    double minmax[2];
    this->GetActiveDataSet()->GetUserRange( minmax );

    vprDEBUG( vesDBG, 2 ) << "|\tMinmax = " << minmax[0] << "\t" << minmax[1]
        << std::endl << vprDEBUG_FLUSH;

    double minmaxDiff = minmax[1] - minmax[0];
    double dx = ( minmaxDiff ) / ( double )this->totalId;

    if( percentage == 999 ) // happens only with the blue menu
    {
        this->value += dx;

        // if way over the limit, reset close to bottom of range
        // (but true bottom will cause error)
        if( this->value > minmax[1] + 0.5 * dx )
        {
            this->value = minmax[0] + minmaxDiff / 100.0;
        }

        // if just over the limit, reset close to end of range
        else if( this->value > ( minmax[1] - minmaxDiff / 100.0 ) )
        {
            this->value = minmax[1] - minmaxDiff / 100.0;
        }
    }
    else
    {
        // The java app slider bar returns integers 0-100 representing percentile.
        this->value = minmax[0] + minmaxDiff * percentage / 100.0;

        // if too low error will occur, so reset close to bottom of range
        if( this->value < ( minmax[0] + minmaxDiff / 100.0 ) )
        {
            this->value = minmax[0] + minmaxDiff / 100.0;
        }

        // if over the limit, reset close to end of range
        if( this->value > ( minmax[1] - minmaxDiff / 100.0 ) )
        {
            this->value = minmax[1] - minmaxDiff / 100.0;
        }
    }
    return this->value;
}
///////////////////////////////////////////////////////////////////////////
void cfdIsosurface::UpdateCommand()
{
    //Call base method - currently does nothing
    cfdObjects::UpdateCommand();

    //Extract the specific commands from the overall command
    ves::open::xml::DataValuePairPtr activeModelDVP = 
        veCommand->GetDataValuePair( "Sub-Dialog Settings" );
    ves::open::xml::CommandPtr objectCommand = 
        boost::dynamic_pointer_cast<ves::open::xml::Command>(
        activeModelDVP->GetDataXMLObject() );

    //Extract the isosurface value
    activeModelDVP = objectCommand->GetDataValuePair( "Iso-Surface Value" );
    double planePosition;
    activeModelDVP->GetData( planePosition );
    SetRequestedValue( static_cast< int >( planePosition ) );

    activeModelDVP = objectCommand->GetDataValuePair( "Color By Scalar" );
    activeModelDVP->GetData( colorByScalar );

    activeModelDVP = objectCommand->GetDataValuePair( "Minimum Scalar Value" );
    activeModelDVP->GetData( minValue );

    activeModelDVP = objectCommand->GetDataValuePair( "Maximum Scalar Value" );
    activeModelDVP->GetData( maxValue );

    DataSet* dataSet = ModelHandler::instance()->GetActiveModel()->GetActiveDataSet();
    if( !colorByScalar.empty() )
    {
        unsigned int activeTempScalar = dataSet->GetActiveScalar();
        dataSet->SetActiveScalar( colorByScalar );
        DataSetScalarBar* scalarBar = dataSet->GetDataSetScalarBar();
        if( scalarBar )
        {
            scalarBar->AddScalarBarToGroup();
        }
        dataSet->SetActiveScalar( activeTempScalar );
    }
}
