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
#include <ves/xplorer/event/viz/cfdObjects.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/open/xml/Command.h>

// Juggler Includes
#include <ves/xplorer/Debug.h>

// VTK Includes
#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkCompositeDataGeometryFilter.h>
#include <vtkGeometryFilter.h>
#include <vtkDataSetSurfaceFilter.h>
#include <vtkAlgorithmOutput.h>
#include <vtkPointData.h>
#include <vtkDoubleArray.h>

using namespace ves::xplorer::scenegraph;
using namespace ves::xplorer;

////////////////////////////////////////////////////////////////////////////////
cfdObjects::cfdObjects( void )
    :
    GlobalBase(),
    activeDataSet( 0 ),
    pointSource( 0 ),
    m_multiGroupGeomFilter( vtkCompositeDataGeometryFilter::New() ),
    m_geometryFilter( vtkGeometryFilter::New() ),
    updateFlag( false ),
    vtkToPFDebug( 0 ),
    objectType( 0 ),
    requestedValue( 0 ),
    cursorType( 0 ),
    usePreCalcData( false ),
    m_gpuTools( false )
{
    m_surfaceFilter = vtkDataSetSurfaceFilter::New();
    for( size_t i = 0; i < 3; ++i )
    {
        origin[ i ] = 0;
        center[ i ] = 0;
        normal[ i ] = 0;
    }
    
    for( size_t i = 0; i < 6; ++i )
    {
        box_size[ i ] = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
cfdObjects::cfdObjects( const cfdObjects& src )
    : 
    GlobalBase( src ),
    activeDataSet( 0 ),
    pointSource( src.pointSource ),
    m_multiGroupGeomFilter( vtkCompositeDataGeometryFilter::New() ),
    m_geometryFilter( vtkGeometryFilter::New() ),
    updateFlag( false ),
    vtkToPFDebug( 0 ),
    objectType( src.objectType ),
    requestedValue( src.requestedValue ),
    cursorType( src.cursorType ),
    usePreCalcData( false ),
    m_gpuTools( false )
{
    m_surfaceFilter = vtkDataSetSurfaceFilter::New();
    for( size_t i = 0; i < 3; ++i )
    {
        origin[ i ] = 0;
        center[ i ] = 0;
        normal[ i ] = 0;
    }
    
    for( size_t i = 0; i < 6; ++i )
    {
        box_size[ i ] = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
cfdObjects::~cfdObjects()
{
    m_multiGroupGeomFilter->Delete();
    m_geometryFilter->Delete();
}
////////////////////////////////////////////////////////////////////////////////
void cfdObjects::SetObjectType( int type )
{
    this->objectType = type;
}
////////////////////////////////////////////////////////////////////////////////
std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > cfdObjects::GetGeodes( void )
{
    return geodes;
}
////////////////////////////////////////////////////////////////////////////////
void cfdObjects::ClearGeodes( void )
{
    geodes.clear();
}
////////////////////////////////////////////////////////////////////////////////
void cfdObjects::SetOrigin( float o[ 3 ] )
{
    for( int i = 0; i < 3; i++ )
    {
        this->origin[ i ] = o[ i ];
    }
}
////////////////////////////////////////////////////////////////////////////////
double * cfdObjects::GetOrigin()
{
    return this->origin;
}
////////////////////////////////////////////////////////////////////////////////
void cfdObjects::GetOrigin( double o[ 3 ] )
{
    for( int i = 0; i < 3; i++ )
    {
        o[ i ] = this->origin[ i ];
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdObjects::SetNormal( double n[ 3 ] )
{
    for( int i = 0; i < 3; i++ )
    {
        this->normal[ i ] = n[ i ];
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdObjects::SetBoxSize( double b[ 6 ] )
{
    for( int i = 0; i < 6; i++ )
    {
        this->box_size[ i ] = b[ i ];
    }

    this->center[0] = ( this->box_size[0] + this->box_size[1] ) / 2;
    this->center[1] = ( this->box_size[2] + this->box_size[3] ) / 2;
    this->center[2] = ( this->box_size[4] + this->box_size[5] ) / 2;
}
////////////////////////////////////////////////////////////////////////////////
void cfdObjects::SetSourcePoints( vtkPolyData* pointSource )
{
    this->pointSource = pointSource;
}
////////////////////////////////////////////////////////////////////////////////
void cfdObjects::UpdateCommand()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void cfdObjects::UpdateActors()
{}
////////////////////////////////////////////////////////////////////////////////
vtkAlgorithmOutput* cfdObjects::ApplyGeometryFilterNew( vtkAlgorithmOutput* input )
{
    if( this->activeDataSet->GetDataSet()->IsA( "vtkCompositeDataSet" ) )
    {
        m_multiGroupGeomFilter->SetInputConnection( input );
        return m_multiGroupGeomFilter->GetOutputPort(0);
    }
    else
    {
        //m_geometryFilter->SetInputConnection( input );
        //return m_geometryFilter->GetOutputPort();
        m_surfaceFilter->SetInputConnection( input );
        return m_surfaceFilter->GetOutputPort();
    }
}
////////////////////////////////////////////////////////////////////////////////
DataSet* cfdObjects::GetActiveDataSet()
{
    return activeDataSet;
}
////////////////////////////////////////////////////////////////////////////////
void cfdObjects::SetActiveDataSet( DataSet* dataset )
{
    /*vprDEBUG(vesDBG, 4)
       << "cfdObjects::SetActiveDataSet: " << dataset
       << std::endl << vprDEBUG_FLUSH;*/

    activeDataSet = dataset;
}
////////////////////////////////////////////////////////////////////////////////
bool cfdObjects::IsGPUTools()
{
    return m_gpuTools;
}
////////////////////////////////////////////////////////////////////////////////
vtkPolyData* cfdObjects::ComputeVolumeFlux( vtkPolyData* inputPD )
{
    vtkPolyData* normalsOutputPD = inputPD;
    
    vtkDataArray* normalsArray = 
        normalsOutputPD->GetPointData()->GetNormals();
    vtkIdType n_points = normalsOutputPD->GetNumberOfPoints();
	
    vtkDoubleArray* vol_flux_array = vtkDoubleArray::New();
    vol_flux_array->SetNumberOfTuples(n_points);
    vol_flux_array->SetName("VolumeFlux");
	
    normalsOutputPD->Update();
	
    vtkPointData* pointData = normalsOutputPD->GetPointData();
    if( pointData == NULL )
    {
        std::cout << " pd point data is null " << std::endl;
        return 0;
    }

    vtkDataArray* vectorArray = 
        pointData->GetVectors( 
        GetActiveDataSet()->GetActiveVectorName().c_str() );
	
    if( vectorArray == NULL )
    {
        std::cout << " vectors are null " << std::endl;
        return 0;
    }
	
    if( normalsArray == NULL )
    {
        std::cout << " normals are null " << std::endl;
        return 0;
    }
	
    double normalVec[3], vectorVec[3], volume_flux;
	
    for( vtkIdType i = 0; i < n_points; ++i )
    {
        vectorArray->GetTuple( i, vectorVec );
        normalsArray->GetTuple( i, normalVec );
        volume_flux = 
            vectorVec[0]*normalVec[0]+
            vectorVec[1]*normalVec[1]+
            vectorVec[2]*normalVec[2];
        vol_flux_array->SetTuple1(i, volume_flux);
    }
    
    normalsOutputPD->GetPointData()->AddArray( vol_flux_array );
    vol_flux_array->Delete();
    
    return normalsOutputPD;
}
////////////////////////////////////////////////////////////////////////////////
void cfdObjects::SetUUID( std::string const& uuid )
{
    m_uuid = uuid;
}
////////////////////////////////////////////////////////////////////////////////
std::string const& cfdObjects::GetUUID() const
{
    return m_uuid;
}
////////////////////////////////////////////////////////////////////////////////
void cfdObjects::SetPropertySet( ves::xplorer::data::PropertySetPtr set )
{
    m_propertySet = set;
}
////////////////////////////////////////////////////////////////////////////////
void cfdObjects::SetDataMapSurfaceName( std::string const& surfName )
{
    m_surfDataset = surfName;
}
////////////////////////////////////////////////////////////////////////////////
