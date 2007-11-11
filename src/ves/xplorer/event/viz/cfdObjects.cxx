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
#include <ves/xplorer/event/viz/cfdObjects.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/cfdCommandArray.h>

// Juggler Includes
#include <ves/xplorer/cfdDebug.h>

// VTK Includes
#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkMultiGroupDataGeometryFilter.h>
#include <vtkGeometryFilter.h>
#include <vtkAlgorithmOutput.h>
#include <vtkCompositeDataPipeline.h>
#include <vtkDemandDrivenPipeline.h>

using namespace ves::xplorer::scenegraph;
using namespace ves::xplorer;

cfdObjects::cfdObjects( void )
{
   vprDEBUG(vesDBG, 1) << " New cfdObjects ! " 
                           << std::endl << vprDEBUG_FLUSH;
   //this->_geode = NULL;
   //this->_sequence = NULL;
   this->pointSource = NULL;
   this->vtkToPFDebug = 0;
   this->usePreCalcData = false;
   activeDataSet = 0;
   m_multiGroupGeomFilter = vtkMultiGroupDataGeometryFilter::New();
   m_geometryFilter = vtkGeometryFilter::New();

   //this->actor = NULL;
   //this->PDactor = NULL;
   //this->addTransientGeode = 0;
}

cfdObjects::cfdObjects( const cfdObjects& src)
:GlobalBase(src)
{
   this->objectType = src.objectType;
   this->pointSource = src.pointSource;
}

cfdObjects::~cfdObjects( void )
{
    m_multiGroupGeomFilter->Delete();
    m_geometryFilter->Delete();
}

void cfdObjects::SetObjectType( int type )
{
   this->objectType = type;
}

std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > cfdObjects::GetGeodes( void )
{
   return geodes;
}

void cfdObjects::ClearGeodes( void )
{
	/*
   for ( unsigned int i = 0; i < geodes.size(); ++i )
   {
      delete geodes.at( i );
   }
	*/

   geodes.clear();
}

void cfdObjects::SetOrigin( float o[ 3 ] )
{
   for ( int i = 0; i < 3; i++ )
   {
      this->origin[ i ] = o[ i ];
   }
}

double * cfdObjects::GetOrigin()
{
   return this->origin;
}

void cfdObjects::GetOrigin( double o[ 3 ] )
{
   for ( int i = 0; i < 3; i++ )
   {
      o[ i ] = this->origin[ i ];
   }
}

void cfdObjects::SetNormal( double n[ 3 ]  )
{
   for ( int i = 0; i < 3; i++ )
   {
      this->normal[ i ] = n[ i ];
   }
}

void cfdObjects::SetBoxSize( double b[ 6 ]  )
{
   for ( int i = 0; i < 6; i++ )
   {
      this->box_size[ i ] = b[ i ];
   }
   
   this->center[0] = (this->box_size[0] + this->box_size[1])/2;
   this->center[1] = (this->box_size[2] + this->box_size[3])/2;
   this->center[2] = (this->box_size[4] + this->box_size[5])/2;
}

void cfdObjects::SetSourcePoints( vtkPolyData* pointSource )
{
   this->pointSource = pointSource;
}

// THIS CODE SHOULD BE REMOVED
// THIS DOESN'T BELONG IN THIS CLASS
// Fix this
bool cfdObjects::CheckCommandId( cfdCommandArray* commandArray )
{
   return false;
}

void cfdObjects::UpdateCommand()
{
   ;
}
///////////////////////////////////////
void cfdObjects::SetActiveVtkPipeline()
{
	if(this->activeDataSet->GetDataSet()->IsA("vtkMultiGroupDataSet"))
    {
        // we have to use a compsite pipeline
        vtkCompositeDataPipeline* prototype = vtkCompositeDataPipeline::New();
        vtkAlgorithm::SetDefaultExecutivePrototype(prototype);
        prototype->Delete();
	}
	else
	{   
        vtkAlgorithm::SetDefaultExecutivePrototype(0);
	}
}
///////////////////////////////
void cfdObjects::UpdateActors()
{

}
/////////////////////////////////////////////////////////////////////////
vtkPolyData* cfdObjects::ApplyGeometryFilter(vtkAlgorithmOutput* input)
{
	if(this->activeDataSet->GetDataSet()->IsA("vtkMultiGroupDataSet"))
    {
        m_multiGroupGeomFilter->SetInputConnection(input);
		return m_multiGroupGeomFilter->GetOutput();
	}
	else
	{
        m_geometryFilter->SetInputConnection(input);
		return m_geometryFilter->GetOutput();
	}
}
/////////////////////////////////////////////
DataSet cfdObjects::GetActiveDataSet()
{
   return activeDataSet;
}

void cfdObjects::SetActiveDataSet( DataSet dataset )
{
   /*vprDEBUG(vesDBG, 4) 
      << "cfdObjects::SetActiveDataSet: " << dataset
      << std::endl << vprDEBUG_FLUSH;*/

   activeDataSet = dataset;
}
