/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#include "VE_Xplorer/XplorerHandlers/cfdObjects.h"
#include "VE_Xplorer/XplorerHandlers/cfdReadParam.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/XplorerHandlers/cfdCommandArray.h"

// Juggler Includes
#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#include <vpr/Sync/Guard.h>

// VTK Includes
#include <vtkPolyData.h>
#include <vtkActor.h>

using namespace VE_SceneGraph;
using namespace VE_Xplorer;

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
   //this->actor = NULL;
   //this->PDactor = NULL;
   //this->addTransientGeode = 0;
}

cfdObjects::cfdObjects( const cfdObjects& src)
:cfdGlobalBase(src)
{
   this->objectType = src.objectType;
   this->pointSource = src.pointSource;
}

cfdObjects::~cfdObjects( void )
{
   //pfDelete( this->geode );
}

void cfdObjects::SetObjectType( int type )
{
   this->objectType = type;
}

std::vector< osg::ref_ptr< VE_SceneGraph::Geode > > cfdObjects::GetGeodes( void )
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
   /*if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_PARTICLE_VIEW_OPTION )
   {
      cfdObjects::SetParticleOption( 
                               commandArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE ) );

      vprDEBUG(vesDBG,0) << " CHANGE_PARTICLE_VIEW_OPTION, value = " 
         << commandArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE )
         << std::endl << vprDEBUG_FLUSH;

      //return true;
   //}
   //else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SPHERE_SIZE )
   //{
      vprDEBUG(vesDBG,0) << " CHANGE_SPHERE_SIZE, value = " 
         << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << std::endl << vprDEBUG_FLUSH;

      cfdObjects::SetParticleScale( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );

      return true;
   }*/
   return false;
}

void cfdObjects::UpdateCommand()
{
   ;
}

cfdDataSet* cfdObjects::GetActiveDataSet()
{
   return activeDataSet;
}

void cfdObjects::SetActiveDataSet( cfdDataSet* dataset )
{
   /*vprDEBUG(vesDBG, 4) 
      << "cfdObjects::SetActiveDataSet: " << dataset
      << std::endl << vprDEBUG_FLUSH;*/

   activeDataSet = dataset;
}
