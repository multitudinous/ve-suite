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
 * File:          $RCSfile: cfdObjects.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdObjects.h"
#include "cfdReadParam.h"
#include "cfdDataSet.h"
#include "cfdGeode.h"
#include "cfdDCS.h"
#include "cfdEnum.h"
#include "cfdTempAnimation.h"
#include "cfdTransientInfo.h"
#include "cfdSequence.h"
#include "cfdCommandArray.h"

// Juggler Includes
#include <vpr/Util/Debug.h>
#include <vpr/Sync/Guard.h>

// VTK Includes
#include <vtkPolyData.h>
#include <vtkPolyDataSource.h>
#include <vtkActor.h>

 // C++ Libs
#include <vector>

cfdObjects::cfdObjects( cfdGeode* temp, int type )
{
   SetcfdGeode( temp );
   SetObjectType( type );
   this->pointSource = NULL;
   this->vtkToPFDebug = 0;
   this->usePreCalcData = false;
   this->actor = NULL;
   this->PDactor = NULL;

   this->activeDataSet = NULL;
}

cfdObjects::cfdObjects( void )
{
   vprDEBUG(vprDBG_ALL, 1) << " New cfdObjects ! " 
                           << std::endl << vprDEBUG_FLUSH;
   this->_geode = NULL;
   this->_sequence = NULL;
   this->pointSource = NULL;
   this->vtkToPFDebug = 0;
   this->usePreCalcData = false;
   this->actor = NULL;
   this->PDactor = NULL;
}

cfdObjects::cfdObjects( const cfdObjects& src)
{
   this->objectType = src.objectType;
   this->pointSource = src.pointSource;
}

cfdObjects::~cfdObjects( void )
{
   //pfDelete( this->geode );
}

void cfdObjects::SetcfdGeode( cfdGeode* temp )
{
   //this->geode = temp;
   exit( 1 );
}

void cfdObjects::SetObjectType( int type )
{
   this->objectType = type;
}

cfdGeode* cfdObjects::GetcfdGeode( void )
{ 
   return this->_geode;
}

/*
void cfdObjects::GetGeoSet( pfGeoSet *gset[] )
{
   for ( int i = 0; i < 4; i++ )
      gset[ i ] = geosets[ i ];
}
*/

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

void cfdObjects::DeletecfdGeode( void )
{
   delete this->_geode;
}

void cfdObjects::SetSequence( cfdTempAnimation * x )
{
   this->_sequence = x;
}

cfdTempAnimation* cfdObjects::GetSequence( void )
{
   return this->_sequence;
}

void cfdObjects::SetSourcePoints( vtkPolyDataSource* pointSource )
{
   this->pointSource = pointSource;
}

// This function just creates a geode from the actor for a particular
// visualization feature. It is not responsible for adding the 
// newly created geode to the scene graph. 
void cfdObjects::UpdatecfdGeode( void )
{
   vprDEBUG(vprDBG_ALL, 1) << "cfdObjects::UpdateGeode..."
                           << std::endl << vprDEBUG_FLUSH;
   

   if ( this->updateFlag )
   {
      vprDEBUG(vprDBG_ALL, 1) << "cfdObjects::Allocate Geode..."
                           << updateFlag<< std::endl << vprDEBUG_FLUSH;
   
      this->_geode = new cfdGeode();
      this->addGeode = true;      
   }
   else
   {
      vprDEBUG(vprDBG_ALL, 0) 
         << "cfdObjects::UpdateGeode... updateFlag == false for ObjectType = "
         << this->objectType << std::endl << vprDEBUG_FLUSH;
   }
}

void cfdObjects::CreatecfdGeode( void )
{
   // used by cfdAnimatedStreamlineCone
   this->_geodes.push_back( new cfdGeode );
 
   // Function implements respective vtkActorToGeode function
   ((cfdGeode*)this->_geodes.back())->TranslateTocfdGeode( this->actor );
}

// This function simply adds the created geode from function UpdateGeode
void cfdObjects::AddcfdGeodeToDCS( void )
{
   vprDEBUG(vprDBG_ALL, 1) << "cfdObjects::AddGeodeToDCS"
      << std::endl << vprDEBUG_FLUSH;
  
   vprDEBUG(vprDBG_ALL, 2) << "cfdObjects::UpdateGeode... updateFlag == true"
                           << std::endl << vprDEBUG_FLUSH;

   this->_geodes.push_back( this->_geode );

   vprDEBUG(vprDBG_ALL, 2) << "cfdObjects::UpdateGeode... pushback new geode"
                           << std::endl << vprDEBUG_FLUSH;

   if ( this->usePreCalcData && this->PDactor != NULL )
   {
      vprDEBUG(vprDBG_ALL, 2) 
         << "cfdObjects::UpdateGeode... create geode from PDactor"
         << std::endl << vprDEBUG_FLUSH;

      // Function implements respective vtkActorToGeode function
      ((cfdGeode*)this->_geodes.back())->TranslateTocfdGeode( this->PDactor );
   }
   else
   {
      vprDEBUG(vprDBG_ALL, 2) 
         << "cfdObjects::UpdateGeode... create geode from actor"
         << std::endl << vprDEBUG_FLUSH;

      // Function implements respective vtkActorToGeode function
      ((cfdGeode*)this->_geodes.back())->TranslateTocfdGeode( this->actor );
   }

   vprDEBUG(vprDBG_ALL, 2) 
      << "cfdObjects::UpdateGeode... set active geode pointer "
      << this->GetActiveDataSet()->IsNewlyActivated() << " : " << this->_geodes.size() << std::endl << vprDEBUG_FLUSH;
   //this->geode = (pfGeode *)this->geodes.back();
   //this->geode = tempGeode;
   //this->updateFlag = true;

   if ( this->GetActiveDataSet()->IsNewlyActivated() )
   {
      // add new with old
      this->GetActiveDataSet()->SetNotNewlyActivated();
      // geodes.size is not zero based therefore the -1 is needed
      int num = this->_geodes.size() - 1;
      vprDEBUG(vprDBG_ALL,1) << " adding child num = " << num
                             << std::endl << vprDEBUG_FLUSH;
      vprDEBUG(vprDBG_ALL,1) << "this->geodes[ 0 ] = " << this->_geodes[ 0 ]
         << std::endl << "this->geodes[ num ] = " << this->_geodes[ num ]
         << std::endl << vprDEBUG_FLUSH;
      this->_dcs->AddChild( ((cfdGeode*)this->_geodes[ num ]) );

      if ( this->_geodes.size() > 2 ) // remove oldest
      {
         int num = (this->_geodes.size() - 1) - 2;
         cfdSceneNode* parent = (cfdSceneNode*)((cfdGeode*)this->_geodes[ num ])->GetParent(0);
         ((cfdDCS*)parent)->RemoveChild( ((cfdGeode*)this->_geodes[ num ]) );
         delete this->_geodes[ num ];
         this->_geodes.erase( this->_geodes.end() - 3 );
      }
   }
   else if ( this->_geodes.size() > 1 ) // replace old with new
   {
      // geodes.size is not zero based therefore the first -1 is needed
      // the second -1 is to get the second to last geode on the list
      int num = (this->_geodes.size() - 1) - 1;
      vprDEBUG(vprDBG_ALL,1) << " removing child num = " << num << " : " << this->_geodes[ num ]
                             << std::endl << vprDEBUG_FLUSH;
      cfdSceneNode* parent = (cfdSceneNode*)((cfdGeode*)this->_geodes[ num ])->GetParent(0);
      ((cfdDCS*)parent)->RemoveChild( ((cfdGeode*)this->_geodes[ num ]) );
      delete this->_geodes[ num ];

      this->_geodes.erase( this->_geodes.end() - 2 );
      vprDEBUG(vprDBG_ALL,1) << " removing child num = " << num << " : " << this->_geodes[ num ]
                             << std::endl << vprDEBUG_FLUSH;
      this->_dcs->AddChild( ((cfdGeode*)this->_geodes[ num ]) );
   }
   else //if ( this->geodes.size() == 1 )
   { 
      vprDEBUG(vprDBG_ALL,1) << " adding child geode = " << this->_geodes.at( 0 )
                             << std::endl << vprDEBUG_FLUSH;
      this->_dcs->AddChild( ((cfdGeode*)this->_geodes.at( 0 )) );     
   }
}

void cfdObjects::RemovecfdGeodeFromDCS( void )
{
   int i, num;
     
   num = this->_geodes.size();
  
   // Iterate backwards for performance
   for ( i = num - 1; i >= 0; i-- )
   {
      // Need to find tha parent becuase with multiple models
      // Not all geodes are going to be on the same dcs
      cfdSceneNode* parent = this->_geodes[ i ]->GetParent(0);
      ((cfdDCS*)parent)->RemoveChild( ((cfdGeode*)this->_geodes[ i ]) );
      delete this->_geodes[ i ];
   }
   this->_geodes.clear();
}

void cfdObjects::SetDCS( cfdDCS *dcs )
{
   this->_dcs = dcs;
}

cfdDCS* cfdObjects::GetDCS( void )
{
   return this->_dcs;
}

//void cfdObjects::SetcfdReadParam( cfdReadParam *param )
//{
//   this->paramFile = param;   
//}

// Function probably shouldn't be here
// Fix this
void cfdObjects::AddSequenceToTree( void )
{
   if ( this->_dcs->SearchChild( this->_sequence->GetSequence() ) < 0 )
   {
      if ( this->objectType == ANIMATED_IMAGES )
      {
         if ( this->_dcs->SearchChild( this->_sequence->GetSequence()->GetParent(0) ) < 0 )
         {         
            this->_dcs->AddChild( this->_sequence->GetSequence()->GetParent(0) );
         }
         else
         {
            // Already added ( this is a hack until cfDModel is inmplemented )
         }
      }
      else
      {
         this->_dcs->AddChild( this->_sequence->GetSequence() );
      }
   }
}

void cfdObjects::SetGeodeFlag( bool x ) 
{ 
   this->addGeode = x; 
}

bool cfdObjects::GetGeodeFlag( void ) 
{ 
   return this->addGeode; 
}

// THIS CODE SHOULD BE REMOVED
// THIS DOESN'T BELONG IN THIS CLASS
// Fix this
bool cfdObjects::CheckCommandId( cfdCommandArray* commandArray )
{
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_PARTICLE_VIEW_OPTION )
   {
      cfdObjects::SetParticleOption( 
                               commandArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE ) );

      vprDEBUG(vprDBG_ALL,0) << " CHANGE_PARTICLE_VIEW_OPTION, value = " 
         << commandArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE )
         << std::endl << vprDEBUG_FLUSH;

      //return true;
   //}
   //else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SPHERE_SIZE )
   //{
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_SPHERE_SIZE, value = " 
         << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << std::endl << vprDEBUG_FLUSH;

      cfdObjects::SetParticleScale( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );

      return true;
   }
   return false;
}

void cfdObjects::UpdateCommand()
{
   cerr << "doing nothing in cfdObjects::UpdateCommand()" << endl;
}

cfdDataSet* cfdObjects::GetActiveDataSet()
{
   return activeDataSet;
}

void cfdObjects::SetActiveDataSet( cfdDataSet * dataset )
{
   vprDEBUG(vprDBG_ALL, 1) 
      << "cfdObjects::SetActiveDataSet: " << dataset
      << std::endl << vprDEBUG_FLUSH;

   activeDataSet = dataset;
}

/////////////////// STATIC member functions follow ///////////////////


float cfdObjects::vectorScale = 0.0;
int   cfdObjects::particleOption = 0;
float cfdObjects::particleScale = 0.0;

// used by vectors and intended to be used for warped contours
void cfdObjects::SetVectorScale( float x )
{
   vectorScale = x;
}

float cfdObjects::GetVectorScale()
{
   return vectorScale;
}

// used by cfdPolydata for setting the type and size of particles
void cfdObjects::SetParticleOption( int option )
{
   particleOption = option;
}

int cfdObjects::GetParticleOption()
{
   return particleOption;
}

void cfdObjects::SetParticleScale( float x )
{
   particleScale = x;
}

float cfdObjects::GetParticleScale()
{
   return particleScale;
}

