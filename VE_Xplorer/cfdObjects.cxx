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
#include "cfdEnum.h"
#include "cfdSequence.h"
#include "cfdTransientInfo.h"

// Juggler Includes
#include <vpr/Util/Debug.h>

// Performer Includes
#include <Performer/pf.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfGeode.h>

// VTK Includes
#include <vtkPolyData.h>
#include <vtkPolyDataSource.h>
#include <vtkActor.h>

// vtkActorToPF
#include "vtkActorToPF.h"

// C++ Libs
#include <vector>

cfdObjects::cfdObjects( pfGeode *temp, int type )
{
   SetGeode( temp );
   SetObjectType( type );
   this->pointSource = NULL;
   this->vtkToPFDebug = 0;
   this->usePreCalcData = false;
   this->actor = NULL;
   this->PDactor = NULL;
}

cfdObjects::cfdObjects( void )
{
   vprDEBUG(vprDBG_ALL, 1) << " New cfdObjects ! " 
                           << std::endl << vprDEBUG_FLUSH;
   this->geode = NULL;
   this->sequence = NULL;
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

void cfdObjects::SetGeode( pfGeode *temp )
{
   //this->geode = temp;
   exit( 1 );
}

void cfdObjects::SetObjectType( int type )
{
   this->objectType = type;
}

/*
void cfdObjects::UpdateObject( void )
{
   cfdGeodeSetsUpdate( this->geosets, this->geode );
}
*/

/*
void cfdObjects::DeleteObject( void )
{
   cfdGeodeSetsDelete( this->geode );
}
*/

pfGeode * cfdObjects::GetGeode( void )
{ 
   return this->geode;
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

void cfdObjects::DeleteGeode( )
{
   pfDelete( this->geode );
}

void cfdObjects::SetSequence( cfdSequence * x )
{
   this->sequence = x;
}

cfdSequence* cfdObjects::GetSequence( )
{
   return this->sequence;
}

/*
void cfdObjects::DeleteSequence( )
{
   pfDelete( this->sequence );
}
*/

void cfdObjects::SetSourcePoints( vtkPolyDataSource* pointSource )
{
   this->pointSource = pointSource;
}

// This function just creates a geode from the actor for a particular
// visualization feature. It is not responsible for adding the 
// newly created geode to the scene graph. 
void cfdObjects::UpdateGeode( void )
{
   vprDEBUG(vprDBG_ALL, 1) << "cfdObjects::UpdateGeode..."
                           << std::endl << vprDEBUG_FLUSH;
   

   if ( this->updateFlag )
   {
      vprDEBUG(vprDBG_ALL, 1) << "cfdObjects::Allocate Geode..."
                           << updateFlag<< std::endl << vprDEBUG_FLUSH;
   
      this->geode = new pfGeode();
      this->addGeode = true;      
   }
   else
   {
      vprDEBUG(vprDBG_ALL, 0) 
         << "cfdObjects::UpdateGeode... updateFlag == false for ObjectType = "
         << this->objectType << std::endl << vprDEBUG_FLUSH;
   }
}

void cfdObjects::CreateGeode( void )
{
   // used by cfdAnimatedStreamlineCone
   this->geodes.push_back( new pfGeode );
 
   vtkActorToPF( this->actor, (pfGeode *)this->geodes.back(), this->vtkToPFDebug );
}

// This function simply adds the created geode from function UpdateGeode
void cfdObjects::AddGeodeToDCS( void )
{
   vprDEBUG(vprDBG_ALL, 1) << "cfdObjects::AddGeodeToDCS"
      << std::endl << vprDEBUG_FLUSH;
  
   vprDEBUG(vprDBG_ALL, 2) << "cfdObjects::UpdateGeode... updateFlag == true"
                           << std::endl << vprDEBUG_FLUSH;

   this->geodes.push_back( this->geode );

   vprDEBUG(vprDBG_ALL, 2) << "cfdObjects::UpdateGeode... pushback new geode"
                           << std::endl << vprDEBUG_FLUSH;

   if ( this->usePreCalcData && this->PDactor != NULL )
   {
      vprDEBUG(vprDBG_ALL, 2) 
         << "cfdObjects::UpdateGeode... create geode from PDactor"
         << std::endl << vprDEBUG_FLUSH;

      vtkActorToPF( this->PDactor, (pfGeode *)geodes.back(), this->vtkToPFDebug );
   }
   else
   {
      vprDEBUG(vprDBG_ALL, 2) 
         << "cfdObjects::UpdateGeode... create geode from actor"
         << std::endl << vprDEBUG_FLUSH;

      vtkActorToPF( this->actor, (pfGeode *)geodes.back(), this->vtkToPFDebug );
   }

   vprDEBUG(vprDBG_ALL, 2) 
      << "cfdObjects::UpdateGeode... set active geode pointer"
      << std::endl << vprDEBUG_FLUSH;
   //this->geode = (pfGeode *)this->geodes.back();
   //this->geode = tempGeode;
   //this->updateFlag = true;

   if ( this->GetActiveDataSet()->IsNewlyActivated() )
   {
      // add new with old
      this->GetActiveDataSet()->SetNotNewlyActivated();
      // geodes.size is not zero based therefore the -1 is needed
      int num = this->geodes.size() - 1;
      vprDEBUG(vprDBG_ALL,1) << " adding child num = " << num
                             << std::endl << vprDEBUG_FLUSH;
      vprDEBUG(vprDBG_ALL,1) << "this->geodes[ 0 ] = " << this->geodes[ 0 ]
         << std::endl << "this->geodes[ num ] = " << this->geodes[ num ]
         << std::endl << vprDEBUG_FLUSH;
      this->dcs->addChild( this->geodes[ num ] );

      if ( this->geodes.size() > 2 ) // remove oldest
      {
         int num = (this->geodes.size() - 1) - 2;
         this->dcs->removeChild( this->geodes[ num ] );
         pfDelete( this->geodes[ num ] );
         this->geodes.erase( this->geodes.end() - 3 );
      }
   }
   else if ( this->geodes.size() > 1 ) // replace old with new
   {
      // geodes.size is not zero based therefore the first -1 is needed
      // the second -1 is to get the second to last geode on the list
      int num = (this->geodes.size() - 1) - 1;
      vprDEBUG(vprDBG_ALL,1) << " removing child num = " << num 
                             << std::endl << vprDEBUG_FLUSH;
      this->dcs->removeChild( this->geodes[ num ] );
      pfDelete( this->geodes[ num ] );
/*
      vprDEBUG(vprDBG_ALL,0) 
         << "this->geodes.begin() = " << this->geodes.begin() << std::endl 
         << "this->geodes.end()-2 = " << this->geodes.end()-2 << std::endl
         << "this->geodes.end()-1 = " << this->geodes.end()-1 << std::endl
         << "this->geodes.end() =   " << this->geodes.end()
         << std::endl << vprDEBUG_FLUSH;
*/
      this->geodes.erase( this->geodes.end() - 2 );
      this->dcs->addChild( this->geodes[ num ] );
   }
   else //if ( this->geodes.size() == 1 )
   {
      this->dcs->addChild( this->geodes[ 0 ] );     
   }
}

void cfdObjects::RemoveGeodeFromDCS( void )
{
   int i, num;
     
   num = this->geodes.size();
  
   // Iterate backwards for performance
   for ( i = num - 1; i >= 0; i-- )
   {
      this->dcs->removeChild( this->geodes[ i ] );
      pfDelete( this->geodes[ i ] );
   }
   this->geodes.clear();
}

void cfdObjects::SetDCS( pfDCS *dcs )
{
   this->dcs = dcs;
}

pfDCS *cfdObjects::GetDCS( void )
{
   return this->dcs;
}

void cfdObjects::SetcfdReadParam( cfdReadParam *param )
{
   this->paramFile = param;   
}

void cfdObjects::StopSequence( void )
{
   this->sequence->setPlayMode( CFDSEQ_STOP );
}

void cfdObjects::StartSequence( void )
{
   this->sequence->setPlayMode( CFDSEQ_START );   
}

void cfdObjects::ResumeSequence( void )
{
   this->sequence->setPlayMode( CFDSEQ_RESUME );   
}

void cfdObjects::PauseSequence( void )
{
   this->sequence->setPlayMode( CFDSEQ_PAUSE );   
}

void cfdObjects::ClearSequence( void )
{
   // This function is called for one particular type of transientFlowManager
   // It removes and deletes geodes and changes the cfdObjects geodes list
   vprDEBUG(vprDBG_ALL,1) << " ***********************ClearSequence"
                          << std::endl << vprDEBUG_FLUSH;

   int numSequenceChildren = this->sequence->getNumChildren();
   vprDEBUG(vprDBG_ALL,1) << " numSequenceChildren: " << numSequenceChildren
                          << std::endl << vprDEBUG_FLUSH;

   int numGeodes = this->geodes.size();
   vprDEBUG(vprDBG_ALL,1) << " numGeodes: " << numGeodes
                          << std::endl << vprDEBUG_FLUSH; 

   if ( numSequenceChildren > 0 && numGeodes > 0 )
   {
      for ( int i = numSequenceChildren-1; i >= 0; i-- )
      {
         // transient sequences have groups attached directly to sequence nodes
         if ( this->sequence->getChild( i )->getType() 
                                                == pfGroup::getClassType() )
         {
            // Each group in a transient sequence should have the same number of children
            // One particular node (at most) in each group pertains to the TFM
            // We want to remove that node (geode) that pertain to that TFM
            int numChildrenPerGroup = ((pfGroup *)this->sequence->getChild( i ))->getNumChildren();

            vprDEBUG(vprDBG_ALL,1) << " looking at child " << i << ", a group with "
                                   << numChildrenPerGroup << " children"
                                   << std::endl << vprDEBUG_FLUSH; 

            pfGroup * group = (pfGroup *)this->sequence->getChild( i );

            std::vector< pfNode * >::iterator iter;
            for ( iter = this->geodes.begin(); iter != this->geodes.end(); iter++ )
            {
               int geodeIndex = group->searchChild( *iter );
   
               if ( geodeIndex >= 0 )                                           
               {
                  vprDEBUG(vprDBG_ALL,1) << "\twill remove geode " << geodeIndex
                                         << std::endl << vprDEBUG_FLUSH; 
                  pfGeode * _geode_ = (pfGeode *)group->getChild( geodeIndex );
                  group->removeChild( _geode_ );
                  pfDelete( _geode_ );
                  this->geodes.erase( iter );
                  break;
               }
               else
               {
                  vprDEBUG(vprDBG_ALL,1) << "\twill NOT remove geode"
                                         << std::endl << vprDEBUG_FLUSH; 
               }
            } 
         }
         // animated particles have geodes attached directly to sequence nodes
         else if ( this->sequence->getChild( i )->getType() 
                                             == pfGeode::getClassType() )
         {
            std::vector< pfNode * >::iterator iter;
            for ( iter = this->geodes.begin(); iter != this->geodes.end(); iter++ )
            {
               int geodeIndex = this->sequence->searchChild( *iter );
   
               if ( geodeIndex >= 0 )                                           
               {
                  vprDEBUG(vprDBG_ALL,1) << " child " << i 
                                         << " is a geode that will be removed"
                                         << std::endl << vprDEBUG_FLUSH; 
                  pfGeode * _geode_ = (pfGeode *)this->sequence->getChild( geodeIndex );
                  int error = this->sequence->removeChild( _geode_ );
                  vprDEBUG(vprDBG_ALL,1) << " Removal of child " << i 
                                         << " was : " << error
                                         << std::endl << vprDEBUG_FLUSH; 
                  pfDelete( _geode_ );
                  this->geodes.erase( iter );
                  break;
               }
               else
               {
                  vprDEBUG(vprDBG_ALL,1) << " child " << i 
                                         << " is a geode that will NOT be removed"
                                         << std::endl << vprDEBUG_FLUSH; 
               }
            } 
         }
         else
         {
            std::cerr << "ERROR: Don't know this kind of sequence" << std::endl;
            exit( 1 );
         }
      }
   }
}

void cfdObjects::AddToSequence( void )
{
   int i;
   int num = this->sequence->getNumChildren();
   vprDEBUG(vprDBG_ALL, 2) << " Number of children in sequence: " << num
                           << std::endl << vprDEBUG_FLUSH;

   pfGeode *temp;
   if ( this->objectType == ANIMATED_STREAMLINES )
   {
      vprDEBUG(vprDBG_ALL, 2) << " For animated Streamlines: Enter Loop" 
                              << std::endl << vprDEBUG_FLUSH;
      this->StopSequence();
      // For animated streamlines
      for ( i = 0; i < num; i++ )
      {
         temp = (pfGeode *)this->sequence->getChild( 0 );
         this->sequence->removeChild( temp );
         geodes.erase( geodes.begin() );
         pfDelete( temp );
      }
      vprDEBUG(vprDBG_ALL, 2) << "For animated Streamlines: End creating Geodes"
                              << std::endl << vprDEBUG_FLUSH;

      int numPts = this->geodes.size();
      for ( i = 0; i < numPts; i++ )
      {
         this->sequence->addChild( this->geodes[ i ] );
      }      
      vprDEBUG(vprDBG_ALL, 2) << " For animated Streamlines: Create Sequence" 
                              << std::endl << vprDEBUG_FLUSH;

      this->sequence->setInterval( CFDSEQ_CYCLE, 0 , numPts - 1 );
      this->sequence->setDuration( 0.1 * numPts );
      this->StartSequence();
      vprDEBUG(vprDBG_ALL, 2) << " For animated Streamlines: End Loop" 
                              << std::endl << vprDEBUG_FLUSH;
   }
   else if ( this->objectType == ANIMATED_IMAGES )
   {
      vprDEBUG(vprDBG_ALL, 2) << " For animated Images: Enter Loop" 
                              << std::endl << vprDEBUG_FLUSH;
      this->StopSequence();
      // For animated streamlines
      // If there are already children on the sequence
      for ( i = 0; i < num; i++ )
      {
         temp = (pfGeode *)this->sequence->getChild( 0 );
         this->sequence->removeChild( temp );
         geodes.erase( geodes.begin() );
         pfDelete( temp );
      }
      vprDEBUG(vprDBG_ALL, 2) << " For animated Images: End creating Geodes"
                              << std::endl << vprDEBUG_FLUSH;

      int numPts = this->geodes.size();
      for ( i = 0; i < numPts; i++ )
      {
         this->sequence->addChild( this->geodes[ i ] );
      }      
      vprDEBUG(vprDBG_ALL, 2) << " For animated Images: Create Sequence" 
                              << std::endl << vprDEBUG_FLUSH;

      this->sequence->setInterval( CFDSEQ_CYCLE, 0 , numPts - 1 );
      this->sequence->setDuration( 0.1 * numPts );

      this->StartSequence();
      vprDEBUG(vprDBG_ALL, 2) << " For animated Images: End Loop" 
                              << std::endl << vprDEBUG_FLUSH;
   }
   else if ( this->sequence->getChild( 0 )->getType() == pfGroup::getClassType() )
   {
      // For transient data
      for ( i = 0; i < num; i++ )
      {         
         if ( this->geodes[ i ] != NULL )
         {
            if ( ((pfGroup *)this->sequence->getChild( i ))->searchChild( 
                                                   this->geodes[ i ] ) >= 0 )
            {
               // This is the check for geometry already on the SG
               vprDEBUG(vprDBG_ALL, 1) << " geometry already on the SG"
                                       << std::endl << vprDEBUG_FLUSH;
               break;
            }
            else
            {
               vprDEBUG(vprDBG_ALL, 1) << " adding child " << i << " : "
                                       << this->geodes[ i ] << " to sequence group"
                                       << std::endl << vprDEBUG_FLUSH;
               ((pfGroup *)this->sequence->getChild( i ))->addChild( 
                                                         this->geodes[ i ] );
            }
         }
      }
      this->sequence->setInterval( CFDSEQ_CYCLE, 0 , num - 1 );
/*
      this->sequence->setDuration( 
                       this->paramFile->transientInfo[ 0 ]->GetDuration() );
      std::cout << "sequence->Duration = " << this->paramFile->transientInfo[ 0 ]->GetDuration() << std::endl;
*/
   }
   else
   {
      std::cerr << "ERROR: Don't know this kind of sequence" << std::endl;
      exit( 1 );
   }
   vprDEBUG(vprDBG_ALL, 2) << " done with add to sequence"
                           << std::endl << vprDEBUG_FLUSH;
   
   if ( this->dcs->searchChild( (pfNode *)this->sequence ) < 0 )
   {
      if ( this->objectType == ANIMATED_IMAGES )
      {
         //this->dcs->addChild( this->sequence );
         if ( this->dcs->searchChild( this->sequence->getParent(0) ) < 0 )
         {         
            this->dcs->addChild( this->sequence->getParent(0) );
         }
         else
         {
            // Already added ( this is a hack until cfDModel is inmplemented )
         }
      }
      else
      {
         this->dcs->addChild( (pfNode*)this->sequence );
      }
   }
}

void cfdObjects::ReverseSequence( void )
{
   //check the direction of the sequence
   if(sequence->getDirection() == 1){
      //reverse the direction of the interval
      sequence->changeSequenceDirection();
   }
   //this is added functionality to step through the sequence correctly
   sequence->setPlayMode(CFDSEQ_PLAYING);
   sequence->stepSequence();
}

void cfdObjects::ForwardSequence( void )
{
   //check the direction of the sequence
   if(sequence->getDirection() == -1){
      //reverse the direction of the interval
      sequence->changeSequenceDirection();
   }
   //this is added functionality to step through the sequence correctly
   sequence->setPlayMode(CFDSEQ_PLAYING);
   sequence->stepSequence();
}

int cfdObjects::GetFrameOfSequence( void )
{
   return this->sequence->getCurrentFrame();
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
#ifdef _CFDCOMMANDARRAY
bool cfdObjects::CheckCommandId( cfdCommandArray* commandArray )
{
   if ( commandArray->GetCommandValue( CFD_ID ) == CHANGE_PARTICLE_VIEW_OPTION )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_PARTICLE_VIEW_OPTION, value = " 
         << commandArray->GetCommandValue( CFD_GEOSTATE ) << std::endl << vprDEBUG_FLUSH;

      // if view option is set to point cloud, set sphere scale out of range
      if ( commandArray->GetCommandValue( CFD_GEOSTATE ) == 0 )
      {
         vprDEBUG(vprDBG_ALL,0) << " setting sphere scale out of range" 
                                << std::endl << vprDEBUG_FLUSH;
         cfdObjects::SetSphereScale( -999 );
      }
      else if ( commandArray->GetCommandValue( CFD_GEOSTATE ) == 1 )
      {
         vprDEBUG(vprDBG_ALL,0) << " setting sphere scale to " 
            << commandArray->GetCommandValue( CFD_GEOSTATE ) << std::endl << vprDEBUG_FLUSH;
         cfdObjects::SetSphereScale( commandArray->GetCommandValue( CFD_GEOSTATE ) );
      }

      return true;
   }
   else if ( commandArray->GetCommandValue( CFD_ID ) == CHANGE_SPHERE_SIZE )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_SPHERE_SIZE, value = " 
         << commandArray->GetCommandValue( CFD_ISOVALUE )
         << std::endl << vprDEBUG_FLUSH;

      cfdObjects::SetSphereScale( commandArray->GetCommandValue( CFD_ISOVALUE ) );

      return true;
   }
   else if ( commandArray->GetCommandValue( CFD_ID ) == CHANGE_VECTOR )
   { 
      int vectorIndex = commandArray->GetCommandValue( CFD_SC );
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_VECTOR, vectorIndex = " << vectorIndex
                             << std::endl << vprDEBUG_FLUSH;

      cfdObjects::GetActiveDataSet()->SetActiveVector( vectorIndex );
      cfdObjects::GetActiveDataSet()->GetParent()
                                    ->SetActiveVector( vectorIndex );

      return true;
   }
   else if ( commandArray->GetCommandValue( CFD_ID ) == CHANGE_STEADYSTATE_DATASET )
   {
      vprDEBUG(vprDBG_ALL,1) 
         << "CHANGE_STEADYSTATE_DATASET " << this->cfdIso_value
         //<< ", scalarIndex = " << this->cfdSc
         //<< ", min = " << this->cfdMin 
         //<< ", max = " << this->cfdMax
         << std::endl << vprDEBUG_FLUSH;

      i = commandArray->GetCommandValue( CFD_ISOVALUE );
      if ( i < (int)this->paramReader->GetNumberOfDataSets() )
      {
         vprDEBUG(vprDBG_ALL,0) << " dataset = "
            << this->paramReader->GetDataSet( i )->GetFileName()
            << ", dcs = " << this->paramReader->GetDataSet( i )->GetDCS()
            << std::endl << vprDEBUG_FLUSH;

         int cfdType = this->paramReader->GetDataSet( i )->GetType();
         vprDEBUG(vprDBG_ALL,1) << " cfdType: " << cfdType
                                << std::endl << vprDEBUG_FLUSH;

         // set the dataset as the appropriate dastaset type
         // (and the active dataset as well)
         if ( cfdType == 0 )
         {
            cfdObjects::SetActiveMeshedVolume( this->paramReader->GetDataSet(i) );
         }
         else if ( cfdType == 1 )
         {
            cfdObjects::SetActiveParticleData( this->paramReader->GetDataSet(i) );
         }
         else if ( cfdType == 2 )
         {
            cfdObjects::SetActiveSurfaceData( this->paramReader->GetDataSet(i) );
         }
         else
         {
            std::cerr << "Unsupported cfdType: " << cfdType << std::endl;
         }

         vprDEBUG(vprDBG_ALL,1) << "last active dataset name = " 
                                << oldDatasetName
                                << std::endl << vprDEBUG_FLUSH;

         vprDEBUG(vprDBG_ALL,1) << "Activating steady state file " 
                   << cfdObjects::GetActiveDataSet()->GetFileName()
                   << std::endl << vprDEBUG_FLUSH;

         // make sure that the user did not just hit same dataset button
         // (or change scalar since that is routed through here too)
         if ( strcmp( oldDatasetName, 
                      cfdObjects::GetActiveDataSet()->GetFileName() ) )
         {
            vprDEBUG(vprDBG_ALL,1) << " setting dataset as newly activated" 
                                   << std::endl << vprDEBUG_FLUSH;
            cfdObjects::GetActiveDataSet()->SetNewlyActivated();
            strcpy( oldDatasetName, 
                    cfdObjects::GetActiveDataSet()->GetFileName() );
         }

         // update scalar bar for possible new scalar name
         this->setId( CHANGE_SCALAR );
      }
      else
      {
         std::cerr << "ERROR: requested steady state dataset " 
                   << commandArray->GetCommandValue( CFD_ISOVALUE ) << " must be less than " 
                   << this->paramReader->GetNumberOfDataSets()
                   << std::endl;
         return true;
      }
   }
   else if ( commandArray->GetCommandValue( CFD_ID ) == CHANGE_SCALAR || 
             commandArray->GetCommandValue( CFD_ID ) == CHANGE_SCALAR_RANGE )
   { 
      int scalarIndex = commandArray->GetCommandValue( CFD_SC );
      vprDEBUG(vprDBG_ALL,1) << "CHANGE_SCALAR || CHANGE_SCALAR_RANGE"
         << ", scalarIndex = " << scalarIndex
         << ", min = " << commandArray->GetCommandValue( CFD_MIN )
         << ", max = " << commandArray->GetCommandValue( CFD_MAX )
         << std::endl << vprDEBUG_FLUSH;

      cfdObjects::GetActiveDataSet()->SetActiveScalar( scalarIndex );
      cfdObjects::GetActiveDataSet()->GetParent()
                                    ->SetActiveScalar( scalarIndex );

      cfdObjects::GetActiveDataSet()->ResetScalarBarRange( 
                           commandArray->GetCommandValue( CFD_MIN ), commandArray->GetCommandValue( CFD_MAX ) );
      cfdObjects::GetActiveDataSet()->GetParent()->ResetScalarBarRange( 
                           commandArray->GetCommandValue( CFD_MIN ), commandArray->GetCommandValue( CFD_MAX ) );

      // if already displayed, set a flag to update the scalar bar
      if ( this->scalarBarActor )
      {
         this->isTimeToUpdateScalarBar = true;
      }

      // when scalar is changed reset vector thresholding to none...
      if ( commandArray->GetCommandValue( CFD_ID ) == CHANGE_SCALAR  )
      {
         cfdVectorBase::UpdateThreshHoldValues();
      }
#ifdef _TAO
      if ( commandArray->GetCommandValue( CFD_ID ) == CHANGE_SCALAR )
      {
         this->executive->SetCalculationsFlag( true );
      }
#endif // _TAO
      return true;
   }
   return false;
}

void cfdObjects::UpdateCommand()
{
   cerr << "doing nothing in cfdObjects::UpdateCommand()" << endl;
}
#endif //_CFDCOMMANDARRAY
/////////////////// STATIC member functions follow ///////////////////

// initial definition of the static variable
cfdDataSet * cfdObjects::activeDataSet = NULL;
cfdDataSet * cfdObjects::activeMeshedVolume = NULL;
cfdDataSet * cfdObjects::activeParticleData = NULL;
cfdDataSet * cfdObjects::activeSurfaceData = NULL;
float cfdObjects::vectorScale = 0.0;
bool  cfdObjects::timeToUpdate = false;
float cfdObjects::sphereScale = -999.0;

bool cfdObjects::GetTimeToUpdateFlag( void )
{
   return timeToUpdate;
}

void cfdObjects::SetTimeToUpdateFlag( bool x )
{
   timeToUpdate = x;
}

void cfdObjects::SetActiveDataSet( cfdDataSet * dataset )
{
   vprDEBUG(vprDBG_ALL, 1) 
      << "cfdObjects::SetActiveDataSet: " << dataset
      << std::endl << vprDEBUG_FLUSH;

   activeDataSet = dataset;
}

cfdDataSet * cfdObjects::GetActiveDataSet()
{
   return activeDataSet;
}

void cfdObjects::SetActiveMeshedVolume( cfdDataSet * dataset )
{
   vprDEBUG(vprDBG_ALL, 1) 
      << "cfdObjects::SetActiveMeshedVolume: " << dataset
      << std::endl << vprDEBUG_FLUSH;

   activeMeshedVolume = dataset;
   activeDataSet = dataset;   // only one active dataset for scalar bar
}

cfdDataSet * cfdObjects::GetActiveMeshedVolume()
{
   return activeMeshedVolume;
}

void cfdObjects::SetActiveParticleData( cfdDataSet * dataset )
{
   vprDEBUG(vprDBG_ALL, 1) 
      << "cfdObjects::SetActiveParticleData: " << dataset
      << std::endl << vprDEBUG_FLUSH;

   activeParticleData = dataset;
   activeDataSet = dataset;   // only one active dataset for scalar bar
}

cfdDataSet * cfdObjects::GetActiveParticleData()
{
   return activeParticleData;
}

void cfdObjects::SetActiveSurfaceData( cfdDataSet * dataset )
{
   vprDEBUG(vprDBG_ALL, 1) 
      << "cfdObjects::SetActiveSurfaceData: " << dataset
      << std::endl << vprDEBUG_FLUSH;

   activeSurfaceData = dataset;
   activeDataSet = dataset;   // only one active dataset for scalar bar
}

cfdDataSet * cfdObjects::GetActiveSurfaceData()
{
   return activeSurfaceData;
}

// used by vectors and intended to be used for warped contours
void cfdObjects::SetVectorScale( float x )
{
   vectorScale = x;
}

float cfdObjects::GetVectorScale()
{
   return vectorScale;
}

// used by cfdPolydata for setting the size of sphere particles
void cfdObjects::SetSphereScale( float x )
{
   sphereScale = x;
}

float cfdObjects::GetSphereScale()
{
   return sphereScale;
}

