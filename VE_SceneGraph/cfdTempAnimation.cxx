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
 * File:          $RCSfile: cfdTempAnimation.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdTempAnimation.h"
#include "cfdSequence.h"
#include "cfdEnum.h"
#include "cfdGroup.h"
#include "cfdGeode.h"
#include "cfdDCS.h"
#include "cfdSceneNode.h"

#include <iostream>

#include <vpr/Util/Debug.h>

#include <vtkActor.h>

cfdTempAnimation::cfdTempAnimation()
{
   this->_sequence = new cfdSequence();
   this->numFrames = 0;
   this->groups = 0;
}

cfdTempAnimation::~cfdTempAnimation()
{
   // This Delete also takes care of this->groups
   delete this->_sequence;
   // TODO: Need to add delete of the nodes on the sequence
}

// create pfGroups and add to the cfdSequence node
void cfdTempAnimation::SetGroups( void )
{
   // This function is called after the various flowManagers
   // are added to the vector of flowManagers.
   // And after the optional SetDuration function is called.

   // This function can only be called once...
   if ( this->groups != NULL )
   {
      std::cerr << "Error: SetpfGroups can only be called once" << std::endl;
      exit ( 1 );
   }

   // get the number of frames from the first flowManager 
   // (all flowMangers will have the same number of frames)
   if ( numFrames == 0 )
   {
      std::cerr << "Error: No flowMangers have been defined yet" << std::endl;
      exit ( 1 );
   }

   vprDEBUG(vprDBG_ALL,1) << " Making groups" << std::endl << vprDEBUG_FLUSH;
   this->groups = new cfdGroup*[ this->numFrames ];
   for ( int i = 0; i < this->numFrames; i++)
   {
      this->groups[ i ] = new cfdGroup();
      this->_sequence->AddChild( this->groups[ i ] );
   }
   this->_sequence->setInterval( CFDSEQ_CYCLE, 0 , this->numFrames - 1 );
   this->_sequence->setDuration( this->_duration );
}

// set the duration of the sequence (in seconds)
void cfdTempAnimation::SetDuration( double time )
{
   this->_duration = time;
}

cfdGroup* cfdTempAnimation::GetGroup( int i )
{
   return this->groups[ i ];
}

cfdSequence* cfdTempAnimation::GetSequence( void )
{
   return this->_sequence;
}

void cfdTempAnimation::SetNumberOfFrames( int i )
{
   // Set number of frames to create the number of groups
   numFrames = i;
}

void cfdTempAnimation::CreateGeodeVector( vtkActor* actor )
{
   // used by cfdAnimatedStreamlineCone
   this->_geodes.push_back( new cfdGeode() );
 
   // Function implements respective vtkActorToGeode function
   ((cfdGeode*)this->_geodes.back())->TranslateTocfdGeode( actor );
}

void cfdTempAnimation::AddGeodesToSequence( std::vector< cfdGeode* > geodes )
{
   this->StopSequence();
   
   if ( !groups )
   {
      // For animated streamlines
      unsigned int numPts = geodes.size();
      for ( unsigned int i = 0; i < numPts; ++i )
      {
         this->_sequence->AddChild( geodes.at( i ) );
      }      

      this->_sequence->setInterval( CFDSEQ_CYCLE, 0 , numPts - 1 );
      this->_sequence->setDuration( 0.1 * numPts );
      vprDEBUG(vprDBG_ALL, 2) << " For animated Streamlines: End Loop" 
                           << std::endl << vprDEBUG_FLUSH;
   }
   else 
   {
      for ( int i = 0; i < numFrames; ++i )
      {
         cfdGroup* tempGroup = (cfdGroup*)this->_sequence->GetChild(i);
         // get the DCS that is beneth the group 
         // child 0 should be the dataset dcs while child 1 should be geom
         // the DCS should be added at intialization
         ((cfdDCS*)tempGroup->GetChild( 0 ))->AddChild( geodes.at( i ) );
      }
   }
   this->StartSequence();
}

void cfdTempAnimation::AddToSequence( int objectType )
{
   int i;
   int num = this->_sequence->GetNumChildren();
   vprDEBUG(vprDBG_ALL, 2) << " Number of children in sequence: " << num
                           << std::endl << vprDEBUG_FLUSH;
   
   cfdGeode* temp = NULL;
   //unsigned int type = this->_sequence->getChild( 0 )->GetNodeType();
   
   if ( objectType == ANIMATED_STREAMLINES )
   {
      vprDEBUG(vprDBG_ALL, 2) << " For animated Streamlines: Enter Loop" 
                              << std::endl << vprDEBUG_FLUSH;
      this->StopSequence();
      // For animated streamlines
      for ( i = 0; i < num; i++ )
      {
         temp = ( cfdGeode* )this->_sequence->GetChild( 0 );
         this->_sequence->RemoveChild( temp );
         _geodes.erase( _geodes.begin() );
         delete temp;
      }
      vprDEBUG(vprDBG_ALL, 2) << "For animated Streamlines: End creating Geodes"
                              << std::endl << vprDEBUG_FLUSH;

      int numPts = this->_geodes.size();
      for ( i = 0; i < numPts; i++ )
      {
         this->_sequence->AddChild( this->_geodes[ i ] );
      }      
      vprDEBUG(vprDBG_ALL, 2) << " For animated Streamlines: Create Sequence : " << numPts
                              << std::endl << vprDEBUG_FLUSH;

      this->_sequence->setInterval( CFDSEQ_CYCLE, 0 , numPts - 1 );
      this->_sequence->setDuration( 0.1 * numPts );
      this->StartSequence();
      vprDEBUG(vprDBG_ALL, 2) << " For animated Streamlines: End Loop" 
                              << std::endl << vprDEBUG_FLUSH;
   }
   else if ( objectType == ANIMATED_IMAGES )
   {
      vprDEBUG(vprDBG_ALL, 2) << " For animated Images: Enter Loop" 
                              << std::endl << vprDEBUG_FLUSH;
      this->StopSequence();
      // For animated streamlines
      // If there are already children on the sequence
      for ( i = 0; i < num; i++ )
      {
         temp = (cfdGeode *)this->_sequence->GetChild( 0 );
         this->_sequence->RemoveChild( temp );
         _geodes.erase( _geodes.begin() );
         delete temp;
      }
      vprDEBUG(vprDBG_ALL, 2) << " For animated Images: End creating Geodes"
                              << std::endl << vprDEBUG_FLUSH;

      int numPts = this->_geodes.size();
      for ( i = 0; i < numPts; i++ )
      {
         this->_sequence->AddChild( this->_geodes[ i ] );
      }      
      vprDEBUG(vprDBG_ALL, 2) << " For animated Images: Create Sequence" 
                              << std::endl << vprDEBUG_FLUSH;

      this->_sequence->setInterval( CFDSEQ_CYCLE, 0 , numPts - 1 );
      this->_sequence->setDuration( 0.1 * numPts );

      this->StartSequence();
      vprDEBUG(vprDBG_ALL, 2) << " For animated Images: End Loop" 
                              << std::endl << vprDEBUG_FLUSH;
   }
   else
   {
      std::cerr << "ERROR: Don't know this kind of sequence" << std::endl;
      exit( 1 );
   }
   
   // This is necessary because cfdNodes can't have multiple parents
   if ( this->_sequence->GetParent( 0 ) != NULL )
   {
      ((cfdDCS*)this->_sequence->GetParent( 0 ))->RemoveChild( 
                                       (cfdNode*)this->_sequence );
   }

   vprDEBUG(vprDBG_ALL, 2) << " done with add to sequence"
                           << std::endl << vprDEBUG_FLUSH;
}

void cfdTempAnimation::StopSequence( void )
{
   this->_sequence->setPlayMode( CFDSEQ_STOP );
}

void cfdTempAnimation::StartSequence( void )
{
   this->_sequence->setPlayMode( CFDSEQ_START );   
}

void cfdTempAnimation::ResumeSequence( void )
{
   this->_sequence->setPlayMode( CFDSEQ_RESUME );   
}

void cfdTempAnimation::PauseSequence( void )
{
   this->_sequence->setPlayMode( CFDSEQ_PAUSE );   
}

void cfdTempAnimation::ReverseSequence( void )
{
   //check the direction of the sequence
   if(_sequence->getDirection() == 1){
      //reverse the direction of the interval
      _sequence->changeSequenceDirection();
   }
   //this is added functionality to step through the sequence correctly
   _sequence->setPlayMode(CFDSEQ_PLAYING);
   _sequence->stepSequence();
}

void cfdTempAnimation::ForwardSequence( void )
{
   //check the direction of the sequence
   if(_sequence->getDirection() == -1){
      //reverse the direction of the interval
      _sequence->changeSequenceDirection();
   }
   //this is added functionality to step through the sequence correctly
   _sequence->setPlayMode(CFDSEQ_PLAYING);
   _sequence->stepSequence();
}

int cfdTempAnimation::GetFrameOfSequence( void )
{
   return this->_sequence->getCurrentFrame();
}

void cfdTempAnimation::SetCurrentFrame( int frameIndex )
{
   this->_sequence->setCurrentFrame( frameIndex );
}

void cfdTempAnimation::ClearSequence( void )
{
   // This function is called for one particular type of transientFlowManager
   // It removes and deletes geodes and changes the cfdObjects geodes list
   vprDEBUG(vprDBG_ALL,1) << " ***********************ClearSequence"
                          << std::endl << vprDEBUG_FLUSH;

   int numSequenceChildren = this->_sequence->GetNumChildren();
   vprDEBUG(vprDBG_ALL,1) << "cfdTempAnimation::numSequenceChildren: " << numSequenceChildren
                          << std::endl << vprDEBUG_FLUSH;

   int numGeodes = this->_geodes.size();
   vprDEBUG(vprDBG_ALL,1) << "cfdTempAnimation::numGeodes: " << numGeodes
                          << std::endl << vprDEBUG_FLUSH; 

   if ( numSequenceChildren > 0 && numGeodes > 0 )
   {
      for ( int i = numSequenceChildren-1; i >= 0; i-- )
      {
         // transient sequences have groups attached directly to sequence nodes
         cfdSceneNode::cfdNodeType type = this->_sequence->GetChild( i )->GetCFDNodeType();

         if ( type == cfdSceneNode::CFD_GROUP )//group )
         {
            // Each group in a transient sequence should have the same number of children
            // One particular node (at most) in each group pertains to the DCS
            int numChildrenPerGroup = ((cfdGroup *)this->_sequence->GetChild( i ))->GetNumChildren();

            vprDEBUG(vprDBG_ALL,1) << "cfdTempAnimation::looking at child " << i << ", a group with "
                                   << numChildrenPerGroup << " children"
                                   << std::endl << vprDEBUG_FLUSH; 

            vprDEBUG(vprDBG_ALL,1) << "cfdTempAnimation::removing nodes from the sequence!!" 
                                    << std::endl << vprDEBUG_FLUSH;
            vprDEBUG(vprDBG_ALL,1) << "cfdTempAnimation::AddGeodesToSequence" 
                                    << std::endl << vprDEBUG_FLUSH;
            cfdGroup* tempGroup = (cfdGroup*)this->_sequence->GetChild(i);
            int nDCSs = tempGroup->GetNumChildren();
            vprDEBUG(vprDBG_ALL,1) <<"cfdTempAnimation::number of dcs: "<< nDCSs<<std::endl<< vprDEBUG_FLUSH;
            for(int j = 0; j < nDCSs; j++)
            {
               cfdDCS* tempDCS = (cfdDCS*)tempGroup->GetChild(j);
               int nGeodes = tempDCS->GetNumChildren();
               for(int k = 0; k < nGeodes; k++)
               {
                  vprDEBUG(vprDBG_ALL,1) << "cfdTempAnimation::removing geode : " 
                                          << k << std::endl << vprDEBUG_FLUSH;
                  cfdGeode* tempGeode = (cfdGeode*)tempDCS->GetChild(0);
                  tempDCS->RemoveChild(tempGeode);
                  delete tempGeode;
               }
            } 
         }
         // animated particles have geodes attached directly to sequence nodes
         else if ( type == cfdSceneNode::CFD_GEODE )//geode )
         {
            std::vector< cfdGeode* >::iterator iter;
            for ( iter = this->_geodes.begin(); iter != this->_geodes.end(); iter++ )
            {
               int geodeIndex = this->_sequence->SearchChild( *iter );
   
               if ( geodeIndex >= 0 )                                           
               {
                  vprDEBUG(vprDBG_ALL,1) << " child " << i 
                                         << " is a geode that will be removed"
                                         << std::endl << vprDEBUG_FLUSH; 
                  cfdGeode* geode = (cfdGeode *)this->_sequence->GetChild( geodeIndex );
                  int error = this->_sequence->RemoveChild( geode );
                  vprDEBUG(vprDBG_ALL,1) << " Removal of child " << i 
                                         << " was : " << error
                                         << std::endl << vprDEBUG_FLUSH; 
                  delete geode;

                  this->_geodes.erase( iter );
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
            std::cerr << "cfdTempAnimation::ERROR: Don't know this kind of sequence" << std::endl;
            exit( 1 );
         }
      }
   }
}
