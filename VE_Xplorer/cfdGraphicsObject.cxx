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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdGraphicsObject.h"
#include "cfdGeode.h"
#include "cfdGroup.h"

#include <vtkActor.h>

#include <vpr/Util/Debug.h>

// constructor
cfdGraphicsObject::cfdGraphicsObject( void )
{
   geode = 0;
   parentNode = 0;
   worldNode = 0;
   type = OTHER;
   actor = 0;
}

// destructor
cfdGraphicsObject::~cfdGraphicsObject( void )
{
   if ( geode )
   {
      delete geode;
      geode = 0;
   }
   type = OTHER;
}

// copy constructor
cfdGraphicsObject::cfdGraphicsObject( const cfdGraphicsObject& input )
{
   // do nothing yet
}

// equal operator
cfdGraphicsObject& cfdGraphicsObject::operator=( const cfdGraphicsObject& input )
{
   // do nothing yet
   if ( this != &input )
   {
   }
   return *this;
}

// Set parent node to add "graphics node" to
void cfdGraphicsObject::SetParentNode( cfdGroup* input )
{
   this->parentNode = input;
}

// node the parent node will be added to
void cfdGraphicsObject::SetWorldNode( cfdGroup* input )
{
   this->worldNode = input;
}

// add "child node" to scene graph
void cfdGraphicsObject::AddGraphicsObjectToSceneGraph( void )
{
   if ( this->type == CLASSIC )
   {
      vprDEBUG(vprDBG_ALL,2) << " have geode flag"
                             << std::endl << vprDEBUG_FLUSH;

      if ( this->worldNode->SearchChild( this->parentNode ) < 0 )
      {
         vprDEBUG(vprDBG_ALL,1) << " adding active DCS to worldDCS"
                             << std::endl << vprDEBUG_FLUSH;
         this->worldNode->AddChild( this->parentNode );
      }

      this->geode = new cfdGeode();
      this->geode->TranslateTocfdGeode( this->actor );
      this->parentNode->AddChild( this->geode );
   }
   else if ( this->type == TRANSIENT )
   {
      if ( this->worldNode->SearchChild( this->parentNode ) < 0 )
      {
         vprDEBUG(vprDBG_ALL,1) << " adding active DCS to worldDCS"
                             << std::endl << vprDEBUG_FLUSH;
         this->worldNode->AddChild( this->parentNode );
      }
      
      this->geodes.push_back( new cfdGeode() );
      this->geodes.back()->TranslateTocfdGeode( this->actor );
   }
}

// set type of viz: trans, classic, texture
void cfdGraphicsObject::SetTypeOfViz( VizType x )
{
   this->type = x;
}

// set actor for classic and trans viz objects
void cfdGraphicsObject::SetActor( vtkActor* input )
{
   this->actor = input;
}

// Return parent node for a this object
cfdGroup* cfdGraphicsObject::GetParentNode( void )
{
   return this->parentNode;
}
/*
void cfdGraphicsObject::AddGeodesToSequence( void )
{
   int nTransGeodes = 0; 
   
   int nGroups = animation->GetSequence()->GetNumChildren();
   int nDCSs = 0;
   int nGeodes = 0;
   cfdGroup* tempGroup = 0;
   cfdDCS* tempDCS = 0;
   cfdGeode* tempGeode = 0;
      
   std::cout<<"removing nodes from the sequence!!"<<std::endl;
   std::cout<<"cfdObjects::AddGeodesToSequence"<<std::endl;
   for(int i = 0; i < nGroups; i++)
   {
      tempGroup = (cfdGroup*)this->animation->GetSequence()->GetChild(i);
      nDCSs = tempGroup->GetNumChildren();
      std::cout<<"number of dcs: "<< nDCSs<<std::endl;
      for(int j = 0; j < nDCSs; j++)
      {
         tempDCS = (cfdDCS*)tempGroup->GetChild(j);
         nGeodes = tempDCS->GetNumChildren();
         for(int k = 0; k < nGeodes; k++)
         {
            std::cout<<"removing geode : "<< k<<std::endl;
            tempGeode = (cfdGeode*)tempDCS->GetChild(0);
            tempDCS->RemoveChild(tempGeode);
            geodes.erase(geodes.begin());
            delete tempGeode;
         }
      }
   }

   std::cout<<"adding geodes to sequence!! "<< geodes.size() << std::endl;
   std::cout<<"cfdObjects::AddGeodesToSequence"<<std::endl;
   nTransGeodes =  geodes.size();
   for(int g = 0; g < nGroups; g++)
   {
      tempGroup = this->animation->GetGroup(g);
      std::cout<<" adding to group: "<<g<<std::endl;
      nDCSs = tempGroup->GetNumChildren();
      for(int i = 0; i < nDCSs; i ++)
      {
         tempDCS = (cfdDCS*)tempGroup->GetChild(i);
         tempDCS->AddChild(this->transientGeodes.at(g));
      }
   }
   //this->_sequence->GetSequence()->setInterval( CFDSEQ_CYCLE, 0 , nTransGeodes - 1 );
   //this->_sequence->GetSequence()->setDuration( 0.1 * nTransGeodes );
   this->animation->StartSequence();
   this->parentNode->AddChild( this->animation );

   std::cout<<"finished adding to the sequence"<<std::endl;
   std::cout<<"cfdObjects::AddGeodesToSequence"<<std::endl;
}
// This function just creates a geode from the actor for a particular
// visualization feature. It is not responsible for adding the 
// newly created geode to the scene graph. 
void cfdGraphicsObject::UpdatecfdGeode( void )
{
   vprDEBUG(vprDBG_ALL, 1) << "cfdObjects::UpdateGeode..."
                           << std::endl << vprDEBUG_FLUSH;
   
   if ( this->updateFlag )
   {
      //check if current data set is transient
      if(this->activeDataSet->IsPartOfTransientSeries()){
         this->transientGeodes.push_back(new cfdGeode());
   if ( this->usePreCalcData && this->PDactor != NULL )
   {
      vprDEBUG(vprDBG_ALL, 2) 
         << "cfdObjects::UpdateGeode... create geode from PDactor"
         << std::endl << vprDEBUG_FLUSH;

      // Function implements respective vtkActorToGeode function
      ((cfdGeode*)this->transientGeodes.back())->TranslateTocfdGeode( this->PDactor );
   }
   else
   {
      vprDEBUG(vprDBG_ALL, 2) 
         << "cfdObjects::UpdateGeode... create geode from actor"
         << std::endl << vprDEBUG_FLUSH;

      // Function implements respective vtkActorToGeode function
      ((cfdGeode*)this->transientGeodes.back())->TranslateTocfdGeode( this->actor );
   }
         //check if this is causing problems 
         if ( this->transientGeodes.size()%_sequence->GetNumberOfFrames() == 0 )
            this->addTransientGeode = true;
      }else{
         vprDEBUG(vprDBG_ALL, 1) << "cfdObjects::Allocate Geode..."
                           << updateFlag<< std::endl << vprDEBUG_FLUSH;
   
         this->_geode = new cfdGeode();
         this->addGeode = true;      
      }
   }
   else
   {
      vprDEBUG(vprDBG_ALL, 0) 
         << "cfdObjects::UpdateGeode... updateFlag == false for ObjectType = "
         << this->objectType << std::endl << vprDEBUG_FLUSH;
   }
}

// This function simply adds the created geode from function UpdateGeode
void cfdGraphicsObject::AddcfdGeodeToDCS( void )
{
   vprDEBUG(vprDBG_ALL, 1) << "cfdObjects::AddGeodeToDCS"
      << std::endl << vprDEBUG_FLUSH;
  
   vprDEBUG(vprDBG_ALL, 2) << "cfdObjects::UpdateGeode... updateFlag == true "
                           << this->_geodes.size() << std::endl << vprDEBUG_FLUSH;
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
         cfdGroup* parent = (cfdGroup*)this->_geodes[ num ]->GetParent(0);
         parent->RemoveChild(this->_geodes[ num ] );
         delete this->_geodes[ num ];
         this->_geodes.erase( this->_geodes.end() - 3 );
      }
   }
   else if ( this->_geodes.size() > 1 ) // replace old with new
   {
      // geodes.size is not zero based therefore the first -1 is needed
      // the second -1 is to get the second to last geode on the list
      int num = (this->_geodes.size() - 1) - 1;
      vprDEBUG(vprDBG_ALL,1) << " 1. removing child num = " << num << " : " << this->_geodes[ num ] << " : " << _geodes.size()
                             << std::endl << vprDEBUG_FLUSH;
      cfdGroup* parent = (cfdGroup*)this->_geodes[ num ]->GetParent(0);
      vprDEBUG(vprDBG_ALL,2) << " 1. removing child parent = " << parent 
                             << std::endl << vprDEBUG_FLUSH;
      parent->RemoveChild( this->_geodes[ num ] );
      vprDEBUG(vprDBG_ALL,2) << " 1. removing child succesful" 
                             << std::endl << vprDEBUG_FLUSH;
      delete this->_geodes[ num ];
      vprDEBUG(vprDBG_ALL,2) << " 1. delete child sucessful" 
                             << std::endl << vprDEBUG_FLUSH;

      this->_geodes.erase( this->_geodes.end() - 2 );
      vprDEBUG(vprDBG_ALL,2) << " 1. erase child succesful" 
                             << std::endl << vprDEBUG_FLUSH;
      this->_dcs->AddChild( ((cfdGeode*)this->_geodes[ num ]) );
      vprDEBUG(vprDBG_ALL,1) << " 1. add child succesful " 
                             << std::endl << vprDEBUG_FLUSH;
   }
   else //if ( this->geodes.size() == 1 )
   { 
      vprDEBUG(vprDBG_ALL,1) << " adding child geode = " << this->_geodes.at( 0 )
                             << std::endl << vprDEBUG_FLUSH;
      this->_dcs->AddChild( ((cfdGeode*)this->_geodes.at( 0 )) );     
   }
}
*/
void cfdGraphicsObject::RemovecfdGeodeFromDCS( void )
{
   if ( this->type == TRANSIENT )
   {
      int num = this->geodes.size();
  
      // Iterate backwards for performance
      for ( int i = num - 1; i >= 0; i-- )
      {
         // Need to find tha parent becuase with multiple models
         // Not all geodes are going to be on the same dcs
         cfdGroup* parent = (cfdGroup*)this->geodes[ i ]->GetParent(0);
         parent->RemoveChild( this->geodes[ i ] );
         delete this->geodes[ i ];
      }
      this->geodes.clear();
   }
   else if ( this->type == CLASSIC )
   {
      cfdGroup* parent = (cfdGroup*)this->geode->GetParent(0);
      parent->RemoveChild( this->geode);
      delete this->geode;
	  this->geode = 0;
   } 
}

