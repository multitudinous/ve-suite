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
#include "VE_Xplorer/XplorerHandlers/cfdGraphicsObject.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"

#include "VE_Xplorer/SceneGraph/Utilities/PhongLoader.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#ifdef _PERFORMER
#include <Performer/pfdb/pfpfb.h>
#elif _OSG
#include <osg/BlendFunc>
#include <osgDB/WriteFile>
#endif
using namespace VE_Xplorer;
using namespace VE_SceneGraph;

// constructor
cfdGraphicsObject::cfdGraphicsObject( void )
{
   parentNode = 0;
   worldNode = 0;
   type = OTHER;
   //animation = 0;
   model = 0;
}

// destructor
cfdGraphicsObject::~cfdGraphicsObject( void )
{
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
      ;
   }
   return *this;
}

// Set parent node to add "graphics node" to
void cfdGraphicsObject::SetParentNode( VE_SceneGraph::DCS* input )
{
   this->parentNode = input;
}

// node the parent node will be added to
void cfdGraphicsObject::SetWorldNode( VE_SceneGraph::DCS* input )
{
   this->worldNode = input;
}

/*
VE_SceneGraph::cfdTempAnimation* cfdGraphicsObject::GetAnimation( void )
{
   return animation;
}
*/

// add "child node" to scene graph
void cfdGraphicsObject::AddGraphicsObjectToSceneGraph( void )
{
   if ( this->type == CLASSIC )
   {
      // is parent on graph
      if ( !this->worldNode->SearchChild( parentNode ) )
      {
         vprDEBUG(vesDBG,1) << "|\t\tadding active switch node to worldDCS"
                             << std::endl << vprDEBUG_FLUSH;
         this->worldNode->AddChild( parentNode );
      }

      // is it transient, classic, or animated class
      // add animation or dcs
      VE_SceneGraph::Switch* temp = this->model->GetActiveDataSet()->GetSwitchNode();
      if ( this->geodes.size() == 1 )
      {
         // classic ss
         if ( !parentNode->SearchChild( temp ))
         {
            vprDEBUG(vesDBG,1) << "|\t\tadding active dcs node to worldDCS for classic ss "
                             << std::endl << vprDEBUG_FLUSH;
            parentNode->AddChild( temp );
         }
         vprDEBUG(vesDBG,1) << "|\t\tadding geode to active dataset dcs "
                             << std::endl << vprDEBUG_FLUSH;
         // we can do this because classic group is always
         // child 0 see line 58 of cfdModel.cxx
			VE_SceneGraph::Group* test = dynamic_cast< VE_SceneGraph::Group* >(temp->GetChild( 0 ));
         test->AddChild( this->geodes.back().get() );
         vprDEBUG(vesDBG,1) << "|\tFinished classic ss add to graph"
                             << std::endl << vprDEBUG_FLUSH;
      }
      /*
		else if ( this->geodes.size() > 1 && 
               ( !(model->GetAnimation()) || 
                 !(model->GetActiveDataSet()->IsPartOfTransientSeries() ) )
              )
      {
         // classic ss animated images, planes, tracked particles
         // even if model contains transient data
         this->animation = new VE_SceneGraph::cfdTempAnimation();
         this->animation->AddGeodesToSequence( this->geodes );
         if ( parentNode->SearchChild( temp ) < 0 )
         {
            vprDEBUG(vesDBG,1) << " adding active dcs node to worldDCS for classic ss animation"
                             << std::endl << vprDEBUG_FLUSH;
            parentNode->AddChild( temp );
         }
         //this->animation->SetDuration();
         temp->GetChild( 0 )->AddChild( this->animation->GetSequence() );
      }
      else if ( (this->geodes.size() > 1) && 
                  model->GetActiveDataSet()->IsPartOfTransientSeries()
              )
      {
         if ( this->worldNode->SearchChild( temp ) < 0 )
         {
            vprDEBUG(vesDBG,1) << " adding active switch node to worldDCS"
                             << std::endl << vprDEBUG_FLUSH;
            this->worldNode->AddChild( temp );
         }

         // classic transient data
         VE_SceneGraph::cfdTempAnimation* transAnimation = this->model->GetAnimation();
         // the following functions should be called upon creation in cfdModel
         transAnimation->AddGeodesToSequence( this->geodes );
         if(  temp->GetChild( 0 )->SearchChild( transAnimation->GetSequence() ) < 0 )
         {
            vprDEBUG(vesDBG,1) << " adding active dcs node to worldDCS for classic trans"
                             << std::endl << vprDEBUG_FLUSH;
            (temp->GetChild( 0 ))->AddChild( transAnimation->GetSequence() );
         }
      }
		*/
   }
   else if ( type == TEXTURE )
   {
      
   }
}

// set type of viz: trans, classic, texture
void cfdGraphicsObject::SetTypeOfViz( VizType x )
{
   this->type = x;
}

// set actor for classic and trans viz objects
void cfdGraphicsObject::SetGeodes( std::vector< osg::ref_ptr< VE_SceneGraph::Geode > > input )
{
   for ( unsigned int i = 0; i < input.size(); ++i )
   {
      //std::cout << "1 " << input.at( i ).get() << std::endl;
      this->geodes.push_back( new VE_SceneGraph::Geode( *input.at( i ) ) );
      //std::cout << input.at( i ).get() << std::endl;
      //Add phong shading to the geodes
      osg::ref_ptr<osg::StateSet> geodeProperties = geodes.at(i)->getOrCreateStateSet();
		VE_SceneGraph::Utilities::PhongLoader phongShader;
		phongShader.SetStateSet(geodeProperties.get());
		phongShader.SyncShaderAndStateSet();
   }
}

// Return parent node for a this object
VE_SceneGraph::DCS* cfdGraphicsObject::GetParentNode( void )
{
   return this->parentNode;
}

// Return parent node for a this object
void cfdGraphicsObject::SetActiveModel( cfdModel* input )
{
   model = input;
}

/*
void cfdGraphicsObject::AddGeodesToSequence( void )
{
   int nTransGeodes = 0; 
   
   int nGroups = animation->GetSequence()->GetNumChildren();
   int nDCSs = 0;
   int nGeodes = 0;
      
   std::cout<<"removing nodes from the sequence!!"<<std::endl;
   std::cout<<"cfdObjects::AddGeodesToSequence"<<std::endl;
   for(int i = 0; i < nGroups; i++)
   {
      tempGroup = (Group*)this->animation->GetSequence()->GetChild(i);
      nDCSs = tempGrouSetDurationp->GetNumChildren();
      std::cout<<"number of dcs: "<< nDCSs<<std::endl;
      for(int j = 0; j < nDCSs; j++)
      {
         tempDCS = (cfdDCS*)tempGroup->GetChild(j);
         nGeodes = tempDCS->GetNumChildren();
         for(int k = 0; k < nGeodes; k++)
         {
            std::cout<<"removing geode : "<< k<<std::endl;
            tempGeode = (VE_SceneGraph::Geode*)tempDCS->GetChild(0);
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
void cfdGraphicsObject::UpdateGeode( void )
{
   vprDEBUG(vesDBG, 1) << "cfdObjects::UpdateGeode..."
                           << std::endl << vprDEBUG_FLUSH;
   
   if ( this->updateFlag )
   {
      //check if current data set is transient
      if(this->activeDataSet->IsPartOfTransientSeries()){
         this->transientGeodes.push_back(new VE_SceneGraph::Geode());
   if ( this->usePreCalcData && this->PDactor != NULL )
   {
      vprDEBUG(vesDBG, 2) 
         << "cfdObjects::UpdateGeode... create geode from PDactor"
         << std::endl << vprDEBUG_FLUSH;

      // Function implements respective vtkActorToGeode function
      ((VE_SceneGraph::Geode*)this->transientGeodes.back())->TranslateToGeode( this->PDactor );
   }
   else
   {
      vprDEBUG(vesDBG, 2) 
         << "cfdObjects::UpdateGeode... create geode from actor"
         << std::endl << vprDEBUG_FLUSH;

      // Function implements respective vtkActorToGeode function
      ((VE_SceneGraph::Geode*)this->transientGeodes.back())->TranslateToGeode( this->actor );
   }
         //check if this is causing problems 
         if ( this->transientGeodes.size()%_sequence->GetNumberOfFrames() == 0 )
            this->addTransientGeode = true;
      }else{
         vprDEBUG(vesDBG, 1) << "cfdObjects::Allocate Geode..."
                           << updateFlag<< std::endl << vprDEBUG_FLUSH;
   
         this->_geode = new VE_SceneGraph::Geode();
         this->addGeode = true;      
      }
   }
   else
   {
      vprDEBUG(vesDBG, 0) 
         << "cfdObjects::UpdateGeode... updateFlag == false for ObjectType = "
         << this->objectType << std::endl << vprDEBUG_FLUSH;
   }
}

// This function simply adds the created geode from function UpdateGeode
void cfdGraphicsObject::AddGeodeToDCS( void )
{
   vprDEBUG(vesDBG, 1) << "cfdObjects::AddGeodeToDCS"
      << std::endl << vprDEBUG_FLUSH;
  
   vprDEBUG(vesDBG, 2) << "cfdObjects::UpdateGeode... updateFlag == true "
                           << this->_geodes.size() << std::endl << vprDEBUG_FLUSH;
   this->_geodes.push_back( this->_geode );
   vprDEBUG(vesDBG, 2) << "cfdObjects::UpdateGeode... pushback new geode"
                           << std::endl << vprDEBUG_FLUSH;

   if ( this->usePreCalcData && this->PDactor != NULL )
   {
      vprDEBUG(vesDBG, 2) 
         << "cfdObjects::UpdateGeode... create geode from PDactor"
         << std::endl << vprDEBUG_FLUSH;

      // Function implements respective vtkActorToGeode function
      ((VE_SceneGraph::Geode*)this->_geodes.back())->TranslateToGeode( this->PDactor );
   }
   else
   {
      vprDEBUG(vesDBG, 2) 
         << "cfdObjects::UpdateGeode... create geode from actor"
         << std::endl << vprDEBUG_FLUSH;

      // Function implements respective vtkActorToGeode function
      ((VE_SceneGraph::Geode*)this->_geodes.back())->TranslateToGeode( this->actor );
   }

   vprDEBUG(vesDBG, 2) 
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
      vprDEBUG(vesDBG,1) << " adding child num = " << num
                             << std::endl << vprDEBUG_FLUSH;
      vprDEBUG(vesDBG,1) << "this->geodes[ 0 ] = " << this->_geodes[ 0 ]
         << std::endl << "this->geodes[ num ] = " << this->_geodes[ num ]
         << std::endl << vprDEBUG_FLUSH;
      this->_dcs->AddChild( ((VE_SceneGraph::Geode*)this->_geodes[ num ]) );

      if ( this->_geodes.size() > 2 ) // remove oldest
      {
         int num = (this->_geodes.size() - 1) - 2;
         VE_SceneGraph::Group* parent = (Group*)this->_geodes[ num ]->GetParent(0);
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
      vprDEBUG(vesDBG,1) << " 1. removing child num = " << num << " : " << this->_geodes[ num ] << " : " << _geodes.size()
                             << std::endl << vprDEBUG_FLUSH;
      VE_SceneGraph::Group* parent = (Group*)this->_geodes[ num ]->GetParent(0);
      vprDEBUG(vesDBG,2) << " 1. removing child parent = " << parent 
                             << std::endl << vprDEBUG_FLUSH;
      parent->RemoveChild( this->_geodes[ num ] );
      vprDEBUG(vesDBG,2) << " 1. removing child succesful" 
                             << std::endl << vprDEBUG_FLUSH;
      delete this->_geodes[ num ];
      vprDEBUG(vesDBG,2) << " 1. delete child sucessful" 
                             << std::endl << vprDEBUG_FLUSH;

      this->_geodes.erase( this->_geodes.end() - 2 );
      vprDEBUG(vesDBG,2) << " 1. erase child succesful" 
                             << std::endl << vprDEBUG_FLUSH;
      this->_dcs->AddChild( ((VE_SceneGraph::Geode*)this->_geodes[ num ]) );
      vprDEBUG(vesDBG,1) << " 1. add child succesful " 
                             << std::endl << vprDEBUG_FLUSH;
   }
   else //if ( this->geodes.size() == 1 )
   { 
      vprDEBUG(vesDBG,1) << " adding child geode = " << this->_geodes.at( 0 )
                             << std::endl << vprDEBUG_FLUSH;
      this->_dcs->AddChild( ((VE_SceneGraph::Geode*)this->_geodes.at( 0 )) );     
   }
}
*/
void cfdGraphicsObject::RemoveGeodeFromDCS( void )
{
   if ( this->type == CLASSIC )
   {
      unsigned int num = this->geodes.size();
      for ( unsigned int i = 0; i < num; ++i )
      {
         // Need to find tha parent because with multiple models
         // Not all geodes are going to be on the same dcs
			VE_SceneGraph::Group* parent = dynamic_cast< VE_SceneGraph::Group* >(this->geodes.at( i )->GetParent(0));
         parent->RemoveChild( this->geodes.at( i ).get() );
#ifdef _PERFORMER
         delete this->geodes.at( i );
#endif
      }
      this->geodes.clear();

      /*if ( animation )
      {
         VE_SceneGraph::Group* parent = this->animation->GetSequence()->GetParent(0);
         parent->RemoveChild( this->animation->GetSequence() );
#ifdef _PERFORMER
         delete animation;
#endif
         animation = 0;
      }*/
   }
}

