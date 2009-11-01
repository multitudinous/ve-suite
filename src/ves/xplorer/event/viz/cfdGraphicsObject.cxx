/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
// --- VE-Suite Includes --- //
#include <ves/xplorer/event/viz/cfdGraphicsObject.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/event/viz/cfdStreamers.h>

#include <ves/xplorer/scenegraph/util/PhongLoader.h>
#include <ves/xplorer/Debug.h>

#include <ves/open/xml/Command.h>

// --- OSG Includes --- //
#include <osg/BlendFunc>
//#include <osgDB/WriteFile>
#include <osg/Sequence>

using namespace ves::xplorer;

////////////////////////////////////////////////////////////////////////////////
cfdGraphicsObject::cfdGraphicsObject()
{
    parentNode = 0;
    worldNode = 0;
    type = OTHER;
    model = 0;
}
////////////////////////////////////////////////////////////////////////////////
cfdGraphicsObject::~cfdGraphicsObject()
{
    type = OTHER;
}
////////////////////////////////////////////////////////////////////////////////
cfdGraphicsObject::cfdGraphicsObject( const cfdGraphicsObject& input )
{
    ;// do nothing yet
}
////////////////////////////////////////////////////////////////////////////////
cfdGraphicsObject& cfdGraphicsObject::operator=( const cfdGraphicsObject& input )
{
    // do nothing yet
    if( this != &input )
    {
        ;
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void cfdGraphicsObject::SetParentNode( ves::xplorer::scenegraph::DCS* input )
{
    parentNode = input;
}
////////////////////////////////////////////////////////////////////////////////
void cfdGraphicsObject::SetWorldNode( ves::xplorer::scenegraph::DCS* input )
{
    worldNode = input;
}
////////////////////////////////////////////////////////////////////////////////
/*
ves::xplorer::scenegraph::cfdTempAnimation* cfdGraphicsObject::GetAnimation()
{
    return animation;
}
*/
////////////////////////////////////////////////////////////////////////////////
void cfdGraphicsObject::AddGraphicsObjectToSceneGraph()
{
    if( type == CLASSIC )
    {
        // is parent on graph
        if( !worldNode->SearchChild( parentNode ) )
        {
            vprDEBUG( vesDBG, 1 ) << "|\t\tAdding active switch node to worldDCS"
            << std::endl << vprDEBUG_FLUSH;
            worldNode->AddChild( parentNode );
        }

        // is it transient, classic, or animated class
        // add animation or dcs
        ves::xplorer::scenegraph::Switch* temp = model->GetActiveDataSet()->GetSwitchNode();
        if( geodes.size() == 1 )
        {
            // classic ss
            if( !parentNode->SearchChild( temp ) )
            {
                vprDEBUG( vesDBG, 1 ) << "|\t\tAdding active dcs node to worldDCS for classic ss "
                << std::endl << vprDEBUG_FLUSH;
                parentNode->AddChild( temp );
            }

            vprDEBUG( vesDBG, 1 ) << "|\t\tAdding geode to active dataset dcs "
            << std::endl << vprDEBUG_FLUSH;

            // we can do this because classic group is always
            // child 0 see line 58 of cfdModel.cxx
            ves::xplorer::scenegraph::Group* test = dynamic_cast< ves::xplorer::scenegraph::Group* >( temp->GetChild( 0 ) );
            test->AddChild( geodes.back().get() );
            vprDEBUG( vesDBG, 1 ) << "|\tFinished classic ss add to graph"
            << std::endl << vprDEBUG_FLUSH;
        }
        // classic ss animated images, planes, tracked particles
        // even if model contains transient data
        else if( geodes.size() > 1 )
                //&& 
                //( !(model->GetAnimation() ) || 
                // !(model->GetActiveDataSet()->IsPartOfTransientSeries() ) ) )
        {
            ves::xplorer::scenegraph::Switch* temp = model->GetActiveDataSet()->GetSwitchNode();

            if( !parentNode->SearchChild( temp ) )
            {
                vprDEBUG(vesDBG,1) << "|\t\tAdding active dcs node to worldDCS for classic ss animation"
                    << std::endl << vprDEBUG_FLUSH;
                parentNode->AddChild( temp );
            }
            m_animation = new osg::Sequence();
            //m_animation->setDefaultTime( 1.0f );
            m_animation->
                setInterval( osg::Sequence::LOOP, 0, -1 );
            for( size_t i = 0; i < geodes.size(); ++i )
            {
                m_animation->addChild( geodes.at( i ).get(), 0.3f );
            }
            m_animation->setDuration( 1.0f, -1 );
            m_animation->setMode( osg::Sequence::START );

            ves::xplorer::scenegraph::Group* test = dynamic_cast< ves::xplorer::scenegraph::Group* >( temp->GetChild( 0 ) );
            test->addChild( m_animation.get() );
        }
        /*else if( ( geodes.size() > 1 ) && 
                 model->GetActiveDataSet()->IsPartOfTransientSeries() )
        {
            if( worldNode->SearchChild( temp ) < 0 )
            {
                vprDEBUG(vesDBG,1) << " adding active switch node to worldDCS"
                << std::endl << vprDEBUG_FLUSH;
                worldNode->AddChild( temp );
            }

            // classic transient data
            ves::xplorer::scenegraph::cfdTempAnimation* transAnimation = model->GetAnimation();
            // the following functions should be called upon creation in cfdModel
            transAnimation->AddGeodesToSequence( geodes );
            if(  temp->GetChild( 0 )->SearchChild( transAnimation->GetSequence() ) < 0 )
            {
                vprDEBUG(vesDBG,1) << " adding active dcs node to worldDCS for classic trans"
                << std::endl << vprDEBUG_FLUSH;
                (temp->GetChild( 0 ))->AddChild( transAnimation->GetSequence() );
            }
        }
        */
    }
    else if( type == TEXTURE )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdGraphicsObject::SetTypeOfViz( VizType x )
{
    type = x;
}
////////////////////////////////////////////////////////////////////////////////
void cfdGraphicsObject::SetGeodes( ves::xplorer::cfdObjects* input )
{
    bool isStreamLine = false;
    if( dynamic_cast< ves::xplorer::cfdStreamers* >( input ) )
    {
        isStreamLine = true;
    }
    std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > geodeList =
        input->GetGeodes();
        
    for( size_t i = 0; i < geodeList.size(); ++i )
    {
        if(  geodeList.at( i ).valid() )
        {
            geodes.push_back( geodeList.at( i ) );
        }

        if( !isStreamLine && !input->IsGPUTools() )
        {
            //Add phong shading to the geodes
            osg::ref_ptr< osg::StateSet > geodeProperties = geodes.at( i )->getOrCreateStateSet();
            ves::xplorer::scenegraph::util::PhongLoader phongShader;
            phongShader.SetStateSet( geodeProperties.get() );
            phongShader.SyncShaderAndStateSet();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* cfdGraphicsObject::GetParentNode()
{
    return parentNode;
}
////////////////////////////////////////////////////////////////////////////////
void cfdGraphicsObject::SetActiveModel( Model* input )
{
    model = input;
}
////////////////////////////////////////////////////////////////////////////////
/*
void cfdGraphicsObject::AddGeodesToSequence()
{
   int nTransGeodes = 0;

   int nGroups = animation->GetSequence()->GetNumChildren();
   int nDCSs = 0;
   int nGeodes = 0;

   std::cout<<"removing nodes from the sequence!!"<<std::endl;
   std::cout<<"cfdObjects::AddGeodesToSequence"<<std::endl;
   for(int i = 0; i < nGroups; i++)
   {
      tempGroup = (Group*)animation->GetSequence()->GetChild(i);
      nDCSs = tempGrouSetDurationp->GetNumChildren();
      std::cout<<"number of dcs: "<< nDCSs<<std::endl;
      for(int j = 0; j < nDCSs; j++)
      {
         tempDCS = (cfdDCS*)tempGroup->GetChild(j);
         nGeodes = tempDCS->GetNumChildren();
         for(int k = 0; k < nGeodes; k++)
         {
            std::cout<<"removing geode : "<< k<<std::endl;
            tempGeode = (ves::xplorer::scenegraph::Geode*)tempDCS->GetChild(0);
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
      tempGroup = animation->GetGroup(g);
      std::cout<<" adding to group: "<<g<<std::endl;
      nDCSs = tempGroup->GetNumChildren();
      for(int i = 0; i < nDCSs; i ++)
      {
         tempDCS = (cfdDCS*)tempGroup->GetChild(i);
         tempDCS->AddChild(transientGeodes.at(g));
      }
   }
   //_sequence->GetSequence()->setInterval( CFDSEQ_CYCLE, 0 , nTransGeodes - 1 );
   //_sequence->GetSequence()->setDuration( 0.1 * nTransGeodes );
   animation->StartSequence();
   parentNode->AddChild( animation );

   std::cout<<"finished adding to the sequence"<<std::endl;
   std::cout<<"cfdObjects::AddGeodesToSequence"<<std::endl;
}
////////////////////////////////////////////////////////////////////////////////
// This function just creates a geode from the actor for a particular
// visualization feature. It is not responsible for adding the
// newly created geode to the scene graph.
void cfdGraphicsObject::UpdateGeode()
{
   vprDEBUG(vesDBG, 1) << "cfdObjects::UpdateGeode..."
                           << std::endl << vprDEBUG_FLUSH;

   if(updateFlag )
   {
      //check if current data set is transient
      if(activeDataSet->IsPartOfTransientSeries()){
         transientGeodes.push_back(new ves::xplorer::scenegraph::Geode());
   if(usePreCalcData && PDactor != NULL )
   {
      vprDEBUG(vesDBG, 2)
         << "cfdObjects::UpdateGeode... create geode from PDactor"
         << std::endl << vprDEBUG_FLUSH;

      // Function implements respective vtkActorToGeode function
      ((ves::xplorer::scenegraph::Geode*)transientGeodes.back())->TranslateToGeode( PDactor );
   }
   else
   {
      vprDEBUG(vesDBG, 2)
         << "cfdObjects::UpdateGeode... create geode from actor"
         << std::endl << vprDEBUG_FLUSH;

      // Function implements respective vtkActorToGeode function
      ((ves::xplorer::scenegraph::Geode*)transientGeodes.back())->TranslateToGeode( actor );
   }
         //check if this is causing problems
         if(transientGeodes.size()%_sequence->GetNumberOfFrames() == 0 )
            addTransientGeode = true;
      }else{
         vprDEBUG(vesDBG, 1) << "cfdObjects::Allocate Geode..."
                           << updateFlag<< std::endl << vprDEBUG_FLUSH;

         _geode = new ves::xplorer::scenegraph::Geode();
         addGeode = true;
      }
   }
   else
   {
      vprDEBUG(vesDBG, 0)
         << "cfdObjects::UpdateGeode... updateFlag == false for ObjectType = "
         << objectType << std::endl << vprDEBUG_FLUSH;
   }
}
////////////////////////////////////////////////////////////////////////////////
// This function simply adds the created geode from function UpdateGeode
void cfdGraphicsObject::AddGeodeToDCS()
{
   vprDEBUG(vesDBG, 1) << "cfdObjects::AddGeodeToDCS"
      << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vesDBG, 2) << "cfdObjects::UpdateGeode... updateFlag == true "
                           << _geodes.size() << std::endl << vprDEBUG_FLUSH;
   _geodes.push_back( _geode );
   vprDEBUG(vesDBG, 2) << "cfdObjects::UpdateGeode... pushback new geode"
                           << std::endl << vprDEBUG_FLUSH;

   if(usePreCalcData && PDactor != NULL )
   {
      vprDEBUG(vesDBG, 2)
         << "cfdObjects::UpdateGeode... create geode from PDactor"
         << std::endl << vprDEBUG_FLUSH;

      // Function implements respective vtkActorToGeode function
      ((ves::xplorer::scenegraph::Geode*)_geodes.back())->TranslateToGeode( PDactor );
   }
   else
   {
      vprDEBUG(vesDBG, 2)
         << "cfdObjects::UpdateGeode... create geode from actor"
         << std::endl << vprDEBUG_FLUSH;

      // Function implements respective vtkActorToGeode function
      ((ves::xplorer::scenegraph::Geode*)_geodes.back())->TranslateToGeode( actor );
   }

   vprDEBUG(vesDBG, 2)
      << "cfdObjects::UpdateGeode... set active geode pointer "
      << GetActiveDataSet()->IsNewlyActivated() << " : " << _geodes.size() << std::endl << vprDEBUG_FLUSH;
   //geode = (pfGeode *)geodes.back();
   //geode = tempGeode;
   //updateFlag = true;

   if(GetActiveDataSet()->IsNewlyActivated() )
   {
      // add new with old
      GetActiveDataSet()->SetNotNewlyActivated();
      // geodes.size is not zero based therefore the -1 is needed
      int num = _geodes.size() - 1;
      vprDEBUG(vesDBG,1) << " adding child num = " << num
                             << std::endl << vprDEBUG_FLUSH;
      vprDEBUG(vesDBG,1) << "geodes[ 0 ] = " << _geodes[ 0 ]
         << std::endl << "geodes[ num ] = " << _geodes[ num ]
         << std::endl << vprDEBUG_FLUSH;
      _dcs->AddChild( ((ves::xplorer::scenegraph::Geode*)_geodes[ num ]) );

      if(_geodes.size() > 2 ) // remove oldest
      {
         int num = (_geodes.size() - 1) - 2;
         ves::xplorer::scenegraph::Group* parent = (Group*)_geodes[ num ]->GetParent(0);
         parent->RemoveChild(_geodes[ num ] );
         delete _geodes[ num ];
         _geodes.erase( _geodes.end() - 3 );
      }
   }
   else if(_geodes.size() > 1 ) // replace old with new
   {
      // geodes.size is not zero based therefore the first -1 is needed
      // the second -1 is to get the second to last geode on the list
      int num = (_geodes.size() - 1) - 1;
      vprDEBUG(vesDBG,1) << " 1. removing child num = " << num << " : " << _geodes[ num ] << " : " << _geodes.size()
                             << std::endl << vprDEBUG_FLUSH;
      ves::xplorer::scenegraph::Group* parent = (Group*)_geodes[ num ]->GetParent(0);
      vprDEBUG(vesDBG,2) << " 1. removing child parent = " << parent
                             << std::endl << vprDEBUG_FLUSH;
      parent->RemoveChild( _geodes[ num ] );
      vprDEBUG(vesDBG,2) << " 1. removing child succesful"
                             << std::endl << vprDEBUG_FLUSH;
      delete _geodes[ num ];
      vprDEBUG(vesDBG,2) << " 1. delete child sucessful"
                             << std::endl << vprDEBUG_FLUSH;

      _geodes.erase( _geodes.end() - 2 );
      vprDEBUG(vesDBG,2) << " 1. erase child succesful"
                             << std::endl << vprDEBUG_FLUSH;
      _dcs->AddChild( ((ves::xplorer::scenegraph::Geode*)_geodes[ num ]) );
      vprDEBUG(vesDBG,1) << " 1. add child succesful "
                             << std::endl << vprDEBUG_FLUSH;
   }
   else //if ( geodes.size() == 1 )
   {
      vprDEBUG(vesDBG,1) << " adding child geode = " << _geodes.at( 0 )
                             << std::endl << vprDEBUG_FLUSH;
      _dcs->AddChild( ((ves::xplorer::scenegraph::Geode*)_geodes.at( 0 )) );
   }
}
*/
////////////////////////////////////////////////////////////////////////////////
void cfdGraphicsObject::RemoveGeodeFromDCS()
{
    if( type == CLASSIC )
    {
        for( size_t i = 0; i < geodes.size(); ++i )
        {
            // Need to find tha parent because with multiple models
            // Not all geodes are going to be on the same dcs
            ves::xplorer::scenegraph::Group* parent =
                dynamic_cast< ves::xplorer::scenegraph::Group* >
                ( geodes.at( i )->GetParent( 0 ) );
            if( parent )
            {
                parent->RemoveChild( geodes.at( i ).get() );
            }
        }

        geodes.clear();

        if( m_animation.valid() )
        {
            osg::Group* parent = m_animation->getParent(0);
            parent->removeChild( m_animation.get() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > cfdGraphicsObject::GetGeodes()
{
    return geodes;
}
////////////////////////////////////////////////////////////////////////////////
