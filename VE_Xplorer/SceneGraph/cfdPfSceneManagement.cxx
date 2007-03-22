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
#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"

#include "VE_Xplorer/SceneGraph/Text.h"
#include "VE_Xplorer/SceneGraph/Triangles.h"
#include "VE_Xplorer/SceneGraph/CADEntity.h"

/// Performer libraries
#ifdef _PERFORMER
#include <Performer/pf/pfLightSource.h>
#include <Performer/pr/pfLight.h>
#include <Performer/pf/pfGroup.h>
#include <Performer/pf/pfNode.h>
#elif _OSG
#include <osg/Group>
#include <osg/Node>
#include <osgDB/Registry>
#include <osgDB/ReaderWriter>
#include <osgDB/WriteFile>
#include <osg/PositionAttitudeTransform>
#include <osg/MatrixTransform>
#include <osg/Switch>
#endif

#ifndef WIN32
#ifdef _PERFORMER
#include <malloc.h>
#endif
#include <sys/types.h>
//biv--check here if build/run problems occur
//#include <Performer/pfdb/pfiv.h>
#else
//#include <windows.h>
#endif

//C/C++ Libraries
#include <iostream>
#include <string>
#include <istream>
#include <sstream>

using namespace VE_SceneGraph;

vprSingletonImp(cfdPfSceneManagement );

cfdPfSceneManagement::cfdPfSceneManagement( void )
{
   this->_param.erase();// = 0;

   _textPart = 0;
   _movingPyramidsAssembly = 0;

#ifdef _PERFORMER
   this->sunModel = 0;
   this->sun = 0;
   this->lit = 0;
#endif
}
//////////////////////////////////////////////////////////
void cfdPfSceneManagement::Initialize( std::string param )
{
   _param = param;
}
///////////////////////////////////////////////////
void cfdPfSceneManagement::CleanUp( void )
{
   // Do nothing right now
   if(_textPart)
   {
      delete _textPart;
      _textPart = 0;
   }
	
   if(_movingPyramidsAssembly)
   {
      delete _movingPyramidsAssembly;
      _movingPyramidsAssembly = 0;
   }
}

void cfdPfSceneManagement::InitScene( void )
{
#ifdef _IRIX 
#ifdef _PERFORMER
   std::cout << "|   Performer Arena Size *** " << pfGetSharedArenaSize()/ (1024 * 1024) << std::endl;
   std::cout << "|   Shared arena base is *** " << pfGetSharedArenaBase() << std::endl;
   amallopt(M_MXCHK,10000000,pfGetSharedArena());
   amallopt(M_FREEHD, 1, pfGetSharedArena() );
#endif
#endif
   //
   // Establish Iris Performer Scenegraph.
   //
   std::cout << "|  1. Initializing................................ Performer scenes |" << std::endl;
   
   // Setup performer pipeline
#ifdef _PERFORMER
   this->sunModel = new pfLightModel();
   this->sun      = new pfLightSource();
   // Create lights
   this->sun->setPos( 100.0f, -100.0f, 100.0f, 0.0f );
   //this->sun->setPos( 0.0f, -1.0f, 0.0f, 0.0f );
   //this->sun->setColor( PFLT_DIFFUSE, 0.64f, 0.64f, 0.64f );
   this->sun->setColor( PFLT_DIFFUSE, 1.0f, 1.0f, 1.0f );
   //this->sun->setColor( PFLT_AMBIENT, 0.0f, 0.0f, 0.0f );
   this->sun->setColor( PFLT_AMBIENT, 0.4f, 0.4f, 0.4f );
   //this->sun->setColor( PFLT_SPECULAR, 0.64f, 0.64f, 0.64f );
   this->sun->setColor( PFLT_SPECULAR, 1.0f, 1.0f, 1.0f );
   this->sun->setVal(PFLS_INTENSITY, 1.0);
   this->sun->on();
#endif

	this->rootNode = new VE_SceneGraph::Group();
   this->rootNode->SetName( "Root Node" );
	this->worldDCS = new VE_SceneGraph::DCS();
   this->worldDCS->SetName( "World DCS" );
	networkDCS  = new VE_SceneGraph::DCS();
   networkDCS->SetName( "Network DCS" );

   //create the switch for our logo
   _createLogo();

   _logoSwitch->AddChild( worldDCS.get() );
	_logoSwitch->AddChild( _logoNode.get() );
   _logoSwitch->AddChild( networkDCS.get() );

   //Now lets put it on the main group node
   //remember that the logo switch is right below the group node 
   //NOT the world dcs
   rootNode->AddChild( _logoSwitch.get() );

#ifdef _PERFORMER
   ((pfGroup*)(this->rootNode->GetRawNode()))->addChild( this->sun );
#endif
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::Group* cfdPfSceneManagement::GetRootNode( void )
{
   return this->rootNode.get();
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* cfdPfSceneManagement::GetWorldDCS( void )
{
   return this->worldDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* cfdPfSceneManagement::GetNetworkDCS( void )
{
   return networkDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
void cfdPfSceneManagement::ViewLogo(bool trueFalse)
{
   if ( trueFalse )
   {
      SetActiveSwitchNode( 1 );
   }
   else
   {
      SetActiveSwitchNode( 0 );
   }
}
////////////////////////////////////////////////////////////////////////////////
void cfdPfSceneManagement::_createLogo()
{
#ifdef _OSG
   if(!_logoSwitch)
   {
      _logoSwitch = new VE_SceneGraph::Switch();   
   }
   
   if ( !_logoNode.valid() )
   {
		_logoNode = new VE_SceneGraph::DCS();
      float translation[3] = {0,5,4};
      float scale[3] = {.02,.02,.02};
      _logoNode->SetTranslationArray(translation);
      _logoNode->SetScaleArray(scale);

		//VE_SceneGraph::CADEntity* 
      //_textPart = new VE_SceneGraph::CADEntityHelper();  

		//VE_SceneGraph::CADEntity* 
      //_movingPyramidsAssembly = new VE_SceneGraph::CADEntityHelper();  

      _textPart = new VE_SceneGraph::CADEntity(GetVESuite_Text(),_logoNode.get(),true);
      _movingPyramidsAssembly = new VE_SceneGraph::CADEntity(GetVESuite_Triangles(),_logoNode.get(),true);
   }

#elif _PERFORMER
#endif
}
////////////////////////////////////////////////////////////////////////////////
void cfdPfSceneManagement::SetActiveSwitchNode( int activeNode )
{
   _logoSwitch->SetVal( activeNode );
}
////////////////////////////////////////////////////////////////////////////////
void cfdPfSceneManagement::PreFrameUpdate( void )
{
   networkDCS->SetTranslationArray( worldDCS->GetVETranslationArray() );
   networkDCS->SetRotationArray( worldDCS->GetRotationArray() );
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* cfdPfSceneManagement::GetActiveSwitchNode( void )
{
   osg::Switch::ValueList boolList = _logoSwitch->getValueList();
   
   for ( size_t i = 0; i < boolList.size(); ++i )
   {
      if ( boolList.at( i ) )
      {
         return dynamic_cast< VE_SceneGraph::DCS* >( _logoSwitch->getChild( i ) );
      }
   }
}
