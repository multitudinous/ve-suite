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
 * File:          $RCSfile: cfdPfSceneManagement.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdPfSceneManagement.h"

/// Performer libraries
#ifdef _PERFORMER
#include <Performer/pf/pfLightSource.h>
#include <Performer/pr/pfLight.h>
#include <Performer/pf/pfGroup.h>
#include <Performer/pf/pfNode.h>
#elif _OSG
#include <osg/Group>
#include <osg/Node>
#endif
#include "cfdNode.h"
#include "cfdGroup.h"

#ifndef WIN32
#include <malloc.h>
#include <sys/types.h>
//biv--check here if build/run problems occur
//#include <Performer/pfdb/pfiv.h>
#else
//#include <windows.h>
#endif


#include <iostream>

#include "cfdDCS.h"
#include "cfdGroup.h"

cfdPfSceneManagement::cfdPfSceneManagement( void )
{
   this->_param = 0;
   this->rootNode = 0;
   this->worldDCS = 0;
#ifdef _PERFORMER
   this->sunModel = 0;
   this->sun = 0;
   this->lit = 0;
#elif _OSG
   transformDCS = new cfdDCS();
   float rot[ 3 ];
   rot[ 0 ] = 0;
   rot[ 1 ] = -90;
   rot[ 2 ] = 0;
   transformDCS->SetRotationArray( rot );
#endif
}

void cfdPfSceneManagement::Initialize( char* param )
{
   _param = param;
}

///////////////////////////////////////////////////
void cfdPfSceneManagement::CleanUp( void )
{
   // Do nothing right now
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
#endif
   this->rootNode = new cfdGroup();
   this->rootNode->SetName( "Root Node" );
   this->worldDCS = new cfdDCS();
   this->worldDCS->SetName( "World DCS" );
#ifdef _PERFORMER
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

   // Add pfDCS and sun for the world

#ifdef _PERFORMER
   this->rootNode->AddChild( this->worldDCS );
   // TODO: Might need to add this back in
#elif _OSG
   this->rootNode->AddChild( this->transformDCS );   
   this->transformDCS->AddChild( this->worldDCS );   
   // We have to do this o "trick" the user into thinking that the coordinate
   // system is he same as performers or Z up, X right, Y into the screen
   // DO NOT REMOVE OR CHANGE!!!!!!!!!!!!!!!!!!!!!!
#endif
#ifdef _PERFORMER
   ((pfGroup*)(this->rootNode->GetRawNode()))->addChild( this->sun );
#endif
}

cfdGroup* cfdPfSceneManagement::GetRootNode( void )
{
   return this->rootNode;
}

cfdDCS* cfdPfSceneManagement::GetWorldDCS( void )
{
   return this->worldDCS;
}
