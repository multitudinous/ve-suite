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
 * File:          $RCSfile: cfdApp.cxx,v $
 * Date modified: $Date: 2004-08-02 12:13:03 -0500 (Mon, 02 Aug 2004) $
 * Version:       $Rev: 734 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdPfSceneManagement.h"
#include <sys/types.h>
#ifndef WIN32
#include <unistd.h>
#else
//#include <windows.h>
#endif
//#include <malloc.h>
/// Performer libraries
#include <Performer/pr/pfGeoState.h>
#include <Performer/pf/pfChannel.h>
#include <Performer/pf/pfEarthSky.h>
#include <Performer/pf/pfLightSource.h>
#include <Performer/pf/pfNode.h>
#include <Performer/pf/pfTraverser.h>
//#include <Performer/pf/pfDCS.h>
#include <Performer/pfdu.h>
#include <Performer/pfutil.h>
//#include <Performer/pf/pfGeode.h>
#include <Performer/pr/pfGeoSet.h>
#include <Performer/pr/pfMaterial.h>
#include <Performer/pr.h>
#include <Performer/pf/pfSwitch.h>

#ifndef WIN32
//biv--check here if build/run problems occur
#include <Performer/pfdb/pfiv.h>
#endif
#include <Performer/pr/pfLight.h>
#include <Performer/pf/pfSequence.h>


#include <iostream>

#include "cfdDCS.h"
#include "cfdGroup.h"

cfdPfSceneManagement::cfdPfSceneManagement( char* param )
{
   worldNode = NULL;
   sunModel = NULL;
   sun = NULL;
   lit = NULL;
   gstate = NULL;
   rootNode = NULL;
   worldDCS = NULL;
   activeDataSetDCS = NULL;
}

cfdPfSceneManagement::~cfdPfSceneManagement( void )
{
}

void cfdPfSceneManagement::InitScene( void )
{
# ifdef _IRIX
   std::cout << "|   Performer Arena Size *** " << pfGetSharedArenaSize()/ (1024 * 1024) << std::endl;
   std::cout << "|   Shared arena base is *** " << pfGetSharedArenaBase() << std::endl;
   amallopt(M_MXCHK,10000000,pfGetSharedArena());
   amallopt(M_FREEHD, 1, pfGetSharedArena() );
#endif
   //
   // Establish Iris Performer Scenegraph.
   //
   std::cout << "|  1. Initializing................................ Performer scenes |" << std::endl;
   // Setup performer pipeline
   this->sunModel = new pfLightModel();
   this->rootNode = new cfdGroup();
   this->worldDCS = new cfdDCS();
   this->sun      = new pfLightSource();
   //this->lit      = new pfLightSource();
   this->gstate   = new pfGeoState();
   //this->scene    = new pfScene();

   // Setup geosets properties
   //this->sunModel->setTwoSide( PF_ON );
   //this->sunModel->apply();

   /* Override so that all geometry is lit with 'lmodel' */
   //pfOverride(PFSTATE_LIGHTMODEL, PF_ON);
   //this->gstate->setMode( PFSTATE_ENLIGHTING, PF_ON );
   //this->gstate->setMode( PFSTATE_ANTIALIAS, PFAA_ON );
   //this->gstate->setAttr( PFSTATE_LIGHTMODEL, sunModel );
   //this->gstate->setMode( PFSTATE_CULLFACE, PFCF_OFF );
   //this->scene->setGState( this->gstate );
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

   //this->lit->setPos( 100.0f, 0.0f, 0.0f, 0.0f );
   //this->sun->setPos( 0.0f, -1.0f, 0.0f, 0.0f );
   //this->lit->setColor( PFLT_DIFFUSE, 0.64f, 0.64f, 0.64f );
   //this->lit->setColor( PFLT_AMBIENT, 0.0f, 0.0f, 0.0f );
   //this->sun->setColor( PFLT_SPECULAR, 1.0f, 1.0f, 1.0f );
   //this->lit->setColor( PFLT_SPECULAR, 0.64f, 0.64f, 0.64f );
   //this->lit->on();
   // Add pfDCS and sun for the world
   this->rootNode->AddChild( (cfdSceneNode*)this->worldDCS );
   // TODO: Might need to add this back in
   //this->rootNode->addChild( this->sun );

   //this->rootNode->setGState( this->gstate );
   //this->rootNode->addChild( this->lit );
   //this->temp_text = new pfDCS();
}

cfdGroup* cfdPfSceneManagement::GetRootNode( void )
{
   return this->rootNode;
}

cfdDCS* cfdPfSceneManagement::GetWorldDCS( void )
{
   return this->worldDCS;
}
