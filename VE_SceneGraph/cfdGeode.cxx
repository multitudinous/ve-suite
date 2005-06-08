/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdGeode.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdGeode.h"

#ifdef _PERFORMER
#include <Performer/pf/pfGeode.h>
#include <Performer/pf/pfNode.h>
#include "vtkActorToPF.h"
#elif _OSG
#include <osg/Geode>
#include <osg/Node>
#include <osg/CopyOp>
#include "vtkActorToOSG.h"
#elif _OPENSG
#endif

#include <vtkActor.h>

#include <iostream>
#include <vpr/Util/Debug.h>

cfdGeode::cfdGeode( void )
:cfdNode()
{
#ifdef _PERFORMER
   _geode = new pfGeode();
#elif _OSG
   _geode = new osg::Geode();
#elif _OPENSG
#endif
   _vtkDebugLevel = 0;
   SetCFDNodeType(CFD_GEODE);
}
////////////////////////////////////////////
cfdGeode::cfdGeode( const cfdGeode& input )
:cfdNode(input)
{
#ifdef _PERFORMER
   this->_geode = input._geode;
#elif _OSG
   _geode = new osg::Geode(*input._geode,osg::CopyOp::DEEP_COPY_ALL);
#elif _OPENSG
#endif
   this->_vtkDebugLevel = input._vtkDebugLevel;
    
   SetCFDNodeType(CFD_GEODE);
}
//////////////////////////////////////////////////////
cfdGeode& cfdGeode::operator=( const cfdGeode& input )
{
   if ( this != (&input) )
   {
#ifdef _PERFORMER
      pfDelete( _geode );
      this->_geode = input._geode;
#elif _OSG
      _geode = input._geode;
#elif _OPENSG
#endif
      this->_vtkDebugLevel = input._vtkDebugLevel;
      SetCFDNodeType(CFD_GEODE);
   }
   return *this;
}
////////////////////////////////////////////////////
/*bool cfdGeode::operator== ( cfdNode& node1 )
{
   if ( _geode != dynamic_cast< cfdGeode& >( node1 )._geode )
   {
      return false;
   }

   return true;
}*/
///////////////////////////
cfdGeode::~cfdGeode( void )
{
   vprDEBUG(vprDBG_ALL,1) << " 1 destructor for cfdGeode " 
                           << std::endl << vprDEBUG_FLUSH;
#ifdef _PERFORMER
   // Fix this
   //if ( _geode != NULL )
   pfDelete( _geode );
   vprDEBUG(vprDBG_ALL,1) << " 2 destructor for cfdGeode " 
                           << std::endl << vprDEBUG_FLUSH;
#elif _OSG
#elif _OPENSG
#endif
}


//////////////////////////////////////////////////////
void cfdGeode::TranslateTocfdGeode( vtkActor* actor )
{
#ifdef _PERFORMER
   vtkActorToPF( actor, this->_geode, _vtkDebugLevel );
#elif _OSG
   vtkActorToOSG(actor,_geode.get(),_vtkDebugLevel);
#elif _OPENSG
#endif
}
// Reimplement for other graphs
#ifdef _PERFORMER
pfNode* cfdGeode::GetRawNode( void )
#elif _OSG
osg::Node* cfdGeode::GetRawNode(void)
#elif _OPENSG
#endif
{
#ifdef _PERFORMER
   return _geode;
#elif _OSG
   return _geode.get();
#elif _OPENSG
#endif
}
