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
 * Date modified: $Date: 2006-07-08 22:57:46 -0500 (Sat, 08 Jul 2006) $
 * Version:       $Rev: 4907 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/Geode.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#ifdef _PERFORMER
#include <Performer/pf/pfGeode.h>
#include <Performer/pf/pfNode.h>
#include "VE_Xplorer/SceneGraph/vtkActorToPF.h"
#elif _OSG
#include "VE_Xplorer/SceneGraph/vtkActorToOSG.h"

#include <osg/Geode>
#include <osg/Node>
#include <osg/CopyOp>
#elif _OPENSG
#endif

#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkMapper.h>
#include <vtkPolyDataMapper.h>
#include <vtkDataSet.h>

//C/C++ Libraries
#include <iostream>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
Geode::Geode( void )
//:
//Node()
{
   #ifdef _PERFORMER
   _geode = new pfGeode();
   #elif _OSG
   _geode = new osg::Geode();
   #elif _OPENSG
   #endif

   _vtkDebugLevel = 0;

   //SetVENodeType(VE_GEODE);
}
#ifdef _OSG
////////////////////////////////////////////////////////////////////////////////
Geode::Geode(const osg::Geode& oGeode)
{
   _geode = new osg::Geode(oGeode,osg::CopyOp::DEEP_COPY_ALL);
   _vtkDebugLevel = 0;
   //SetVENodeType(VE_GEODE);
}
////////////////////////////////////////////////////////////////////////////////
Geode& Geode::operator=(const osg::Geode& oGeode)
{
   if(_geode != &oGeode){
      if(_geode.valid()){
         _geode = dynamic_cast<osg::Geode*>(oGeode.clone(osg::CopyOp::DEEP_COPY_ALL));
      }
   }
   return *this;
}
#elif _PERFORMER
////////////////////////////////////////////////////////////////////////////////
Geode::Geode(const pfGeode& oGeode)
{
   _geode = new pfGeode(oGeode);
   _vtkDebugLevel = 0;
   SetNodeType(VE_GEODE);
}
////////////////////////////////////////////////////////////////////////////////
Geode& Geode::operator=(const pfGeode& oGeode)
{
   if(_geode != &oGeode){
      //_geode = dynamic_cast<pfGeode*>(oGeode.clone(0));
   }
   return *this;
}
#elif _OPENSG
#endif
////////////////////////////////////////////////////////////////////////////////
Geode::Geode( const Geode& input )
//:Node(input)
{
#ifdef _PERFORMER
   this->_geode = input._geode;
#elif _OSG
   _geode = new osg::Geode(*input._geode,osg::CopyOp::DEEP_COPY_ALL);
#elif _OPENSG
#endif
   this->_vtkDebugLevel = input._vtkDebugLevel;
    
   //SetVENodeType(VE_GEODE);
}
////////////////////////////////////////////////////////////////////////////////
Geode& Geode::operator=( const Geode& input )
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
      //SetVENodeType(VE_GEODE);
   }
   return *this;
}
////////////////////////////////////////////////////////////////////////////////
/*bool Geode::operator== ( cfdNode& node1 )
{
   if ( _geode != dynamic_cast< Geode& >( node1 )._geode )
   {
      return false;
   }

   return true;
}*/
////////////////////////////////////////////////////////////////////////////////
Geode::~Geode( void )
{
   vprDEBUG(vesDBG,2) << "|\tdestructor for Geode " 
                           << std::endl << vprDEBUG_FLUSH;
#ifdef _PERFORMER
   // Fix this
   //if ( _geode != NULL )
   pfDelete( _geode );
   vprDEBUG(vesDBG,2) << "|\tAfter pfDelete : destructor for Geode " 
                           << std::endl << vprDEBUG_FLUSH;
#elif _OSG
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
void Geode::TranslateToGeode( vtkActor* actor )
{
#ifdef _PERFORMER
   VE_SceneGraph::vtkActorToPF( actor, this->_geode, _vtkDebugLevel );
#elif _OSG
   VE_SceneGraph::vtkActorToOSG(actor, _geode,_vtkDebugLevel);
#elif _OPENSG
#endif
}
// Reimplement for other graphs
#ifdef _PERFORMER
pfNode* Geode::GetRawNode( void )
#elif _OSG
osg::Node* Geode::GetRawNode(void)
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
////////////////////////////////////////////////////////////////////////////////
