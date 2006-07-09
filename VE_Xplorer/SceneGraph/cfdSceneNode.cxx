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
#include "VE_Xplorer/SceneGraph/cfdSceneNode.h"
#include <iostream>
//#include "VE_Xplorer/cfdDebug.h"
#ifdef _PERFORMER
#include <Performer/pf/pfNode.h>
#elif _OSG
#include <osg/Node>
#include <osg/Geode>
#include <osg/Group>
#include <osg/MatrixTransform>
#include <osg/Switch>
#elif _OPENSG
#endif
using namespace VE_SceneGraph;
//////////////////////////////////
cfdSceneNode::cfdSceneNode( void )
{
   //this->_nodeType = -1;
   this->_numParents = 0;
   this->_parent = NULL;
   this->_nt = CFD_OTHER;
   /*this->_node = NULL;
   this->_group = NULL;
   this->_dcs = NULL;
   this->_geode = NULL;
   this->_sequence = NULL;
   this->_switch = 0;*/
   //guid = vpr::GUID::generateTag;
}
//////////////////////////////////////////
cfdSceneNode::cfdSceneNode(cfdNodeType nt)
{
   this->_nt = nt;
   //this->_nodeType = -1;
   this->_numParents = 0;
   this->_parent = NULL;
   /*this->_node = NULL;
   this->_group = NULL;
   this->_dcs = NULL;
   this->_geode = NULL;
   this->_sequence = NULL;
   this->_switch = 0;*/
   //guid = vpr::GUID::generateTag;
}
///////////////////////////////////
cfdSceneNode::~cfdSceneNode( void )
{
   //this->_nodeType = -1;
   this->_numParents = 0;
   this->_parent = NULL;
}
////////////////////////////////////////////////////////
cfdSceneNode::cfdSceneNode( const cfdSceneNode& input )
{
   this->_numParents = input._numParents;
   this->_parent = input._parent;
   _nt = input._nt;
   //guid = input.guid;
}
//////////////////////////////////////////////////////////////////
cfdSceneNode& cfdSceneNode::operator=( const cfdSceneNode& input )
{
   if ( this != &input )
   {
      this->_numParents = input._numParents;
      this->_parent = input._parent;
      _nt = input._nt;
      //guid = input.guid;
   }
   return *this;
}
//////////////////////////////////////
/*int cfdSceneNode::GetNodeType( void )
{
   return _nodeType;
}*/
//////////////////////////////////////////////
cfdNode* cfdSceneNode::GetParent( int parent )
{
   if ( parent > 0 )
   {
      std::cerr << " Error: GetParent " << std::endl;
      exit( 1 );   
   }

   return _parent;
}
/////////////////////////////////////////////////////
void cfdSceneNode::SetParent( cfdNode* parent )
{
   if ( parent == NULL )
   {
      this->_numParents -= 1;
   }
   else if ( parent != NULL )
   {
      this->_numParents += 1;
   }

   if ( this->_numParents > 1 )
   {
      // Is this really bad?
      //no -- cloning cause multiple parents
      ///dcs-original     dcs-clone
      ///  \                /
      ///   \              /
      ///    \            /
      ///      nodeToClone
      std::cerr << "Warning : SetParent "<< _numParents << " this may cause problems with cleanup and other graph manipulations." << std::endl;
      //exit( 1 );      
   }
   
   this->_parent = parent;
}


// Need to fix later
// Maybe shouldn't even be here
// seems to be a hack
/*
void cfdSceneNode::clearGeodesFromNode( pfNode * node )
{
   if ( node == NULL )
      return;

   const char * name = node->getName();

   if ( name != NULL )
   {
      vprDEBUG(vesDBG,1) << " node name = \"" << name << "\""
                             << std::endl << vprDEBUG_FLUSH;
   }

   if ( name != NULL && 
         (  ( ! strcmp(name, "scalarBar") ) || 
            ( ! strcmp(name, "cfdExecutive_Node") ) || 
            ( ! strcmp(name, "geometry") ) 
         ) 
      )
   {
      vprDEBUG(vesDBG,1) << "\twon't touch this node"
                             << std::endl << vprDEBUG_FLUSH;
      return;
   }

   if ( node->isOfType( pfGroup::getClassType() ) )
   {
      int numChildren = ((pfGroup*)node)->getNumChildren();
      vprDEBUG(vesDBG,1) << " group node has numChildren = " << numChildren
                             << std::endl << vprDEBUG_FLUSH;

      // Iterate backwards for performance
      for ( int i = numChildren-1; i >= 0; i-- )
      {
         pfNode * childNode = ((pfGroup*)node)->getChild( i );

         clearGeodesFromNode( childNode );
      }
   }
   else if ( node->isOfType( pfGeode::getClassType() ) )
   {
      vprDEBUG(vesDBG,1) << "\tremoving this node"
                             << std::endl << vprDEBUG_FLUSH;

      int numParents = node->getNumParents();
      if ( numParents != 1 )
      {
         vprDEBUG(vesDBG,1) << "\t!!!!!!!!numParents = " << numParents
                                << std::endl << vprDEBUG_FLUSH;
      }

      node->getParent( 0 )->removeChild( node );
      //pfDelete( node );  // don't do this
   }
   else
   {
      cout << "\twon't remove this node" << endl;
   }
}
*/
