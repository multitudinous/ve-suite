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
 * File:          $RCSfile: cfdSceneNode.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdSceneNode.h"
#include <iostream>
using namespace std;
#ifdef _PERFORMER
#include <Performer/pf/pfNode.h>
#elif _OSG
#elif _OPENSG
#endif

cfdSceneNode::cfdSceneNode( void )
{
   this->_nodeType = -1;
   this->_numParents = 0;
   this->_parent = NULL;
}

cfdSceneNode::~cfdSceneNode( void )
{
   this->_nodeType = -1;
   this->_numParents = 0;
   this->_parent = NULL;
}

cfdSceneNode::cfdSceneNode( const cfdSceneNode& input )
{
   this->_nodeType = input._nodeType;
   this->_numParents = input._numParents;
   this->_parent = input._parent;
}

cfdSceneNode& cfdSceneNode::operator=( const cfdSceneNode& input )
{
   if ( this != &input )
   {
      this->_nodeType = input._nodeType;
      this->_numParents = input._numParents;
      this->_parent = input._parent;
   }
   return *this;
}

int cfdSceneNode::GetNodeType( void )
{
   return _nodeType;
}

cfdSceneNode* cfdSceneNode::GetParent( int parent )
{
   if ( parent > 0 )
   {
      cout << " Error: GetParent " << endl;
      exit( 1 );   
   }

   return _parent;
}

void cfdSceneNode::SetParent( cfdSceneNode* parent )
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
      cout << "Error : SetParent "<< _numParents << endl;
      exit( 1 );      
   }
   
   this->_parent = parent;
}

