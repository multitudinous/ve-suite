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
 * File:          $RCSfile: cfdGroup.cxx,v $
 * Date modified: $Date: 2004-06-06 13:49:58 -0700 (Sun, 06 Jun 2004) $
 * Version:       $Rev: 451 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdNode.h"
#include <iostream>
#include <cstdlib>

using namespace std;

#ifdef _PERFORMER
#include <Performer/pf.h>
#include <Performer/pfdu.h>
#include <Performer/pfutil.h>
#include <Performer/pf/pfNode.h>
#elif _OSG
#elif _OPENSG
#endif
 
cfdNode::cfdNode( float* scale, float* trans, float* rot ):cfdSceneNode()
{
   //this->_group = new pfNode();
}

cfdNode::cfdNode( const cfdNode& input )
{
#ifdef _PERFORMER
   this->_node = input._node;
#elif _OSG
#elif _OPENSG
#endif
}

cfdNode& cfdNode::operator=( const cfdNode& input )
{
   if ( this != &input )
   {
#ifdef _PERFORMER
   pfDelete( this->_node );
   this->_node = input._node;
#elif _OSG
#elif _OPENSG
#endif
   }
   return *this;
}

cfdNode::cfdNode( void ):cfdSceneNode()
{
   //this->_group = new pfGroup();
}

cfdNode::~cfdNode( void )
{
   // If neccesary
#ifdef _PERFORMER
   pfDelete( this->_node );
#elif _OSG
#elif _OPENSG
#endif
}

// Reimplement for other graphs
#ifdef _PERFORMER
pfNode* cfdNode::GetRawNode( void )
#elif _OSG
#elif _OPENSG
#endif
{
#ifdef _PERFORMER
   return _node;
#elif _OSG
   cout << " Error:GetRawNode !!! " << endl;
   exit( 1 );
#elif _OPENSG
   cout << " Error:GetRawNode !!! " << endl;
   exit( 1 );
#endif
}

void cfdNode::LoadFile( char* filename )
{
#ifdef _PERFORMER
   this->_node = pfdLoadFile( filename );  // pfNode
#elif _OSG
   cout << " Error:LoadFile !!! " << endl;
   exit( 1 );
#elif _OPENSG
   cout << " Error:LoadFile !!! " << endl;
   exit( 1 );
#endif
}

cfdSceneNode* cfdNode::Clone( int level )
{
#ifdef _PERFORMER
   // fix this
   //this->_node->clone( level );
   exit( 1 );
   return NULL;
#elif _OSG
   cout << " Error:Clone !!! " << endl;
   exit( 1 );
   return NULL;
#elif _OPENSG
   cout << " Error:Clone !!! " << endl;
   exit( 1 );
   return NULL;
#endif
}

