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
#include "VE_Xplorer/SceneGraph/Group.h"

#ifdef _PERFORMER
#include <Performer/pf/pfGroup.h>
#include <Performer/pf/pfNode.h>
#elif _OSG
#include <osg/Group>
#include <osg/Node>
#elif _OPENSG
#endif

//C/C++ Libraries
#include <iostream>
#include <algorithm>
#include <string>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
Group::Group( const Group& input )
//:
//Node(input)
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
Group& Group::operator=( const Group& input)
{
   /*
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      Node::operator =(input);

      for ( unsigned int i = 0; i < childNodes.size(); i++ )
      {
         delete childNodes.at( i );
      }
      childNodes.clear();
   
      this->childNodes = input.childNodes;
#ifdef _PERFORMER
      pfDelete( this->_group );
      this->_group = input._group;
#elif _OSG
      _group = input._group;
#elif _OPENSG
#endif
      
   }
   */

   return *this;
   
}
////////////////////////////////////////////////////////////////////////////////
/*bool Group::operator== ( Node& node1 )
{
   if ( _dcs != dynamic_cast< Group& >( node1 )._group )
   {
      return false;
   }
   else
   {
      return true;
   }
   return true;
}*/
////////////////////////////////////////////////////////////////////////////////
Group::Group( void )
//:Node()
{
      ;
}
////////////////////////////////////////////////////////////////////////////////
Group::~Group( void )
{
   //If neccesary
   #ifdef _PERFORMER
   #elif _OSG
   #elif _OPENSG
   #endif
}

////////////////////////////////////////////////////////////////////////////////
/*
Node* Group::Clone( int )
{
   // Need to fix this
#ifdef _PERFORMER
   std::cerr << " ERROR: Group::Clone is NOT implemented " << std::endl;
   exit( 1 );
   return NULL;
#elif _OSG
   std::cerr << " ERROR: Group::Clone is NOT implemented " << std::endl;
   exit( 1 );
   return NULL;
#elif _OPENSG
   std::cerr << " ERROR: Group::Clone is NOT implemented " << std::endl;
   exit( 1 );
   return NULL;
#endif
}
*/
////////////////////////////////////////////////////////////////////////////////
