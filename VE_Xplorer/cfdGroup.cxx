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
#include "cfdGroup.h"
#include <iostream>
#include <algorithm>
#include <cstdlib>
using namespace std;

#ifdef _PERFORMER
#include <Performer/pf/pfGroup.h>
#include <Performer/pf/pfNode.h>
#elif _OSG
#elif _OPENSG
#endif

cfdGroup::cfdGroup( float* scale, float* trans, float* rot ):cfdSceneNode()
{
#ifdef _PERFORMER
   this->_group = new pfGroup();
#elif _OSG
#elif _OPENSG
#endif
}

cfdGroup::cfdGroup( const cfdGroup& input )
{
   (*this->_translation) = (*input._translation);
   (*this->_rotation) = (*input._rotation);
   (*this->_scale) = (*input._scale);
   
   this->childNodes = input.childNodes;
#ifdef _PERFORMER
   this->_group = input._group;
#elif _OSG
#elif _OPENSG
#endif
}

cfdGroup& cfdGroup::operator=( const cfdGroup& input)
{
   if ( this != &input )
   {
      (*this->_translation) = (*input._translation);
      (*this->_rotation) = (*input._rotation);
      (*this->_scale) = (*input._scale);
   
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
#elif _OPENSG
#endif
   }
   return *this;
}

cfdGroup::cfdGroup( void ):cfdSceneNode()
{
#ifdef _PERFORMER
   this->_group = new pfGroup();
#elif _OSG
#elif _OPENSG
#endif
}

cfdGroup::~cfdGroup( void )
{
   for ( unsigned int i = 0; i < childNodes.size(); i++ )
   {
      delete childNodes.at( i );
   }   
   childNodes.clear();
   
   // If neccesary
#ifdef _PERFORMER
   pfDelete ( this->_group );
#elif _OSG
#elif _OPENSG
#endif
}

int cfdGroup::RemoveChild( cfdSceneNode* child )
{
#ifdef _PERFORMER
   vector< cfdSceneNode* >::iterator oldChild;
   oldChild = std::find( childNodes.begin(), childNodes.end(), child );
   
   // Check to make sure he is on this node
   if ( oldChild != childNodes.end() )
   {
      this->_group->removeChild( (*oldChild)->GetRawNode() );
      child->SetParent( NULL );
      return 1;  
   }
   else
   {
      cout << " Child Not found " << endl;
      return -1;
   }
#elif _OSG
   cerr << " ERROR: cfdGroup::RemoveChild is NOT implemented " << endl;
   exit( 1 );
#elif _OPENSG
   cerr << " ERROR: cfdGroup::RemoveChild is NOT implemented " << endl;
   exit( 1 );
#endif
}

int cfdGroup::AddChild( cfdSceneNode* child )
{
#ifdef _PERFORMER
   childNodes.push_back( child );
   this->_group->addChild( child->GetRawNode() );
   child->SetParent( this );
   return 1;
#elif _OSG
   cerr << " ERROR: cfdGroup::AddChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OPENSG
   cerr << " ERROR: cfdGroup::AddChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#endif
}

void cfdGroup::InsertChild( int position, cfdSceneNode* child )
{
#ifdef _PERFORMER
   this->_group->insertChild( position, child->GetRawNode() );
   vector< cfdSceneNode* >::iterator newPosition;
   newPosition = std::find( childNodes.begin(), childNodes.end(), childNodes[ position ] );
   childNodes.insert( newPosition, child );
   child->SetParent( this );
#elif _OSG
   cerr << " ERROR: cfdGroup::InsertChild is NOT implemented " << endl;
   exit( 1 );
#elif _OPENSG
   cerr << " ERROR: cfdGroup::InsertChild is NOT implemented " << endl;
   exit( 1 );
#endif
}

int  cfdGroup::SearchChild( cfdSceneNode* child )
{
#ifdef _PERFORMER
   for ( unsigned int i = 0; i < childNodes.size(); i++ )
      if ( childNodes[ i ] == child )
      {
         return (int)i;
      }
   // if not found
   return -1;
#elif _OSG
   cerr << " ERROR: cfdGroup::SearchChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OPENSG
   cerr << " ERROR: cfdGroup::SearchChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#endif
}

cfdSceneNode* cfdGroup::GetChild( int child )
{
#ifdef _PERFORMER
   return childNodes.at( child );
#elif _OSG
   cerr << " ERROR: cfdGroup::GetChild is NOT implemented " << endl;
   exit( 1 );
   return NULL;
#elif _OPENSG
   cerr << " ERROR: cfdGroup::GetChild is NOT implemented " << endl;
   exit( 1 );
   return NULL;
#endif
}

// Reimplement for other graphs
#ifdef _PERFORMER
pfNode* cfdGroup::GetRawNode( void )
#elif _OSG
#elif _OPENSG
#endif
{
#ifdef _PERFORMER
   return _group;
#elif _OSG
   cerr << " ERROR: cfdGroup::GetRawNode is NOT implemented " << endl;
   exit( 1 );
   return NULL;
#elif _OPENSG
   cerr << " ERROR: cfdGroup::GetRawNode is NOT implemented " << endl;
   exit( 1 );
   return NULL;
#endif
}

int cfdGroup::GetNumChildren( void )
{
#ifdef _PERFORMER
   int numChildren = this->_group->getNumChildren();
   if ( numChildren != (int) this->childNodes.size() )
   {
      cout << " ERROR: Number of children don't equal " << endl;
      exit( 1 );
   }
   return numChildren;
#elif _OSG
   cerr << " ERROR: cfdGroup::GetNumChildren is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OPENSG
   cerr << " ERROR: cfdGroup::GetNumChildren is NOT implemented " << endl;
   exit( 1 );
   return -1;
#endif
}

void cfdGroup::SetName( char* name )
{
#ifdef _PERFORMER
   this->_group->setName( name );
#elif _OSG
   cerr << " ERROR: cfdGroup::SetName is NOT implemented " << endl;
   exit( 1 );
#elif _OPENSG
   cerr << " ERROR: cfdGroup::SetName is NOT implemented " << endl;
   exit( 1 );
#endif
}

int cfdGroup::ReplaceChild( cfdSceneNode* childToBeReplaced, cfdSceneNode* newChild)
{
#ifdef _PERFORMER
   vector< cfdSceneNode* >::iterator oldChild;
   oldChild = find( childNodes.begin(), childNodes.end(), childToBeReplaced );
   
   // Check to make sure he is on this node
   if ( oldChild != childNodes.end() )
   {
      // Just erases from the vector doesn't delete memory
      childNodes.erase( oldChild );
      this->_group->replaceChild( childToBeReplaced->GetRawNode(), 
                                                newChild->GetRawNode() );

      // Set new parent for the new child
      newChild->SetParent( this );
      // Show that he no longer has a parent
      childToBeReplaced->SetParent( NULL );
      return 1;
   }
   else
   {
      cout << " Error : Child not found " << endl;
      return -1;
   }
#elif _OSG
   cerr << " ERROR: cfdGroup::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OPENSG
   cerr << " ERROR: cfdGroup::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#endif
}

cfdSceneNode* cfdGroup::Clone( int )
{
   // Need to fix this
#ifdef _PERFORMER
   cerr << " ERROR: cfdGroup::Clone is NOT implemented " << endl;
   exit( 1 );
   return NULL;
#elif _OSG
   cerr << " ERROR: cfdGroup::Clone is NOT implemented " << endl;
   exit( 1 );
   return NULL;
#elif _OPENSG
   cerr << " ERROR: cfdGroup::Clone is NOT implemented " << endl;
   exit( 1 );
   return NULL;
#endif
}
