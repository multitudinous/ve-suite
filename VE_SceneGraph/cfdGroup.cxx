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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdGroup.h"
#include <iostream>
#include <algorithm>
#include <string>

#ifdef _PERFORMER
#include <Performer/pf/pfGroup.h>
#include <Performer/pf/pfNode.h>
#elif _OSG
#include <osg/Group>
#include <osg/Node>
#elif _OPENSG
#endif
///////////////////////////////////////////
cfdGroup::cfdGroup( const cfdGroup& input )
:cfdNode(input)
{
   int numChildren = input.childNodes.size();

   for(int i = 0; i < numChildren; i++){
      this->childNodes.push_back(input.childNodes[i]);
   }

#ifdef _PERFORMER
   this->_group = input._group;
#elif _OSG
   _group = new osg::Group(*input._group);
#elif _OPENSG
#endif
   SetCFDNodeType(CFD_GROUP);
}
/////////////////////////////////////////////////////
cfdGroup& cfdGroup::operator=( const cfdGroup& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      cfdNode::operator =(input);

      //biv-- why not just call clear here?
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
   return *this;
}
//////////////////////////
cfdGroup::cfdGroup( void )
:cfdNode()
{
#ifdef _PERFORMER
   this->_group = new pfGroup();
#elif _OSG
   _group = new osg::Group();
#elif _OPENSG
#endif
   SetCFDNodeType(CFD_GROUP);
}
///////////////////////////
cfdGroup::~cfdGroup( void )
{
   //biv--do we need to delete or can we just call clear
   for ( unsigned int i = 0; i < childNodes.size(); i++ )
   {
      delete childNodes.at( i );
   }   
   childNodes.clear();
   
   // If neccesary
#ifdef _PERFORMER
   if ( this->_group != NULL )
   {
      pfDelete ( this->_group );
   }
#elif _OSG
  // _group->unref();
#elif _OPENSG
#endif
}
////////////////////////////////////////////////
int cfdGroup::RemoveChild( cfdNode* child )
{
#ifdef _OPENSG
   std::cerr << " ERROR: cfdGroup::RemoveChild is NOT implemented " << std::endl;
   exit( 1 );
#endif
   std::vector< cfdNode* >::iterator oldChild;
   oldChild = std::find( childNodes.begin(), childNodes.end(), child );
   
   // Check to make sure he is on this node
   if ( oldChild != childNodes.end() )
   {
      this->_group->removeChild( (*oldChild)->GetRawNode() );
      childNodes.erase( oldChild );
      child->SetParent( NULL );
      return 1;  
   }
   else
   {
      std::cout << " Child Not found " << std::endl;
      return -1;
   }
}
/////////////////////////////////////////////
int cfdGroup::AddChild( cfdNode* child )
{
#ifdef _OPENSG
   std::cerr << " ERROR: cfdGroup::AddChild is NOT implemented " << std::endl;
   exit( 1 );
   return -1;
#endif
   //add node to real graph rep
   int good = this->_group->addChild( child->GetRawNode() );
   if ( good )
   {
      //add the child to cfdscene
      childNodes.push_back( child );
      //set the parent in the cfdApp side
      child->SetParent( this );
      return 1;
   }
   
   return -1;
}
///////////////////////////////////////////////////////////////
void cfdGroup::InsertChild( int position, cfdNode* child )
{
#ifdef _OPENSG
   std::cerr << " ERROR: cfdGroup::InsertChild is NOT implemented " << std::endl;
   exit( 1 );
#endif

   this->_group->insertChild( position, child->GetRawNode() );
  
   std::vector< cfdNode* >::iterator newPosition;

   newPosition = std::find( childNodes.begin(), childNodes.end(), childNodes[ position ] );

   childNodes.insert( newPosition, child );
   child->SetParent( this );

}
/////////////////////////////////////////////////
int  cfdGroup::SearchChild( cfdNode* child )
{
   //biv--could be replaced w/ getChildIndex(cfdNode*)
   /* return _group->getChildIndex(child->GetRawNode());*/

   for ( unsigned int i = 0; i < childNodes.size(); ++i )
      if ( childNodes[ i ] == child ){
         return (int)i;
      }
   // if not found
   return -1;
}
/////////////////////////////////////////////
cfdNode* cfdGroup::GetChild( int child )
{
   return childNodes.at( child );
}
/////////////////////////////////////
int cfdGroup::GetNumChildren( void )
{
   
#ifdef _OPENSG
   std::cerr << " ERROR: cfdGroup::GetNumChildren is NOT implemented " << std::endl;
   exit( 1 );
   return -1;
#endif

   int numChildren = this->_group->getNumChildren(); 
   if ( numChildren!=(int)childNodes.size() )
   {
      std::cout << " cfdGroup::ERROR: Number of children don't equal " 
               << numChildren << " : " << childNodes.size() << std::endl;
      exit( 1 );
   }
   return numChildren;
}
////////////////////////////////////
void cfdGroup::SetName( char* name )
{
   const std::string test(name);
#ifdef _OPENSG
   std::cerr << " ERROR: cfdGroup::SetName is NOT implemented " << std::endl;
   exit( 1 );
#endif
#ifdef _PERFORMER
   _group->setName( test.c_str() );
#elif _OSG
   _group->setName( test );
#endif
}
//////////////////////////////////////
const char* cfdGroup::GetName( void )
{
#ifdef _OPENSG
   return 0;
#endif
#ifdef _PERFORMER
   return _group->getName();
#elif _OSG
    return _group->getName().data() ;
#endif
}
////////////////////////////////////////////////////////////
int cfdGroup::ReplaceChild( cfdNode* childToBeReplaced,
                         cfdNode* newChild)
{
#ifdef _OPENSG
   cerr << " ERROR: cfdGroup::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#endif
   std::vector< cfdNode* >::iterator oldChild;
   oldChild = std::find( childNodes.begin(), childNodes.end(), childToBeReplaced );
   
   // Check to make sure he is on this node
   if ( oldChild != childNodes.end() )
   {
      // Just erases from the vector doesn't delete memory
      childNodes.erase( oldChild );
      this->_group->replaceChild( childToBeReplaced->GetRawNode(), 
                                      newChild->GetRawNode() );

      //add the child to cfdscene
      childNodes.push_back( newChild );
      // Set new parent for the new child
      newChild->SetParent( this );
      // Show that he no longer has a parent
      childToBeReplaced->SetParent( NULL );
      return 1;
   }
   else
   {
      std::cout << " Error : Child not found " << std::endl;
      return -1;
   }
}
////////////////////////////////////
cfdNode* cfdGroup::Clone( int )
{
   // Need to fix this
#ifdef _PERFORMER
   std::cerr << " ERROR: cfdGroup::Clone is NOT implemented " << std::endl;
   exit( 1 );
   return NULL;
#elif _OSG
   std::cerr << " ERROR: cfdGroup::Clone is NOT implemented " << std::endl;
   exit( 1 );
   return NULL;
#elif _OPENSG
   std::cerr << " ERROR: cfdGroup::Clone is NOT implemented " << std::endl;
   exit( 1 );
   return NULL;
#endif
}
// Reimplement for other graphs
#ifdef _PERFORMER
pfNode* cfdGroup::GetRawNode( void )
#elif _OSG
osg::Node* cfdGroup::GetRawNode(void)
#elif _OPENSG
#endif
{
#ifdef _PERFORMER
   return _group;
#elif _OSG
   return _group.get();
#elif _OPENSG
#endif
}
