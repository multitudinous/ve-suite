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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/cfdSwitch.h"
#include "VE_Xplorer/SceneGraph/cfdNode.h"
#include <iostream>
#include <algorithm>
#include <string>
#ifdef _PERFORMER
#include <Performer/pf/pfSwitch.h>
#endif

using namespace VE_SceneGraph;
//////////////////////////////////
//Constructors                  //
//////////////////////////////////
cfdSwitch::cfdSwitch()
:cfdGroup()
{
#ifdef _PERFORMER
   _switch = new pfSwitch();
#elif _OSG
   _switch = new osg::Switch();
   _switch->setAllChildrenOff();
#elif _OPENSG
#endif
   SetCFDNodeType(CFD_SWITCH);
   
}
//////////////////////////////////////////
cfdSwitch::cfdSwitch(const cfdSwitch& rhs)
:cfdGroup(rhs)
{
   
#ifdef _PERFORMER
   _switch = rhs._switch;
#elif _OSG
   _switch = new osg::Switch(*rhs._switch);
#elif _OPENSG
#endif
   childNodes = rhs.childNodes;
   SetCFDNodeType(CFD_SWITCH);
}
///////////////////////
cfdSwitch::~cfdSwitch()
{
#ifdef _PERFORMER
   if ( _switch )
   {
     pfDelete( _switch );
   }
   _switch = 0;
#elif _OSG
#elif _OPENSG
#endif
}
//////////////////////////////////////////////
void cfdSwitch::SetVal(int whichChildIsActive)
{
#ifdef _PERFORMER
   if ( _switch )
   {
      if(whichChildIsActive == OFF)
         _switch->setVal(-1);
      else
         _switch->setVal(whichChildIsActive);
#elif _OSG
   if ( _switch.valid() )
   {
      if(whichChildIsActive == OFF)
         _switch->setAllChildrenOff();
      else
         _switch->setSingleChildOn(whichChildIsActive);
#elif _OPENSG
#endif
   }
}
/////////////////////////////////////////////////////
cfdSwitch& cfdSwitch::operator=(const cfdSwitch& rhs)
{
   if(&rhs != this){
      cfdGroup::operator=(rhs);
#ifdef _PERFORMER
      _switch = rhs._switch;
#elif _OSG
      _switch = rhs._switch;
#elif _OPENSG
#endif
     childNodes = rhs.childNodes;
   }
   return *this;
}
// Reimplement for other graphs
#ifdef _PERFORMER
pfNode* cfdSwitch::GetRawNode( void )
#elif _OSG
osg::Node* cfdSwitch::GetRawNode(void)
#elif _OPENSG
#endif
{
#ifdef _PERFORMER
   return _switch;
#elif _OSG
   return _switch.get();
#elif _OPENSG
#endif
}
////////////////////////////////////////////////
int cfdSwitch::RemoveChild( cfdNode* child )
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
      this->_switch->removeChild( child->GetRawNode() );
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
/////////////////////////////////////
int cfdSwitch::GetNumChildren( void )
{
   
#ifdef _OPENSG
   std::cerr << " ERROR: cfdSwitch::GetNumChildren is NOT implemented " << std::endl;
   exit( 1 );
   return -1;
#endif

   int numChildren = this->_switch->getNumChildren(); 
   if ( numChildren!=(int)childNodes.size() )
   {
      std::cout << " ERROR: Number of children don't equal " 
               << numChildren << " : " << childNodes.size() << std::endl;
      exit( 1 );
   }
   return numChildren;
}
/////////////////////////////////////////////
int cfdSwitch::AddChild( cfdNode* child )
{
#ifdef _OPENSG
   std::cerr << " ERROR: cfdSwitch::AddChild is NOT implemented " << std::endl;
   exit( 1 );
   return -1;
#endif
   //add node to real graph rep
   int good = this->_switch->addChild( child->GetRawNode() );
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
////////////////////////////////////////////////////////////
int cfdSwitch::ReplaceChild( cfdNode* childToBeReplaced,
                         cfdNode* newChild)
{
#ifdef _OPENSG
   cerr << " ERROR: cfdSwitch::ReplaceChild is NOT implemented " << endl;
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
      this->_switch->replaceChild( childToBeReplaced->GetRawNode(), 
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
///////////////////////////////////////////////////////////////
void cfdSwitch::InsertChild( int position, cfdNode* child )
{
#ifdef _OPENSG
   std::cerr << " ERROR: cfdSwitch::InsertChild is NOT implemented " << std::endl;
   exit( 1 );
#endif

   this->_switch->insertChild( position, child->GetRawNode() );
  
   std::vector< cfdNode* >::iterator newPosition;

   newPosition = std::find( childNodes.begin(), childNodes.end(), childNodes[ position ] );

   childNodes.insert( newPosition, child );
   child->SetParent( this );

}
