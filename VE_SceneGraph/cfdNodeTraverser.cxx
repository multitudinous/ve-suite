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
 * File:          $RCSfile: cfdNodeTraverser.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdNodeTraverser.h"
#include <iostream>
#include "cfdGroup.h"

#ifdef _PERFORMER
#include <Performer/pf/pfGroup.h>
#elif _OSG
#include <osg/Group>
#endif
////////////////////////////////////
//Constructors                    //
////////////////////////////////////
cfdNodeTraverser::cfdNodeTraverser()
{ 
   _root = 0;
   _preFunc = 0;
   _postFunc = 0;
}
/////////////////////////////////////////////////////////////////
cfdNodeTraverser::cfdNodeTraverser(const cfdNodeTraverser& cfdNT)
{
   _root = 0;
   _preFunc = 0;
   _postFunc = 0;

   _root = cfdNT._root;
   _preFunc = cfdNT._preFunc;
   _postFunc = cfdNT._postFunc;
}
/////////////////////////////////////
//Destructor                       //
/////////////////////////////////////
cfdNodeTraverser::~cfdNodeTraverser()
{
   
}
////////////////////////////////////////////
//set the node to traverse                //
////////////////////////////////////////////
void cfdNodeTraverser::setNode(cfdNode* root)
{
   _root = root;
}
/////////////////////////////////
//traverse the node            //
/////////////////////////////////
void cfdNodeTraverser::traverse()
{
   if(_root){
      //the pre-callback
      if(_preFunc){
         _preFunc(this,_root);
      }

      //recurse the root node
      _traverseNode(_root);

      //the post-callback
      if(_postFunc){
         _postFunc(this,_root);
      }
   }else{
      std::cout<<"Error: cfdNodeTraverser::traverse()!"<<std::endl;
      std::cout<<"Root node not set!"<<std::endl;
      return; 
   }
}
/////////////////////////////////////////////////////
//depth first recursion of a node/scene graph      //
/////////////////////////////////////////////////////
void cfdNodeTraverser::_traverseNode(cfdNode* cNode)
{
   int nChildren = 0;
   
#ifdef _PERFORMER
   /*if(!node->isOfType(pfGroup::getClassType())){
      return;
   }*/
#elif _OSG

#endif
   if(cNode->GetCFDNodeType() != cfdNode::CFD_GROUP){
      return;
   }
   //grab the children of this group
   //pfGroup* curGroup = (pfGroup*)node;
   cfdGroup* curGroup = (cfdGroup*)cNode;
   nChildren = curGroup->GetNumChildren();

   //the pre-callback
   if(_preFunc){
      _preFunc(this,cNode);
   }

   //recurse the children of this node
   for(int i = 0; i < nChildren; i++){
      _traverseNode(curGroup->GetChild(i));
   }

   //the post-callback
   if(_postFunc){
      _postFunc(this,cNode);
   }
}

//////////////////////////////////////////////////////////
//the equal operator                                    //
//////////////////////////////////////////////////////////
cfdNodeTraverser&
cfdNodeTraverser::operator=(const cfdNodeTraverser& cfdNT)
{
   if(this!= &cfdNT){
      _root = cfdNT._root;
      _preFunc = cfdNT._preFunc;
      _postFunc = cfdNT._postFunc;
   }
   return *this;
   
}
