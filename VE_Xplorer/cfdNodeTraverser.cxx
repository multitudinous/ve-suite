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
 * File:          $RCSfile: cfdNodeTraverser.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdNodeTraverser.h"
#include <iostream>
using namespace std;
#include <Performer/pf/pfGroup.h>
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
void cfdNodeTraverser::setNode(pfNode* root)
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
      cout<<"Error: cfdNodeTraverser::traverse()!"<<endl;
      cout<<"Root node not set!"<<endl;
      return; 
   }
}
/////////////////////////////////////////////////////
//depth first recursion of a node/scene graph      //
/////////////////////////////////////////////////////
void cfdNodeTraverser::_traverseNode(pfNode* node)
{
   int nChildren = 0;
   
   if(!node->isOfType(pfGroup::getClassType())){
      return;
   }
   //grab the children of this group
   pfGroup* curGroup = (pfGroup*)node;
   nChildren = curGroup->getNumChildren();

   //the pre-callback
   if(_preFunc){
      _preFunc(this,node);
   }

   //recurse the children of this node
   for(int i = 0; i < nChildren; i++){
      _traverseNode(curGroup->getChild(i));
   }

   //the post-callback
   if(_postFunc){
      _postFunc(this,node);
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
