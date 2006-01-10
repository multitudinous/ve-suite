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
 * File:          $RCSfile: CADNodeTraverser.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/VE_XML/CAD/CADNodeTraverser.h"
#include "VE_Open/VE_XML/CAD/CADAssembly.h"
#include "VE_Open/VE_XML/CAD/CADNode.h"
#include "VE_Open/VE_XML/CAD/CADPart.h"
#include "VE_Open/VE_XML/CAD/CADClone.h"
#include <iostream>

using namespace VE_CAD;
////////////////////////////////////
//Constructors                    //
////////////////////////////////////
CADNodeTraverser::CADNodeTraverser()
{ 
   _root = 0;
   _preFunc = 0;
   _postFunc = 0;
   _ts = CONT;
}
/////////////////////////////////////////////////////////////////
CADNodeTraverser::CADNodeTraverser(const CADNodeTraverser& cfdNT)
{
   _root = 0;
   _preFunc = 0;
   _postFunc = 0;
   _ts = cfdNT._ts;
   _root = cfdNT._root;
   _preFunc = cfdNT._preFunc;
   _postFunc = cfdNT._postFunc;
}
/////////////////////////////////////
//Destructor                       //
/////////////////////////////////////
CADNodeTraverser::~CADNodeTraverser()
{
   
}
/////////////////////////////////////////////////////
//set the node to traverse                         //
/////////////////////////////////////////////////////
void CADNodeTraverser::SetRootNode(VE_CAD::CADNode* root)
{
   _root = root;
}
/////////////////////////////////
//traverse the node            //
/////////////////////////////////
void CADNodeTraverser::Traverse()
{
   if(_root){
      //the pre-callback
      if(_preFunc){
         _preFunc->Apply(this,_root);
         if(_ts == SKIP ||
            _ts == STOP)
         {
            _ts = CONT;
            return;
         }
      }

      //recurse the root node
      _traverseNode(_root);

      //the post-callback
      if(_postFunc){
         _postFunc->Apply(this,_root);
      }
   }else{
      std::cout<<"Error: CADNodeTraverser::traverse()!"<<std::endl;
      std::cout<<"Root node not set!"<<std::endl;
      return; 
   }
}
///////////////////////////////////////////////////////////////////////////////////////////
//depth first recursion of a node/scene graph                                            //
///////////////////////////////////////////////////////////////////////////////////////////
void CADNodeTraverser::_traverseNode(VE_CAD::CADNode* cNode,VE_CAD::CADNode* currentParent)
{
   int nChildren = 0;
   //the pre-callback
   if(_preFunc){
      _preFunc->Apply(this,cNode,currentParent);
      if(_ts == SKIP ||
        _ts == STOP)
      {
         //pre func tells us to stop traversing down this branch
         _ts = CONT;
         return;
      }
   }
   //these are the only CADNode types (so far) 
   //that have children so we must traverse the child nodes!!!!
   if(cNode->GetNodeType() == std::string("Assembly")){
      VE_CAD::CADAssembly* assembly = dynamic_cast<VE_CAD::CADAssembly*>(cNode);
      unsigned int nChildren = assembly->GetNumberOfChildren();
      //recurse the children of this node
      for(unsigned int i = 0; i < nChildren; i++)
      {
        _traverseNode(assembly->GetChild(i),assembly);
      }
   }else{
      //set the parent of the part or clone
      cNode->SetParent(currentParent);
   }
   
   //the post-callback
   if(_postFunc){
      _postFunc->Apply(this,cNode,currentParent);
   }
}
//////////////////////////////////////////////////////////////////////////////////////////////////
void CADNodeTraverser::SetPreNodeTraverseCallback(CADNodeTraverser::CADNodeTraverseCallback* func)
{
   _preFunc = func;
}
///////////////////////////////////////////////////////////////////////////////////////////////////
void CADNodeTraverser::SetPostNodeTraverseCallback(CADNodeTraverser::CADNodeTraverseCallback* func)
{
   _postFunc = func;
}
//////////////////////////////////////////////////////////
//the equal operator                                    //
//////////////////////////////////////////////////////////
CADNodeTraverser&
CADNodeTraverser::operator=(const CADNodeTraverser& cfdNT)
{
   if(this!= &cfdNT){
      _root = cfdNT._root;
      _preFunc = cfdNT._preFunc;
      _postFunc = cfdNT._postFunc;
   }
   return *this;
}
