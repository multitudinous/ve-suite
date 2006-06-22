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
 * File:          $RCSfile: cfdNodeTraverser.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/SceneGraph/cfdNodeTraverser.h"
#include <iostream>
#include "VE_Xplorer/SceneGraph/cfdGroup.h"
#include "VE_Xplorer/SceneGraph/cfdSwitch.h"
#include "VE_Xplorer/SceneGraph/cfdDCS.h"
#include "VE_Xplorer/SceneGraph/cfdNode.h"

#ifdef _PERFORMER
#include <Performer/pf/pfGroup.h>
#elif _OSG
#include <osg/Group>
#endif
using namespace VE_SceneGraph;
////////////////////////////////////
//Constructors                    //
////////////////////////////////////
cfdNodeTraverser::cfdNodeTraverser()
{ 
   _root = 0;
   _preFunc = 0;
   _postFunc = 0;
   _ts = CONT;
}
/////////////////////////////////////////////////////////////////
cfdNodeTraverser::cfdNodeTraverser(const cfdNodeTraverser& cfdNT)
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
cfdNodeTraverser::~cfdNodeTraverser()
{
   
}
////////////////////////////////////////////
//set the node to traverse                //
////////////////////////////////////////////
void cfdNodeTraverser::setNode(VE_SceneGraph::cfdNode* root)
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
         _postFunc(this,_root);
      }
   }else{
      std::cout<<"Error: cfdNodeTraverser::traverse()!"<<std::endl;
      std::cout<<"Root node not set!"<<std::endl;
      return; 
   }
}
///////////////////////////////////////////////////////////////////
//depth first recursion of a node/scene graph                    //
///////////////////////////////////////////////////////////////////
void cfdNodeTraverser::_traverseNode(VE_SceneGraph::cfdNode* cNode)
{
   int nChildren = 0;
   //the pre-callback
   if(_preFunc){
      _preFunc(this,cNode);
      if(_ts == SKIP ||
        _ts == STOP)
      {
         //pre func tells us to stop traversing down this branch
         _ts = CONT;
         return;
      }
   }
   //these are the only cfdNode types (so far) 
   //that have children so we must traverse the childnodes!!!!
   if(cNode->GetCFDNodeType() == VE_SceneGraph::cfdNode::CFD_SEQUENCE){
      std::cout<<"The sequence"<<std::endl;
   }else if(cNode->GetCFDNodeType() == VE_SceneGraph::cfdNode::CFD_GROUP){
      VE_SceneGraph::cfdGroup* curGroup = dynamic_cast<VE_SceneGraph::cfdGroup*>(cNode);
      nChildren = curGroup->GetNumChildren();
      //recurse the children of this node
      for(int i = 0; i < nChildren; i++){
        _traverseNode(curGroup->GetChild(i));
      }
	}else if(cNode->GetCFDNodeType() == cfdNode::CFD_SWITCH){
      VE_SceneGraph::cfdSwitch* curGroup = dynamic_cast<VE_SceneGraph::cfdSwitch*>(cNode);
      nChildren = curGroup->GetNumChildren();
      //recurse the children of this node
      for(int i = 0; i < nChildren; i++){
        _traverseNode(curGroup->GetChild(i));
      }
   }else if(cNode->GetCFDNodeType() == cfdNode::CFD_DCS){
      VE_SceneGraph::cfdDCS* curGroup = dynamic_cast<VE_SceneGraph::cfdDCS*>(cNode);
      nChildren = curGroup->GetNumChildren();
      //recurse the children of this node
      for(int i = 0; i < nChildren; i++){
        _traverseNode(curGroup->GetChild(i));
      }
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
