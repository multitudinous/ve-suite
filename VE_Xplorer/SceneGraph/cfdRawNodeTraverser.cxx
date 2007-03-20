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
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/SceneGraph/cfdRawNodeTraverser.h"

#ifdef _PERFORMER
#include <Performer/pf/pfGroup.h>
#include <Performer/pf/pfSequence.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfSwitch.h>
#elif _OSG
#include <osg/Group>
#endif

#include <iostream>

using namespace VE_SceneGraph;

////////////////////////////////////
//Constructors                    //
////////////////////////////////////
cfdRawNodeTraverser::cfdRawNodeTraverser()
{ 
   _root = 0;
   _preFunc = 0;
   _postFunc = 0;
   _ts = CONT;
}
/////////////////////////////////////////////////////////////////
cfdRawNodeTraverser::cfdRawNodeTraverser(const cfdRawNodeTraverser& cfdNT)
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
cfdRawNodeTraverser::~cfdRawNodeTraverser()
{
   
}
////////////////////////////////////////////
//set the node to traverse                //
////////////////////////////////////////////
#ifdef _OSG
void cfdRawNodeTraverser::setNode(osg::Node* root,
                               bool deepClone)
#elif _PERFORMER
void cfdRawNodeTraverser::setNode(pfNode* root,
                               bool deepClone)
#endif
{
#ifdef _OSG
   if(deepClone)
   {
      _root = dynamic_cast<osg::Node*>(root->clone(osg::CopyOp::DEEP_COPY_ALL));
   }else
#endif
   {
      _root = root;
   }
}
////////////////////////////////////
//traverse the node               //
////////////////////////////////////
void cfdRawNodeTraverser::traverse()
{
#ifdef _PERFORMER
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
#elif _OSG
   if(_root.get()){
      //the pre-callback
      if(_preFunc){
         _preFunc(this,_root.get());
         if(_ts == SKIP ||
            _ts == STOP)
         {
            _ts = CONT;
            return;
         }
      }

      //recurse the root node
      _traverseNode(_root.get());

      //the post-callback
      if(_postFunc){
         _postFunc(this,_root.get());
      }
#endif
   }else{
      std::cout<<"Error: cfdRawNodeTraverser::traverse()!"<<std::endl;
      std::cout<<"Root node not set!"<<std::endl;
      return; 
   }
}
///////////////////////////////////////////////////////////////////
//depth first recursion of a node/scene graph                    //
///////////////////////////////////////////////////////////////////
#ifdef _OSG
void cfdRawNodeTraverser::_traverseNode(osg::Node* cNode)
#elif _PERFORMER
void cfdRawNodeTraverser::_traverseNode(pfNode* cNode)
#endif
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
#ifdef _OSG
   if(!strcmp(cNode->className(),"Group")||
      !strcmp(cNode->className(),"DCS")||
      !strcmp(cNode->className(),"Sequence")||
      !strcmp(cNode->className(),"Switch")){
      osg::ref_ptr<osg::Group> curGroup = dynamic_cast<osg::Group*>(cNode);
#elif _PERFORMER
   if(cNode->isOfType(pfSequence::getClassType())||
      cNode->isOfType(pfGroup::getClassType())||
      cNode->isOfType(pfSwitch::getClassType())||
      cNode->isOfType(pfDCS::getClassType())){
      pfGroup* curGroup = (pfGroup*)cNode;
#endif
      nChildren = curGroup->getNumChildren();

      //recurse the children of this node
      for(int i = 0; i < nChildren; i++){
        _traverseNode(curGroup->getChild(i));
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
cfdRawNodeTraverser&
cfdRawNodeTraverser::operator=(const cfdRawNodeTraverser& cfdNT)
{
   if(this!= &cfdNT){
      _root = cfdNT._root;
      _preFunc = cfdNT._preFunc;
      _postFunc = cfdNT._postFunc;
   }
   return *this;
   
}
