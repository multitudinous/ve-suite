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
 * File:          $RCSfile: cfdWriteTraverser.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdWriteTraverser.h"
#include "VE_SceneGraph/cfdGroup.h"
#include "VE_SceneGraph/cfdSequence.h"

#ifdef _PERFORMER
#include <Performer/pf/pfGroup.h>
#include <Performer/pf/pfSequence.h>
#include <Performer/pfdb/pfpfb.h>
#elif _OSG
#include <osg/Group>
#include <osgDB/WriteFile>
#include <osg/Sequence>
#endif

#include <iostream>
//////////////////////////////////////
//Constructor                       //
//////////////////////////////////////
cfdWriteTraverser::cfdWriteTraverser()
:cfdNodeTraverser()
{
   _fName = 0;
   _sequenceIndex = 0;
   _toPfb = 0;
   _preFunc = 0;
}
////////////////////////////////////////////////////////////////////
cfdWriteTraverser::cfdWriteTraverser(const cfdWriteTraverser& cfdWT)
:cfdNodeTraverser(cfdWT)
{
   _fName = 0;
   _sequenceIndex = 0;
   _toPfb = 0;

   _preFunc = cfdWT._preFunc;
   
   _fName = new char[strlen(cfdWT._fName)+1];
   strcpy(_fName,cfdWT._fName);
}
///////////////////////////////////////////////////
cfdWriteTraverser::cfdWriteTraverser(char* outFile)
:cfdNodeTraverser()
{
   _fName = 0;
   _sequenceIndex = 0;
   _toPfb = 0;
   setOutputFileName(outFile);
}
///////////////////////////////////////
//Destructor                         //
///////////////////////////////////////
cfdWriteTraverser::~cfdWriteTraverser()
{
   if(_fName){
      delete [] _fName;
      _fName = 0;
   }
   if(_sequenceList.size()){
      _sequenceList.clear();
   }
}
////////////////////////////////////////////////////////////////////////////
cfdWriteTraverser& cfdWriteTraverser::operator=(const cfdWriteTraverser& rhs)
{
   //make sure it's not the same object
   if(this != &rhs){
      //call the parent operator
      this->cfdNodeTraverser::operator =(rhs);
      
      //copy our new name
      _fName = new char[strlen(rhs._fName)+1];
      strcpy(_fName,rhs._fName);
   }
   return *this;
}
/////////////////////////////////////////////////////
//set which callback to use                        //
/////////////////////////////////////////////////////
void cfdWriteTraverser::setCallback(int swapActivate)
{
   if(swapActivate){
      _preFunc = _swapSequenceNodes;
   }else{
      _preFunc = _turnOnSequence;
   }
}
///////////////////////////////////////////////////
//the prenode callback  is already set           //
///////////////////////////////////////////////////
void cfdWriteTraverser::setPreNodeTraverseCallback(
		preNodeTraverseCallback func)
{
   std::cout<<"WARNING: cfdWriteTraverser::setPreNodeTraverserCallback"<<std::endl;
   std::cout<<"Pre-Node should be set using setCallback(int) function !!!"<<std::endl;
   return;
}
////////////////////////////////////////////////////////
//set the output file name                            //
////////////////////////////////////////////////////////
void cfdWriteTraverser::setOutputFileName(char* outFile)
{
   if(_fName){
      delete [] _fName;
      _fName = 0;
   }
   _fName = new char[strlen(outFile)+1];
   strcpy(_fName,outFile);
}
//////////////////////////////////////////////////////////
//turn on the sequence nodes for proper read back       //
//////////////////////////////////////////////////////////
void _turnOnSequence(cfdNodeTraverser* cfdNT,cfdNode* node)
{
if(node->GetCFDNodeType() == cfdSceneNode::CFD_SEQUENCE){
      //make sure to start the "derned" sequence--otherwise
      //it won't be running in perfly!!!UGGGHHHH!!!!!
      ((cfdSequence*)node)->setPlayMode(CFDSEQ_START);
   } 
}
//////////////////////////////////////////////////////
//swap the sequence nodes                           //
//////////////////////////////////////////////////////
void _swapSequenceNodes(cfdNodeTraverser* cfdNT,cfdNode* node)
{
   //need to implement using getRawNode calls!!

   //Need to fix
   cfdWriteTraverser* cfdWT = (cfdWriteTraverser*)cfdNT;

   //replace cfdSequence nodes
   if(node->GetCFDNodeType() == cfdSceneNode::CFD_SEQUENCE){
      //std::cout<<"\t_swapSequenceNodes: cfdSequence"<<std::endl;

      int nChildren = ((cfdSequence*)node)->getNumChildren();
      
      //add this node to our list of cfdSequences
      cfdWT->_sequenceList.push_back(node);

      //get the parent node

      cfdGroup* parent = dynamic_cast<cfdGroup*>(node->GetParent(0));
      
      //create a pfSequence
#ifdef _PERFORMER
      pfSequence* sequence =  new pfSequence();
#elif _OSG
      osg::Sequence* sequence =  new osg::Sequence();
#elif _OPENSG
#endif

      //copy the children of the cfdSequence
      for(int i = 0; i < nChildren; i++){
         sequence->addChild(((cfdSequence*)node)->GetChild(i)->GetRawNode());
      }

      sequence->setDuration(1.0,-1);      // regular speed, continue forever

      double numSeconds = ((cfdSequence*)node)->getTime();
      sequence->setTime(-1,numSeconds);   // display all children for numSeconds

#ifdef _PERFORMER
      sequence->setInterval(((cfdSequence*)node)->getLoopMode(),
                            ((cfdSequence*)node)->getBegin(),
                            ((cfdSequence*)node)->getEnd());
#elif _OSG
      
      sequence->setInterval(((((cfdSequence*)node)->getLoopMode()==0)?
                             osg::Sequence::SWING:osg::Sequence::LOOP),
                            ((cfdSequence*)node)->getBegin(),
                            ((cfdSequence*)node)->getEnd());
#elif _OPENSG
#endif
//      std::cout<<"\tnChildren = " << nChildren << std::endl;
//      std::cout<<"\tnumSeconds = " << numSeconds << std::endl;
//      std::cout<<"\t((cfdSequence*)curNode)->getLoopMode() = "
//               <<((cfdSequence*)curNode)->getLoopMode() << std::endl;
//      std::cout<<"\t((cfdSequence*)curNode)->getBegin() = "
//               <<((cfdSequence*)curNode)->getBegin() << std::endl;
//      std::cout<<"\t((cfdSequence*)curNode)->getEnd() = "
//               <<((cfdSequence*)curNode)->getEnd() << std::endl;


      //make sure to start the "derned" sequence--otherwise
      //it won't be running in perfly!!!UGGGHHHH!!!!!
#ifdef _PERFORMER
      sequence->setMode(PFSEQ_START);
#elif _OSG
      sequence->setMode(osg::Sequence::START);
#endif
      //replace the node in the graph
#ifdef _PERFORMER
      ((pfGroup*)parent->GetRawNode())->replaceChild(node->GetRawNode(),sequence);
#elif _OSG
      ((osg::Group*)parent->GetRawNode())->replaceChild(node->GetRawNode(),sequence);
#elif _OPENSG
#endif
      //don't need to continue down 
      return;
#ifdef _PERFORMER
   }else if(node->GetRawNode()->isOfType(pfSequence::getClassType())){
#elif _OSG
   }else if(!strcmp(node->GetRawNode()->className(),"Sequence")){
#elif _OPENSG
#endif
      //std::cout<<"\t_swapSequenceNodes: pfSequence"<<std::endl;
      //return tree to original state
      //get the parent node
      cfdGroup* parent = (cfdGroup*)node->GetParent(0);

      //replace the node in the graph
      parent->ReplaceChild(node,
                        (cfdWT->_sequenceList[cfdWT->_sequenceIndex++]));

      //pfDelete(curNode);
      //don't need to continue down 
      return;
   }

   return;

}
////////////////////////////////////////////////
//this functionality should probably be       //
//in another derived class!!!                 //
////////////////////////////////////////////////
void cfdWriteTraverser::activateSequenceNodes()
{
   if(_fName && _root){
      //swap out cfdsequence nodes
      _traverseNode(_root);
#ifdef _PERFORMER
      //store file as pfb file
      pfdStoreFile(_root->GetRawNode(),_fName);
#elif _OSG
      //this may not work
      osgDB::writeNodeFile(*_root->GetRawNode(),_fName);

#endif
   }
}
//////////////////////////////////////
//write out the pfbFile             //
//////////////////////////////////////
void cfdWriteTraverser::writePfbFile()
{
   if(_fName && _root){
      //swap out cfdsequence nodes
      _traverseNode(_root);

#ifdef _PERFORMER
      //store file as pfb file
      pfdStoreFile(_root->GetRawNode(),_fName);
#elif _OSG
      //this may not work
      osgDB::writeNodeFile(*_root->GetRawNode(),_fName);

#endif

      //_sequenceIndex = _sequenceList.size();
      //replace pfSequence nodes
      _traverseNode(_root);
      _sequenceIndex = 0;
   }
}
