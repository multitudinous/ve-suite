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
 * File:          $RCSfile: cfdRawNodeWriteTraverser.cxx,v $
 * Date modified: $Date: 2005-07-09 16:05:15 -0500 (Sat, 09 Jul 2005) $
 * Version:       $Rev: 2638 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/cfdRawNodeWriteTraverser.h"
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

using namespace VE_Xplorer;

//////////////////////////////////////
//Constructor                       //
//////////////////////////////////////
cfdRawNodeWriteTraverser::cfdRawNodeWriteTraverser()
:VE_SceneGraph::cfdRawNodeTraverser()
{
   _fName = 0;
   _toPfb = 0;
   _preFunc = _swapSequenceNodes;
#ifdef _PERFORMER
   _sequenceIndex = 0;
#endif

}
////////////////////////////////////////////////////////////////////
cfdRawNodeWriteTraverser::cfdRawNodeWriteTraverser(const cfdRawNodeWriteTraverser& cfdWT)
:VE_SceneGraph::cfdRawNodeTraverser(cfdWT)
{
   _fName = 0;
   _toPfb = 0;

   _preFunc = cfdWT._preFunc;
   
   _fName = new char[strlen(cfdWT._fName)+1];
   strcpy(_fName,cfdWT._fName);
#ifdef _PERFORMER
   unsigned int nSequences = cfdWT._sequenceList.size();
   for(unsigned int i = 0; i < nSequences; i++){
      _sequenceList.push_back(cfdWT._sequenceList.at(i));
   }
   
   _sequenceIndex = cfdWT._sequenceIndex;

#endif
}
///////////////////////////////////////////////////
cfdRawNodeWriteTraverser::cfdRawNodeWriteTraverser(char* outFile)
:VE_SceneGraph::cfdRawNodeTraverser()
{
   _fName = 0;
   _toPfb = 0;
   setOutputFileName(outFile);
#ifdef _PERFORMER
   _sequenceIndex = 0;
#endif
}
///////////////////////////////////////
//Destructor                         //
///////////////////////////////////////
cfdRawNodeWriteTraverser::~cfdRawNodeWriteTraverser()
{
   if(_fName){
      delete [] _fName;
      _fName = 0;
   }
#ifdef _PERFORMER
   if(_sequenceList.size()){
      _sequenceList.clear();
   }
#endif
}
////////////////////////////////////////////////////////////////////////////
cfdRawNodeWriteTraverser& cfdRawNodeWriteTraverser::operator=(const cfdRawNodeWriteTraverser& rhs)
{
   //make sure it's not the same object
   if(this != &rhs){
      //call the parent operator
      this->VE_SceneGraph::cfdRawNodeTraverser::operator =(rhs);
      
      //copy our new name
      _fName = new char[strlen(rhs._fName)+1];
      strcpy(_fName,rhs._fName);
#ifdef _PERFORMER
      if(_sequenceList.size())
      {
         _sequenceList.clear();
      }

      unsigned int nSequences = rhs._sequenceList.size();
      for(unsigned int i = 0; i < nSequences; i++)
      {
         _sequenceList.push_back(rhs._sequenceList.at(i));
      }
      _sequenceIndex = rhs._sequenceIndex;
#endif
   }
   return *this;
}
/////////////////////////////////////////////////////
//set which callback to use                        //
/////////////////////////////////////////////////////
void cfdRawNodeWriteTraverser::setCallback(int swapActivate)
{
   if(swapActivate)
   {
      _preFunc = _swapSequenceNodes;
   }
#ifdef _PERFORMER
   else
   {
      _preFunc = _activateSequenceNodes;   
   }
#endif
}
///////////////////////////////////////////////////
//the prenode callback  is already set           //
///////////////////////////////////////////////////
void cfdRawNodeWriteTraverser::setPreNodeTraverseCallback(
		preNodeTraverseCallback func)
{
   std::cout<<"WARNING: cfdRawNodeWriteTraverser::setPreNodeTraverserCallback"<<std::endl;
   std::cout<<"Pre-Node should be set using setCallback(int) function !!!"<<std::endl;
   return;
}
////////////////////////////////////////////////////////
//set the output file name                            //
////////////////////////////////////////////////////////
void cfdRawNodeWriteTraverser::setOutputFileName(char* outFile)
{
   if(_fName){
      delete [] _fName;
      _fName = 0;
   }
   _fName = new char[strlen(outFile)+1];
   strcpy(_fName,outFile);
}
////////////////////////////////////////////////////////////////////
void cfdRawNodeWriteTraverser::setNode(VE_SceneGraph::cfdNode* root)
{
   VE_SceneGraph::cfdRawNodeTraverser::setNode(root->GetRawNode(),true);
}
///////////////////////////////////////////////////////////////////////////
//swap the sequence nodes                                                //
///////////////////////////////////////////////////////////////////////////
#ifdef _PERFORMER
void VE_Xplorer::_swapSequenceNodes(VE_SceneGraph::cfdRawNodeTraverser* cfdNT,
                                    pfNode* node)
#elif _OSG
void VE_Xplorer::_swapSequenceNodes(VE_SceneGraph::cfdRawNodeTraverser* cfdNT,
                                    osg::Node* node)
#endif
{
   //need to implement using getRawNode calls!!

   //Need to fix
   VE_Xplorer::cfdRawNodeWriteTraverser* cfdWT = (VE_Xplorer::cfdRawNodeWriteTraverser*)cfdNT;
#ifdef _PERFORMER
   if(node->getName()&&!strcmp(node->getName(),"cfdsequence")){
      VE_SceneGraph::cfdSequence* seq = dynamic_cast<VE_SceneGraph::cfdSequence*>(node);
      if(!seq)
         return;
#elif _OSG
   if(!strcmp(node->className(),"cfdSequence")){
#endif
      int nChildren = ((VE_SceneGraph::cfdSequence*)node)->GetNumChildren();

      //std::cout<<"Found a cfdSequence with: "<<nChildren<<" children."<<std::endl;
      //get the parent node
#ifdef _OSG
      osg::ref_ptr<osg::Group> parent = dynamic_cast<osg::Group*>(node->getParent(0));
      if(!parent.valid()){
         std::cout<<"Couldn't find parent of this cfdSequence!!"<<std::endl;
      }
#elif _PERFORMER
      pfGroup* parent = dynamic_cast<pfGroup*>(node->getParent(0));
      if(!parent){
         std::cout<<"Couldn't find parent of this cfdSequence!!"<<std::endl;
      }
#endif
      //create a pfSequence
#ifdef _PERFORMER
      pfSequence* sequence =  new pfSequence();
#elif _OSG
      osg::ref_ptr<osg::Sequence> sequence =  new osg::Sequence();
#elif _OPENSG
#endif
      //copy the children of the cfdSequence
      for(int i = 0; i < nChildren; i++){
         sequence->addChild(((VE_SceneGraph::cfdSequence*)node)->GetChild(i)->GetRawNode());
      }
      sequence->setDuration(1.0,-1);      // regular speed, continue forever
      double numSeconds = ((VE_SceneGraph::cfdSequence*)node)->getTime();

#ifdef _PERFORMER
      //sequence->setTime(-1,numSeconds);   // display all children for numSeconds
      sequence->setInterval(((VE_SceneGraph::cfdSequence*)node)->getLoopMode(),
                            ((VE_SceneGraph::cfdSequence*)node)->getBegin(),
                            ((VE_SceneGraph::cfdSequence*)node)->getEnd());
#elif _OSG
      
      sequence->setInterval(((((VE_SceneGraph::cfdSequence*)node)->getLoopMode()==0)?
                             osg::Sequence::SWING:osg::Sequence::LOOP),
                            ((VE_SceneGraph::cfdSequence*)node)->getBegin(),
                            ((VE_SceneGraph::cfdSequence*)node)->getEnd());
#elif _OPENSG
#endif
      //make sure to start the "derned" sequence--otherwise
      //it won't be running in perfly!!!UGGGHHHH!!!!!
#ifdef _PERFORMER
      sequence->setMode(PFSEQ_START);
#elif _OSG
      sequence->setMode(osg::Sequence::START);
#endif
      //replace the node in the graph
#ifdef _PERFORMER
      if(parent->replaceChild((VE_SceneGraph::cfdSequence*)node,sequence)){
         //add this node to our list of cfdSequences
         cfdWT->_sequenceList.push_back((VE_SceneGraph::cfdSequence*)node);
#elif _OSG
      if(parent->replaceChild((VE_SceneGraph::cfdSequence*)node,sequence.get())){
#elif _OPENSG
#endif
         //std::cout<<"Replaced the cfdSequence w/ a raw sequence!!"<<std::endl;
      }
      //don't need to continue down 
      cfdWT->setTraversalStatus(VE_SceneGraph::cfdRawNodeTraverser::SKIP);
      return;
   }
   cfdWT->setTraversalStatus(VE_SceneGraph::cfdRawNodeTraverser::CONT);
   return;

}
#ifdef _PERFORMER
/////////////////////////////////////////////////////////////////////////////////////
//put the cfdSequence back on the tree                                             //
/////////////////////////////////////////////////////////////////////////////////////
void VE_Xplorer::_activateSequenceNodes(VE_SceneGraph::cfdRawNodeTraverser* cfdNT,
                                    pfNode* node)
{
   VE_Xplorer::cfdRawNodeWriteTraverser* cfdWT = (VE_Xplorer::cfdRawNodeWriteTraverser*)cfdNT;

   if(node->isOfType(pfSequence::getClassType())){
      std::cout<<"Found a raw sequence!!!"<<std::endl;
      //get the parent node
      pfGroup* parent = dynamic_cast<pfGroup*>(node->getParent(0));
      
      //replace the node in the graph
      parent->replaceChild(node,cfdWT->_sequenceList[cfdWT->_sequenceIndex++]);

      cfdWT->setTraversalStatus(VE_SceneGraph::cfdRawNodeTraverser::SKIP);
      return;
   }
   cfdWT->setTraversalStatus(VE_SceneGraph::cfdRawNodeTraverser::CONT);
   return;
}
#endif
//////////////////////////////////////
//write out the pfbFile             //
//////////////////////////////////////
void cfdRawNodeWriteTraverser::writeFile()
{

#ifdef _PERFORMER
   if(_fName && _root){
      //swap out cfdsequence nodes
      _traverseNode(_root);
      //store file as pfb file
      pfdStoreFile(_root,_fName);
      //replace raw Sequence nodes
      setCallback(0);
      _traverseNode(_root);
      _sequenceIndex = 0;
#elif _OSG
   if(_fName && _root.get()){
      _traverseNode(_root.get());
      //this may not work
      osgDB::writeNodeFile(*_root.get(),_fName);
#endif
      
   }
}
