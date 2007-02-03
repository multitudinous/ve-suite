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

#include "VE_Xplorer/XplorerHandlers/cfdWriteTraverser.h"
#include "VE_Xplorer/SceneGraph/cfdGroup.h"
#include "VE_Xplorer/SceneGraph/cfdSequence.h"

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
cfdWriteTraverser::cfdWriteTraverser()
:VE_SceneGraph::cfdNodeTraverser()
{
   _fName.erase();// = 0;
   _sequenceIndex = 0;
   _toPfb = 0;
   _preFunc = 0;
}
////////////////////////////////////////////////////////////////////
cfdWriteTraverser::cfdWriteTraverser(const cfdWriteTraverser& cfdWT)
:VE_SceneGraph::cfdNodeTraverser(cfdWT)
{
   _fName.erase();// = 0;
   _sequenceIndex = 0;
   _toPfb = 0;

   _preFunc = cfdWT._preFunc;
   
   //_fName = new char[strlen(cfdWT._fName)+1];
   _fName.assign( cfdWT._fName);// strcpy(_fName,cfdWT._fName);
}
///////////////////////////////////////////////////
cfdWriteTraverser::cfdWriteTraverser(std::string outFile)
:VE_SceneGraph::cfdNodeTraverser()
{
   _fName.erase();// = 0;
   _sequenceIndex = 0;
   _toPfb = 0;
   setOutputFileName(outFile);
}
///////////////////////////////////////
//Destructor                         //
///////////////////////////////////////
cfdWriteTraverser::~cfdWriteTraverser()
{
   if(_fName.c_str()){
      //delete [] _fName;
      _fName.erase();// = 0;
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
      this->VE_SceneGraph::cfdNodeTraverser::operator =(rhs);
      
      //copy our new name
      //_fName = new char[strlen(rhs._fName)+1];
      _fName.assign( rhs._fName );//strcpy(_fName,rhs._fName);
   }
   return *this;
}
/////////////////////////////////////////////////////
//set which callback to use                        //
/////////////////////////////////////////////////////
void cfdWriteTraverser::setCallback(int swapActivate)
{
   if(swapActivate)
   {
      //_preFunc = _swapSequenceNodes;
   }
   else
   {
      //_preFunc = _activateSequenceNodes;
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
void cfdWriteTraverser::setOutputFileName(std::string outFile)
{
   if(_fName.c_str()){
      //delete [] _fName;
      _fName.erase();// = 0;
   }
   //_fName = new char[strlen(outFile)+1];
   _fName.assign( outFile );//strcpy(_fName,outFile);
}
//////////////////////////////////////////////////////////
//turn on the sequence nodes for proper read back       //
//////////////////////////////////////////////////////////
/*void VE_Xplorer::_turnOnSequence(VE_SceneGraph::cfdNodeTraverser* cfdNT,
                   VE_SceneGraph::cfdNode* node)
{
   if(node->GetCFDNodeType() == VE_SceneGraph::cfdSceneNode::CFD_SEQUENCE)
   {
      //make sure to start the "derned" sequence--otherwise
      //it won't be running in perfly!!!UGGGHHHH!!!!!
      ((VE_SceneGraph::cfdSequence*)node)->setPlayMode(CFDSEQ_START);
   } 
}
///////////////////////////////////////////////////////////////////////////
//swap the sequence nodes                                                //
///////////////////////////////////////////////////////////////////////////
void _swapSequenceNodes(VE_SceneGraph::cfdNodeTraverser* cfdNT,
                                VE_SceneGraph::cfdNode* node)
{
   //need to implement using getRawNode calls!!

   //Need to fix
   VE_Xplorer::cfdWriteTraverser* cfdWT = (VE_Xplorer::cfdWriteTraverser*)cfdNT;

   //replace cfdSequence nodes
   if(node->GetCFDNodeType() == VE_SceneGraph::cfdSceneNode::CFD_SEQUENCE){
      int nChildren = ((VE_SceneGraph::cfdSequence*)node)->GetNumChildren();

      std::cout<<"Found a cfdSequence with: "<<nChildren<<" children."<<std::endl;
      

      //get the parent node
      VE_SceneGraph::cfdGroup* parent = dynamic_cast<VE_SceneGraph::cfdGroup*>(node->GetParent(0));
      if(!parent){
         std::cout<<"Couldn't find parent of this cfdSequence!!"<<std::endl;
      }
      //create a pfSequence
#ifdef _PERFORMER
      pfSequence* sequence =  new pfSequence();
#elif _OSG
      osg::ref_ptr<osg::Sequence> sequence =  new osg::Sequence();
#elif _OPENSG
#endif
      std::cout<<"Creating raw sequence!!"<<std::endl;
      //copy the children of the cfdSequence
      for(int i = 0; i < nChildren; i++){
         sequence->addChild(((VE_SceneGraph::cfdSequence*)node)->GetChild(i)->GetRawNode());
      }

      sequence->setDuration(1.0,-1);      // regular speed, continue forever

      double numSeconds = ((VE_SceneGraph::cfdSequence*)node)->getTime();
      //sequence->setTime(-1,numSeconds);   // display all children for numSeconds

#ifdef _PERFORMER
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
      if(((pfGroup*)parent->GetRawNode())->replaceChild((VE_SceneGraph::cfdSequence*)node,sequence)){
         //add this node to our list of cfdSequences
         cfdWT->_sequenceList.push_back(sequence);
#elif _OSG
      if(((osg::Group*)parent->GetRawNode())->replaceChild((VE_SceneGraph::cfdSequence*)node,sequence.get())){
         //add this node to our list of cfdSequences
         cfdWT->_sequenceList.push_back(sequence.get());
#elif _OPENSG
#endif
         std::cout<<"Replaced the cfdSequence w/ a raw sequence!!"<<std::endl;
      }
     
      //don't need to continue down 
      cfdWT->setTraversalStatus(VE_SceneGraph::cfdNodeTraverser::SKIP);
      return;
   }
   cfdWT->setTraversalStatus(VE_SceneGraph::cfdNodeTraverser::CONT);
   return;

}
/////////////////////////////////////////////////////////////////////////////////////
//put the cfdSequence back on the tree                                             //
/////////////////////////////////////////////////////////////////////////////////////
void VE_Xplorer::_activateSequenceNodes(VE_SceneGraph::cfdNodeTraverser* cfdNT,
                                          VE_SceneGraph::cfdNode* node)
{

   VE_Xplorer::cfdWriteTraverser* cfdWT = (VE_Xplorer::cfdWriteTraverser*)cfdNT;

#ifdef _PERFORMER
   if(node->GetRawNode()->isOfType(pfSequence::getClassType())){
#elif _OSG
   //if(!strcmp(node->GetRawNode()->className(),"Sequence")){
   if(node->GetCFDNodeType() == VE_SceneGraph::cfdSceneNode::CFD_SEQUENCE){
#elif _OPENSG
#endif
      std::cout<<"Found a raw sequence!!!"<<std::endl;
      //get the parent node
      VE_SceneGraph::cfdGroup* parent = (VE_SceneGraph::cfdGroup*)node->GetParent(0);
     //replace the node in the graph
#ifdef _OSG
      ((osg::Group*)((VE_SceneGraph::cfdGroup*)parent)->GetRawNode())->replaceChild(cfdWT->_sequenceList[cfdWT->_sequenceIndex++],
                                                 node->GetRawNode());
#elif _PERFORMER
      ((pfGroup*)((VE_SceneGraph::cfdGroup*)parent)->GetRawNode())->replaceChild(cfdWT->_sequenceList[cfdWT->_sequenceIndex++],
                                                 node->GetRawNode());

#endif
      cfdWT->setTraversalStatus(VE_SceneGraph::cfdNodeTraverser::SKIP);
      return;
   }
   cfdWT->setTraversalStatus(VE_SceneGraph::cfdNodeTraverser::CONT);
   return;
}*/
//////////////////////////////////////
//write out the pfbFile             //
//////////////////////////////////////
void cfdWriteTraverser::writePfbFile()
{
   if(_fName.c_str() && _root){
      //swap out cfdsequence nodes
      _traverseNode(_root);

#ifdef _PERFORMER
      //store file as pfb file
     pfdStoreFile(_root->GetRawNode(),_fName.c_str());
#elif _OSG
      //this may not work
      osgDB::writeNodeFile(*_root->GetRawNode(),_fName);

#endif
      //replace raw Sequence nodes
      setCallback(0);
      _traverseNode(_root);
      _sequenceIndex = 0;
   }
}
