#include "cfdWriteTraverser.h"
#include "cfdSequence.h"
#include <Performer/pf/pfGroup.h>
#include <Performer/pf/pfSequence.h>
#include <Performer/pfdb/pfpfb.h>
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
void _turnOnSequence(cfdNodeTraverser* cfdNT,pfNode* node)
{
   pfGroup* curNode = (pfGroup*)node;
   //turn on all the sequence nodes
   if(curNode->isOfType(cfdSequence::getClassType())){
      //make sure to start the "derned" sequence--otherwise
      //it won't be running in perfly!!!UGGGHHHH!!!!!
      ((cfdSequence*)curNode)->setPlayMode(CFDSEQ_START);
   }
}
//////////////////////////////////////////////////////
//swap the sequence nodes                           //
//////////////////////////////////////////////////////
void _swapSequenceNodes(cfdNodeTraverser* cfdNT,pfNode* node)
{
   cfdWriteTraverser* cfdWT = (cfdWriteTraverser*)cfdNT;
   pfGroup* curNode = (pfGroup*)node;

   //replace cfdSequence nodes
   if(curNode->isOfType(cfdSequence::getClassType())){
      //std::cout<<"\t_swapSequenceNodes: cfdSequence"<<std::endl;

      int nChildren = ((cfdSequence*)curNode)->getNumChildren();
      
      //add this node to our list of cfdSequences
      cfdWT->_sequenceList.push_back(curNode);

      //get the parent node
      pfGroup* parent = curNode->getParent(0);
      
      //create a pfSequence
      pfSequence* sequence =  new pfSequence();
      //copy the children of the cfdSequence
      for(int i = 0; i < nChildren; i++){
         sequence->addChild(((cfdSequence*)curNode)->getChild(i));
      }

      sequence->setDuration(1.0,-1);      // regular speed, continue forever

      double numSeconds = ((cfdSequence*)curNode)->getTime();
      sequence->setTime(-1,numSeconds);   // display all children for numSeconds

      sequence->setInterval(((cfdSequence*)curNode)->getLoopMode(),
                            ((cfdSequence*)curNode)->getBegin(),
                            ((cfdSequence*)curNode)->getEnd());
/*
      std::cout<<"\tnChildren = " << nChildren << std::endl;
      std::cout<<"\tnumSeconds = " << numSeconds << std::endl;
      std::cout<<"\t((cfdSequence*)curNode)->getLoopMode() = "
               <<((cfdSequence*)curNode)->getLoopMode() << std::endl;
      std::cout<<"\t((cfdSequence*)curNode)->getBegin() = "
               <<((cfdSequence*)curNode)->getBegin() << std::endl;
      std::cout<<"\t((cfdSequence*)curNode)->getEnd() = "
               <<((cfdSequence*)curNode)->getEnd() << std::endl;
*/

      //make sure to start the "derned" sequence--otherwise
      //it won't be running in perfly!!!UGGGHHHH!!!!!
      sequence->setMode(PFSEQ_START);

      //replace the node in the graph
      parent->replaceChild(curNode,sequence);

      //don't need to continue down 
      return;
   }else if(curNode->isOfType(pfSequence::getClassType())){
      //std::cout<<"\t_swapSequenceNodes: pfSequence"<<std::endl;
      //return tree to original state
      //get the parent node
      pfGroup* parent = curNode->getParent(0);

      //replace the node in the graph
      parent->replaceChild(curNode,cfdWT->_sequenceList[cfdWT->_sequenceIndex++]);

      //pfDelete(curNode);
      //don't need to continue down 
      return;
   }
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

      //store file as pfb file
      pfdStoreFile(_root,_fName);
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

      //store file as pfb file
      pfdStoreFile(_root,_fName);

      //_sequenceIndex = _sequenceList.size();
      //replace pfSequence nodes
      _traverseNode(_root);
      _sequenceIndex = 0;
   }
}
