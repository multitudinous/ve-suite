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
   _preFunc = _swapSequenceNodes;
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
   _preFunc = _swapSequenceNodes;
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
///////////////////////////////////////////////////
//the prenode callback  is already set           //
///////////////////////////////////////////////////
void cfdWriteTraverser::setPreNodeTraverseCallback(
		preNodeTraverseCallback func)
{
   std::cout<<"WARNING: cfdWriteTraverser::setPreNodeTraverserCallback"<<std::endl;
  std::cout<<"Pre-Node callback is pre-defined!!!"<<std::endl;
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
//////////////////////////////////////////////////////
//swap the sequence nodes                           //
//////////////////////////////////////////////////////
void _swapSequenceNodes(cfdNodeTraverser* cfdNT,pfNode* node)
{
   cfdWriteTraverser* cfdWT = (cfdWriteTraverser*)cfdNT;
   pfGroup* curNode = (pfGroup*)node;
   

   //replace cfdSequence nodes
   if(curNode->isOfType(cfdSequence::getClassType())){
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
      //TODO:set the properties of the sequence here!!!!
      sequence->setDuration(1.0,-1);
      sequence->setTime(-1,1.0);
      switch (((cfdSequence*)curNode)->loopMode()){
         case CFDSEQ_CYCLE:
            sequence->setInterval(PFSEQ_CYCLE,((cfdSequence*)curNode)->begin(),
                                  ((cfdSequence*)curNode)->end());
            break;
         case CFDSEQ_SWING:
            sequence->setInterval(PFSEQ_SWING,((cfdSequence*)curNode)->begin(),
                                  ((cfdSequence*)curNode)->end());
                                            
            break;
      };
      //make sure to start the "derned" sequence--otherwise
      //it won't be running in perfly!!!UGGGHHHH!!!!!
      sequence->setMode(PFSEQ_START);
      //replace the node in the graph
      parent->replaceChild(curNode,sequence);

      //don't need to continue down 
      return;
   }else if(curNode->isOfType(pfSequence::getClassType())){
      //int nChildren = curNode->getNumChildren();
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
