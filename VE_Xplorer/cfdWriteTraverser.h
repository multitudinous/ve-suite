#ifndef _CFD_WRITE_TRAVERSER_H_
#define _CFD_WRITE_TRAVERSER_H_

#include <vector>
#include <Performer/pf/pfNode.h>
#include "cfdNodeTraverser.h"
////////////////////////////////////////
//This class writes out a performer   // 
//binary file of the node passed in.  // 
//If graph contains any cfdSequence   //
//nodes, we convert them to pfSequence//
//nodes for correct viewing.          //
////////////////////////////////////////

class cfdWriteTraverser
:public cfdNodeTraverser{
public:
   cfdWriteTraverser();
   cfdWriteTraverser(const cfdWriteTraverser& cfdWT);
   cfdWriteTraverser(char* outFile);
   ~cfdWriteTraverser();

   //set the output file name
   void setOutputFileName(char* outFile);

   //write out the pfbFile
   void writePfbFile();

   //set the callback
   //whichCallback == 0 : activate sequence nodes
   //whichCallback == 1: swap sequence nodes
   void setCallback(int whichCallback);

   //activate sequence nodes for writing/reading
   void activateSequenceNodes();

   //set the pre node traverse callback
   virtual void setPreNodeTraverseCallback(preNodeTraverseCallback func);
   
   //equal operator
   cfdWriteTraverser& operator=(const cfdWriteTraverser& rhs);
protected:
   friend void _swapSequenceNodes(cfdNodeTraverser* cfdWT,pfNode* node);
   friend void _turnOnSequence(cfdNodeTraverser* cfdWT,pfNode* node);
   char* _fName;
   int _sequenceIndex;
   int _toPfb;
   std::vector<pfNode*> _sequenceList;
};
 
#endif //_CFD_WRITE_TRAVERSER_H_
