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
 * File:          $RCSfile: cfdWriteTraverser.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _CFD_WRITE_TRAVERSER_H_
#define _CFD_WRITE_TRAVERSER_H_

#include <vector>
#include "VE_SceneGraph/cfdNode.h"
#ifdef _PERFORMER
#include <Performer/pf/pfNode.h>
#elif _OSG
#include <osg/Node>
#elif _OPENSG
#endif
#include "VE_SceneGraph/cfdNodeTraverser.h"
////////////////////////////////////////
//This class writes out a performer   // 
//binary file of the node passed in.  // 
//If graph contains any cfdSequence   //
//nodes, we convert them to pfSequence//
//nodes for correct viewing.          //
////////////////////////////////////////

class cfdWriteTraverser:public VE_SceneGraph::cfdNodeTraverser{
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

   //friend void _swapSequenceNodes(cfdNodeTraverser* cfdWT,pfNode* node);
   //friend void _turnOnSequence(cfdNodeTraverser* cfdWT,pfNode* node);
   friend void _swapSequenceNodes(VE_SceneGraph::cfdNodeTraverser* cfdWT,VE_SceneGraph::cfdNode* node);
   friend void _turnOnSequence(VE_SceneGraph::cfdNodeTraverser* cfdWT,VE_SceneGraph::cfdNode* node);
   
   char* _fName;
   int _sequenceIndex;
   int _toPfb;
   std::vector<VE_SceneGraph::cfdNode*> _sequenceList;
};
 
#endif //_CFD_WRITE_TRAVERSER_H_
