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

#ifndef _CFD_RAW_NODE_WRITE_TRAVERSER_H_
#define _CFD_RAW_NODE_WRITE_TRAVERSER_H_

#include <vector>
#include "VE_SceneGraph/cfdNode.h"
#ifdef _PERFORMER
#include <Performer/pf/pfSequence.h>
#include <Performer/pf/pfNode.h>
#elif _OSG
#include <osg/Sequence>
#include <osg/Node>
#elif _OPENSG
#endif
#include "VE_SceneGraph/cfdRawNodeTraverser.h"

////////////////////////////////////////
//This class writes out a performer   // 
//binary file of the node passed in.  // 
//If graph contains any cfdSequence   //
//nodes, we convert them to pfSequence//
//nodes for correct viewing.          //
////////////////////////////////////////
namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdRawNodeWriteTraverser
	   : public VE_SceneGraph::cfdRawNodeTraverser
   {
      public:
         cfdRawNodeWriteTraverser();
         cfdRawNodeWriteTraverser(const cfdRawNodeWriteTraverser& cfdWT);
         cfdRawNodeWriteTraverser(std::string outFile);
         ~cfdRawNodeWriteTraverser();

         //set the output file name
         void setOutputFileName(std::string outFile);

         //write out the pfbFile
         void writeFile();

         //set the callback
         //whichCallback == 0 : activate sequence nodes
         //whichCallback == 1: swap sequence nodes
         void setCallback(int whichCallback);

         //set the pre node traverse callback
         virtual void setPreNodeTraverseCallback(preNodeTraverseCallback func);
   
         //set the node to traverse
         void setNode(VE_SceneGraph::cfdNode* root);

         //equal operator
         cfdRawNodeWriteTraverser& operator=(const cfdRawNodeWriteTraverser& rhs);
      protected:
#ifdef _OSG
         friend void _swapSequenceNodes(VE_SceneGraph::cfdRawNodeTraverser* , osg::Node* );
#elif _PERFORMER
         friend void _swapSequenceNodes(VE_SceneGraph::cfdRawNodeTraverser* , pfNode* );
         friend void _activateSequenceNodes(VE_SceneGraph::cfdRawNodeTraverser* cfdNT,
                                    pfNode* node);
#endif
         std::string _fName;
         int _toPfb;

      public:      
#ifdef _PERFORMER
         unsigned int _sequenceIndex;
         std::vector<VE_SceneGraph::cfdSequence*> _sequenceList;
#endif
   };
} 
#endif //_CFD_RAW_NODE_WRITE_TRAVERSER_H_
