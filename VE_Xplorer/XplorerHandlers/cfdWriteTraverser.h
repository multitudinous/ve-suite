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
#ifndef _CFD_WRITE_TRAVERSER_H_
#define _CFD_WRITE_TRAVERSER_H_
/*!\file cfdWriteTraverser.h
cfdWriteTraverser API
*/

/*!\class VE_Xplorer::cfdWriteTraverser
*
*/

#include <vector>
#include "VE_Xplorer/SceneGraph/cfdNode.h"
#include "VE_Xplorer/SceneGraph/cfdGroup.h"
#include "VE_Xplorer/SceneGraph/cfdSequence.h"
#ifdef _PERFORMER
#include <Performer/pf/pfSequence.h>
#include <Performer/pf/pfNode.h>
#elif _OSG
#include <osg/Sequence>
#include <osg/Node>
#elif _OPENSG
#endif
#include "VE_Xplorer/SceneGraph/cfdNodeTraverser.h"

////////////////////////////////////////
//This class writes out a performer   // 
//binary file of the node passed in.  // 
//If graph contains any cfdSequence   //
//nodes, we convert them to pfSequence//
//nodes for correct viewing.          //
////////////////////////////////////////
namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdWriteTraverser : public VE_SceneGraph::cfdNodeTraverser
   {
      public:
         cfdWriteTraverser();
         cfdWriteTraverser(const cfdWriteTraverser& cfdWT);
         cfdWriteTraverser(std::string outFile);
         ~cfdWriteTraverser();

         //set the output file name
         void setOutputFileName(std::string outFile);

         //write out the pfbFile
         void writePfbFile();

         //set the callback
         //whichCallback == 0 : activate sequence nodes
         //whichCallback == 1: swap sequence nodes
         void setCallback(int whichCallback);

         

         //set the pre node traverse callback
         virtual void setPreNodeTraverseCallback(preNodeTraverseCallback func);
   
         //equal operator
         cfdWriteTraverser& operator=(const cfdWriteTraverser& rhs);
      protected:
         //friend void _swapSequenceNodes(VE_SceneGraph::cfdNodeTraverser* ,VE_SceneGraph::cfdNode* );
         //friend void _turnOnSequence(VE_SceneGraph::cfdNodeTraverser* ,VE_SceneGraph::cfdNode* );
         //friend void _activateSequenceNodes(VE_SceneGraph::cfdNodeTraverser* cfdNT,
         //                               VE_SceneGraph::cfdNode* node);
         std::string _fName;
         int _toPfb;

      public:      
         int _sequenceIndex;
         //std::vector<VE_SceneGraph::cfdNode*> _sequenceList;
#ifdef _OSG
         std::vector<osg::Sequence*> _sequenceList;
#elif _PERFORMER
         std::vector<pfSequence*> _sequenceList;
#endif
   };
} 
#endif //_CFD_WRITE_TRAVERSER_H_
