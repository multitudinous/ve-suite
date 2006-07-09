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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_TEACHER_H
#define CFD_TEACHER_H

#include <vector>
#include <string>
namespace VE_Xplorer
{
   class cfdWriteTraverser;
   class cfdCommandArray;
}

namespace VE_SceneGraph
{
   class cfdDCS;
   class cfdGroup;
   class cfdNode;
}

#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"

//A reader that reads performer binary files
namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdTeacher : public cfdGlobalBase
   {
      public:
         cfdTeacher( std::string, VE_SceneGraph::cfdDCS* );

         ~cfdTeacher( );

         // compare VjObs_i commandArray with its child's value
         virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );

         // in future, multi-threaded apps will make a copy of VjObs_i commandArray
         virtual void UpdateCommand();
         void writePFBFile( VE_SceneGraph::cfdNode* graph,std::string fileName);

         VE_SceneGraph::cfdDCS* GetcfdDCS( );
         VE_SceneGraph::cfdNode* getpfNode( int );
         int getNumberOfFiles();
         std::string getFileName( int i );

         ///Clear the stored scenes
         void ClearStoredScenes();
         ///Switch the active scene
         ///\param whichScene The scene to display
         void LoadScene(unsigned int whichScene);
         ///Save out the scene
         void RecordScene();

      private:
         VE_SceneGraph::cfdDCS* DCS;
         VE_SceneGraph::cfdDCS* _worldDCS;
         std::vector< VE_SceneGraph::cfdNode* > node;  // array of nodes
         int pfb_count;
	      std::vector<std::string> pfbFileNames;
         std::string directory;
         cfdWriteTraverser* _cfdWT;
   };
}
#endif   // CFD_TEACHER_H
