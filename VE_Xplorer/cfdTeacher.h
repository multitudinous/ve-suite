/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdTeacher.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_TEACHER_H
#define CFD_TEACHER_H

#include <vector>

class cfdDCS;
class cfdGroup;
class cfdNode;
class cfdCommandArray;
class cfdNode;
class cfdWriteTraverser;

#include "cfdGlobalBase.h"

//A reader that reads performer binary files
class cfdTeacher : public cfdGlobalBase
{
   public:
      cfdTeacher( char directory[], cfdDCS* );

      ~cfdTeacher( );

      // compare VjObs_i commandArray with its child's value
      virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );

      // in future, multi-threaded apps will make a copy of VjObs_i commandArray
      virtual void UpdateCommand();
      void writePFBFile( cfdNode* graph,char* fileName);

      cfdDCS* GetcfdDCS( );
      cfdNode* getpfNode( int );
      int getNumberOfFiles();
      char * getFileName( int i );
      char * getDirectory();
      void setDirectory( char * );

   private:
      cfdDCS*     DCS;
      cfdDCS*     _worldDCS;
      cfdNode**   node;  // array of nodes
      int numFiles;
      std::vector<char*> pfbFileNames;
      char * directory;
      int pfb_count;
      cfdWriteTraverser* _cfdWT;
};

#endif
