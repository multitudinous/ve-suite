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

class pfDCS;
class pfGroup;
class pfNode;
#ifdef _CFDCOMMANDARRAY
class cfdCommandArray;
#endif //_CFDCOMMANDARRAY


//A reader that reads performer binary files
class cfdTeacher
{
 public:
  cfdTeacher( char directory[], pfGroup * );

  ~cfdTeacher( );
#ifdef _CFDCOMMANDARRAY
   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();
#endif //_CFDCOMMANDARRAY

  pfDCS * getpfDCS( );
  pfNode * getpfNode( int );
  int getNumberOfFiles();
  char * getFileName( int i );
  char * getDirectory();
  void setDirectory( char * );

 private:
  pfDCS *DCS;
  pfNode **node;  // array of nodes
  int numFiles;
  std::vector<char*> pfbFileNames;
  char * directory;
};

#endif
