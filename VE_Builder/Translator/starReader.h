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
 * File:          $RCSfile: starReader.h,v $
 * Date modified: $Date: 2004/03/23 16:32:49 $
 * Version:       $Revision: 1.4 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef STARREADER_H
#define STARREADER_H
#include <vector>

class vtkUnstructuredGrid;

class starReader
{
   public:
      starReader( char * paramFile );
      ~starReader( void );

      void SetDebugLevel( int );

      float * GetRotations( void );
      float * GetTranslations( void );
      float   GetScaleFactor( void );
      int     GetWriteOption( void );
      char  * GetVTKFileName( void );

      void  ReadParameterFile( void );
      vtkUnstructuredGrid * GetUnsGrid();

   private:
      char  paramFileName[100];
      char  starCellFileName[100];
      char  starVertFileName[100];
      char  starUsrFileName[100];
      char  vtkFileName[100];
      char  textline[256];
      int   numScalars;
      std::vector< char * > scalarName;
      int   numVectors;
      std::vector< char * > vectorName;
      int   debug;
      int   writeOption;
      float rotations[ 3 ];
      float translations[ 3 ];
      float scaleValue;
};
#endif
