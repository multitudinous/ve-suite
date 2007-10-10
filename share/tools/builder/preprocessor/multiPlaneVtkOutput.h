/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#ifndef MULTIPLANEVTKOUTPUT_H
#define MULTIPLANEVTKOUTPUT_H

#include <string>
//class vtkUnstructuredGrid;
class vtkDataSet;
class vtkPolyData;

class multiPlaneVtkOutput
{
 public:
   multiPlaneVtkOutput( std::string dirname );

   ~multiPlaneVtkOutput( );

   void writeMultiPlanes( vtkDataSet *unsGrid, 
                          int X_cutMaxStepRange = 20, 
                          int Y_cutMaxStepRange = 20, 
                          int Z_cutMaxStepRange = 20,
                          int multiPlaneOption = 0,
                          int transientFileNumber = 0,
                          int myid = 1, 
                          int xprocs = 1, 
                          int yprocs = 1, 
                          int zprocs = 1,
                          int numProcs = 1 );

   void readParamFileandWriteMultiPlanes( vtkDataSet *, 
                                          std::string, 
                                          int multiPlaneOption = 0,
                                          int transientFileNumber = 0 );

 private:
   vtkPolyData * MakePolyData( int xyz, float position );

   void WritePolyData( vtkPolyData * polyData, int xyz, int i );

   void WriteMultiPolyData( vtkPolyData * polyData, int xyz, int i );

   vtkDataSet *unsData;

   std::string postDataDir;
};

#endif
