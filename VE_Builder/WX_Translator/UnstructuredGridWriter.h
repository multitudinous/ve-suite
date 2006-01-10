/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *   - National Energy Technology Laboratory, www.netl.doe.gov
 *   - West Virginia Virtual Environments Laboratory, wvvel.csee.wvu.edu
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
 * File:          $RCSfile: UnstructuredGridWriter.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

// The VTK unstructured grid writer.
// Inputs: (char *inFile, char *outDir, int tStep, int maxTs, int type, resHead *resH, spHead *spH, mfixData *mfD)
// Outputs: vtkUnstructuredGrid file to disk
//          if type = 1, output = ASCII legacy format (.vtk)
//          if type = 2, output = binary legacy format (.vtk)
//          if type = 3, output = XML serial format (.vtu)
//          if type = 4, output = XML parallel format (.pvtu)
// Author: Jim Canon (jcanon@csee.wvu.edu)
// Last revision: 04-08-04
// Version: 1.3

#ifndef UNSTRUCTUREDGRIDWRITER_H
#define UNSTRUCTUREDGRIDWRITER_H

#include <iostream>
#include <stdlib.h>
#include <vtkUnstructuredGrid.h>
#include <vtkRectilinearGrid.h>
#include <vtkUnstructuredGridWriter.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkXMLPUnstructuredGridWriter.h>
#include <vtkStructuredGrid.h>
#include <vtkIdList.h>
#include <vtkGenericCell.h>
#include <vtkFloatArray.h>
#include <vtkPoints.h>
#include <vtkCellData.h>
#include <vtkCellArray.h>
#include <vtkHexahedron.h>
#include <vtkDoubleArray.h>

#include "mfixDataHeaders.h"
#include "UnstructuredGridWriter.h"
#include "converter.h"

int UnstructuredGridWriter(char *inFile, char *outDir, int tStep, int MaxTs, int type, resHead *resH, spHead *spH, mfixData *mfD);

#endif
