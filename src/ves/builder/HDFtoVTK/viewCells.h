/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef BIV_VIEWCELLS_H
#define BIV_VIEWCELLS_H

class vtkUnstructuredGrid;
class vtkDataSet;
class vtkRectilinearGrid;
class vtkActor;
class vtkFollower;
class vtkRenderer;

vtkUnstructuredGrid* extractExteriorCellsOnly( vtkUnstructuredGrid *output );
void viewCells( vtkDataSet *output, const float shrinkFactor = 0.95 );
void viewXSectionOfRectilinearGrid( vtkRectilinearGrid *output );
void GetAxesSymbol( vtkActor * axesActor );
void GetAxesLabels( vtkFollower * xActor, vtkFollower * yActor, vtkFollower * zActor );
vtkActor * AddToRenderer( vtkDataSet *dataset, vtkRenderer* ren1, const float shrinkFactor = 1.0 );

#endif    // VIEWCELLS_H
