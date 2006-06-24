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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream.h>
#include <fstream.h>
#include <iomanip.h>
#include <stdlib.h>
#include <sstream>

#include "vtkUnstructuredGrid.h"
#include "vtkXMLUnstructuredGridWriter.h"
#include "vtkMFIXReader.h"

using namespace std;

//**************************************************************************
//*****                     Main                                        ****
//**************************************************************************

int main(int argc, char **argv) 
{  

	vtkMFIXReader *reader = vtkMFIXReader::New();
	reader->SetFileName(argv[1]);
	reader->Update();
	int TimeStepRange[2]; 
	reader->GetTimeStepRange(TimeStepRange);
	cout << "Number of Timesteps = " << TimeStepRange[0] << ", "<< TimeStepRange[1] << endl;
	reader->SetTimeStep(atoi(argv[2]));

	vtkUnstructuredGrid *grid;
	grid = reader->GetOutput();

	vtkXMLUnstructuredGridWriter *Writer = vtkXMLUnstructuredGridWriter::New();
	Writer->SetInput(grid);
	Writer->SetFileName(argv[3]);
	Writer->Write();

	return 1;
}

