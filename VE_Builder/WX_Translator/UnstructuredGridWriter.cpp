// UnstructuredGridWriter.cpp
// The VTK unstructured grid writer.
// Inputs: ((char *inFile, char *outDir, int tStep, int maxTs, int type, resHead *resH, spHead *spH, mfixData *mfD)
// Outputs: vtkUnstructuredGrid file to disk
//          if type = 1, output = ASCII legacy format (.vtk)
//          if type = 2, output = binary legacy format (.vtk)
//          if type = 3, output = XML serial format (.vtu)
//          if type = 4, output = XML parallel format (.pvtu)
// Author: Jim Canon (jcanon@csee.wvu.edu)
// Last revision: 04-08-04
// Version: 1.6


#include "UnstructuredGridWriter.h"


// Add only good cell data to ghost free mesh
void filterTheCellData(vtkCellData *filteredCD, vtkCellData *unfilteredCD, const int *mapVector) {
	// For each array
	for (int i=0; i<unfilteredCD->GetNumberOfArrays(); i++) {
		vtkFloatArray *filteredData = vtkFloatArray::New();
		filteredData->SetNumberOfComponents(unfilteredCD->GetArray(i)->GetNumberOfComponents());
		filteredData->SetName(unfilteredCD->GetArray(i)->GetName());

		int numOriginalTuples = unfilteredCD->GetArray(i)->GetNumberOfTuples();
		// For each tuple
		for (int j=0; j<numOriginalTuples; j++) {
			if (mapVector[j] != -1)
				filteredData->InsertTuple(mapVector[j], unfilteredCD->GetArray(i)->GetTuple(j));
		}
		filteredCD->AddArray(filteredData);
		filteredData->Delete();
	}
	return;
}


// Add only ghost free cells to new mesh
vtkUnstructuredGrid *filterMeshByCellData(vtkUnstructuredGrid *unsGrid) {
	// Loop over the cells and locate ghost cells
	int npts, ptId, ce;
	vtkIdList *pts = vtkIdList::New();
	vtkPoints *nonGhostCellPoints = vtkPoints::New();
	vtkIdList *cellIds = vtkIdList::New();
	vtkGenericCell *cell = vtkGenericCell::New();
	vtkUnstructuredGrid *filteredMesh = vtkUnstructuredGrid::New();
	double *x;

	int numCells = unsGrid->GetNumberOfCells();
	int numPoints = unsGrid->GetNumberOfPoints();
	int *mapVector = new int[numCells];
	for (int i=0; i<numCells; i++)
		mapVector[i] = -1;

	filteredMesh->Allocate(numCells, numCells);

	// Loop for all cells
	for (int cellId=0; cellId<numCells; cellId++) {      
		unsGrid->GetCell(cellId, cell);
		npts = cell->GetNumberOfPoints();
		cell->GetPointIds()->SetNumberOfIds(npts);
		pts->Reset();
		// For this cell load up the point data, assuming that it is a non ghost cell
		// i typically goes from 0 to 7, ptId is the unique identifier of the cell vertex,
		// if any cell is a ghost cell, then do not include this cell in the filtered mesh
		float ghostCell = unsGrid->GetCellData()->GetComponent(cellId,0);
		if (ghostCell < 9.87654e+031) {
			for (int i=0; i<npts; i++) {
				ptId = cell->GetPointId(i);
				if (ptId >= numPoints) {
					cerr << "ERROR: unexpected ptId, exiting." << endl;
					exit(1);
				}
				x = unsGrid->GetPoint(ptId);
				// Insert point into next slot (returns slot id (pt))
				nonGhostCellPoints->InsertPoint(ptId, x);
				pts->InsertId(i, ptId);
			}
			// Insert cell into next slot (returns slot id (cell)) 
			ce = filteredMesh->InsertNextCell(cell->GetCellType(), pts);
			mapVector[cellId] = ce;
		}
	} 

	// Filter out the cell data
	filterTheCellData(filteredMesh->GetCellData(), unsGrid->GetCellData(), mapVector);
	filteredMesh->SetPoints(nonGhostCellPoints);

	// Cleanup and return
	delete [] mapVector;
	mapVector = NULL;
	pts->Delete();
	nonGhostCellPoints->Delete();
	cellIds->Delete();
	cell->Delete();

	return filteredMesh;
}


// Convert rectilinear grid to unstructured grid
vtkUnstructuredGrid * convertToUnstructuredGrid(vtkDataSet *rGrid) {
	int numCells = rGrid->GetNumberOfCells();
	int numPts = rGrid->GetNumberOfPoints();
	vtkUnstructuredGrid *uGrid = vtkUnstructuredGrid::New();
	int pt, npts;
	vtkIdList *pts = vtkIdList::New();
	vtkGenericCell *cell = vtkGenericCell::New();

	// Attach cell information to the unstructured grid
	// For all cells
	for (int cellId = 0; cellId<numCells; cellId++) {
		rGrid->GetCell(cellId, cell);
		npts = cell->GetNumberOfPoints();
		pts->Reset();
		for (int i=0; i<npts; i++) {
			pt = cell->GetPointId(i);
			pts->InsertId(i, pt);
		}
		uGrid->InsertNextCell(cell->GetCellType(), pts);
	}

	// For all points
	vtkPoints *vertices = vtkPoints::New();
	double x[3];
	for (int ptId=0; ptId<numPts; ptId++) {
		rGrid->GetPoint(ptId, x);
		vertices->InsertPoint(ptId, x);
	}

	// Attach point information to the unstructured grid
	uGrid->SetPoints(vertices);

	// Attach data
	int numCDArrays = rGrid->GetCellData()->GetNumberOfArrays();
	if (numCDArrays) {
		uGrid->GetCellData()->AllocateArrays(numCDArrays);
		for (int i=0; i<numCDArrays; i++) {
			// Attach field data
			uGrid->GetCellData()->AddArray(rGrid->GetCellData()->GetArray(i));
		}
	}

	// Cleanup
	pts->Delete();
	cell->Delete();
	vertices->Delete();

	return uGrid;
}
	

void attachDataToCartesianMesh(int numComponents, int numArrays, int varNum, vtkFloatArray **dataArray, vtkRectilinearGrid *rGrid, resHead *resH, mfixData *mfD) {
// Set up array to hold the ghost free data
	float *scalarData = new float[resH->iMax*resH->jMax*resH->kMax];
	if (scalarData == NULL) {
		cerr << "ERROR: Cannot get memory for scalarData, exiting." << endl;
		exit(1);
	}

	// Strip out the boundry ghost cells (mfD->sp1Array1[index] >= 9.87654e+031)
	// Lets loop for each array
	int counter, index;
	for (int paramIndex=0; paramIndex<numArrays; paramIndex++) {
		// Lets loop for each component or each species
		for (int kk=0; kk<numComponents; kk++) {
			counter = 0;
			index = 0;
			for (int k=0; k<resH->kMax2; k++) {
				for (int j=0; j<resH->jMax2; j++) {
					for (int i=0; i<resH->iMax2; i++) {
						// Remove boundry cells
						if (k==0 || k==resH->kMax2-1 || j==0 || j==resH->jMax2-1 || i==0 || i==resH->iMax2-1)
							index++;
						else {
							// Void fraction
							if (varNum==0) {
								scalarData[counter++] = mfD->sp1Array1[index++];
							}
							// Gas pressure
							else if (varNum==1) {
								scalarData[counter++] = mfD->sp2Array1[index++];
							}
							// P-Star
							else if (varNum==2) {
								scalarData[counter++] = mfD->sp2Array2[index++];
							}
							// Gas velocity
							else if (varNum==3) {
								if (kk == 0)
									scalarData[counter++] = mfD->sp3Array1[index++];
								else if (kk == 1)
									scalarData[counter++] = mfD->sp3Array2[index++];
								else if (kk == 2)
									scalarData[counter++] = mfD->sp3Array3[index++];
							}
							// Solid phase velocity
							else if (varNum==4) {
								if (kk == 0)
									scalarData[counter++] = mfD->sp4Array1[paramIndex][index++];
								else if (kk == 1)
									scalarData[counter++] = mfD->sp4Array2[paramIndex][index++];
								else if (kk == 2)
									scalarData[counter++] = mfD->sp4Array3[paramIndex][index++];
							}
							// Solid phase density
							else if (varNum==5) {
								scalarData[counter++] = mfD->sp5Array1[paramIndex][index++];
							}
							// Temperatures
							else if (varNum==6) {
								float version_number = strtod(resH->version+7, (char **)resH->version+11);
								// For versions > 1.15
								if (version_number > 1.15) {
									if (paramIndex == 0)
										scalarData[counter++] = mfD->sp6Array1[index++];
									else
										scalarData[counter++] = mfD->sp6Array2[paramIndex-1][index++];
								}
								// For versions <= 1.15
								else {
									if (paramIndex == 0)
										scalarData[counter++] = mfD->sp6Array1[index++];
									else 
										scalarData[counter++] = mfD->sp6Array2b[index++];
								}
							}
							// Gas phase species
							else if (varNum==7) {
								scalarData[counter++] = mfD->sp7Array1[paramIndex][index++];
							}
							// Solid phase species
							else if (varNum==8) {
								scalarData[counter++] = mfD->sp7Array2[0][0][index++];
							}
							// Granular temperature
							else if (varNum==9) {
								scalarData[counter++] = mfD->sp8Array1[paramIndex][index++];
							}
							// Scalars
							else if (varNum==10) {
								scalarData[counter++] = mfD->sp9Array1[paramIndex][index++];
							}
							// Reaction Rates
							else if (varNum==11) {
								scalarData[counter++] = mfD->spaArray1[paramIndex][index++];
							}
							// Check for ghost cells
							if (resH->flag[index-1] >= 100)
								scalarData[counter-1] = 9.87655e+031;
						}
					}
				}
			}
	
			// Resize scalarData array (boundry ghost cells are now gone)
			float* temp = new float[counter];
			float* temp2 = NULL;
			memcpy(temp, scalarData, sizeof(scalarData)*counter);
			temp2 = scalarData;
			scalarData = temp;
			delete [] temp2;
			temp2 = NULL;

			// For each tuple
			for (int ii=0; ii<counter; ii++) {
				if (numComponents == 1)
					dataArray[paramIndex]->InsertTuple(ii, &scalarData[ii]); 
				else 
					dataArray[paramIndex]->InsertComponent(ii, kk, scalarData[ii]);
			}
		}
		// Attach data to mesh
		(rGrid->GetCellData())->AddArray(dataArray[paramIndex]);
	}

	// Cleanup
	delete [] scalarData;
	scalarData = NULL;

	return;
}


void attachDataToCylindricalMesh(int numComponents, int numArrays, int varNum, vtkFloatArray **dataArray, vtkUnstructuredGrid *uGrid, resHead *resH, mfixData *mfD) {
// Set up array to hold the ghost free data
	float *scalarData = new float[resH->iMax*resH->jMax*resH->kMax];
	if (scalarData == NULL) {
		cerr << "ERROR: Cannot get memory for scalarData, exiting." << endl;
		exit(1);
	}

	// Strip out the boundry ghost cells (mfD->sp1Array1[index] >= 9.87654e+031)
	// Lets loop for each array
	int counter, index;
	for (int paramIndex=0; paramIndex<numArrays; paramIndex++) {
		// Lets loop for each component or each species
		for (int kk=0; kk<numComponents; kk++) {
			counter = 0;
			index = 0;
			for (int k=0; k<resH->kMax2; k++) {
				for (int j=0; j<resH->jMax2; j++) {
					for (int i=0; i<resH->iMax2; i++) {
						// Remove boundry cells
						if (k==0 || k==resH->kMax2-1 || j==0 || j==resH->jMax2-1 || i==0 || i==resH->iMax2-1)
							index++;
						else {
							// Void fraction
							if (varNum==0) {
								scalarData[counter++] = mfD->sp1Array1[index++];
							}
							// Gas pressure
							else if (varNum==1) {
								scalarData[counter++] = mfD->sp2Array1[index++];
							}
							// P-Star
							else if (varNum==2) {
								scalarData[counter++] = mfD->sp2Array2[index++];
							}
							// Gas velocity
							else if (varNum==3) {
								if (kk == 0)
									scalarData[counter++] = mfD->sp3Array1[index++];
								else if (kk == 1)
									scalarData[counter++] = mfD->sp3Array2[index++];
								else if (kk == 2)
									scalarData[counter++] = mfD->sp3Array3[index++];
							}
							// Solid phase velocity
							else if (varNum==4) {
								if (kk == 0)
									scalarData[counter++] = mfD->sp4Array1[paramIndex][index++];
								else if (kk == 1)
									scalarData[counter++] = mfD->sp4Array2[paramIndex][index++];
								else if (kk == 2)
									scalarData[counter++] = mfD->sp4Array3[paramIndex][index++];
							}
							// Solid phase density
							else if (varNum==5) {
								scalarData[counter++] = mfD->sp5Array1[paramIndex][index++];
							}
							// Temperatures
							else if (varNum==6) {
								float version_number = strtod(resH->version+7, (char **)resH->version+11);
								// For versions > 1.15
								if (version_number > 1.15) {
									if (paramIndex == 0)
										scalarData[counter++] = mfD->sp6Array1[index++];
									else
										scalarData[counter++] = mfD->sp6Array2[paramIndex-1][index++];
								}
								// For versions <= 1.15
								else {
									if (paramIndex == 0)
										scalarData[counter++] = mfD->sp6Array1[index++];
									else 
										scalarData[counter++] = mfD->sp6Array2b[index++];
								}
							}
							// Gas phase species
							else if (varNum==7) {
								scalarData[counter++] = mfD->sp7Array1[paramIndex][index++];
							}
							// Solid phase species
							else if (varNum==8) {
								scalarData[counter++] = mfD->sp7Array2[0][0][index++];
							}
							// Granular temperature
							else if (varNum==9) {
								scalarData[counter++] = mfD->sp8Array1[paramIndex][index++];
							}
							// Scalars
							else if (varNum==10) {
								scalarData[counter++] = mfD->sp9Array1[paramIndex][index++];
							}
							// Reaction Rates
							else if (varNum==11) {
								scalarData[counter++] = mfD->spaArray1[paramIndex][index++];
							}
							// Check for ghost cells
							if (resH->flag[index-1] >= 100)
								scalarData[counter-1] = 9.87655e+031;
						}
					}
				}
			}
	
			// Resize scalarData array (boundry ghost cells are now gone)
			float* temp = new float[counter];
			float* temp2 = NULL;
			memcpy(temp, scalarData, sizeof(scalarData)*counter);
			temp2 = scalarData;
			scalarData = temp;
			delete (temp2);
			temp2 = NULL;

			// For each tuple
			for (int ii=0; ii<counter; ii++) {
				if (numComponents == 1)
					dataArray[paramIndex]->InsertTuple(ii, &scalarData[ii]); 
				else 
					dataArray[paramIndex]->InsertComponent(ii, kk, scalarData[ii]);
			}
		}
		// Attach data to mesh
		(uGrid->GetCellData())->AddArray(dataArray[paramIndex]);	
	}

	// Cleanup
	delete [] scalarData;
	scalarData = NULL;

	return;
}


// The main VTK unstructured grid writer
int UnstructuredGridWriter(char *inFile, char *outDir, int tStep, int maxTs, int type, resHead *resH, spHead *spH, mfixData *mfD) {
	int doGhost=0;

	char *inFilename = NULL;
	inFilename = new char[strlen(inFile)+5];

	char projectName[255];
	strncpy(projectName, (strrchr(inFile, '/')+1), 255);

	char *outFilename = NULL;
	outFilename = new char[(strlen(outDir)+strlen(projectName))+3];
	strncpy(outFilename, outDir, strlen(outDir)+1);
	strcat(outFilename, "/");
	strcat(outFilename, projectName);

	// Create grids for cartesian and cylindrical coordinates
	vtkUnstructuredGrid *uGrid = NULL;
	vtkUnstructuredGrid *filteredGrid = NULL;
	vtkRectilinearGrid *rGrid = vtkRectilinearGrid::New();

	// Handle cylindrical coordinates
	if (!strncmp(resH->coordinates, "CYLINDRICAL", 11)) {
		uGrid = vtkUnstructuredGrid::New();
		cout << "Creating cylindrical grid.." << endl;
		vtkPoints *uPoints = vtkPoints::New();
		vtkCellArray *hexas = vtkCellArray::New();
		// Convert cylindrical coordinates to cartesian and create points
		cout << "	Converting cylindrical coordinates to cartesian coordinates." << endl;
		int count = 0;
		float radius = 0;
		float height = 0;
		float theta = 0;
		float x = 0;
		float y = 0;
		float z = 0;
		for (int it=0; it<(resH->kMax2-1); it++) {
			for (int ih=0; ih<(resH->jMax2-1); ih++) {
				for (int ir=0; ir<(resH->iMax2-1); ir++) {
					uPoints->InsertPoint(count++, x, y, z);
					radius = radius + resH->dx[ir+1];
					x = radius * cos(theta);
					z = radius * sin(theta);
				}
				y = y + resH->dy[ih+1];
				x = 0;
				z = 0;
				radius = 0;
			}
			theta = theta + resH->dz[it+1];
			x = 0;
			y = 0;
			z = 0;
			radius = 0;
		}
		// Create hexahedron cells
		cout << "	Creating the cells from points." << endl;
		int xi = 0;
		int yi = 0;
		int zi = 0;
		int row = 0;
		int slice = 0;
		int cellPts[8];
		vtkHexahedron *hCell = vtkHexahedron::New();
		for (int cellId = 0; cellId<(resH->iMax2-2)*(resH->jMax2-2)*(resH->kMax2-2); cellId++) {
			hCell->GetPointIds()->SetNumberOfIds(8);
			// Create cell from points
			cellPts[0] = xi+(row*(resH->iMax2-1)+(slice*(resH->iMax2-1)*(resH->jMax2-1)));
			cellPts[1] = cellPts[0]+1;
			cellPts[2] = cellPts[0]+resH->iMax2;
			cellPts[3] = cellPts[0]+resH->iMax2-1;
			cellPts[4] = cellPts[0]+((resH->iMax2-1)*(resH->jMax2-1));
			cellPts[5] = cellPts[4]+1;
			cellPts[6] = cellPts[4]+resH->iMax2;
			cellPts[7] = cellPts[4]+resH->iMax2-1;
			// Attach point information to cell
			for (int i=0; i<8; i++)
				hCell->GetPointIds()->SetId(i, cellPts[i]);
			// Increment the rows and slices
			xi++;
			if (xi == (resH->iMax2-2)) {
				xi = 0;
				row++;
			}
			if (row == (resH->jMax2-2)) {
				row = 0;
				slice++;
			}
			// Add the cell to the cell array
			hexas->InsertNextCell(hCell);
		}
		hCell->Delete();
		// Attach point information to the unstructured grid
		uGrid->SetPoints(uPoints);
		// Attach cell information to the unstructured grid
		int *cellTypePtr;
		*cellTypePtr = 12;
		uGrid->SetCells(cellTypePtr, hexas);

		// Cleanup
		uPoints->Delete();
		hexas->Delete();
		cout << "Grid creation complete." << endl << endl;;
	}

	// Handle cartesian coordinates
	else if (!strncmp(resH->coordinates, "CARTESIAN", 9)){
		cout << "Creating cartesian grid.. " << endl;
		// Assign cell center coordinates based on cell layout
		// Makes rectilinear grid information out of the structured points data
		vtkDoubleArray *xCoords = vtkDoubleArray::New();
		xCoords->SetNumberOfTuples(resH->iMax2);
		double dx=0;
		for (int i=0; i<resH->iMax2; i++) {
			xCoords->SetTuple1((vtkIdType)i, (double)dx);
			dx += resH->dx[i];
		}
		vtkDoubleArray *yCoords = vtkDoubleArray::New();
		yCoords->SetNumberOfTuples(resH->jMax2);
		double dy=0;
		for (int i=0; i<resH->jMax2; i++) {
			yCoords->SetTuple1((vtkIdType)i, (double)dy);
			dy += resH->dy[i];
		}
		vtkDoubleArray *zCoords = vtkDoubleArray::New();
		zCoords->SetNumberOfTuples(resH->kMax2);
		double dz=0;
		for (int i=0; i<resH->kMax2; i++) {
			zCoords->SetTuple1((vtkIdType)i, (double)dz);
			dz += resH->dz[i];
		}
		// Set up the rectilinear grid
		rGrid->SetDimensions(resH->iMax2-1,resH->jMax2-1,resH->kMax2-1);
		rGrid->SetXCoordinates(xCoords);
		rGrid->SetYCoordinates(yCoords);
		rGrid->SetZCoordinates(zCoords);

		xCoords->Delete();
		yCoords->Delete();
		zCoords->Delete();
		cout << "Grid creation complete." << endl << endl;
	}
	else {
		cerr << "ERROR: Invalid coordinate system, exiting." << endl;
		return(1);
	}

	// Set up the data arrays for each SP file
	vtkFloatArray **dataArray = NULL;
	int numArrays = 0;
	int numComponents = 0;

	// Void fraction (sp1)
	if (resH->varSelectFlag[0] == 1) {
		cout << "Starting Void Fraction (SP1) translation.. " << endl;
		strcpy(inFilename, inFile);
		strcat(inFilename, ".SP1");

		// Read in number of timesteps;
		cout << "	";
		float numTs = ReadSPTimesteps(inFilename);
		cout << "	Number of time steps: " << numTs+1 << endl;
		int roundedTs = 0;
		if ((int)numTs > 0) {
			// The TS difference
			float diffTs = (float)maxTs/(float)numTs;
			// Actual TS to use
			float actualTs = tStep/diffTs;
			// Round the actual TS up
			float factor = pow(10.0, 0.0);
			roundedTs = (int)(floor((actualTs * factor) + 0.5) / factor);
		} 
		else
			roundedTs = (int)numTs;
			
		cout << "	Rounded time step to use: " << roundedTs << endl;

		// Read in the sp file
		cout << "	";
		ReadSPFile(inFilename, roundedTs, resH, spH, mfD);

		// Set up data arrays
		numArrays = 1;
		numComponents = 1;
		doGhost = 1;
		dataArray = new vtkFloatArray * [numArrays];
		for (int i=0; i<numArrays; i++) {
			dataArray[i] = vtkFloatArray::New();
			dataArray[i]->SetNumberOfComponents(numComponents);
		}
		dataArray[0]->SetName("Void Fraction");

		// Attach data to mesh
		if (!strncmp(resH->coordinates, "CARTESIAN", 9))
			attachDataToCartesianMesh(numComponents, numArrays, 0, dataArray, rGrid, resH, mfD);
		else
			attachDataToCylindricalMesh(numComponents, numArrays, 0, dataArray, uGrid, resH, mfD);
		
		// Cleanup
		for (int i=0; i<numArrays; i++)
			dataArray[i]->Delete();
		delete [] dataArray;
		dataArray = NULL;

		delete [] mfD->sp1Array1;
		mfD->sp1Array1 = NULL;

		sp_cleanup(spH);

		cout << "Translation complete." << endl << endl;
	}

	// Gas pressure(sp2)
	if (resH->varSelectFlag[1] == 1){
		cout << "Starting Gas Pressure (SP2) translation.. " << endl;
		strcpy(inFilename, inFile);
		strcat(inFilename, ".SP2");

		// Read in number of timesteps;
		cout << "	";
		float numTs = ReadSPTimesteps(inFilename);
		cout << "	Number of time steps: " << numTs+1 << endl;
		// The TS difference
		float diffTs = (float)maxTs/(float)numTs;
		// Actual TS to use
		float actualTs = tStep/diffTs;
		// Round the actual TS up
		float factor = pow(10.0, 0.0);
		int roundedTs = (int)(floor((actualTs * factor) + 0.5) / factor);
		cout << "	Rounded time step to use: " << roundedTs << endl;

		// Read in the sp file
		cout << "	";
		ReadSPFile(inFilename, roundedTs, resH, spH, mfD);

		// Set up data arrays
		numArrays = 1;
		numComponents = 1;
		doGhost = 1;
		dataArray = new vtkFloatArray * [numArrays];
		for (int i=0; i<numArrays; i++) {
			dataArray[i] = vtkFloatArray::New();
			dataArray[i]->SetNumberOfComponents(numComponents);
		}
		dataArray[0]->SetName("Gas Pressure");

		// Attach data to mesh
		if (!strncmp(resH->coordinates, "CARTESIAN", 9))
			attachDataToCartesianMesh(numComponents, numArrays, 1, dataArray, rGrid, resH, mfD);
		else
			attachDataToCylindricalMesh(numComponents, numArrays, 1, dataArray, uGrid, resH, mfD);
		
		// Cleanup
		for (int i=0; i<numArrays; i++)
			dataArray[i]->Delete();
		delete [] dataArray;
		dataArray = NULL;

		delete [] mfD->sp2Array1;
		mfD->sp2Array1 = NULL;

		delete [] mfD->sp2Array2;
		mfD->sp2Array2 = NULL;

		sp_cleanup(spH);

		cout << "Translation complete." << endl << endl;
	}

	// P-star (sp2)
	if (resH->varSelectFlag[2] == 1){
		cout << "Starting P-Star (SP2) translation.. " << endl;
		strcpy(inFilename, inFile);
		strcat(inFilename, ".SP2");

		// Read in number of timesteps;
		cout << "	";
		float numTs = ReadSPTimesteps(inFilename);
		cout << "	Number of time steps: " << numTs+1 << endl;
		// The TS difference
		float diffTs = (float)maxTs/(float)numTs;
		// Actual TS to use
		float actualTs = tStep/diffTs;
		// Round the actual TS up
		float factor = pow(10.0, 0.0);
		int roundedTs = (int)(floor((actualTs * factor) + 0.5) / factor);
		cout << "	Rounded time step to use: " << roundedTs << endl;

		// Read in the sp file
		cout << "	";
		ReadSPFile(inFilename, roundedTs, resH, spH, mfD);

		// Set up data arrays
		numArrays = 1;
		numComponents = 1;
		doGhost = 1;
		dataArray = new vtkFloatArray * [numArrays];
		for (int i=0; i<numArrays; i++) {
			dataArray[i] = vtkFloatArray::New();
			dataArray[i]->SetNumberOfComponents(numComponents);
		}
		dataArray[0]->SetName("P-Star");

		// Attach data to mesh
		if (!strncmp(resH->coordinates, "CARTESIAN", 9))
			attachDataToCartesianMesh(numComponents, numArrays, 2, dataArray, rGrid, resH, mfD);
		else
			attachDataToCylindricalMesh(numComponents, numArrays, 2, dataArray, uGrid, resH, mfD);
		
		// Cleanup
		for (int i=0; i<numArrays; i++)
			dataArray[i]->Delete();
		delete [] dataArray;
		dataArray = NULL;

		delete [] mfD->sp2Array1;
		mfD->sp2Array1 = NULL;

		delete [] mfD->sp2Array2;
		mfD->sp2Array2 = NULL;

		sp_cleanup(spH);
		
		cout << "Translation complete." << endl << endl;
	}

	// Gas velocity (sp3)
	if (resH->varSelectFlag[3] == 1){
		cout << "Starting Gas Velocity (SP3) translation.. " << endl;
		strcpy(inFilename, inFile);
		strcat(inFilename, ".SP3");

		// Read in number of timesteps;
		cout << "	";
		float numTs = ReadSPTimesteps(inFilename);
		cout << "	Number of time steps: " << numTs+1 << endl;
		// The TS difference
		float diffTs = (float)maxTs/(float)numTs;
		// Actual TS to use
		float actualTs = tStep/diffTs;
		// Round the actual TS up
		float factor = pow(10.0, 0.0);
		int roundedTs = (int)(floor((actualTs * factor) + 0.5) / factor);
		cout << "	Rounded time step to use: " << roundedTs << endl;

		// Read in the sp file
		cout << "	";
		ReadSPFile(inFilename, roundedTs, resH, spH, mfD);

		// Set up data arrays
		numArrays = 1;
		numComponents = 3;
		doGhost = 1;
		dataArray = new vtkFloatArray * [numArrays];
		for (int i=0; i<numArrays; i++) {
			dataArray[i] = vtkFloatArray::New();
			dataArray[i]->SetNumberOfComponents(numComponents);
		}
		dataArray[0]->SetName("Gas Velocity");
		// Attach data to mesh
		if (!strncmp(resH->coordinates, "CARTESIAN", 9))
			attachDataToCartesianMesh(numComponents, numArrays, 3, dataArray, rGrid, resH, mfD);
		else
			attachDataToCylindricalMesh(numComponents, numArrays, 3, dataArray, uGrid, resH, mfD);
		
		// Cleanup
		for (int i=0; i<numArrays; i++)
			dataArray[i]->Delete();
		delete [] dataArray;
		dataArray = NULL;

		delete [] mfD->sp3Array1;
		mfD->sp3Array1 = NULL;

		delete [] mfD->sp3Array2;
		mfD->sp3Array2 = NULL;

		delete [] mfD->sp3Array3;
		mfD->sp3Array3 = NULL;

		sp_cleanup(spH);
		
		cout << "Translation complete." << endl << endl;
	}

	// Solid velocity phases (sp4)
	if (resH->varSelectFlag[4] == 1){
		cout << "Starting Solid Velocity Phases (SP4) translation.. " << endl;
		strcpy(inFilename, inFile);
		strcat(inFilename, ".SP4");

		// Read in number of timesteps;
		cout << "	";
		float numTs = ReadSPTimesteps(inFilename);
		cout << "	Number of time steps: " << numTs+1 << endl;
		// The TS difference
		float diffTs = (float)maxTs/(float)numTs;
		// Actual TS to use
		float actualTs = tStep/diffTs;
		// Round the actual TS up
		float factor = pow(10.0, 0.0);
		int roundedTs = (int)(floor((actualTs * factor) + 0.5) / factor);
		cout << "	Rounded time step to use: " << roundedTs << endl;

		// Read in the sp file
		cout << "	";
		ReadSPFile(inFilename, roundedTs, resH, spH, mfD);

		// Set up data arrays
		numArrays = resH->mMax;
		numComponents = 3;
		if (numArrays > 0)
			doGhost = 1;
		dataArray = new vtkFloatArray * [numArrays];
		char temp[23];
		for (int i=0; i<numArrays; i++) {
			dataArray[i] = vtkFloatArray::New();
			dataArray[i]->SetNumberOfComponents(numComponents);
			sprintf(temp, "Solid Velocity Phase %d", i+1);
			dataArray[i]->SetName(temp);
		}

		// Attach data to mesh
		if (!strncmp(resH->coordinates, "CARTESIAN", 9))
			attachDataToCartesianMesh(numComponents, numArrays, 4, dataArray, rGrid, resH, mfD);
		else
			attachDataToCylindricalMesh(numComponents, numArrays, 4, dataArray, uGrid, resH, mfD);
		
		// Cleanup
		for (int i=0; i<numArrays; i++)
			dataArray[i]->Delete();
		delete [] dataArray;
		dataArray = NULL;

		if (numArrays > 0) {
			delete [] mfD->sp4Array1[0];
			delete [] mfD->sp4Array1;
			mfD->sp4Array1 = NULL;

			delete [] mfD->sp4Array2[0];
			delete [] mfD->sp4Array2;
			mfD->sp4Array2 = NULL;

			delete [] mfD->sp4Array3[0];
			delete [] mfD->sp4Array3;
			mfD->sp4Array3 = NULL;
		}

		sp_cleanup(spH);
		
		cout << "Translation complete." << endl << endl;
	}

	// Solid density phases (sp5)
	if (resH->varSelectFlag[5] == 1){
		cout << "Starting Solid Density Phases (SP5) translation.. " << endl;
		strcpy(inFilename, inFile);
		strcat(inFilename, ".SP5");

		// Read in number of timesteps;
		cout << "	";
		float numTs = ReadSPTimesteps(inFilename);
		cout << "	Number of time steps: " << numTs+1 << endl;
		// The TS difference
		float diffTs = (float)maxTs/(float)numTs;
		// Actual TS to use
		float actualTs = tStep/diffTs;
		// Round the actual TS up
		float factor = pow(10.0, 0.0);
		int roundedTs = (int)(floor((actualTs * factor) + 0.5) / factor);
		cout << "	Rounded time step to use: " << roundedTs << endl;

		// Read in the sp file
		cout << "	";
		ReadSPFile(inFilename, roundedTs, resH, spH, mfD);

		// Set up data arrays
		numArrays = resH->mMax;
		numComponents = 1;
		if (numArrays > 0)
			doGhost = 1;
		dataArray = new vtkFloatArray * [numArrays];
		char temp[22];
		for (int i=0; i<numArrays; i++) {
			dataArray[i] = vtkFloatArray::New();
			dataArray[i]->SetNumberOfComponents(numComponents);
			sprintf(temp, "Solid Density Phase %d", i+1);
			dataArray[i]->SetName(temp);
		}

		// Attach data to mesh
		if (!strncmp(resH->coordinates, "CARTESIAN", 9))
			attachDataToCartesianMesh(numComponents, numArrays, 5, dataArray, rGrid, resH, mfD);
		else
			attachDataToCylindricalMesh(numComponents, numArrays, 5, dataArray, uGrid, resH, mfD);
		
		// Cleanup
		for (int i=0; i<numArrays; i++)
			dataArray[i]->Delete();
		delete [] dataArray;
		dataArray = NULL;

		if (numArrays > 0) {
			delete [] mfD->sp5Array1[0];
			delete [] mfD->sp5Array1;
			mfD->sp5Array1 = NULL;
		}

		sp_cleanup(spH);
		
		cout << "Translation complete." << endl << endl;
	}

	// Temperatures (sp6)
	if (resH->varSelectFlag[6] == 1){
		cout << "Starting Temperatures (SP6) translation.. " << endl;
		strcpy(inFilename, inFile);
		strcat(inFilename, ".SP6");

		// Read in number of timesteps;
		cout << "	";
		float numTs = ReadSPTimesteps(inFilename);
		cout << "	Number of time steps: " << numTs+1 << endl;
		// The TS difference
		float diffTs = (float)maxTs/(float)numTs;
		// Actual TS to use
		float actualTs = tStep/diffTs;
		// Round the actual TS up
		float factor = pow(10.0, 0.0);
		int roundedTs = (int)(floor((actualTs * factor) + 0.5) / factor);
		cout << "	Rounded time step to use: " << roundedTs << endl;

		// Read in the sp file
		cout << "	";
		ReadSPFile(inFilename, roundedTs, resH, spH, mfD);

		// Set up data arrays
		numArrays = 1+resH->mMax;
		numComponents = 1;
		doGhost = 1;
		float version_number = 0.0;
		dataArray = new vtkFloatArray * [numArrays];
		for (int i=0; i<numArrays; i++) {
			dataArray[i] = vtkFloatArray::New();
			dataArray[i]->SetNumberOfComponents(numComponents);
			if (i == 0)
				dataArray[i]->SetName("Gas Temperature");
			else {
				version_number = strtod(resH->version+7, (char **)resH->version+11);
				if (version_number > 1.15) {
					char temp[26];
					sprintf(temp, "Solid Temperature Phase %d", i);
					dataArray[i]->SetName(temp);
				}
				else
					dataArray[i]->SetName("Solid Temperature");
			}
		}

		// Attach data to mesh
		if (!strncmp(resH->coordinates, "CARTESIAN", 9))
			attachDataToCartesianMesh(numComponents, numArrays, 6, dataArray, rGrid, resH, mfD);
		else
			attachDataToCylindricalMesh(numComponents, numArrays, 6, dataArray, uGrid, resH, mfD);
		
		// Cleanup
		for (int i=0; i<numArrays; i++)
			dataArray[i]->Delete();
		delete [] dataArray;
		dataArray = NULL;
	
		delete [] mfD->sp6Array1;
		mfD->sp6Array1 = NULL;

		if (numArrays > 0) {
			if (version_number > 1.15) {
				delete [] mfD->sp6Array2[0];
				delete [] mfD->sp6Array2;
				mfD->sp6Array2 = NULL;
			}
			else {
				delete [] mfD->sp6Array2b;
				mfD->sp6Array2b = NULL;
			}
		}

		sp_cleanup(spH);
		
		cout << "Translation complete." << endl << endl;
	}

	// Gas species (sp7)
	if (resH->varSelectFlag[7] == 1){
		cout << "Starting Gas Species (SP7) translation.. " << endl;
		strcpy(inFilename, inFile);
		strcat(inFilename, ".SP7");

		// Read in number of timesteps;
		cout << "	";
		float numTs = ReadSPTimesteps(inFilename);
		cout << "	Number of time steps: " << numTs+1 << endl;
		int roundedTs = 0;
		if ((int)numTs > 0) {
			// The TS difference
			float diffTs = (float)maxTs/(float)numTs;
			// Actual TS to use
			float actualTs = tStep/diffTs;
			// Round the actual TS up
			float factor = pow(10.0, 0.0);
			int roundedTs = (int)(floor((actualTs * factor) + 0.5) / factor);
		}
		cout << "	Rounded time step to use: " << roundedTs << endl;

		// Read in the sp file
		cout << "	";
		ReadSPFile(inFilename, roundedTs, resH, spH, mfD);

		// Set up data arrays
		numArrays = resH->nMax[0];
		numComponents = 1;
		if (numArrays > 0)
			doGhost = 1;
		dataArray = new vtkFloatArray * [numArrays];

		// For all gas species
		for (int i=0; i<numArrays; i++) {
			char temp[28];
			dataArray[i] = vtkFloatArray::New();
			dataArray[i]->SetNumberOfComponents(numComponents);
			sprintf(temp, "Gas Species %d", i+1);
			dataArray[i]->SetName(temp);
		}

		// Attach data to mesh
		if (!strncmp(resH->coordinates, "CARTESIAN", 9))
			attachDataToCartesianMesh(numComponents, numArrays, 7, dataArray, rGrid, resH, mfD);
		else
			attachDataToCylindricalMesh(numComponents, numArrays, 7, dataArray, uGrid, resH, mfD);
		
		// Cleanup
		for (int i=0; i<numArrays; i++)
			dataArray[i]->Delete();
		delete [] dataArray;
		dataArray = NULL;

		if (numArrays > 0) {
			delete [] mfD->sp7Array1[0];
			delete [] mfD->sp7Array1;
			mfD->sp7Array1 = NULL;

			for (int i=0; i<resH->mMax; i++) {
				delete mfD->sp7Array2[i][0];
				delete mfD->sp7Array2[i];
			}
			delete [] mfD->sp7Array2;
			mfD->sp7Array2 = NULL;
		}

		sp_cleanup(spH);
		
		cout << "Translation complete." << endl << endl;
	}

	// Solid phase, species (sp7)
	if (resH->varSelectFlag[8] == 1){
		cout << "Starting Solid Phase, Species (SP7) translation.. " << endl;
		strcpy(inFilename, inFile);
		strcat(inFilename, ".SP7");

		// Read in number of timesteps;
		cout << "	";
		float numTs = ReadSPTimesteps(inFilename);
		cout << "	Number of time steps: " << numTs+1 << endl;
		// The TS difference
		float diffTs = (float)maxTs/(float)numTs;
		// Actual TS to use
		float actualTs = tStep/diffTs;
		// Round the actual TS up
		float factor = pow(10.0, 0.0);
		int roundedTs = (int)(floor((actualTs * factor) + 0.5) / factor);
		cout << "	Rounded time step to use: " << roundedTs << endl;

		// Read in the sp file
		cout << "	";
		ReadSPFile(inFilename, roundedTs, resH, spH, mfD);

		// Set up data arrays
		numArrays = resH->mMax;
		numComponents = 0;
		for (int i=1; i<=numArrays; i++)
			numComponents += resH->nMax[i];
		if (numArrays > 0)
			doGhost = 1;
		dataArray = new vtkFloatArray * [numComponents];

		// For all solid phases
		int count = 0;
		for (int i=0; i<numArrays; i++) {
			// For all solid phase species
			for (int j=0; j<numComponents; j++) {
				char temp[25];
				dataArray[count] = vtkFloatArray::New();
				dataArray[count]->SetNumberOfComponents(1);
				sprintf(temp, "Solid Phase %d, Species %d", (i+1), (j+1));
				dataArray[count]->SetName(temp);
				count++;
			}

			// Attach data to mesh
			if (!strncmp(resH->coordinates, "CARTESIAN", 9))
				attachDataToCartesianMesh(numComponents, numArrays, 8, dataArray, rGrid, resH, mfD);
			else
				attachDataToCylindricalMesh(numComponents, numArrays, 8, dataArray, uGrid, resH, mfD);
		}

		// Cleanup
		//for (int i=1; i<=numArrays; i++)
		//	for (int j=0; j<numComponents; j++)
		//		dataArray[(i-1)+j]->Delete();

		for (int i=0; i<numComponents; i++)
			dataArray[i]->Delete();
		delete [] dataArray;
		dataArray = NULL;

		if (numArrays > 0) {
			delete [] mfD->sp7Array1[0];
			delete [] mfD->sp7Array1;
			mfD->sp7Array1 = NULL;

			for (int i=0; i<resH->mMax; i++) {
				delete mfD->sp7Array2[i][0];
				delete mfD->sp7Array2[i];
			}
			delete [] mfD->sp7Array2;
			mfD->sp7Array2 = NULL;
		}

		sp_cleanup(spH);

		cout << "Translation complete." << endl << endl;
	}

	// Granular temperature (sp8)
	if (resH->varSelectFlag[9] == 1){
		cout << "Starting Granular Temperature (SP8) translation.. " << endl;
		strcpy(inFilename, inFile);
		strcat(inFilename, ".SP8");

		// Read in number of timesteps;
		cout << "	";
		float numTs = ReadSPTimesteps(inFilename);
		cout << "	Number of time steps: " << numTs+1 << endl;
		// The TS difference
		float diffTs = (float)maxTs/(float)numTs;
		// Actual TS to use
		float actualTs = tStep/diffTs;
		// Round the actual TS up
		float factor = pow(10.0, 0.0);
		int roundedTs = (int)(floor((actualTs * factor) + 0.5) / factor);
		cout << "	Rounded time step to use: " << roundedTs << endl;

		// Read in the sp file
		cout << "	";
		ReadSPFile(inFilename, roundedTs, resH, spH, mfD);

		// Set up data arrays
		numArrays = resH->mMax;
		numComponents = 1;
		if (numArrays > 0)
			doGhost = 1;
		dataArray = new vtkFloatArray * [numArrays];
		char temp[23];
		for (int i=0; i<numArrays; i++) {
			dataArray[i] = vtkFloatArray::New();
			dataArray[i]->SetNumberOfComponents(numComponents);
			sprintf(temp, "Granular Temperature %d", i+1);
			dataArray[i]->SetName(temp);
		}

		// Attach data to mesh
		if (!strncmp(resH->coordinates, "CARTESIAN", 9))
			attachDataToCartesianMesh(numComponents, numArrays, 9, dataArray, rGrid, resH, mfD);
		else
			attachDataToCylindricalMesh(numComponents, numArrays, 9, dataArray, uGrid, resH, mfD);
		
		// Cleanup
		for (int i=0; i<numArrays; i++)
			dataArray[i]->Delete();
		delete [] dataArray;
		dataArray = NULL;

		if (numArrays > 0) {
			delete [] mfD->sp8Array1[0];
			delete [] mfD->sp8Array1;
			mfD->sp8Array1 = NULL;
		}

		sp_cleanup(spH);
		
		cout << "Translation complete." << endl << endl;
	}

	// Scalars (sp9)
	if (resH->varSelectFlag[10] == 1){
		cout << "Starting Scalars (SP9) translation.. " << endl;
		strcpy(inFilename, inFile);
		strcat(inFilename, ".SP9");

		// Read in number of timesteps;
		cout << "	";
		float numTs = ReadSPTimesteps(inFilename);
		cout << "	Number of time steps: " << numTs+1 << endl;
		// The TS difference
		float diffTs = (float)maxTs/(float)numTs;
		// Actual TS to use
		float actualTs = tStep/diffTs;
		// Round the actual TS up
		float factor = pow(10.0, 0.0);
		int roundedTs = (int)(floor((actualTs * factor) + 0.5) / factor);
		cout << "	Rounded time step to use: " << roundedTs << endl;

		// Read in the sp file
		cout << "	";
		ReadSPFile(inFilename, roundedTs, resH, spH, mfD);

		// Set up data arrays
		numArrays = resH->nscalar;
		numComponents = 1;
		dataArray = new vtkFloatArray * [numArrays];
		if (numArrays > 0)
			doGhost = 1;
		char temp[9];
		for (int i=0; i<numArrays; i++) {
			dataArray[i] = vtkFloatArray::New();
			dataArray[i]->SetNumberOfComponents(numComponents);
			sprintf(temp, "Scalar %d", i+1);
			dataArray[i]->SetName(temp);
		}

		// Attach data to mesh
		if (!strncmp(resH->coordinates, "CARTESIAN", 9))
			attachDataToCartesianMesh(numComponents, numArrays, 10, dataArray, rGrid, resH, mfD);
		else
			attachDataToCylindricalMesh(numComponents, numArrays, 10, dataArray, uGrid, resH, mfD);
		
		// Cleanup
		for (int i=0; i<numArrays; i++)
			dataArray[i]->Delete();
		delete [] dataArray;
		dataArray = NULL;

		if (numArrays > 0) {
			delete [] mfD->sp9Array1[0];
			delete [] mfD->sp9Array1;
			mfD->sp9Array1 = NULL;
		}

		sp_cleanup(spH);
		
		cout << "Translation complete." << endl << endl;
	}

	// Reaction rates (spa)
	if (resH->varSelectFlag[11] == 1){
		cout << "Starting Reaction Rates (SPA) translation.. " << endl;
		strcpy(inFilename, inFile);
		strcat(inFilename, ".SPA");

		// Read in number of timesteps;
		cout << "	";
		float numTs = ReadSPTimesteps(inFilename);
		cout << "	Number of time steps: " << numTs+1 << endl;
		// The TS difference
		float diffTs = (float)maxTs/(float)numTs;
		// Actual TS to use
		float actualTs = tStep/diffTs;
		// Round the actual TS up
		float factor = pow(10.0, 0.0);
		int roundedTs = (int)(floor((actualTs * factor) + 0.5) / factor);
		cout << "	Rounded time step to use: " << roundedTs << endl;

		// Read in the sp file
		cout << "	";
		ReadSPFile(inFilename, roundedTs, resH, spH, mfD);

		// Set up data arrays
		numArrays = resH->nrr;
		numComponents = 1;
		if (numArrays > 0)
			doGhost = 1;
		dataArray = new vtkFloatArray * [numArrays];
		char temp[16];
		for (int i=0; i<numArrays; i++) {
			dataArray[i] = vtkFloatArray::New();
			dataArray[i]->SetNumberOfComponents(numComponents);
			sprintf(temp, "Reaction Rate %d", i+1);
			dataArray[i]->SetName(temp);
		}

		// Attach data to mesh
		if (!strncmp(resH->coordinates, "CARTESIAN", 9))
			attachDataToCartesianMesh(numComponents, numArrays, 11, dataArray, rGrid, resH, mfD);
		else
			attachDataToCylindricalMesh(numComponents, numArrays, 11, dataArray, uGrid, resH, mfD);
		
		// Cleanup
		for (int i=0; i<numArrays; i++)
			dataArray[i]->Delete();
		delete [] dataArray;
		dataArray = NULL;

		if (numArrays > 0) {
			delete [] mfD->spaArray1[0];
			delete [] mfD->spaArray1;
			mfD->spaArray1 = NULL;
		}

		sp_cleanup(spH);
		
		cout << "Translation complete." << endl << endl;
	}

	// Convert rectilinear grid to unstructured grid (Cartesian coordinates)
	if (!strncmp(resH->coordinates, "CARTESIAN", 9))
		uGrid = convertToUnstructuredGrid(rGrid);

	// Remove ghost cells
	if (doGhost)
		filteredGrid = filterMeshByCellData(uGrid);

	// Find the time step precision (leading zeros)
	int precision = 0;
	char number_str[10];
	sprintf(number_str, "%d", maxTs);
	precision = strlen(number_str);

	// Write vtk file
	char vtkFilename[256];
	cout << "Writing VTK file.." << endl;
	// ASCII legacy format (.vtk)
	if (type == 1) {
		sprintf(vtkFilename, "%s-%2$.*3$d.vtk", outFilename, tStep, precision);
		vtkUnstructuredGridWriter *writer = vtkUnstructuredGridWriter::New();
		writer->SetInput(filteredGrid);
		writer->SetFileName(vtkFilename);
		writer->Write();
		writer->Delete();
	}
	// binary legacy format (.vtk)
	else if (type == 2) {
		sprintf(vtkFilename, "%s-%2$.*3$d.vtk", outFilename, tStep, precision);
		vtkUnstructuredGridWriter *writer = vtkUnstructuredGridWriter::New();
		writer->SetInput(filteredGrid);
		writer->SetFileName(vtkFilename);
		writer->SetFileTypeToBinary();
		writer->Write();
		writer->Delete();
	}
	// XML serial format (.vtu)
	else if (type == 3) {
		sprintf(vtkFilename, "%s-%2$.*3$d.vtu", outFilename, tStep, precision);
		vtkXMLUnstructuredGridWriter *writer = vtkXMLUnstructuredGridWriter::New();
		writer->SetInput(filteredGrid);
		writer->SetFileName(vtkFilename);
		writer->Write();
		writer->Delete();
	}
	// XML parallel format (.pvtu)
	else if (type == 4) {
		sprintf(vtkFilename, "%s-%2$.*3$d.pvtu", outFilename, tStep, precision);
		vtkXMLPUnstructuredGridWriter *writer = vtkXMLPUnstructuredGridWriter::New();
		writer->SetInput(filteredGrid);
		writer->SetFileName(vtkFilename);
		writer->Write();
		writer->Delete();
	}
	else {
		cerr << "ERROR: Invalid output type (" << type << "), exiting." << endl;
		return(1);
	}

	// Cleanup
	delete [] inFilename;
	inFilename = NULL;
	delete [] outFilename;
	outFilename = NULL;
	rGrid->Delete();
	uGrid->Delete();
	filteredGrid->Delete();

	cout << "DONE" << endl;

	return(0);
}
