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
