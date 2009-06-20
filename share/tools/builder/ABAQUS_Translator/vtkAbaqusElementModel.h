/*=========================================================================

  Program:   Visualization Toolkit
  Module:    $RCSfile: vtkDebugLeaks.cxx,v $

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef __vtkAbaqusElementModel_h
#define __vtkAbaqusElementModel_h

#include "vtkUnstructuredGrid.h"

class vtkPolyData;
class vtkIdTypeArray;
class vtkDataArrayCollection;

/** @class vtkAbaqusElementModel

  Developed for Abaqus Input decks (*.inp files) produced by MSC.Patran 
  contain code for a geometric model including node and node set 
  definitions and element and element set definitions.

  Class includes functions for generating new datasets from named element 
  sets, named node sets and single elements.  The caveat of getting an 
  element or node set as a new dataset is that the new element ids will 
  not match the original input element ids, so if you are going to 
  do any picking you will have to work around that.

  The class is intentionally agnostic of cell type.
*/

class VTK_IO_EXPORT vtkAbaqusElementModel : public vtkUnstructuredGrid
{
public:
  static vtkAbaqusElementModel* New();
  vtkTypeRevisionMacro(vtkAbaqusElementModel, vtkUnstructuredGrid);
  void PrintSelf(ostream& os, vtkIndent indent);
  
  vtkSetStringMacro(Heading);
  vtkGetStringMacro(Heading);
  
  virtual void SetNodeSets(vtkDataArrayCollection*);
  vtkGetObjectMacro(NodeSets, vtkDataArrayCollection);
  
  virtual void SetElementSets(vtkDataArrayCollection*);
  vtkGetObjectMacro(ElementSets, vtkDataArrayCollection);

  vtkIdTypeArray *GetElementSet( const char *setName )
    { return this->SeekByName( this->ElementSets, setName ); }

  vtkIdTypeArray *GetNodeSet( const char *setName )
    { return this->SeekByName( this->NodeSets, setName ); }

  long NewDatasetFromElementSet( const char *setName, vtkUnstructuredGrid *target );
  long NewDatasetFromNodeSet( const char *setName, vtkPolyData *target );
  long NewDatasetFromCell( vtkIdType cellId, vtkUnstructuredGrid *target );

  void AddElementSet( vtkIdTypeArray *elset );
  void AddNodeSet( vtkIdTypeArray *nset );

protected:
  vtkAbaqusElementModel();
  ~vtkAbaqusElementModel();

  char *Heading;

  // < NSET and ELSET entries in the input deck signify MSC.Patran Groups
  vtkDataArrayCollection *ElementSets;
  // < NSET and ELSET entries in the input deck signify MSC.Patran Groups
  vtkDataArrayCollection *NodeSets;

  vtkIdTypeArray *SeekByName( vtkDataArrayCollection *items, 
    const char *name );

private:

  vtkAbaqusElementModel(const vtkAbaqusElementModel&);  // Not implemented.
  void operator=(const vtkAbaqusElementModel&);  // Not implemented.
};

#endif
