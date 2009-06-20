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
#include "vtkObjectFactory.h"
#include "vtkAbaqusElementModel.h"

#include "vtkIdTypeArray.h"
#include "vtkDataArrayCollection.h"
#include "vtkPolyData.h"
#include "vtkCell.h"
#include "vtkGenericCell.h"
#include "vtkDataArray.h"
#include "vtkIntArray.h"
#include "vtkPoints.h"
#include "vtkPointData.h"
#include "vtkCellArray.h"
#include "vtkDoubleArray.h"
#include "vtkDataSetAttributes.h"

vtkCxxRevisionMacro(vtkAbaqusElementModel, "$Revision$");
vtkStandardNewMacro(vtkAbaqusElementModel);

vtkCxxSetObjectMacro(vtkAbaqusElementModel,NodeSets,vtkDataArrayCollection);
vtkCxxSetObjectMacro(vtkAbaqusElementModel,ElementSets,vtkDataArrayCollection);

//----------------------------------------------------------------------------
vtkAbaqusElementModel::vtkAbaqusElementModel() : 
    Heading(NULL), ElementSets(NULL), NodeSets(NULL)
{
  //hiya
}

//----------------------------------------------------------------------------
vtkAbaqusElementModel::~vtkAbaqusElementModel()
{
  if( this->Heading )
  {
    delete [] this->Heading;
  }

  if( this->ElementSets )
  {
    this->ElementSets->RemoveAllItems();
    this->ElementSets->Delete();
  }

  if( this->NodeSets )
  {
    this->NodeSets->RemoveAllItems();
    this->NodeSets->Delete();
  }
  //cya
}

//----------------------------------------------------------------------------

void vtkAbaqusElementModel::PrintSelf(ostream& os, vtkIndent indent)
{
  vtkIdTypeArray *tmp = NULL;
  int num = 0;
  int i = 0;

  os << indent << "Heading : " << this->Heading << endl;
  
  if( this->ElementSets )
    num = this->ElementSets->GetNumberOfItems();
  else
    num = 0;

  os << indent << "Number Of Element Sets : " << num << endl;
  for( i=0; i<num; i++ )
    if( (tmp = vtkIdTypeArray::SafeDownCast( this->ElementSets->GetItemAsObject( i ) ) ) )
      os << indent << indent << i << " : " << tmp->GetName() << endl;


  if( this->NodeSets )
    num = this->NodeSets->GetNumberOfItems();
  else
    num = 0;
  os << indent << "Number Of Node Sets : " << num << endl;
  for( i=0; i<num; i++ )
    if( (tmp = vtkIdTypeArray::SafeDownCast( this->NodeSets->GetItemAsObject( i ) ) ) )
      os << indent << indent << i << " : " << tmp->GetName() << endl;

  this->Superclass::PrintSelf(os,indent);
}

//----------------------------------------------------------------------------

/** Add a named element set to the element set collection
  */
void vtkAbaqusElementModel::AddElementSet( vtkIdTypeArray *elset )
{
  const char *name = elset->GetName();
  if( name )
  {
    if( !this->ElementSets )
    {
      this->ElementSets = vtkDataArrayCollection::New();
    }

    //iterate the elsets, if we find one that matches this name, we will replace it
    vtkIdTypeArray *tmp = this->SeekByName( this->ElementSets, name );
    if( tmp )
      {
      vtkWarningMacro( << "Replacing old ELSETS" );
      this->ElementSets->RemoveItem( tmp );
      }

    this->ElementSets->AddItem( elset );
  }
  else
  {
    vtkWarningMacro( << "Failed to add element set because it has no name" );
  }
}

//----------------------------------------------------------------------------

void vtkAbaqusElementModel::AddNodeSet( vtkIdTypeArray *nset )
{
  const char *name = nset->GetName();
  if( name )
  {
    if( !this->NodeSets )
    {
      this->NodeSets = vtkDataArrayCollection::New();
    }

    //iterate the elsets, if we find one that matches this name, we will replace it
    vtkIdTypeArray *tmp = this->SeekByName( this->NodeSets, name );
    if( tmp )
      {
      vtkWarningMacro( << "Replacing old ELSETS" );
      this->NodeSets->RemoveItem( tmp );
      }

    this->NodeSets->AddItem( nset );
  }
  else
  {
    vtkWarningMacro( << "Failed to add node set because it has no name" );
  }
}

//----------------------------------------------------------------------------

vtkIdTypeArray *vtkAbaqusElementModel::SeekByName( vtkDataArrayCollection *items, const char *name )
{
  //iterate the collection comparing the names...
  vtkIdTypeArray *tmp;
  for( items->InitTraversal(); (tmp = ( vtkIdTypeArray::SafeDownCast( items->GetNextItemAsObject() ) )) ; )
  {
    if (tmp && strcmp( name, tmp->GetName() ) == 0 )
    {
    return tmp;
    }
  }

  return NULL;
}

//----------------------------------------------------------------------------

/**  Return a new dataset containing just the one cell.

  */
long vtkAbaqusElementModel::NewDatasetFromCell( vtkIdType cellId, vtkUnstructuredGrid *target )
{
  if( !target )
  {
    return 0;
  }

  vtkCell *cell = NULL;
  cell = this->GetCell( cellId );
  if( !cell )
  {
    return 0;
  }

  target->Initialize();
  target->Allocate( 1, 1 );

  //get the points out to set them into the new dataset with new ids
  vtkPoints *pts = vtkPoints::New();
  vtkDoubleArray *scalars =vtkDoubleArray::New(); 
  vtkIdList *cellPointsIds =vtkIdList::New(); 

    //always use this instead of Allocate for vtkPoints
    pts->SetNumberOfPoints( cell->GetNumberOfPoints() );

    scalars->SetNumberOfComponents(1);
    scalars->SetNumberOfValues( cell->GetNumberOfPoints() );

    //read pointer
    vtkPoints *rPts = cell->GetPoints();
    this->GetCellPoints(cellId,cellPointsIds);
    //build a simple idlist for the new dataset cell links
    vtkIdList *ptIds = vtkIdList::New();
      //always use this instead of Allocate for vtkIdList
      ptIds->SetNumberOfIds( cell->GetNumberOfPoints() );

        for( int i=0; i<cell->GetNumberOfPoints(); i++ )
        {
          ptIds->SetId( i, i );
          double *vals = rPts->GetPoint( i );
          pts->SetPoint( i, vals[0], vals[1], vals[2] );
          
          vtkIdType realId = cellPointsIds->GetId(i);
          scalars->SetValue(i,this->GetPointData()->GetScalars()->GetTuple1(realId));
        }

        pts->ComputeBounds();

      target->InsertNextCell( cell->GetCellType(), ptIds );
      target->SetPoints( pts );
      target->GetPointData()->SetScalars(scalars);
    ptIds->Delete();
  scalars->Delete();
  cellPointsIds->Delete();    
  pts->Delete();

  target->BuildLinks();
  target->SetUpdateExtent( 1, 1, 1 );
  target->Update();
  target->ComputeBounds();

  return 1;
}

//----------------------------------------------------------------------------

/** NewDatasetFromElementSet

  Given the name of the id set you want, and a valid pointer to a new dataset,
  this function will re-initialize and fill out the dataset with the requested set elements or nodes.
  
  Caveat : The cell and point ids of the new dataset will not match the cell and point ids of this dataset.
  */
long 
vtkAbaqusElementModel::NewDatasetFromElementSet( const char *setName, 
             vtkUnstructuredGrid *target )
{
  if( !target )
  {
    return 0;
  }

  vtkIdTypeArray *elset = this->GetElementSet( setName );
  if( !elset )
  {
    return 0;
  }

  vtkCell *cell = NULL;
  vtkIdType loc = 0;
  long i = 0;
    
  //find out how many cells are valid cells in this set
  vtkIdType accPts = 0;
  vtkIdType accCells = 0;
  for( i=0; i<elset->GetSize(); i++ )
  {
    cell = NULL;
    cell = this->GetCell( elset->GetValue(i) );
    //cout << "Cell Id : " << elset->GetValue(i);

    if( cell )
    {
      //cout << "    Cell Type : " << cell->GetCellType() << endl;
      if( cell->GetCellType() != VTK_EMPTY_CELL )
      {
        accCells++;
        accPts += cell->GetNumberOfPoints();
      }
    //  else
    //    cout << "...dicarding cell " << elset->GetValue(i) << " as it is an empty cell." << endl;
    }
  }

  target->Initialize();
  target->Allocate( accCells );

  //we need a vtkPoints to hold all the points in the subset
  vtkPoints *pts = vtkPoints::New();
  vtkDoubleArray *scalars = vtkDoubleArray::New();

    //allocate once for the points
    pts->SetNumberOfPoints( accPts );
    scalars->SetNumberOfComponents(1);
    scalars->SetNumberOfValues( accPts );

    for( i=0; i<elset->GetSize(); i++ )
    {  
      vtkIdType cellId = elset->GetValue(i);
      
      cell = NULL;
      cell = this->GetCell( cellId );

      if( cell )
      {
        //the loader uses vtkEmptyCells as place markers in the cell arrays
        //  this is to ensure that cell ids that may or may not have contiguous ids
        //  in the input deck are represented herin with the same id the input model uses
        if( cell->GetCellType() != VTK_EMPTY_CELL )
        {
          //cout << "CellId is " << cellId << endl;
          
          //read pointer
          vtkPoints *rPts = cell->GetPoints();
          
          //build a simple idlist for the new dataset cell links
          vtkIdList *ptIds = vtkIdList::New();
          vtkIdList *cellPtIds = vtkIdList::New();
            
            this->GetCellPoints( cellId,cellPtIds );
            //always use this instead of Allocate for vtkIdList
            ptIds->SetNumberOfIds( cell->GetNumberOfPoints() );

            for( int j=0; j<cell->GetNumberOfPoints(); j++ )
            {
              ptIds->SetId( j, loc+j );
              //cout << "cell point id=" << j << " : ";
              //cout << "point id=" << loc+j << " : ";
              
              double *vals = rPts->GetPoint(j);
              pts->SetPoint( loc+j, vals[0], vals[1], vals[2] );
              //cout << vals[0] << "," << vals[1] << "," << vals[2] << endl;

              vtkIdType realId = cellPtIds->GetId(j);
              //cout << "old scalar id=" << realId << " : ";
              double scal = this->GetPointData()->GetScalars()->GetTuple1(realId);
              
              //cout << "scalar=" << scal << " : ";
              //cout << "new id=" << loc+j << endl;
              scalars->SetValue( loc+j, scal );

            }

            loc += cell->GetNumberOfPoints();
            target->InsertNextCell( cell->GetCellType(), ptIds );

          cellPtIds->Delete();
          ptIds->Delete();
        }
      //  else
      //  {
      //    cout << "...dicarding cell " << cellId << " as it is an empty cell." << endl;
      //  }
      }
    }

    pts->ComputeBounds();

    target->SetPoints( pts );
    target->GetPointData()->SetScalars(scalars);

  scalars->Delete();
  pts->Delete();
  
  target->BuildLinks();
  target->SetUpdateExtent( 1, 1, 1 );
  target->ComputeBounds();

  return target->GetNumberOfCells();
}

//----------------------------------------------------------------------------

/** NewDatasetFromNodeSet

  Given the name of the id set you want, and a valid pointer to a new dataset,
  this function will re-initialize and fill out the dataset with the requested set elements or nodes.
  
  This function configures the PolyData param to have :
    vtkPoints that contains all the points in a given node set, 
    a 1-component int array for pointdata scalars and 
    a 3-component double array for pointdata normals

  The result is a polydata that is ready made for glyphing.
  */
long vtkAbaqusElementModel::NewDatasetFromNodeSet(const char *setName, 
                                                  vtkPolyData *target )
{
  if( !target )
  {
    return 0;
  }

  vtkIdTypeArray *nset = this->GetNodeSet( setName );
  if( !nset )
  {
    return 0;
  }

  target->Initialize();

  vtkIdType sz = 0;
  sz = nset->GetSize();

  target->Allocate( sz );

  vtkPoints *pts = vtkPoints::New();
  pts->SetNumberOfPoints( sz );
  target->SetPoints( pts );
  pts->Delete();

  //we need category scalars forthe pointdata
  vtkIntArray *vals = vtkIntArray::New();
  vals->SetNumberOfValues( sz );
  target->GetPointData()->SetScalars( vals );
  vals->Delete();               

  //we need category normals to orient glyphs in the mark selection view
  vtkDoubleArray *nor = vtkDoubleArray::New();
  nor->SetNumberOfComponents(3);
  nor->SetNumberOfTuples( sz );
  target->GetPointData()->SetNormals( nor );
  nor->Delete();

  target->BuildCells();
  target->BuildLinks();

  int scalar = 100;

  for( int i=0; i<sz; i++ )
  {
    double *v = this->GetPoint( nset->GetValue(i) );
    target->GetPoints()->SetPoint( i, v );
    target->GetPointData()->GetScalars()->SetTuple1( i, scalar );
    target->GetPointData()->GetNormals()->SetTuple3( i, 0, 1, 0 );

    target->GetVerts()->InsertNextCell( 1 );
    target->GetVerts()->InsertCellPoint( i );
  }

  target->GetPoints()->Modified();
  target->GetPoints()->ComputeBounds();

  target->SetUpdateExtent( 1, 1, 1 );
  target->Modified();
  target->ComputeBounds();

  return target->GetNumberOfPoints();
}
