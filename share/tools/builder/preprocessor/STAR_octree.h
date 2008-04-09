/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
// .NAME octree.h - An octree based decomposition method.
// .SECTION Description
// A class used to decomposed a large unstructured grid into
// smaller octants of gridsets.  Octants sizes are controlled by
// specifying the number of cells in each octant.

#ifndef OCTREE
#define OCTREE

#include "STAR_octant.h"
#include "STAR_node.h"

#include <vtkContourFilter.h>
#include <vtkDecimatePro.h>
#include <vtkExtractUnstructuredGrid.h>
#include <vtkGeometryFilter.h>
#include <vtkMergePoints.h>
#include <vtkOutlineSource.h>
#include <vtkPointData.h>
#include <vtkPolyData.h>
#include <vtkPolyDataNormals.h>
#include <vtkPolyDataWriter.h>
#include <vtkSmoothPolyDataFilter.h>
#include <vtkTriangleFilter.h>
#include <vtkUnstructuredGrid.h>
#include <vtkUnstructuredGridWriter.h>
#include <sstream>

const int maxLevel = 10;
const int deg = 8;
const int maxOctantObj = 100000; //used only by STAR_octree.h

class Octree
{
public:
  Octree( );
  ~Octree( ){ 
              octantPnts->Delete( ); 
	      octantGrid->Delete( );
	      mergePoints->Delete( );
            }

  // Description:
  // Initialized the dataset to be decomposed and the minimum number of cells
  // for termination criterion.
  void InitOctreeDecomposition( vtkUnstructuredGrid *grid, int cells );

  // Description:
  // Get the number of cells in each octant
  int GetCellsPerOctant( ) { return cellsNo; }

  // Description:
  // Return the node with data and information stored.
  Node * GetOctreeNode( int i ) { return theNode[i + nodeID[0]]; }

  // Description:
  // Extract the boundary of the input dataset( for example, vtkUnstructuredGrid )
  void ExtractUnsGridBound( );

  // Description:
  // Given a diagonal boundary, this function will return back the number 
  // of cells within that boundary.
  void OctantNoOfCells( double subBound[6], int &noOfCells );

  // Description:
  // Execute the decompostion method.
  void Decompose( );

  // Description:
  // Function to init the octants table parameters and variables.
  void InitOctantsTable( );

  // Description:
  // Function to insert points and cells of the octants.
  void InsertOctantsTable( );

  // Decription:
  // Write the octants unstructuredgrid tables in vtk format.
  void WriteOctantsTable( );

  // Description:
  // Function to output the octants data with boundary expanded to one cell size
  void WriteOctantsData( );

  // Description:
  // Function to output the octants with neigboring cells
/*   void WriteOctantNeighborsTable( ); */

  // Description:
  // Function to return the height of the tree after decomposition.
  int GetHeight( ) const { return height; }

  // Description:
  // Function to return the number of nodes at the height.
  int GetNumberOfNodes( ) const { return totalNumberOfNodes; }

  // Description:
  // Function to find the largest cell, given the extracted grid
  void FindMaxCellLength( vtkExtractUnstructuredGrid *extractGrid  );

private:
  vtkUnstructuredGrid *unsGrid; 
  vtkUnstructuredGrid *octantGrid;
  vtkMergePoints *mergePoints;
  vtkPoints *octantPnts;

  Node* theNode[maxOctantObj];
  Octant * pOctant;

  int headNodeIdAtLevel[maxLevel];
  int tailNodeIdAtLevel[maxLevel];
  int nodeID[2];
  int cellsNo;
  int level;
  int height;
  int id;
  int level_N_nodes;
  int siblingID;
  int degree;
  int eachOctantCells;
  int totalNumberOfNodes;
  int ptIdx;
  int cellIdx;

  static double cellBound[6];
  double bound[6];
  float xout[27];
  float yout[27];
  float zout[27];
  float cellLength;
};

double Octree::cellBound[6] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };

Octree::Octree( )
{
  // initialization
  cellsNo = 0;
  level = 0;
  height = 0;
  id = 0;
  level_N_nodes = 0;
  siblingID = 0;
}  

void Octree::InitOctreeDecomposition( vtkUnstructuredGrid *grid, int cells )
{
  double bd[6];

  unsGrid = grid;

  eachOctantCells = cells;

  unsGrid->GetCellBounds( unsGrid->GetMaxCellSize( ) , bd );

  cellLength = sqrt( (bd[0] - bd[1]) * (bd[0] - bd[1]) +
		     (bd[2] - bd[3]) * (bd[2] - bd[3]) +
		     (bd[4] - bd[5]) * (bd[4] - bd[5]) );

  ExtractUnsGridBound( );

  Decompose( );

  //std::cout << "Decomposition ended.... " << std::endl;
}

void Octree::ExtractUnsGridBound( )
{
  unsGrid->GetBounds( bound );
}

void Octree::OctantNoOfCells ( double subBound[6], int &noOfCells )
{
// To extract the number of cells in a given boundary 
vtkExtractUnstructuredGrid *extractGrid = vtkExtractUnstructuredGrid::New( );
   extractGrid->SetInput( unsGrid );
   extractGrid->ExtentClippingOn( );
   extractGrid->SetExtent( subBound );
   extractGrid->Update( );
   noOfCells = extractGrid->GetOutput( )->GetNumberOfCells( );

   if ( noOfCells != 0 )
   {
     FindMaxCellLength( extractGrid );
   }

   extractGrid->Delete( );
}

void Octree::Decompose( )
{
  InitOctantsTable( );

  int decompose = 1;
  int currOctantCells = unsGrid->GetNumberOfCells( );

  // pointer for first node
  Node * pNode = 0; 

  // setting octants properties for the first node
  // Octant properties are : boundary, no of cells
  pOctant = new Octant( );
  pOctant->SetBound( bound );
  pOctant->SetNumberOfCells( currOctantCells );

  // setting the octants properties and 
  // identify the parent into the node
  Node *pRoot = new Node( );
  pRoot->SetProperties( pOctant );
  pRoot->SetParent( pRoot );

  // identify the node id for head and tail of the node at each level
  headNodeIdAtLevel[0] = 0;
  tailNodeIdAtLevel[0] = 0;

  // inserting the node into an array of nodes for easy retrieval
  theNode[ headNodeIdAtLevel[0] ] = pRoot;

  // nodes insertion
  while ( decompose )
  {
        decompose = 0;
	level++;
	height++;
	  
	// number of nodes at N level
	level_N_nodes = (int) pow( (double)deg, (double)height );

	// indexing for the 8 children of each node
	int j = 0;
	int i;

	// at each level
	for ( i=0; i<level_N_nodes; i++ )
	{
	    // node id ( in global )
	    id++;
	    
	    // setting node properties
	    // node properties are: the parent node, level, id
	    pNode = new Node( );
	    pNode->SetParent( theNode[ headNodeIdAtLevel[height-1] + j ] );
	    pNode->SetLevel( level );
	    pNode->SetID( id );

	    // avoid unnecessary refinement of octants
	    if ( pNode->GetParent( )->GetOctant( )->GetNumberOfCells( ) > eachOctantCells )
	    {
	       if ( !siblingID )
	       {
		  // Get the parent boundary
		  // Subdivide into 8 octants and return the 27 points
		  pNode->GetParent( )->GetOctant( )->GetBound( bound );
		  pOctant->GetOctantsPoints( bound, xout, yout, zout );
	       }

	       // Given each octant siblingID, and 27 points found, 
	       // Orgnize the siblings accordingly to its boundary
	       pOctant->GetSiblingBound( siblingID, xout, yout, zout, bound );

	       // Find the no. of cells, given the boundary
	       OctantNoOfCells ( bound , cellsNo );

	       // Create new octant for storing their property
	       // Set the boundary that is retrieved from the GetSiblingBound
	       // Set the number of cells found
	       pOctant = new Octant();
	       pOctant->SetBound( bound );
	       pOctant->SetNumberOfCells( cellsNo );

		   std::cout << "Id " << i << "\t Cell no. " << cellsNo << std::endl;

	       // Increment to the next sibling
	       siblingID ++;

	       // If sibling equal to 8, then all siblings identified,
	       // reset to zero for next set of children
	       if ( siblingID == deg )
	       {
		  siblingID = 0;
		  j++;
	       }

	       // inserting the octant properties and the linked list
	       pNode->SetProperties( pOctant );
	       pRoot->Insert( pNode );
	    }
	    else
	    {
	       // Setting the properties of an octant when the
	       // cells in octant has been reached and setting the 
	       // descendants to have no inheritance from its parent
	       pOctant = new Octant( );
	       siblingID ++;

	       if ( siblingID == deg )
	       {
		  siblingID = 0;
		  j++;
	       }
		
	       pNode->SetProperties( pOctant );
	       pRoot->Insert( pNode );		
	    }
	    
	}
	  
	// Get the head and tail id of the node at height
	headNodeIdAtLevel[height] = pRoot->GetNumberOfNodesAt_h_Height( height - 1 );
	tailNodeIdAtLevel[height] = pRoot->GetNumberOfNodesAt_h_Height( height ) - 1;

	// Making the nodes into an array for easy identification and retrieval
	for ( i=headNodeIdAtLevel[height]; i<=tailNodeIdAtLevel[height]; i++ )
	{
	    pRoot = pRoot->GetNode();
	    theNode[i] = pRoot;

	    if ( theNode[i]->GetOctant( )->GetNumberOfCells( ) <= eachOctantCells &&
		 theNode[i]->GetOctant( )->GetNumberOfCells( ) > 0 )
	    {
	       theNode[i]->GetOctant( )->GetBound( bound );

	       WriteOctantsData( );

	       InsertOctantsTable( );
	    }
	}

	// Check the nodes at height are below the required minimum amounts
	// If no, continue decompose
	for ( i=headNodeIdAtLevel[height]; i<=tailNodeIdAtLevel[height]; i++ )
	{
	    if ( theNode[i]->GetOctant( )->GetNumberOfCells( ) > eachOctantCells )
	    {
	       decompose = 1;
	       i = tailNodeIdAtLevel[height] + 1;
	    }
	}

	// output to the screen
	totalNumberOfNodes = pRoot->GetNumberOfNodesAt_h_Height( height );
	for ( int k=0; k<totalNumberOfNodes; k++ )
	{
	    std::cout <<  "The Node " << theNode[k]->GetID( )
		 << " Parent " << theNode[k]->GetParent( )->GetID( ) 
		 << " Level " << theNode[k]->GetLevel( )
		 << " Number of cells " << theNode[k]->GetOctant( )->GetNumberOfCells( ) 
		 << std::endl;
	}
	std::cout << std::endl;
  }

  WriteOctantsTable( );
}

void Octree::InitOctantsTable( )
{
  octantPnts = vtkPoints::New( );

  octantGrid = vtkUnstructuredGrid::New( );
  octantGrid->Allocate( maxOctantObj, maxOctantObj );

  mergePoints = vtkMergePoints::New( );
  mergePoints->Initialize( );
  mergePoints->InitPointInsertion( octantPnts, bound );

  ptIdx = 0;
}

void Octree::InsertOctantsTable( )
{
  vtkIdType ptId;
  vtkIdType cellPtId[8];
  double xtmp[3];

  // Set the boundary of the octant
  // and generate the 8 points for the octant
  vtkOutlineSource *outline = vtkOutlineSource::New();
     outline->SetBounds( bound );
     outline->Update( );

  // Check whether point has been inserted
  // If yes, get the ptId
  for ( int j=0; j<deg; j++ )
  {
      outline->GetOutput( )->GetPoints( )->GetPoint( j, xtmp );
      ptId = mergePoints->IsInsertedPoint( xtmp );

      if ( ptId == -1 )
      {
	      cellPtId[j] = ptIdx;
	      mergePoints->InsertPoint( ptIdx, xtmp );
	      ptIdx++;
      }
      else
      {
	      cellPtId[j] = ptId;
      }
  }

  octantGrid->InsertNextCell( VTK_VOXEL, deg, cellPtId );

  outline->Delete( );
}

void Octree::WriteOctantsTable( )
{
  octantGrid->SetPoints( octantPnts );
  octantGrid->Squeeze( );

  vtkUnstructuredGridWriter *writer = vtkUnstructuredGridWriter::New( );
     writer->SetInput( octantGrid );
     writer->SetFileName( "./POST_DATA/octreeTable.vtk" );
     writer->Write( );

     writer->Delete( );
}

void Octree::WriteOctantsData( )
{
  double bd[6];
  double dCell[3];
  //char* writer_title;
  
  dCell[0] = fabs( cellBound[1] - cellBound[0] );
  dCell[1] = fabs( cellBound[3] - cellBound[2] );
  dCell[2] = fabs( cellBound[5] - cellBound[4] );
/*   dCell[0] = 3.0f*fabs( cellBound[1] - cellBound[0] ); */
/*   dCell[1] = 2.0f*fabs( cellBound[3] - cellBound[2] ); */
/*   dCell[2] = 3.0f*fabs( cellBound[5] - cellBound[4] ); */
  bd[0] = bound[0] - dCell[0];
  bd[1] = bound[1] + dCell[0];
  bd[2] = bound[2] - dCell[1];
  bd[3] = bound[3] + dCell[1];
  bd[4] = bound[4] - dCell[2];
  bd[5] = bound[5] + dCell[2];

  vtkExtractUnstructuredGrid *extractGrid = vtkExtractUnstructuredGrid::New( );
     extractGrid->SetInput( unsGrid );
     extractGrid->ExtentClippingOn( );
     extractGrid->SetExtent( bd );
     extractGrid->Update( );

   std::ostringstream dirStringStream;
   dirStringStream << "./POST_DATA/octant" << octantGrid->GetNumberOfCells( ) << ".vtk";

  vtkUnstructuredGridWriter *writer = vtkUnstructuredGridWriter::New();
     writer->SetInput( extractGrid->GetOutput( ) );
     writer->SetFileName( dirStringStream.str().c_str() );
     writer->SetFileTypeToBinary( );
     writer->Write( );
     writer->Delete( );

     extractGrid->Delete( );
}

void Octree::FindMaxCellLength( vtkExtractUnstructuredGrid *extractGrid  )
{
  extractGrid->GetOutput( )->GetCellBounds( extractGrid->GetOutput( )->GetMaxCellSize( ), cellBound );
}

#endif
