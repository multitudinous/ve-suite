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
 * File:          $RCSfile: ansysReader.h,v $
 * Date modified: $Date: 2004-05-18 16:09:54 -0500 (Tue, 18 May 2004) $
 * Version:       $Rev: 385 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef ANSYSREADER_H
#define ANSYSREADER_H

#include <iostream>
typedef int int32;
typedef long long int64;

class vtkUnstructuredGrid;
class vtkIntArray;
class vtkDoubleArray;

class ansysReader
{
   public:
      ansysReader( char * );

      ~ansysReader();

      vtkUnstructuredGrid * GetUGrid();

   private:
      void FlipEndian();
      int32 ReadNthInteger( int32 & n );
      int64 ReadNthDoubleLong( int32 & n );
      float ReadNthFloat( int32 & n );
      double ReadNthDouble( int32 & n );

      void ReadHeader();
      void ReadRSTHeader();
      void ReadDOFBlock();
      void ReadNodalEquivalencyTable();
      void ReadElementEquivalencyTable();
      void ReadDataStepsIndexTable();
      void ReadTimeTable();
      void ReadGeometryTable();
      void ReadElementTypeIndexTable();
      void ReadRealConstantsIndexTable();
      void ReadCoordinateSystemsIndexTable();
      void ReadNodalCoordinates();
      void ReadElementDescriptionIndexTable();
      void ReadHeaderExtension();

      void ReadSolutionDataHeader( int32 ptrDataSetSolution );
      void ReadNodalSolutions( int32 ptrDataSetSolution );
      void ReadElementSolutions( int32 ptrDataSetSolution );
      void ReadElementDescription( int32 elemIndex, int32 ptr );
      int32 * ReadElementTypeDescription( int32 ptr );
      double * ReadElementRealConstants( int32 ptr );
      double * ReadCoordinateSystemDescription( int32 ptr );
      void ReadGenericBlock( int32 int32Position );
      void ReadElementIndexTable( int32, int32 );
      int32 VerifyNumberOfValues( int32 reportedNumValues, int32 blockSize_1 );
      void VerifyBlock( int32 blockSize_1, int32 blockSize_2 );
      void ReadNodalComponentStresses( int32 );
      void AttachStressToGrid();
      void StoreNodalStessesForThisElement( int32 elemIndex );

      char * ansysFileName;
      FILE *s1;
      bool endian_flip;
      int32 integerPosition;
      char ansysVersion [ 5 ];

      int32 numNodes;
      int32 numExpandedNodes;
      int32 maxNumberDataSets;
      int32 numElems;
      int32 numDOF;
      int32 * dofCode;
      vtkIntArray * nodeID;
      int32 * elemID;

      int32 ptrNodalEquivalencyTable;
      int32 ptrElementEquivalencyTable;
      int32 ptrDataStepsIndexTable;
      int32 ptrTIM;
      int32 ptrLoadStepTable;
      int32 ptrGEO;
      int32 currentDataSetSolution;

      int32 maxety;
      int32 maxrl;
      int32 maxcsy;
      int32 ptrETY;
      int32 ptrREL;
      int32 ptrNOD;
      int32 ptrCSY;
      int32 ptrELM;
      int32 csysiz;

      int32 etysiz;
      int32 * ptrToElemType;
      int32 * ptrToElemRealConstants;
      int32 * ptrToCoordinateSystems;
      int32 ** elemDescriptions;
      double ** elemRealConstants;
      double ** coordinateSystemDescriptions;
      double ** nodalCoordinates;
      int32 * ptrElemDescriptions;
      int32 * ptrDataSetSolutions;
      int32 ptrNSL; // Nodal solutions
      int32 ptrESL; // Element solutions
      int32 ptrEXT;
      int32 * ptrENS;
      int32 * materialInElement;
      int32 * realConstantsForElement;
      int32 * coordSystemforElement;
      int32 * numCornerNodesInElement;
      int32 ** cornerNodeNumbersForElement;
      vtkDoubleArray * summedFullGraphicsS1Stress;
      vtkDoubleArray * summedFullGraphicsS3Stress;
      vtkDoubleArray * summedFullGraphicsVonMisesStress;
      vtkDoubleArray * summedPowerGraphicsS1Stress;
      vtkDoubleArray * summedPowerGraphicsS3Stress;
      vtkDoubleArray * summedPowerGraphicsVonMisesStress;
      vtkIntArray * numContributingFullGraphicsElements;
      vtkIntArray * numContributingPowerGraphicsElements;
      char * displacementUnits;
      char * stressUnits;
      vtkUnstructuredGrid * ugrid;
      vtkIntArray * pointerToMidPlaneNode;
};
#endif
