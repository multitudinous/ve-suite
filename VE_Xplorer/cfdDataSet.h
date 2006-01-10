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
 * File:          $RCSfile: cfdDataSet.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_DATA_H
#define CFD_DATA_H

#ifdef USE_OMP
#define MAX_DATA 20
#endif
#include <string>
#include <vector>

class vtkLookupTable;
class vtkPolyData;
class vtkUnstructuredGrid;
class vtkUnstructuredGridReader;
class vtkDataSet;
namespace VE_Xplorer
{
   class cfdPlanes;
}

namespace VE_SceneGraph
{
   class cfdDCS;
   class cfdGroup;
   class cfdSwitch;
   class cfdTempAnimation;
}

namespace VE_Util
{
   class cfdVTKFileHandler;
}
//! CFD data set loader and handler.
/*!
   A class to load data set and pre-compute flow parameters 
   or properties for virtual environment interactive 
   computation.
*/
#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdDataSet
   {
      public:
         cfdDataSet();    // Construct vtkUnstructuredGrid and vtkLookupTable objects.
  
         ~cfdDataSet();   // Destruct vtkUnstructuredGrid and vtkLookupTable objects.

         // Initialize the number of data to load and parallel process.
         // By default, use the octree table.
         void LoadData( const std::string fileName );
         void LoadData(vtkUnstructuredGrid*,int);
         void LoadData();
  
         // Set/get the range of velocity based on the data set.
         void SetRange( double dataRange[2] );
         void SetRange( double dataMin, double dataMax );
         void GetRange( double dataRange[2] );
         void GetRange( double &dataMin, double &dataMax );
         double * GetRange();

         void GetRange(int* range);
  
         // Set/get the min/max velocity, used defined.
         void SetUserRange( double userRange[2] );
         void SetUserRange( double userMin, double userMax );
         void GetUserRange( double userRange[2] );
         void GetUserRange( double &userMin, double &userMax );
         double * GetUserRange();

         // Set/get the length of the diagonal of the bounding box for data set.
         void SetLength( float len );
         void GetLength( float &len );
         float GetLength();

         // Get the length of the diagonal of the bounding box of the average cell
         void GetMeanCellLength( float &len );
         float GetMeanCellLength();

         // Set/get the step length for streamline integration.
         void SetStepLength( float sLen );
         void GetStepLength( float &sLen );
         float GetStepLength();

         // Set/get the maximum streamline integration time.
         void SetMaxTime( float mT );
         void GetMaxTime( float &mT );
         float GetMaxTime();

         // Set/get time step for streamline integration
         void SetTimeStep( float tStep );
         void GetTimeStep( float &tStep );
         float GetTimeStep();

         // Get the vtk look up table.
         vtkLookupTable * GetLookupTable();

         // Get the single piece original data.
         vtkUnstructuredGrid * GetUnsData();
         vtkPolyData * GetPolyData();
         vtkDataSet * GetDataSet();

         void SetType();       // compute dataset type by looking at the file
         void SetType( int );  // manually set the dataset type
         int GetType();        // get the dataset type

         // SetActiveScalar and compute the actual scalar range and the pretty range for display purposes
         // 0 <= activeScalar < numScalars
         void SetActiveScalar( int );
         int GetActiveScalar();

         void SetActiveVector( int );
         int GetActiveVector();

         // Update the geometrical properties of the mesh
         void UpdatePropertiesForNewMesh();

         static void AutoComputeUserRange( const double rawRange[2],
                                    double prettyRange[2] );

         void ResetScalarBarRange( int min, int max );

         void SetFileName( const std::string filename );
         void SetFileName_OnFly(int);
         std::string GetFileName();

         void SetPrecomputedDataSliceDir( const std::string newDir );
         std::string GetPrecomputedDataSliceDir();

         void SetPrecomputedSurfaceDir( const std::string newDir );
         std::string GetPrecomputedSurfaceDir();

         cfdPlanes * GetPrecomputedXSlices();
         cfdPlanes * GetPrecomputedYSlices();
         cfdPlanes * GetPrecomputedZSlices();
         cfdPlanes * GetPrecomputedSlices( int xyz );

         void StoreScalarInfo();

         #ifdef USE_OMP
            vtkUnstructuredGrid * GetData(int i);
            int GetNoOfDataForProcs();       // Set/get number of data for parallel process.
         #endif

         void SetArrow( vtkPolyData * );
         vtkPolyData * GetArrow();

         void SetNewlyActivated();
         void SetNotNewlyActivated();
         int IsNewlyActivated();

         int GetNumberOfScalars();
         std::string GetScalarName( int );

         int GetNumberOfVectors();
         std::string GetVectorName( int );

         cfdDataSet * GetParent();
         void SetParent( cfdDataSet * );

         void SetActualScalarRange( int, double * );
         void GetActualScalarRange( int, double * );
         double * GetActualScalarRange( int );

         // returns displayed range of active scalar
         double * GetDisplayedScalarRange();

         // get/set displayed range of any scalar
         double * GetDisplayedScalarRange( int );
         void SetDisplayedScalarRange( int , double * );

         double * GetVectorMagRange();

         // get/set this dataset's DCS
         VE_SceneGraph::cfdDCS * GetDCS();
         void SetDCS( VE_SceneGraph::cfdDCS * );

         VE_SceneGraph::cfdSwitch* GetSwitchNode( void );

         VE_SceneGraph::cfdTempAnimation* GetAnimation( void );
         void SetAnimation( VE_SceneGraph::cfdTempAnimation* );

         int IsPartOfTransientSeries();
         void SetAsPartOfTransientSeries();

         void Print();

      private:
         double** actualScalarRange;
         double** displayedScalarRange;

         cfdDataSet* parent;

         int isNewlyActivated;

         int CountNumberOfParameters( const int numComponents );
         std::vector<std::string> GetParameterNames( const int numComponents, const int numParameters );

         double* range;          // Range of scalar.
   
         double* definedRange;   // 'prettied' range of scalar that is automatically computed or user-defined.

         double* vectorMagRange; // assumes just one vector

         float bbDiagonal;        // length of the diagonal of the bounding box.

         double meanCellBBLength; // length of diagonal of average cell bounding box. 
  
         float stepLength;        // Step length for streamline integration.

         float maxTime;           // Maximum time of integration for streamline.

         float timeStep;          // Time step for streamline integration.

         vtkLookupTable* lut;    // Lookup table.

         vtkDataSet* dataSet;    // Original piece of vtk data.
         int datasetType;         // used by gui to place in appropriate column

         int activeScalar;
         int activeVector;

         std::string fileName;
         std::string precomputedDataSliceDir;
         std::string precomputedSurfaceDir;

         cfdPlanes* x_planes;
         cfdPlanes* y_planes;
         cfdPlanes* z_planes;

         vtkPolyData* arrow;

         int numPtDataArrays;
         int numScalars;
         int numVectors;
         std::vector< std::string > scalarName;
         std::vector< std::string > vectorName;

         VE_SceneGraph::cfdDCS* dcs;
         VE_SceneGraph::cfdTempAnimation* animation;
         VE_SceneGraph::cfdSwitch* switchNode;
         VE_SceneGraph::cfdGroup* classic;
         VE_SceneGraph::cfdGroup* textureBased; 

         VE_Util::cfdVTKFileHandler* _vtkFHndlr;
         int partOfTransientSeries;
         int intRange[2];

      #ifdef USE_OMP 
         unsigned int noOfData;   // Total no. of octants.
         vtkUnstructuredGridReader *dataReader[MAX_DATA];
         vtkUnstructuredGrid *data[MAX_DATA];
      #endif
   };
}
#endif
