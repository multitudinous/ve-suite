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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
//! CFD data set loader and handler.
/*!
   A class to load data set and pre-compute flow parameters 
   or properties for virtual environment interactive 
   computation.
*/

#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdPlanes.h"
#include "VE_Xplorer/XplorerHandlers/DataSetAxis.h"
#include "VE_Xplorer/XplorerHandlers/DataSetScalarBar.h"

#include "VE_Xplorer/Utilities/cfdAccessoryFunctions.h"
#include "VE_Xplorer/Utilities/fileIO.h"
#include "VE_Xplorer/Utilities/readWriteVtkThings.h"
#include "VE_Xplorer/Utilities/cfdGrid2Surface.h"
#include "VE_Xplorer/Utilities/cfdVTKFileHandler.h"

#include <vtkLookupTable.h>
#include <vtkPointData.h>
#include <vtkDataSet.h>
#include <vtkPolyData.h>
#include <vtkFloatArray.h>
#include <vtkUnstructuredGrid.h>
#include <vtkUnstructuredGridReader.h>
#include <vtkSystemIncludes.h>  // for VTK_POLY_DATA
#include <vtkCellTypes.h>
#include <vtkCellDataToPointData.h>
#include <vtkCellData.h>
#include <vtkOutlineFilter.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkProperty.h>
#include <vtkGeometryFilter.h>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include <iostream>
#include <sstream>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_Util;

cfdDataSet::cfdDataSet( )
{
   this->dataSet = NULL;
   this->lut = vtkLookupTable::New();
   this->arrow = NULL;
   this->activeScalar = -1;         // 0 <= activeScalar < numScalars
   this->activeVector = -1;
   this->numScalars = 0;
   this->numVectors = 0;
   this->scalarName.empty();// = NULL;
   this->vectorName.empty();// = NULL;
   this->x_planes = NULL;
   this->y_planes = NULL;
   this->z_planes = NULL;
   this->range = new double [ 2 ];
   this->range[ 0 ] = 0.0f;
   this->range[ 1 ] = 1.0f;
   this->definedRange = new double [ 2 ];
   this->definedRange[ 0 ] = 0.0f;
   this->definedRange[ 0 ] = 1.0f;
   this->isNewlyActivated = 0;
   this->fileName.empty();// = NULL;
   this->precomputedDataSliceDir.empty();// = NULL;
   this->precomputedSurfaceDir.empty();// = NULL;
   // precomputed data that descends from a flowdata.vtk should
   // automatically have the same color mapping as the "parent" 
   // By default, the dataset is assumed to have no parent, that is,
   // use its own range to determine color mapping.
   this->parent = this;
   //this->dcs = NULL;
   this->switchNode = new VE_SceneGraph::Switch();
   this->switchNode->SetName( "switch_for_data_viz" );
   this->classic = new VE_SceneGraph::Group();
   this->classic->SetName( "classic" );
   this->switchNode->AddChild( this->classic.get() );
   this->textureBased = new VE_SceneGraph::Group();
   this->textureBased->SetName( "textureBased" );
   this->switchNode->AddChild( this->textureBased.get() );
   this->switchNode->SetVal(0);

   this->partOfTransientSeries = 0;
   this->datasetType = -1;
   this->vectorMagRange = NULL;
   this->actualScalarRange = NULL;
   this->displayedScalarRange = NULL;
   this->meanCellBBLength = 0.0;
   //this->intRange[0] = 0;
   //this->intRange[1] =1000000;
   dataSetAxes = 0;
   dataSetScalarBar = 0;
   //this->animation = 0;
   _vtkFHndlr = 0;
}

cfdDataSet::~cfdDataSet()
{
   this->lut->Delete();
   this->lut = NULL;

   delete [] this->definedRange;
   this->definedRange = NULL;

   delete [] this->range;
   this->range = NULL;

   int i;

   if ( this->numScalars > 0 )
   {
      scalarName.clear();

      for ( i=0; i<this->numScalars; i++ )
      {
         delete [] this->actualScalarRange[i];
         delete [] this->displayedScalarRange[i];
      }
      delete [] this->actualScalarRange;
      this->actualScalarRange = NULL;
      delete [] this->displayedScalarRange;
      this->displayedScalarRange = NULL;
   }

   if ( this->numVectors > 0 )
   {
      vectorName.clear();

      delete [] this->vectorMagRange;
      this->vectorMagRange = NULL;
   }

   if ( this->x_planes != NULL ) 
   {
      delete this->x_planes;
      this->x_planes = NULL;
      vprDEBUG(vesDBG,1) << "deleting this->x_planes" 
                             << std::endl << vprDEBUG_FLUSH;
   }

   if ( this->y_planes != NULL ) 
   {
      delete this->y_planes;
      this->y_planes = NULL;
      vprDEBUG(vesDBG,1) << "deleting this->y_planes" 
                             << std::endl << vprDEBUG_FLUSH;
   }
   
   if ( this->z_planes != NULL ) 
   {
      delete this->z_planes;
      this->z_planes = NULL;
      vprDEBUG(vesDBG,1) << "deleting this->z_planes" 
                              << std::endl << vprDEBUG_FLUSH;
   }
  
   if ( this->precomputedDataSliceDir.c_str() != NULL )
   {
      precomputedDataSliceDir.erase();//delete [] this->precomputedDataSliceDir;
      precomputedDataSliceDir.empty();//this->precomputedDataSliceDir = NULL;
   }

   if ( this->precomputedSurfaceDir.c_str() != NULL )
   {
      precomputedSurfaceDir.erase();//      delete [] this->precomputedSurfaceDir;
      precomputedSurfaceDir.empty();//      this->precomputedSurfaceDir = NULL;
   }

   if ( this->dataSet != NULL )
   {
      this->dataSet->Delete();
      this->dataSet = NULL;
   }

   if ( this->fileName.c_str() != NULL )
   {
      vprDEBUG(vesDBG,2) << "deleting filename " << this->fileName
                             << std::endl << vprDEBUG_FLUSH;
      fileName.erase();//delete [] this->fileName;
      fileName.empty();//this->fileName = NULL;
   }
 
   if ( _vtkFHndlr )
   {
      delete _vtkFHndlr;
      _vtkFHndlr = 0;
      vprDEBUG(vesDBG,2) << "deleting _vtkFHndlr " << std::endl << vprDEBUG_FLUSH;
   }
}

void cfdDataSet::SetRange( double * dataRange )
{
   this->SetRange( dataRange[ 0 ], dataRange[ 1 ] );
}

void cfdDataSet::SetRange( double dataMin, double dataMax )
{
   this->range[ 0 ] = dataMin;
   this->range[ 1 ] = dataMax;
}

void cfdDataSet::GetRange( double * dataRange )
{
   this->GetRange( dataRange[ 0 ], dataRange[ 1 ] );
}

void cfdDataSet::GetRange( double &dataMin, double &dataMax )
{
   dataMin = this->range[ 0 ];
   dataMax = this->range[ 1 ];
}

double * cfdDataSet::GetRange()
{
   return this->range;
}

void cfdDataSet::SetUserRange( double userRange[2] )
{
   vprDEBUG(vesDBG,1) << "cfdDataSet::SetUserRange OLD userRange = " 
      << userRange[0] << " : " << userRange[1]
      << std::endl << vprDEBUG_FLUSH;

   this->SetUserRange( userRange[0], userRange[1] );
   
   vprDEBUG(vesDBG,1) << "cfdDataSet::SetUserRange NEW userRange = " 
      << userRange[0] << " : " << userRange[1]
      << std::endl << vprDEBUG_FLUSH;
}

void cfdDataSet::SetUserRange( double userMin, double userMax )
{
   this->definedRange[0] = userMin;
   this->definedRange[1] = userMax;
   this->SetDisplayedScalarRange( this->activeScalar, this->definedRange );
}

void cfdDataSet::GetUserRange( double userRange[2] )
{
   this->GetUserRange( userRange[0], userRange[1] );
}

void cfdDataSet::GetUserRange( double &userMin, double &userMax )
{
   userMin = this->definedRange[0];
   userMax = this->definedRange[1];
}

double * cfdDataSet::GetUserRange()
{
   return this->definedRange;
}

void cfdDataSet::SetLength( float len )
{
   this->bbDiagonal = len;
}

/*void cfdDataSet::GetLength( float &len )
{
   len = this->bbDiagonal;
}

float cfdDataSet::GetLength()
{
   return this->bbDiagonal;
}*/

void cfdDataSet::GetMeanCellLength( float &len )
{
   len = this->meanCellBBLength;
}

float cfdDataSet::GetMeanCellLength()
{
   return this->meanCellBBLength;
}

void cfdDataSet::SetStepLength( float sLen )
{
   this->stepLength = sLen;
}

void cfdDataSet::GetStepLength( float &sLen )
{
   sLen = this->stepLength;
}

float cfdDataSet::GetStepLength()
{
   return this->stepLength;
}

void cfdDataSet::SetMaxTime( float mT )
{
   this->maxTime = mT;
}

void cfdDataSet::GetMaxTime( float &mT )
{
   mT = this->maxTime;
}

float cfdDataSet::GetMaxTime()
{
   return this->maxTime;
}

void cfdDataSet::SetTimeStep( float tStep )
{
   this->timeStep = tStep;
}

void cfdDataSet::GetTimeStep( float &tStep )
{
   tStep = this->timeStep;
}

float cfdDataSet::GetTimeStep()
{
   return this->timeStep;
}

vtkLookupTable * cfdDataSet::GetLookupTable()
{
   return this->lut;
}

vtkUnstructuredGrid * cfdDataSet::GetUnsData()
{
   if ( ! this->dataSet ) 
   {
      return NULL;
   }

   if ( ! this->dataSet->IsA("vtkUnstructuredGrid") ) 
   {
      vprDEBUG(vesDBG,0) 
         << "cfdDataSet::GetUnsData - dataset is not an unsGrid !!"
         << std::endl << vprDEBUG_FLUSH;

      return NULL;
   }

   return (vtkUnstructuredGrid*)this->dataSet;
}

vtkPolyData * cfdDataSet::GetPolyData()
{
   if ( ! this->dataSet ) 
   {
      return NULL;
   }

   if ( ! this->dataSet->IsA("vtkPolyData") ) 
   {
      vprDEBUG(vesDBG,0) 
         << "cfdDataSet::GetPolyData - dataset is not a vtkPolyData !!"
         << std::endl << vprDEBUG_FLUSH;

      return NULL;
   }

   return (vtkPolyData*)this->dataSet;
}

vtkDataSet* cfdDataSet::GetDataSet()
{
   return this->dataSet;
}

void cfdDataSet::SetType()
{
   // this code only needs to be done once for each dataset...
   if ( this->datasetType == -1 )
   {
      int dataObjectType = this->dataSet->GetDataObjectType();
      vprDEBUG(vesDBG,1) << "\tdataObjectType: " << dataObjectType
                             << std::endl << vprDEBUG_FLUSH;
                                
      this->datasetType = 0;
      // see if file is a polydata containing only vertex cells
      // (droplet or particle) 
      if ( dataObjectType == VTK_POLY_DATA )
      {
         vtkCellTypes *types = vtkCellTypes::New();
         vtkPolyData * pData = this->GetPolyData();
         pData->GetCellTypes( types );
         if ( types->GetNumberOfTypes() == 1 && 
              pData->GetCellType( 0 ) == VTK_VERTEX )
         {
            this->datasetType = 1;
         }
         else
         {
            this->datasetType = 2;
         }
         types->Delete();
      }
   }
   vprDEBUG(vesDBG,1) << "\tdatasetType: " << this->datasetType
                          << std::endl << vprDEBUG_FLUSH;
}

void cfdDataSet::SetType( int type )
{
   if ( 0 <= type && type < 3 )
      this->datasetType = type;
}

int cfdDataSet::GetType()
{
   return this->datasetType;
}

#ifdef USE_OMP
vtkUnstructuredGrid * cfdDataSet::GetData( int i )
{
   return this->data[i];
}

int cfdDataSet::GetNoOfDataForProcs()
{
   return this->noOfData;
}
#endif

void cfdDataSet::LoadData( const std::string filename )
{
   this->SetFileName( filename );
   this->LoadData();
}

void cfdDataSet::LoadData(vtkUnstructuredGrid* dataset, int datasetindex)
{
   std::cout<<"[DBG]...Inside LoadData of cfdDataSet"<<std::endl;
   this->SetFileName_OnFly(datasetindex);
   
   vtkPointSet* pointset = NULL;
   pointset = dataset;
   this->meanCellBBLength =cfdAccessoryFunctions::ComputeMeanCellBBLength(pointset);

   vtkFloatArray* array = vtkFloatArray::New();
   array->SetName("meanCellBBLength");
   array->SetNumberOfComponents(1);
   array->SetNumberOfTuples(1);
   array->SetTuple1(0,meanCellBBLength);
   pointset->GetFieldData()->AddArray(array);
   
   //array->Delete();
   //dumpVerticesNotUsedByCells(pointset);
   
   // This MUST BE FIXED mccdo
   writeVtkThing(pointset,this->fileName,1);
   this->LoadData();
}
 /*  this->numPtDataArrays = this->dataSet->GetPointData()
                                        ->GetNumberOfArrays();

   std::cout<<"[DBG]...Inside LoadData ugrid Function"<<std::endl;
   std::cout<<"[DBG]...numPtDataArrays = "<<this->numPtDataArrays<<std::endl;

   vprDEBUG(vesDBG,1) << "\tnumPtDataArrays = " << this->numPtDataArrays
                          << std::endl << vprDEBUG_FLUSH;

   int numCellArrays = this->dataSet->GetCellData()->GetNumberOfArrays();
   vprDEBUG(vesDBG,1) << "\tnumCellArrays = " << numCellArrays
                          << std::endl << vprDEBUG_FLUSH;

   std::cout<<"[DBG]...numCellArrays = "<<numCellArrays<<std::endl;

   if ( numCellArrays > 0 && this->numPtDataArrays == 0 )
   {
      std::cout <<"\nThe dataset has no point data -- "
         << "will try to convert cell data to point data\n" << std::endl;

      vtkCellDataToPointData * converter = vtkCellDataToPointData::New();
      converter->SetInput( this->dataSet );
      converter->Update();

      if ( this->dataSet->GetDataObjectType() == VTK_UNSTRUCTURED_GRID )
      {
         std::cout<<"[DBG]...This is vtkUnstructuredGrid"<<std::endl;
         vtkDataSet * newdataset = vtkUnstructuredGrid::New();
         newdataset->DeepCopy( converter->GetOutput() );
         converter->Delete();

         this->dataSet->Delete();
         this->dataSet = newdataset;
         this->numPtDataArrays = this->dataSet->GetPointData()
                                              ->GetNumberOfArrays();

         vprDEBUG(vesDBG,1) << "\tnumPtDataArrays = "
                                << this->numPtDataArrays
                                << std::endl << vprDEBUG_FLUSH;
      }
      else
      {
         converter->Delete();
         std::cout <<"\nAttempt failed: can not currently handle "
                   << "this type of data\n" << std::endl;
         exit(1);
      }
   }

#ifdef USE_OMP
   char label[100];
   vtkUnstructuredGridReader * tableReader = vtkUnstructuredGridReader::New();
   tableReader->SetFileName("./POST_DATA/octreeTable.vtk");
   tableReader->Update();
   vtkUnstructuredGrid * table = ( vtkUnstructuredGrid * ) tableReader->GetOutput();
   if ( this->noOfData == 0 )
   {
      this->noOfData = table->GetNumberOfCells();
   }

   vprDEBUG(vesDBG,1) <<"noOfData:" << this->noOfData 
                          << std::endl << vprDEBUG_FLUSH;
   tableReader->Delete();

# pragma omp parallel for private(label,i)
   for ( int i=0; i<noOfData; i++ ) 
   {
      this->dataReader[i] = vtkUnstructuredGridReader::New();
      //sprintf( label, "./POST_DATA/octant%d.vtk", i);
      std::ostringstream dirStringStream;
      dirStringStream << "./POST_DATA/octant" << i << ".vtk";
      std::string dirString = dirStringStream.str();
      label = dirString.c_str();

      this->dataReader[i]->SetFileName( label );
      this->dataReader[i]->Update();
      this->data[i] = ( vtkUnstructuredGrid * ) this->dataReader[i]->GetOutput();
   }
#endif

   // Compute the geometrical properties of the mesh
   this->UpdatePropertiesForNewMesh();
   
   // count the number of scalars and store names and ranges...
   this->StoreScalarInfo();

   // count the number of vectors and store names ...
   this->numVectors = CountNumberOfParameters( 3 );
   if ( this->numVectors )
   {
      this->vectorName = GetParameterNames( 3, this->numVectors );
   }

   // if there are point data, set the first scalar and vector as active...
   if ( this->numPtDataArrays )
   {
      // set the first scalar and vector as active
      if ( this->numScalars )
         this->SetActiveScalar( 0 );

      if ( this->numVectors )
      {
         this->SetActiveVector( 0 );
         this->vectorMagRange = cfdAccessoryFunctions::
                           ComputeVectorMagnitudeRange( 
                           this->GetDataSet()->GetPointData()->GetVectors() );
      }
   }
   else
   {
      vprDEBUG(vesDBG,0) << "\tWARNING: No Point Data"
                             << std::endl << vprDEBUG_FLUSH;
   }

   if ( this->GetDataSet()->GetPointData()->GetScalars() ) 
   {
      vprDEBUG(vesDBG,1) << "cfdDataSet: active scalar is \"" 
           <<  this->GetDataSet()->GetPointData()->GetScalars()->GetName() 
           << "\"" << std::endl << vprDEBUG_FLUSH;
   }

   if ( this->GetDataSet()->GetPointData()->GetVectors() ) 
   {
      vprDEBUG(vesDBG,1) << "cfdDataSet: active vector is \""
           <<  this->GetDataSet()->GetPointData()->GetVectors()->GetName() 
           << "\"" << std::endl << vprDEBUG_FLUSH;
   }

   this->SetType();


}*/


void cfdDataSet::LoadData()
{
   if ( this->dataSet != NULL )
   {
      vprDEBUG(vesDBG,1) <<" Already have loaded the data for " 
                             << this->fileName
                             << std::endl << vprDEBUG_FLUSH;
      return;
   }
   else
   {
      vprDEBUG(vesDBG,1) <<" LoadData: filename = " << this->fileName
                             << std::endl << vprDEBUG_FLUSH;
   }

   //this->dataSet = readVtkThing( this->fileName, 0 );
   if(!_vtkFHndlr){
      _vtkFHndlr = new cfdVTKFileHandler();
   }
   //_vtkFHndlr->SetInputFileName(fileName);
   ///This will need to be changed to handle mutliblockdatasets!!!!!!
   this->dataSet = dynamic_cast<vtkDataSet*>(_vtkFHndlr->GetDataSetFromFile(fileName));
   this->numPtDataArrays = this->dataSet->GetPointData()
                                        ->GetNumberOfArrays();
   vprDEBUG(vesDBG,1) << "\tnumPtDataArrays = " << this->numPtDataArrays
                          << std::endl << vprDEBUG_FLUSH;

   int numCellArrays = this->dataSet->GetCellData()->GetNumberOfArrays();
   vprDEBUG(vesDBG,1) << "\tnumCellArrays = " << numCellArrays
                          << std::endl << vprDEBUG_FLUSH;
   //std::cout<<"[DBG]...Inside LoadData()"<<std::endl;
   std::cout<<"[DBG]...numPtDataArrays = "<<this->numPtDataArrays<<std::endl;
   std::cout<<"[DBG]...numCellArrays = "<<numCellArrays<<std::endl;

   if ( numCellArrays > 0 && this->numPtDataArrays == 0 )
   {
      std::cout <<"\nThe dataset has no point data -- "
         << "will try to convert cell data to point data\n" << std::endl;

      vtkCellDataToPointData * converter = vtkCellDataToPointData::New();
      //converter->DebugOn();
      //double* testrange = this->dataSet->GetCellData()->GetArray( 0 )->GetRange();
      //   vprDEBUG(vesDBG,1) << "\tnumPtDataArrays = "
      //                          << testrange[ 0 ] << " : " << testrange[ 1 ]
       //                         << std::endl << vprDEBUG_FLUSH;
      converter->SetInput( this->dataSet );
      converter->PassCellDataOff();
      converter->Update();
      //converter->Print( std::cout );

      if ( this->dataSet->GetDataObjectType() == VTK_UNSTRUCTURED_GRID )
      {
         vtkDataSet * newdataset = vtkUnstructuredGrid::New();
         newdataset->DeepCopy( converter->GetOutput() );
         converter->Delete();

         this->dataSet->Delete();
         this->dataSet = newdataset;
         this->numPtDataArrays = this->dataSet->GetPointData()
                                              ->GetNumberOfArrays();
//this->dataSet->DebugOn();
//this->dataSet->Print(std::cout);
         vprDEBUG(vesDBG,1) << "\tnumPtDataArrays = "
                                << this->numPtDataArrays
                                << std::endl << vprDEBUG_FLUSH;
//double* testrange = this->dataSet->GetPointData()->GetArray( 0 )->GetRange();
//         vprDEBUG(vesDBG,1) << "\tnumPtDataArrays = "
//                                << testrange[ 0 ] << " : " << testrange[ 1 ]
//                                << std::endl << vprDEBUG_FLUSH;
      }
      else
      {
         converter->Delete();
         std::cout <<"\nAttempt failed: can not currently handle "
                   << "this type of data\n" << std::endl;
         exit(1);
      }
   }

#ifdef USE_OMP
   char label[100];
   vtkUnstructuredGridReader * tableReader = vtkUnstructuredGridReader::New();
   tableReader->SetFileName("./POST_DATA/octreeTable.vtk");
   tableReader->Update();
   vtkUnstructuredGrid * table = ( vtkUnstructuredGrid * ) tableReader->GetOutput();
   if ( this->noOfData == 0 )
   {
      this->noOfData = table->GetNumberOfCells();
   }

   vprDEBUG(vesDBG,1) <<"noOfData:" << this->noOfData 
                          << std::endl << vprDEBUG_FLUSH;
   tableReader->Delete();

# pragma omp parallel for private(label,i)
   for ( int i=0; i<noOfData; i++ ) 
   {
      this->dataReader[i] = vtkUnstructuredGridReader::New();
      //sprintf( label, "./POST_DATA/octant%d.vtk", i);
      std::ostringstream dirStringStream;
      dirStringStream << "./POST_DATA/octant" << i << ".vtk";
      std::string dirString = dirStringStream.str();
      //label = dirString.c_str();

      this->dataReader[i]->SetFileName( dirString.c_str() );
      this->dataReader[i]->Update();
      this->data[i] = ( vtkUnstructuredGrid * ) this->dataReader[i]->GetOutput();
   }
#endif

   // Compute the geometrical properties of the mesh
   this->UpdatePropertiesForNewMesh();

   if ( this->GetPrecomputedDataSliceDir().c_str() )
   {   
      double bounds[ 6 ];
      this->GetDataSet()->GetBounds( bounds );

      vprDEBUG(vesDBG,0) << "\tLoading precomputed planes from " 
           << this->GetPrecomputedDataSliceDir() << std::endl << vprDEBUG_FLUSH;
      this->x_planes = new cfdPlanes( 0, this->GetPrecomputedDataSliceDir().c_str(), bounds );
      this->y_planes = new cfdPlanes( 1, this->GetPrecomputedDataSliceDir().c_str(), bounds );
      this->z_planes = new cfdPlanes( 2, this->GetPrecomputedDataSliceDir().c_str(), bounds );
   }

   // count the number of scalars and store names and ranges...
   this->StoreScalarInfo();

   // count the number of vectors and store names ...
   this->numVectors = CountNumberOfParameters( 3 );
   if ( this->numVectors )
   {
      this->vectorName = GetParameterNames( 3, this->numVectors );
   }

   // if there are point data, set the first scalar and vector as active...
   if ( this->numPtDataArrays )
   {
      // set the first scalar and vector as active
      if ( this->numScalars )
         this->SetActiveScalar( 0 );

      if ( this->numVectors )
      {
         this->SetActiveVector( 0 );
         this->vectorMagRange = cfdAccessoryFunctions::
                           ComputeVectorMagnitudeRange( 
                           this->GetDataSet()->GetPointData()->GetVectors() );
      }
   }
   else
   {
      vprDEBUG(vesDBG,0) << "\tWARNING: No Point Data"
                             << std::endl << vprDEBUG_FLUSH;
   }

   if ( this->GetDataSet()->GetPointData()->GetScalars() ) 
   {
      vprDEBUG(vesDBG,1) << "cfdDataSet: active scalar is \"" 
           <<  this->GetDataSet()->GetPointData()->GetScalars()->GetName() 
           << "\"" << std::endl << vprDEBUG_FLUSH;
   }

   if ( this->GetDataSet()->GetPointData()->GetVectors() ) 
   {
      vprDEBUG(vesDBG,1) << "cfdDataSet: active vector is \""
           <<  this->GetDataSet()->GetPointData()->GetVectors()->GetName() 
           << "\"" << std::endl << vprDEBUG_FLUSH;
   }

   this->SetType();
}

int cfdDataSet::CountNumberOfParameters( const int numComponents )
{
   int numParameters = 0;

   // count the number of paraneters containing numComponents components...
   for ( int i=0; i < this->numPtDataArrays; i++ )
   {
      vtkDataArray * array = this->GetDataSet()->GetPointData()->GetArray(i);

      if ( array->GetNumberOfComponents() != numComponents )
      {
         continue;
      }

      // also, ignore arrays of normals...
      if ( numComponents == 3 && ( ! strcmp( array->GetName(), "normals" ) ) )
      {
         vprDEBUG(vesDBG,1) 
            << "cfdDataSet: not counting the parameter called \"normals\"" 
            << std::endl << vprDEBUG_FLUSH;
         continue; 
      }

      numParameters++;
   }
   return numParameters;
}

std::vector<std::string> cfdDataSet::GetParameterNames( const int numComponents, 
                                       const int numParameters )
{
   //char ** name = new char * [numParameters];
   std::vector<std::string> name;
   int ii = 0;

   for ( int i=0; i < this->numPtDataArrays; i++ )
   {
      vtkDataArray * array = this->GetDataSet()->GetPointData()->GetArray(i);

      if ( array->GetNumberOfComponents() != numComponents )
      {
         continue;
      }

      // also, ignore arrays of normals...
      if ( numComponents == 3 && ( ! strcmp( array->GetName(), "normals" ) ) )
      {
         continue; 
      }

      //name[ii] = new char [ strlen( array->GetName() ) + 1 ];
      //strcpy( name[ii], array->GetName() );
      name.push_back( std::string( array->GetName() ));
      ii++;
   }
   return name;
}

void cfdDataSet::UpdatePropertiesForNewMesh()
{
   vprDEBUG(vesDBG,1) << "|\tthis->GetDataSet = " << this->GetDataSet()
                           << std::endl << vprDEBUG_FLUSH;
   // Get the length of the diagonal of the bounding box. 
   // THIS METHOD IS THREAD SAFE IF FIRST CALLED FROM A SINGLE THREAD
   // AND THE DATASET IS NOT MODIFIED
   //This is expensive so has been removed
   //This is needed for streamline calcs
   this->bbDiagonal = this->GetDataSet()->GetLength();
   vprDEBUG(vesDBG,1) << "\tthis->bbDiagonal = " << this->bbDiagonal
                          << std::endl << vprDEBUG_FLUSH;

   // Read or compute the length of the diagonal of the bounding box
   // of the average cell. 
   // First set it to zero as the default:
   this->meanCellBBLength = 0.0;

   // Read the dataset fields and see if any match with member variable names
   vtkFieldData * field = this->GetDataSet()->GetFieldData();
   int numFieldArrays = field->GetNumberOfArrays();
   vprDEBUG(vesDBG,1) << " numFieldArrays = " << numFieldArrays
                          << std::endl << vprDEBUG_FLUSH;
   for ( int i = 0; i < numFieldArrays; i++ )
   {
      // print some debug information
      vprDEBUG(vesDBG,0) << " Reading field \""
           << field->GetArray( i )->GetName()
           << "\"" << std::endl << vprDEBUG_FLUSH;
      vprDEBUG(vesDBG,1) << "\tNumComponents = "
           << field->GetArray( i )->GetNumberOfComponents()
           << std::endl << vprDEBUG_FLUSH;
      vprDEBUG(vesDBG,1) << "\tNumTuples= "
           << field->GetArray( i )->GetNumberOfTuples()
           << std::endl << vprDEBUG_FLUSH;
      vprDEBUG(vesDBG,1) << "\tfirst value = "
           << field->GetArray( i )->GetComponent( 0, 0 )
           << std::endl << vprDEBUG_FLUSH;
      if ( field->GetArray( i )->GetNumberOfTuples() > 1 )
      {
         vprDEBUG(vesDBG,1) << "\tsecond value = "
              << field->GetArray( i )->GetComponent( 0, 1 )
              << std::endl << vprDEBUG_FLUSH;
      }

      // If any field names match with member variable names, use them:
      if ( !strcmp(field->GetArray( i )->GetName(),"meanCellBBLength") )
      {
         this->meanCellBBLength = field->GetArray( i )->GetComponent( 0, 0 );
      }
   }

   // If not provided in the dataset field, compute :
   if ( this->meanCellBBLength == 0.0 )
   {
      std::cout<<"[DBG]...meanCellBBLength = 0.0"<<std::endl;
      this->meanCellBBLength = cfdAccessoryFunctions::
                               ComputeMeanCellBBLength( this->GetDataSet() );
   }
   vprDEBUG(vesDBG,0) << "\tmeanCellBBLength = " << this->meanCellBBLength
                          << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::SetActiveScalar( std::string tempActiveScalar )
{
   if ( tempActiveScalar.empty() )
   {
      this->activeScalar = -1;
      return;
   }
   
   int scalar = -1;
   for ( int i = 0; i <  numScalars; ++i )
   {
      if ( scalarName[ i ] == tempActiveScalar )
      {   
         scalar = i;
         break;
      }
   }
   
   if ( scalar < 0 || this->numScalars <= scalar )
   {
      std::cerr << "Error: SetActiveScalar: out-of-range scalar " 
      << scalar << ", will use first scalar " << this->scalarName[ 0 ]
      << std::endl;
      this->activeScalar = 0;
   }
   else
   {
      this->activeScalar = scalar;
      
      vprDEBUG(vesDBG,1) 
         << "cfdDataSet::SetActiveScalar: requested activeScalar = "
         << this->activeScalar << ", scalarName = " 
         << this->scalarName[ this->activeScalar ]
         << std::endl << vprDEBUG_FLUSH;
   }
   
   this->GetDataSet()->GetPointData()->SetActiveScalars( tempActiveScalar.c_str() );
   
   vprDEBUG(vesDBG,1) << "\tSetActiveScalar: Active scalar is \""
      << this->GetDataSet()->GetPointData()->GetScalars()->GetName()
      << "\"" << std::endl << vprDEBUG_FLUSH;
   
   for (int i=0; i<3; i++)
   {
      int numPlanes = 0;
      if ( this->GetPrecomputedSlices( i ) )
      {
         numPlanes = this->GetPrecomputedSlices( i )->GetNumberOfPlanes();
      }
      vprDEBUG(vesDBG,1) << "\tnumPlanes = " << numPlanes
         << std::endl << vprDEBUG_FLUSH;
      
      if ( numPlanes > 0 )
      {
         
         /*this->GetPrecomputedSlices( i )->GetPlanesData()
         ->GetPointData()->SetActiveScalars( 
                                             this->scalarName[ this->activeScalar ].c_str() );
         */
         
         for (int j=0; j<numPlanes; j++)
         {
            this->GetPrecomputedSlices( i )->GetPlane( j )
            ->GetPointData()->SetActiveScalars( 
                                                this->scalarName[ this->activeScalar ].c_str() );
         }
      }
   }
   // Store the actual range of the active scalar...
   double * temp = this->GetActualScalarRange( this->activeScalar );
   this->range [ 0 ] = temp[ 0 ];
   this->range [ 1 ] = temp[ 1 ];
   vprDEBUG(vesDBG,1) << "range[0] = " << this->range[0]
      << ", range[1] = " << this->range[1]
      << std::endl << vprDEBUG_FLUSH;
   
   temp = this->GetDisplayedScalarRange( this->activeScalar );
   this->definedRange[ 0 ] = temp[ 0 ];
   this->definedRange[ 1 ] = temp[ 1 ];
   vprDEBUG(vesDBG,1) << "definedRange[0] = " << this->definedRange[0]
      << ", definedRange[1] = " << this->definedRange[1]
      << std::endl << vprDEBUG_FLUSH;
   
   vprDEBUG(vesDBG,1) << "actualScalarRange[0][0] = " 
      << this->actualScalarRange[0][0]
      << ", actualScalarRange[0][1] = " 
      << this->actualScalarRange[0][1]
      << std::endl << vprDEBUG_FLUSH;
   
   vprDEBUG(vesDBG,1) << "displayedScalarRange[0][0] = " 
      << this->displayedScalarRange[0][0]
      << ", displayedScalarRange[0][1] = "
      << this->displayedScalarRange[0][1]
      << std::endl << vprDEBUG_FLUSH;
   
   // Step length for streamline integration
   this->stepLength = this->bbDiagonal/5.0f ;
   vprDEBUG(vesDBG,1) << "\tSetActiveScalar: stepLength = " 
      << this->stepLength << std::endl << vprDEBUG_FLUSH;
   
   // Maximum integration time for streamline integration
   this->maxTime = 5.0f * this->bbDiagonal / 
      ( ( this->range[1] - this->range[0] ) * 0.5f );
   vprDEBUG(vesDBG,1) << "\tSetActiveScalar: maxTime = " 
      << this->maxTime << std::endl << vprDEBUG_FLUSH;
   
   // Time step for streamline integration
   this->timeStep = this->bbDiagonal / this->definedRange[1];
   vprDEBUG(vesDBG,1) << "\tSetActiveScalar: timeStep = " 
      << this->timeStep << std::endl << vprDEBUG_FLUSH;
   
   // set up the vtkLookupTable
   this->lut->SetNumberOfColors( 256 );            //default is 256
   this->lut->SetHueRange( 2.0f/3.0f, 0.0f );      //a blue-to-red scale
   this->lut->SetTableRange( this->definedRange );
   this->lut->Build();
}
////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::SetActiveScalar( int scalar )
{
   if ( this->numScalars == 0 )
   {
      this->activeScalar = -1;
      return;
   }

   if ( scalar < 0 || this->numScalars <= scalar )
   {
      std::cerr << "Error: SetActiveScalar: out-of-range scalar " 
         << scalar << ", will use first scalar " << this->scalarName[ 0 ]
         << std::endl;
      this->activeScalar = 0;
   }
   else
   {
      this->activeScalar = scalar;

      vprDEBUG(vesDBG,1) 
         << "cfdDataSet::SetActiveScalar: requested activeScalar = "
         << this->activeScalar << ", scalarName = " 
         << this->scalarName[ this->activeScalar ]
         << std::endl << vprDEBUG_FLUSH;
   }

   this->GetDataSet()->GetPointData()->SetActiveScalars( 
                                     this->scalarName[ this->activeScalar ].c_str() );

   vprDEBUG(vesDBG,1) << "\tSetActiveScalar: Active scalar is \""
        << this->GetDataSet()->GetPointData()->GetScalars()->GetName()
        << "\"" << std::endl << vprDEBUG_FLUSH;

   for (int i=0; i<3; i++)
   {
      int numPlanes = 0;
      if ( this->GetPrecomputedSlices( i ) )
      {
         numPlanes = this->GetPrecomputedSlices( i )->GetNumberOfPlanes();
      }
      vprDEBUG(vesDBG,1) << "\tnumPlanes = " << numPlanes
           << std::endl << vprDEBUG_FLUSH;

      if ( numPlanes > 0 )
      {

         /*this->GetPrecomputedSlices( i )->GetPlanesData()
             ->GetPointData()->SetActiveScalars( 
                                 this->scalarName[ this->activeScalar ].c_str() );
         */

         for (int j=0; j<numPlanes; j++)
         {
            this->GetPrecomputedSlices( i )->GetPlane( j )
                ->GetPointData()->SetActiveScalars( 
                                 this->scalarName[ this->activeScalar ].c_str() );
         }
      }
   }
   // Store the actual range of the active scalar...
   double * temp = this->GetActualScalarRange( this->activeScalar );
   this->range [ 0 ] = temp[ 0 ];
   this->range [ 1 ] = temp[ 1 ];
   vprDEBUG(vesDBG,1) << "range[0] = " << this->range[0]
                          << ", range[1] = " << this->range[1]
                          << std::endl << vprDEBUG_FLUSH;

   temp = this->GetDisplayedScalarRange( this->activeScalar );
   this->definedRange[ 0 ] = temp[ 0 ];
   this->definedRange[ 1 ] = temp[ 1 ];
   vprDEBUG(vesDBG,1) << "definedRange[0] = " << this->definedRange[0]
                          << ", definedRange[1] = " << this->definedRange[1]
                          << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vesDBG,1) << "actualScalarRange[0][0] = " 
                          << this->actualScalarRange[0][0]
                          << ", actualScalarRange[0][1] = " 
                          << this->actualScalarRange[0][1]
                          << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vesDBG,1) << "displayedScalarRange[0][0] = " 
                          << this->displayedScalarRange[0][0]
                          << ", displayedScalarRange[0][1] = "
                          << this->displayedScalarRange[0][1]
                          << std::endl << vprDEBUG_FLUSH;

   // Step length for streamline integration
   this->stepLength = this->bbDiagonal/5.0f ;
   vprDEBUG(vesDBG,1) << "\tSetActiveScalar: stepLength = " 
      << this->stepLength << std::endl << vprDEBUG_FLUSH;

   // Maximum integration time for streamline integration
   this->maxTime = 5.0f * this->bbDiagonal / 
                              ( ( this->range[1] - this->range[0] ) * 0.5f );
   vprDEBUG(vesDBG,1) << "\tSetActiveScalar: maxTime = " 
      << this->maxTime << std::endl << vprDEBUG_FLUSH;

   // Time step for streamline integration
   this->timeStep = this->bbDiagonal / this->definedRange[1];
   vprDEBUG(vesDBG,1) << "\tSetActiveScalar: timeStep = " 
      << this->timeStep << std::endl << vprDEBUG_FLUSH;

   // set up the vtkLookupTable
   this->lut->SetNumberOfColors( 256 );            //default is 256
   this->lut->SetHueRange( 2.0f/3.0f, 0.0f );      //a blue-to-red scale
   this->lut->SetTableRange( this->definedRange );
   this->lut->Build();
}
////////////////////////////////////////////////////////////////////////////////
int cfdDataSet::GetActiveScalar()
{
   // 0 <= activeScalar < numScalars
   return this->activeScalar;
}
////////////////////////////////////////////////////////////////////////////////
double * cfdDataSet::GetVectorMagRange()
{
   return this->vectorMagRange;
}
////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::SetActiveVector( int vector )
{
   if ( this->numVectors == 0 )
   {
      this->activeVector = -1;
      return;
   }

   if ( vector < 0 || this->numVectors <= vector )
   {
      std::cerr << "Error: SetActiveVector: out-of-range vector " 
         << vector << ", will use first vector " << this->vectorName[ 0 ]
         << std::endl;
      this->activeVector = 0;
   }
   else
   {
      this->activeVector = vector;

      vprDEBUG(vesDBG,1) 
         << "cfdDataSet::SetActiveVector: requested activeVector = "
         << this->activeVector << ", vectorName= " 
         << this->vectorName[ this->activeVector ]
         << std::endl << vprDEBUG_FLUSH;
   }

   this->GetDataSet()->GetPointData()->SetActiveVectors( 
                                  this->vectorName[ this->activeVector ].c_str() );

   for (int i=0; i<3; i++)
   {
      int numPlanes = 0;
      if ( this->GetPrecomputedSlices( i ) )
      {
         numPlanes = this->GetPrecomputedSlices( i )->GetNumberOfPlanes();
      }
      vprDEBUG(vesDBG,1) << "\tnumPlanes = " << numPlanes
                             << std::endl << vprDEBUG_FLUSH;

      if ( numPlanes > 0 )
      {
         /*this->GetPrecomputedSlices( i )->GetPlanesData()
             ->GetPointData()->SetActiveVectors( 
                                 this->vectorName[ this->activeVector ].c_str() );
         */
         for (int j=0; j<numPlanes; j++)
         {
            this->GetPrecomputedSlices( i )->GetPlane( j )
                ->GetPointData()->SetActiveVectors( 
                                 this->vectorName[ this->activeVector ].c_str() );
         }
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::SetActiveVector( std::string tempVectorName )
{
   if ( tempVectorName.empty() )
   {
      this->activeVector = -1;
      return;
   }
   
   int vector = -1;
   for ( int i = 0; i < numVectors; ++i )
   {
      if ( vectorName[ i ] == tempVectorName )
      {   
         vector = i;
         break;
      }
   }
   
   if ( vector < 0 || this->numVectors <= vector )
   {
      std::cerr << "Error: SetActiveVector: out-of-range vector " 
      << vector << ", will use first vector " << this->vectorName[ 0 ]
      << std::endl;
      this->activeVector = 0;
   }
   else
   {
      this->activeVector = vector;
      
      vprDEBUG(vesDBG,1) 
         << "cfdDataSet::SetActiveVector: requested activeVector = "
         << this->activeVector << ", vectorName= " 
         << this->vectorName[ this->activeVector ]
         << std::endl << vprDEBUG_FLUSH;
   }
   
   this->GetDataSet()->GetPointData()->SetActiveVectors( tempVectorName.c_str() );
   
   for (int i=0; i<3; i++)
   {
      int numPlanes = 0;
      if ( this->GetPrecomputedSlices( i ) )
      {
         numPlanes = this->GetPrecomputedSlices( i )->GetNumberOfPlanes();
      }
      vprDEBUG(vesDBG,1) << "\tnumPlanes = " << numPlanes
         << std::endl << vprDEBUG_FLUSH;
      
      if ( numPlanes > 0 )
      {
         /*this->GetPrecomputedSlices( i )->GetPlanesData()
         ->GetPointData()->SetActiveVectors( 
                                             this->vectorName[ this->activeVector ].c_str() );
         */
         for (int j=0; j<numPlanes; j++)
         {
            this->GetPrecomputedSlices( i )->GetPlane( j )
            ->GetPointData()->SetActiveVectors( 
                                                this->vectorName[ this->activeVector ].c_str() );
         }
      }
   }
}

int cfdDataSet::GetActiveVector()
{
   return this->activeVector;
}

void cfdDataSet::AutoComputeUserRange( const double rawRange[2],
                                       double prettyRange[2] )
{
   double highMinusLow = rawRange[1] - rawRange[0];
   vprDEBUG(vesDBG,1) << " highMinusLow = " << highMinusLow
      << std::endl << vprDEBUG_FLUSH;

   // if all scalar data is the same, then lower bound = upper bound. 
   // Fix for this case...
   if ( highMinusLow < 1e-15 )
   {
      prettyRange[ 0 ] = rawRange[ 0 ] - 2.0 * 0.1 * rawRange[ 0 ];
      prettyRange[ 1 ] = rawRange[ 0 ] + 2.0 * 0.1 * rawRange[ 0 ];
   }
   else
   {
      prettyRange[ 0 ] = rawRange[ 0 ];
      prettyRange[ 1 ] = rawRange[ 1 ];
   }
   vprDEBUG(vesDBG,1) << " prettyRange: "
      << prettyRange[ 0 ] << " : " << prettyRange[ 1 ]
      << std::endl << vprDEBUG_FLUSH;

/*
   int order = 0;
   // This code created a range that fully contained the input range.
   // Additionally, the new range is divisble by 4 so that the text labels
   // tend to be nice numbers rather than irrational numbers.
   // It is commented out because the space between the two ranges could 
   // be almost 20%, causing the end color ranges to be useless.

   int intHighMinusLow = (int)highMinusLow;
   vprDEBUG(vesDBG,1) << "intHighMinusLow = " << intHighMinusLow
      << std::endl << vprDEBUG_FLUSH;

   //compute power factors to get rawRange spread into ball park of 100..
   do 
   {
      if ( intHighMinusLow >= 16 && intHighMinusLow <= 160 ) break;
      else if ( intHighMinusLow > 160 )
      {
         order++;
         intHighMinusLow  = (int)( highMinusLow / pow(10.0,order) );
      }
      else if ( intHighMinusLow < 16 )
      {
         order--;
         intHighMinusLow  = (int)( highMinusLow / pow(10.0,order) );
      }
   } while (1);
   vprDEBUG(vesDBG,1) << "order = " << order << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vesDBG,1) << "intHighMinusLow = " << intHighMinusLow
      << std::endl << vprDEBUG_FLUSH;

   // round up the normalized rawRange spread so that it is divisible by 4
   // by default scalar bar will show 5 labels -- thus 4 steps between
   // low label and high label
   do 
   {
      if ( intHighMinusLow % 4 == 0 ) break;
      intHighMinusLow++;
   } while (1);

   // use the computed power factors to round down to a nice number
   vprDEBUG(vesDBG,1) << "floor = "
      << floor(rawRange[0] / pow(10.0,order)) * pow(10.0,order)
      << std::endl << vprDEBUG_FLUSH;
   prettyRange[0] = floor(rawRange[0] / pow(10.0,order)) * pow(10.0,order);

   // incorporate the normalized rawRange spread to get the upper limit
   // -- but make sure it includes the upper limit!
   prettyRange[1] = prettyRange[0] + intHighMinusLow * pow(10.0,order);
   if ( prettyRange[1] < rawRange[1] )
      prettyRange[1] = prettyRange[0] + (intHighMinusLow+4) * pow(10.0,order);
*/
}
////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::ResetScalarBarRange( double min, double max )
{
   // converts percentile parameters into decimal values for a particular scalar
   vprDEBUG(vesDBG,1) << "cfdDataSet::ResetScalarBarRange "
                          << "min = " << min << ", max = " << max
                          << std::endl << vprDEBUG_FLUSH;

   if ( this->numScalars == 0 )
   {
      return;
   }

   //Get the actual scalar range for the active scalar
   double rawRange[2];
   this->GetParent()->GetRange( rawRange );

   double newRawRange[2];
   newRawRange[0] = min;//rawRange[0] + (rawRange[1]-rawRange[0])*(double)min/100.;
   newRawRange[1] = max;//rawRange[0] + (rawRange[1]-rawRange[0])*(double)max/100.;

   double newPrettyRange[2];
   this->AutoComputeUserRange( newRawRange, newPrettyRange );
   vprDEBUG(vesDBG,1) << "newPrettyRange[0] = " << newPrettyRange[0]
                          << ", newPrettyRange[1] = " << newPrettyRange[1]
                          << std::endl << vprDEBUG_FLUSH;

   this->SetUserRange( newPrettyRange );

   // Update vtkLookupTable
   this->lut->SetTableRange( this->GetUserRange() );
   this->lut->Build();
}

void cfdDataSet::SetFileName( const std::string newName )
{
   if ( this->fileName.c_str() )
   {
      fileName.erase();//delete [] this->fileName;
   }

   //this->fileName = new char [strlen(newName)+1];  
   fileName.assign( newName );//strcpy( this->fileName, newName );
}

void cfdDataSet::SetFileName_OnFly(int datasetindex)
{
   std :: ostringstream file_name;
   std :: string currentVtkFileName = "NewlyLoadedDataSet_000.vtk";
   file_name<<"NewlyLoadedDataSet_"<<datasetindex<<".vtk";
   currentVtkFileName = file_name.str();
   
   //const std::string newName;
   std::string newName;
   newName.assign(currentVtkFileName);

   this->SetFileName ( newName.c_str() );
}

/*char* cfdDataSet::GetFileName()
{
  return this->fileName.c_str();
}*/

std::string cfdDataSet::GetFileName()
{
  return this->fileName;
}

void cfdDataSet::SetPrecomputedDataSliceDir( const std::string newName )
{
   if ( this->precomputedDataSliceDir.c_str() )
   {
      precomputedDataSliceDir.erase();//delete [] this->precomputedDataSliceDir;
   }

   //if ( newName == NULL )
   if ( newName.empty() )
   {
      precomputedDataSliceDir.empty();//this->precomputedDataSliceDir = NULL;
      return;
   }

   //this->precomputedDataSliceDir = new char [strlen(newName)+1];  
   precomputedDataSliceDir.assign( newName );//strcpy( this->precomputedDataSliceDir, newName );
}

std::string cfdDataSet::GetPrecomputedDataSliceDir()
{
   return this->precomputedDataSliceDir;
}

void cfdDataSet::SetPrecomputedSurfaceDir( const std::string newName )
{
   if ( this->precomputedSurfaceDir.c_str() )
   {
      precomputedSurfaceDir.erase();//delete [] this->precomputedSurfaceDir;
   }

   //if ( newName == NULL )
   if ( newName.empty() )
   {
      this->precomputedSurfaceDir.empty();// = NULL;
      return;
   }

   //this->precomputedSurfaceDir = new char [strlen(newName)+1];  
   precomputedSurfaceDir.assign( newName );//strcpy( this->precomputedSurfaceDir, newName );
}

std::string cfdDataSet::GetPrecomputedSurfaceDir()
{
   return this->precomputedSurfaceDir;
}

cfdPlanes * cfdDataSet::GetPrecomputedXSlices()
{
   return this->x_planes;
}

cfdPlanes * cfdDataSet::GetPrecomputedYSlices()
{
   return this->y_planes;
}

cfdPlanes * cfdDataSet::GetPrecomputedZSlices()
{
   return this->z_planes;
}

cfdPlanes * cfdDataSet::GetPrecomputedSlices( int xyz )
{
   if      ( xyz == 0 ) return this->x_planes;
   else if ( xyz == 1 ) return this->y_planes;
   else if ( xyz == 2 ) return this->z_planes;
   else
   {
      std::cerr << "ERROR: cfdDataSet::GetPrecomputedSlices cannot " 
                << "handle index " << xyz << std::endl;
      exit(1);
      return NULL; // to eliminate compile warning
   }
}

void cfdDataSet::SetArrow( vtkPolyData * arrow)
{
   this->arrow = arrow;
}

vtkPolyData * cfdDataSet::GetArrow( )
{
   return this->arrow;
} 

int cfdDataSet::GetNumberOfScalars()
{
   return this->numScalars;
}

std::string cfdDataSet::GetScalarName( int i )
{
   if ( 0 <= i && i < this->numScalars )
   {
      return this->scalarName[ i ];
   }
   else
   {
      std::cerr << "ERROR: cfdDataSet::GetScalarName cannot "
                << "handle index " << i << std::endl;
      return NULL;
   }
}

int cfdDataSet::GetNumberOfVectors()
{
   return this->numVectors;
}

std::string cfdDataSet::GetVectorName( int i )
{
   if ( 0 <= i && i < this->numVectors )
   {
      return this->vectorName[ i ];
   }
   else
   {
      std::cerr << "ERROR: cfdDataSet::GetScalarName cannot "
                << "handle index " << i << std::endl;
      return NULL;
   }
}

void cfdDataSet::SetNewlyActivated()
{
   this->isNewlyActivated = 1;
}

void cfdDataSet::SetNotNewlyActivated()
{
   this->isNewlyActivated = 0;
}

int cfdDataSet::IsNewlyActivated()
{
   return this->isNewlyActivated;
}

cfdDataSet * cfdDataSet::GetParent()
{
   return this->parent;
}

void cfdDataSet::SetParent( cfdDataSet * myParent )
{
   this->parent = myParent;
}

double * cfdDataSet::GetActualScalarRange( int index )
{
   return this->actualScalarRange[ index ];
}

void cfdDataSet::GetActualScalarRange( int index, double * range )
{
   range[ 0 ] = this->actualScalarRange[ index ][ 0 ];
   range[ 1 ] = this->actualScalarRange[ index ][ 1 ];
}

void cfdDataSet::SetActualScalarRange( int index, double * range )
{
   vprDEBUG(vesDBG,2) 
      << "cfdDataSet::SetActualScalarRange, for file " << this->fileName 
      << ", index: " << index
      << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vesDBG,2) 
      << "cfdDataSet::SetActualScalarRange OLD actualScalarRange[" 
      << index << "] = " 
      << this->actualScalarRange[ index ][ 0 ] << " : " 
      << this->actualScalarRange[ index ][ 1 ]
      << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vesDBG,2) << "cfdDataSet::SetActualScalarRange request range: "
      << range[0] << " : " << range[1]
      << std::endl << vprDEBUG_FLUSH;

   this->actualScalarRange[ index ][ 0 ] = range[ 0 ];
   this->actualScalarRange[ index ][ 1 ] = range[ 1 ];

   vprDEBUG(vesDBG,1) 
      << "cfdDataSet::SetActualScalarRange NEW actualScalarRange[" 
      << index << "] = " 
      << this->actualScalarRange[ index ][ 0 ] << " : " 
      << this->actualScalarRange[ index ][ 1 ]
      << std::endl << vprDEBUG_FLUSH;
}

// returns displayed range of active scalar
double * cfdDataSet::GetDisplayedScalarRange()
{
   vprDEBUG(vesDBG,1) << "cfdDataSet::GetDisplayedScalarRange"
      << " activeScalar = " << this->activeScalar
      << std::endl << vprDEBUG_FLUSH;
   return this->displayedScalarRange[ this->activeScalar ];
}

// get/set displayed range of any scalar
double * cfdDataSet::GetDisplayedScalarRange( int index )
{
   return this->displayedScalarRange[ index ];
}

void cfdDataSet::SetDisplayedScalarRange( int index, double * range )
{
   this->displayedScalarRange[ index ][ 0 ] = range[ 0 ];
   this->displayedScalarRange[ index ][ 1 ] = range[ 1 ];
   //this->definedRange[ 0 ] = range[ 0 ];
   //this->definedRange[ 1 ] = range[ 1 ];
}

VE_SceneGraph::Switch* cfdDataSet::GetSwitchNode()
{
   if ( !switchNode )
   {
      switchNode = new VE_SceneGraph::Switch();
   }

   return switchNode.get();
}

// get/set this dataset's DCS
VE_SceneGraph::DCS* cfdDataSet::GetDCS()
{
   if ( dcs == NULL )
   {
      dcs = new VE_SceneGraph::DCS();
      return this->dcs.get();
   }
   else
      return this->dcs.get();
}

void cfdDataSet::SetDCS( VE_SceneGraph::DCS* myDCS )
{
   if ( dcs == NULL )
      this->dcs = myDCS;
   else
      std::cerr << " ERROR: DCS is already set for this dataset " << std::endl;
}

/*
VE_SceneGraph::cfdTempAnimation* cfdDataSet::GetAnimation( void )
{
   return this->animation;
}

void cfdDataSet::SetAnimation( VE_SceneGraph::cfdTempAnimation* input )
{
   this->animation = input;
}
*/

int cfdDataSet::IsPartOfTransientSeries()
{
   return this->partOfTransientSeries;
}

void cfdDataSet::SetAsPartOfTransientSeries()
{
   this->partOfTransientSeries = 1;
}

void cfdDataSet::StoreScalarInfo()
{
   // called once to initialize the scalar names and ranges of a data set
   this->numScalars = CountNumberOfParameters( 1 );
   vprDEBUG(vesDBG,1) << "\tStoreScalarInfo: numScalars = " 
                          << this->numScalars
                          << std::endl << vprDEBUG_FLUSH;

   if ( this->numScalars )
   {
      this->scalarName = GetParameterNames( 1, this->numScalars );
      this->actualScalarRange = new double * [ this->numScalars ];
      this->displayedScalarRange = new double * [ this->numScalars ];
      for ( int i=0; i<this->numScalars; i++ )
      {
         this->actualScalarRange[ i ]    = new double [ 2 ];
         this->displayedScalarRange[ i ] = new double [ 2 ];
      }

      // store actual range...
      int ii = 0;
      for ( int i=0; i < this->numPtDataArrays; i++ )
      {
         if (this->GetDataSet()->GetPointData()->GetArray( i )
                               ->GetNumberOfComponents() != 1 )
         {
            continue;
         }

         this->GetDataSet()->GetPointData()->GetArray( i )
             ->GetRange( this->actualScalarRange[ ii ] );

         vprDEBUG(vesDBG,1) << "\tarray(" << i << "), scalarName[" 
            << ii << "] = \"" << this->scalarName[ ii ] 
            << "\", actualScalarRange = "
            << this->actualScalarRange[ ii ][ 0 ] << " : "
            << this->actualScalarRange[ ii ][ 1 ]
            << std::endl << vprDEBUG_FLUSH;

         this->AutoComputeUserRange( 
                              this->GetParent()->GetActualScalarRange( ii ),
                              this->displayedScalarRange[ ii ] );

         vprDEBUG(vesDBG,1) << "|\tarray(" << i << "), scalarName[" 
            << ii << "] = \"" << this->scalarName[ ii ] 
            << "\", displayedScalarRange = "
            << this->displayedScalarRange[ ii ][ 0 ] << " : "
            << this->displayedScalarRange[ ii ][ 1 ]
            << std::endl << vprDEBUG_FLUSH;

         ii++;
      }
   }
}

void cfdDataSet::Print()
{
   std::cout << "filename = " << this->fileName << std::endl;

/*
   std::cout << "numPtDataArrays = " << this->numPtDataArrays << std::endl;
   for ( int i=0; i < this->numPtDataArrays; i++ )
   {
      vtkDataArray *array_i = this->GetDataSet()->GetPointData()->GetArray( i );

      if ( array_i->GetNumberOfComponents() == 3 )
      {
         std::cout << "array " << i << " (vector): " << array_i->GetName() << std::endl;
         continue; 
      }
      else if ( array_i->GetNumberOfComponents() != 1 ) 
         continue;

      double minMax[2];
      array_i->GetRange( minMax );
      std::cout << "array " << i << " (scalar): " << array_i->GetName() 
         << "\t"  << minMax[ 0 ] << "\t" << minMax[ 1 ] << std::endl;
   }
*/
   std::cout << "numScalars = " << this->numScalars << std::endl;
   for ( int i=0; i < this->numScalars; i++ )
   {
       std::cout << "\tscalarName[" << i << "] = \"" << this->scalarName[ i ] 
         << "\"\tactualScalarRange = "
         << this->actualScalarRange[ i ][ 0 ] << " : "
         << this->actualScalarRange[ i ][ 1 ]
         << ", displayedScalarRange = "
         << this->displayedScalarRange[ i ][ 0 ] << " : "
         << this->displayedScalarRange[ i ][ 1 ]
         << std::endl;
   }

   std::cout << "numVectors = " << this->numVectors << std::endl;
   for ( int i=0; i < this->numVectors; i++ )
   {
       std::cout << "\tvectorName[" << i << "] = \"" << this->vectorName[ i ] 
         << "\"\tvectorMagRange = "
         << this->vectorMagRange[ 0 ] << " : "
         << this->vectorMagRange[ 1 ]
         << std::endl;
   }
}
////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::SetUUID( std::string attribute, std::string uuid )
{
   dataSetUUIDMap[ attribute ] = uuid;
}
////////////////////////////////////////////////////////////////////////////////
std::string cfdDataSet::GetUUID( std::string attribute )
{
   std::map< std::string, std::string >::iterator iter;
   iter = dataSetUUIDMap.find( attribute );
   if ( iter == dataSetUUIDMap.end() )
   {
      return 0;
   }
   else
   {
      return iter->second;
   }
}
////////////////////////////////////////////////////////////////////////////////
//create visual representation of bounding box
////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::CreateBoundingBoxGeode( void )
{
   vtkOutlineFilter* outlineData = vtkOutlineFilter::New();
   outlineData->SetInput( this->GetDataSet() );
   
   vtkPolyDataMapper* mapOutline = vtkPolyDataMapper::New();
   mapOutline->SetInput( outlineData->GetOutput() );
   
   vtkActor* outline = vtkActor::New();
   outline->SetMapper( mapOutline );
   outline->GetProperty()->SetColor(1,0,0);
   
	bboxGeode = new VE_SceneGraph::Geode();
   bboxGeode->TranslateToGeode( outline );
   
   outlineData->Delete();
   mapOutline->Delete();
   outline->Delete();
}
////////////////////////////////////////////////////////////////////////////////
//create wireframe to ensure accurate representation
////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::CreateWireframeGeode( void )
{
   //vtkGeometryFilter* wireframe = vtkGeometryFilter::New();
   //wireframe->SetInput( this->GetDataSet() );
   //wireframe->Update();

   vtkPolyDataMapper *wireframeMapper = vtkPolyDataMapper::New();
   //wireframeMapper->SetInput(wireframe->GetOutput());
   vtkPolyData* poly = VE_Util::cfdGrid2Surface( this->GetDataSet(), 0.8f );
   wireframeMapper->SetInput( poly );
   
   vtkActor *wireframeActor = vtkActor::New();
   wireframeActor->SetMapper(wireframeMapper);
   wireframeActor->GetProperty()->SetColor(0,0,1);
   wireframeActor->GetProperty()->SetOpacity(0.7f);
   wireframeActor->GetProperty()->SetRepresentationToWireframe();

	wireframeGeode = new VE_SceneGraph::Geode();
   wireframeGeode->TranslateToGeode( wireframeActor );
   
   //wireframe->Delete();
   poly->Delete();
   wireframeMapper->Delete();
   wireframeActor->Delete();
}
////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::SetBoundingBoxState( unsigned int state )
{
   if ( (state == 0) && bboxGeode.valid() )
   {
      GetDCS()->RemoveChild( bboxGeode.get() );
   }
   else if ( state == 1 )
   {
      if ( bboxGeode == 0 )
      {
         CreateBoundingBoxGeode();
      }
      GetDCS()->RemoveChild( bboxGeode.get() );
      GetDCS()->AddChild( bboxGeode.get() );
   }
}
////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::SetWireframeState( unsigned int state )
{
   if ( (state == 0) && wireframeGeode.valid() )
   {
      GetDCS()->RemoveChild( wireframeGeode.get() );
   }
   else if ( state == 1 )
   {
      if ( wireframeGeode == 0 )
      {
         CreateWireframeGeode();
      }
      GetDCS()->RemoveChild( wireframeGeode.get() );
      GetDCS()->AddChild( wireframeGeode.get() );
   }
}
////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::SetAxesState( unsigned int state )
{
   if ( (state == 0) && dataSetAxes )
   {
      GetDCS()->RemoveChild( dataSetAxes->GetAxis() );
   }
   else if ( state == 1 )
   {
      if ( dataSetAxes == 0 )
      {
         dataSetAxes = new DataSetAxis();
         dataSetAxes->SetBoundingBox( GetDataSet()->GetBounds() );
      }
      GetDCS()->RemoveChild( dataSetAxes->GetAxis() );
      dataSetAxes->CreateAxis();
      GetDCS()->AddChild( dataSetAxes->GetAxis() );
   }
}
////////////////////////////////////////////////////////////////////////////////
VE_Xplorer::DataSetAxis* cfdDataSet::GetDataSetAxes( void )
{
   return dataSetAxes;
}
////////////////////////////////////////////////////////////////////////////////
VE_Xplorer::DataSetScalarBar* cfdDataSet::GetDataSetScalarBar( void )
{
   return dataSetScalarBar;
}
////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::SetDataSetScalarState( unsigned int state )
{
   if ( (state == 0) && dataSetScalarBar )
   {
      GetDCS()->RemoveChild( dataSetScalarBar->GetScalarBar() );
   }
   else if ( state == 1 )
   {
      if ( dataSetScalarBar == 0 )
      {
         dataSetScalarBar = new DataSetScalarBar();
         dataSetScalarBar->SetBoundingBox( GetDataSet()->GetBounds() );
      }
      GetDCS()->RemoveChild( dataSetScalarBar->GetScalarBar() );
      dataSetScalarBar->AddScalarBarToGroup();
      GetDCS()->AddChild( dataSetScalarBar->GetScalarBar() );
   }
}
