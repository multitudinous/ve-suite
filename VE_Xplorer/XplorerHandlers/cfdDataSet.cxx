/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#include "VE_Xplorer/Utilities/ComputeVectorMagnitudeRangeCallback.h"
#include "VE_Xplorer/Utilities/ComputeDataObjectBoundsCallback.h"
#include "VE_Xplorer/Utilities/CountNumberOfParametersCallback.h"
#include "VE_Xplorer/Utilities/GetNumberOfPointsCallback.h"
#include "VE_Xplorer/Utilities/ProcessScalarRangeCallback.h"
#include "VE_Xplorer/Utilities/ActiveDataInformationCallback.h"
#include "VE_Xplorer/Utilities/CreateDataObjectBBoxActorsCallback.h"
#include "VE_Builder/Translator/DataLoader/DataLoader.h"

#include <vtkLookupTable.h>
#include <vtkPointData.h>
#include <vtkDataSet.h>
#include <vtkDataObject.h>
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

cfdDataSet::cfdDataSet( ) :
    parent( this ),
    dataSet( 0 ),
    x_planes( 0 ),
    y_planes( 0 ),
    z_planes( 0 ),
    activeScalar( -1 ),
    activeVector( -1 ),
    numScalars( 0 ),
    numVectors( 0 ),
    partOfTransientSeries( 0 ),
    datasetType( -1 ),
    vectorMagRange( 0 ),
    actualScalarRange( 0 ),
    displayedScalarRange( 0 ),
    meanCellBBLength( 0.0 ),
    dataSetAxes( 0 ),
    dataSetScalarBar( 0 ),
    _vtkFHndlr( 0 ),
    m_externalFileLoader( 0 ),
	m_dataObjectHandler( 0 )
{
   this->lut = vtkLookupTable::New();
   this->arrow = NULL;
   this->scalarName.empty();// = NULL;
   this->vectorName.empty();// = NULL;
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

   m_bounds[0] = 100000;
   m_bounds[1] = -100000;
   m_bounds[2] = 100000;
   m_bounds[3] = -100000;
   m_bounds[4] = 100000;
   m_bounds[5] = -100000;

   m_dataObjectOps["Compute Bounds"] = new VE_Util::ComputeDataObjectBoundsCallback();
   m_dataObjectOps["Compute Vector Magnitude Range"] = new VE_Util::ComputeVectorMagnitudeRangeCallback();
   m_dataObjectOps["Count Number Of Vectors And Scalars"] = new VE_Util::CountNumberOfParametersCallback();
   m_dataObjectOps["Number Of Grid Points"] = new VE_Util::GetNumberOfPointsCallback();
   m_dataObjectOps["Scalar Range Information"] = new VE_Util::ProcessScalarRangeCallback();
   m_dataObjectOps["Active Data Information"] = new VE_Util::ActiveDataInformationCallback();
   m_dataObjectOps["Create BBox Actors"] = new VE_Util::CreateDataObjectBBoxActorsCallback();
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
      //vprDEBUG(vesDBG,1) << "deleting this->x_planes" 
      //                       << std::endl << vprDEBUG_FLUSH;
   }

   if ( this->y_planes != NULL ) 
   {
      delete this->y_planes;
      this->y_planes = NULL;
      //vprDEBUG(vesDBG,1) << "deleting this->y_planes" 
      //                       << std::endl << vprDEBUG_FLUSH;
   }
   
   if ( this->z_planes != NULL ) 
   {
      delete this->z_planes;
      this->z_planes = NULL;
      //vprDEBUG(vesDBG,1) << "deleting this->z_planes" 
      //                        << std::endl << vprDEBUG_FLUSH;
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
      //vprDEBUG(vesDBG,2) << "deleting filename " << this->fileName
      //                       << std::endl << vprDEBUG_FLUSH;
      fileName.erase();//delete [] this->fileName;
      fileName.empty();//this->fileName = NULL;
   }
 
   if ( _vtkFHndlr )
   {
      delete _vtkFHndlr;
      _vtkFHndlr = 0;
      //vprDEBUG(vesDBG,2) << "deleting _vtkFHndlr " << std::endl << vprDEBUG_FLUSH;
   }

   if( m_externalFileLoader )
   {
      delete m_externalFileLoader;
      m_externalFileLoader = 0;
      //vprDEBUG(vesDBG,2) << "deleting m_externalFileLoader " << std::endl << vprDEBUG_FLUSH;
   }
   if( m_dataObjectHandler )
   {
	   delete m_dataObjectHandler;
	   m_dataObjectHandler = 0;
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

/*void cfdDataSet::GetMeanCellLength( float &len )
{
   len = this->meanCellBBLength;
}

float cfdDataSet::GetMeanCellLength()
{
   return this->meanCellBBLength;
}*/

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

vtkDataObject* cfdDataSet::GetDataSet()
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
///////////////////////////
void cfdDataSet::LoadData()
{
   if ( this->dataSet != NULL )
   {
      vprDEBUG(vesDBG,1) <<"|\tAlready have loaded the data for " 
                             << this->fileName
                             << std::endl << vprDEBUG_FLUSH;
      return;
   }
   else
   {
      vprDEBUG(vesDBG,1) <<"|\tLoadData: filename = " << this->fileName
                             << std::endl << vprDEBUG_FLUSH;
   }

   std::string extension = VE_Util::fileIO::getExtension( fileName );
   //What should the extension of the star.param file be?
   //The translator expects ".star" but we have things setup to 
   //be ".param". One or the other should be changed for consistency.
   if(extension == "param")
   {
      extension = "star";
   }
   if( ( extension == "case") )
   {
      extension = "ens";
   }

   if( ( extension.find( "vtk" )!= std::string::npos ) ||
      ( extension.find( "vtu" ) != std::string::npos ) ||
      ( extension.find( "vtp" ) != std::string::npos ) ||
      ( extension.find( "vti" ) != std::string::npos ) )
   {
      if( !_vtkFHndlr )
      {
          _vtkFHndlr = new cfdVTKFileHandler();
      }
      ///This will need to be changed to handle mutliblockdatasets!!!!!!
      this->dataSet = _vtkFHndlr->GetDataSetFromFile(fileName);
   }
   else
   {
      if( !m_externalFileLoader )
      {
         m_externalFileLoader = new VE_Builder::DataLoader();
      }
      m_externalFileLoader->SetInputData( "something", "somedir" );
      unsigned int nParams = 7;
      char** parameters = new char*[nParams];
      parameters[0] = new char[strlen("loaderToVtk") + 1];
      strcpy(parameters[0],"loaderToVtk");      
      parameters[1] = new char[strlen( "-singleFile" ) + 1];
      strcpy(parameters[1], "-singleFile" );
      parameters[2] = new char[fileName.length()   + 1];
      strcpy(parameters[2], fileName.c_str() );
      parameters[3] = new char[strlen("-loader") + 1];
      strcpy(parameters[3], "-loader" );
      parameters[4] = new char[extension.length() + 1];
      strcpy(parameters[4], extension.c_str() );
      parameters[5] = new char[strlen( "-o" ) + 1];
      strcpy(parameters[5], "-o" );
      parameters[6] = new char[strlen( "." ) + 1];
      strcpy(parameters[6], "." );/*
      parameters[7] = new char[strlen( "-w" ) + 1];
      strcpy(parameters[7], "-w" );
      parameters[8] = new char[strlen( "stream" ) + 1];
      strcpy(parameters[8], "stream" );*/
      
      dataSet = m_externalFileLoader->GetVTKDataSet( nParams, parameters );
      dataSet->Print( std::cout );
      
      for(unsigned int i = 0; i < nParams; ++i)
      {
         delete [] parameters[i];
      }
      delete parameters;
      if(!dataSet)
      {
         vprDEBUG(vesDBG,1) << "|\tInvalid input file: " << fileName
                          << std::endl << vprDEBUG_FLUSH;
         return;
      }
   }
   if(!m_dataObjectHandler)
   {
	   m_dataObjectHandler = new VE_Util::DataObjectHandler();
   }
   m_dataObjectHandler->OperateOnAllDatasetsInObject(dataSet);

   //Need to get number of pda
   this->numPtDataArrays = m_dataObjectHandler->GetNumberOfDataArrays();

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
   //this->UpdatePropertiesForNewMesh();

   if ( this->GetPrecomputedDataSliceDir().c_str() )
   {   
      double bounds[ 6 ];
      GetBounds(bounds);
	  vprDEBUG(vesDBG,0) << "\tLoading precomputed planes from " 
           << this->GetPrecomputedDataSliceDir() << std::endl << vprDEBUG_FLUSH;
      this->x_planes = new cfdPlanes( 0, this->GetPrecomputedDataSliceDir().c_str(), bounds );
      this->y_planes = new cfdPlanes( 1, this->GetPrecomputedDataSliceDir().c_str(), bounds );
      this->z_planes = new cfdPlanes( 2, this->GetPrecomputedDataSliceDir().c_str(), bounds );
   }

   // count the number of scalars and store names and ranges...
   this->StoreScalarInfo();
   
   // count the number of vectors and store names ...
   this->numVectors = dynamic_cast<VE_Util::CountNumberOfParametersCallback*>
	   (m_dataObjectOps["Count Number Of Vectors And Scalars"])->GetNumberOfParameters(true);
   if ( this->numVectors )
   {
      this->vectorName = dynamic_cast<VE_Util::CountNumberOfParametersCallback*>
	   (m_dataObjectOps["Count Number Of Vectors And Scalars"])->GetParameterNames(true);
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
         if(!this->vectorMagRange)
         {
             this->vectorMagRange = new double[2];
         }
          VE_Util::ComputeVectorMagnitudeRangeCallback* vecMagRangeCbk =
              dynamic_cast<VE_Util::ComputeVectorMagnitudeRangeCallback*>
              (m_dataObjectOps["Compute Vector Magnitude Range"]); 
          m_dataObjectHandler->SetDatasetOperatorCallback(vecMagRangeCbk);
          m_dataObjectHandler->OperateOnAllDatasetsInObject(this->dataSet);
          vecMagRangeCbk->GetVectorMagnitudeRange(this->vectorMagRange);
      }
   }
   else
   {
      vprDEBUG(vesDBG,0) << "\tWARNING: No Point Data"
                             << std::endl << vprDEBUG_FLUSH;
   }

   this->SetType();
  
}
////////////////////////////////////////////
unsigned int cfdDataSet::GetNumberOfPoints()
{
    if(m_dataObjectHandler)
    {
        VE_Util::GetNumberOfPointsCallback* numberOfPointsCallback = 
        dynamic_cast<VE_Util::GetNumberOfPointsCallback*>
                               (m_dataObjectOps["Number Of Grid Points"]);
        m_dataObjectHandler->SetDatasetOperatorCallback(numberOfPointsCallback );
        m_dataObjectHandler->OperateOnAllDatasetsInObject(this->dataSet);
        return numberOfPointsCallback->GetNumberOfPoints();

    }
    return 0;
}
/////////////////////////////////////////
double* cfdDataSet::GetBounds()
{
    GetBounds(m_bounds);
    return m_bounds;
}
//////////////////////////////////////////////////////////
void cfdDataSet::GetBounds(double bounds[6])
{
    
    if(m_dataObjectHandler)
    {
        VE_Util::ComputeDataObjectBoundsCallback* boundsCallback = 
        dynamic_cast<VE_Util::ComputeDataObjectBoundsCallback*>
                               (m_dataObjectOps["Compute Bounds"]);
        m_dataObjectHandler->SetDatasetOperatorCallback(boundsCallback);
        m_dataObjectHandler->OperateOnAllDatasetsInObject(this->dataSet);
        boundsCallback->GetDataObjectBounds(bounds);
        this->bbDiagonal = boundsCallback->GetDataObjectBoundsDiagonal();
    }
}
//////////////////////////////////////////////////////////////////
int cfdDataSet::CountNumberOfParameters( const int numComponents )
{
   int numParameters = 0;
   return numParameters;
}

std::vector<std::string> cfdDataSet::GetParameterNames( const int numComponents, 
                                       const int numParameters )
{

   std::vector<std::string> name;

   return name;
}

void cfdDataSet::UpdatePropertiesForNewMesh()
{

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
   VE_Util::ActiveDataInformationCallback* activeDataInfoCbk = 
       dynamic_cast<VE_Util::ActiveDataInformationCallback*>
       (m_dataObjectOps["Active Data Information"]);
   activeDataInfoCbk->SetActiveDataName(  this->scalarName[ this->activeScalar ] );
   m_dataObjectHandler->SetDatasetOperatorCallback(activeDataInfoCbk);
   m_dataObjectHandler->OperateOnAllDatasetsInObject(this->dataSet);
     
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

   SetActiveScalar(this->scalarName[ this->activeScalar ] );
   
   /*vprDEBUG(vesDBG,1) << "\tSetActiveScalar: Active scalar is \""
        << this->GetDataSet()->GetPointData()->GetScalars()->GetName()
        << "\"" << std::endl << vprDEBUG_FLUSH;*/

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


   SetActiveVector( this->vectorName[ this->activeVector ] );
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
   VE_Util::ActiveDataInformationCallback* activeDataInfoCbk = 
       dynamic_cast<VE_Util::ActiveDataInformationCallback*>
       (m_dataObjectOps["Active Data Information"]);
   activeDataInfoCbk->SetActiveDataName(  this->vectorName[ this->activeVector ],true );
   m_dataObjectHandler->SetDatasetOperatorCallback(activeDataInfoCbk);
   m_dataObjectHandler->OperateOnAllDatasetsInObject(this->dataSet);
   
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
////////////////////////////////////////////
std::string cfdDataSet::GetFileName()
{
  return this->fileName;
}
/////////////////////////////////////////////////////////////////////////////////////////////
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
/////////////////////////////////////////////////////////////////
std::string cfdDataSet::GetPrecomputedDataSliceDir()
{
   return this->precomputedDataSliceDir;
}
///////////////////////////////////////////////////////////////////////////////////////////
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
///////////////////////////////////////////////////////////////
std::string cfdDataSet::GetPrecomputedSurfaceDir()
{
   return this->precomputedSurfaceDir;
}
////////////////////////////////////////////////////////////
cfdPlanes * cfdDataSet::GetPrecomputedXSlices()
{
   return this->x_planes;
}
/////////////////////////////////////////////////////////////
cfdPlanes * cfdDataSet::GetPrecomputedYSlices()
{
   return this->y_planes;
}
/////////////////////////////////////////////////////////////
cfdPlanes * cfdDataSet::GetPrecomputedZSlices()
{
   return this->z_planes;
}
/////////////////////////////////////////////////////////////////////
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
////////////////////////////////////////////////////////////
void cfdDataSet::SetArrow( vtkPolyData * arrow)
{
   this->arrow = arrow;
}
///////////////////////////////////////////////
vtkPolyData * cfdDataSet::GetArrow( )
{
   return this->arrow;
} 
//////////////////////////////////////////////
int cfdDataSet::GetNumberOfScalars()
{
   return this->numScalars;
}
///////////////////////////////////////////////////////
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
///////////////////////////////////////////////
int cfdDataSet::GetNumberOfVectors()
{
   return this->numVectors;
}
////////////////////////////////////////////////////////
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
//////////////////////////////////////////////
void cfdDataSet::SetNewlyActivated()
{
   this->isNewlyActivated = 1;
}
///////////////////////////////////////////////////
void cfdDataSet::SetNotNewlyActivated()
{
   this->isNewlyActivated = 0;
}
//////////////////////////////////////////
int cfdDataSet::IsNewlyActivated()
{
   return this->isNewlyActivated;
}
/////////////////////////////////////////////
cfdDataSet* cfdDataSet::GetParent()
{
   return this->parent;
}
/////////////////////////////////////////////////////////////////
void cfdDataSet::SetParent( cfdDataSet* myParent )
{
   this->parent = myParent;
}
//////////////////////////////////////////////////////////////////////////
double* cfdDataSet::GetActualScalarRange(std::string name)
{
    for(int i = 0; i < numScalars; ++i)
    {
        if(scalarName[i] == name)
        {
            return GetActualScalarRange(i);
        }
    }
    return 0;
}
//////////////////////////////////////////////////////////////////
double* cfdDataSet::GetActualScalarRange( int index )
{
   return this->actualScalarRange[ index ];
}
/////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::GetActualScalarRange( int index, double* range )
{
   range[ 0 ] = this->actualScalarRange[ index ][ 0 ];
   range[ 1 ] = this->actualScalarRange[ index ][ 1 ];
}
///////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::SetActualScalarRange( int index, double* range )
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
//////////////////////////////////////////////////////////
double * cfdDataSet::GetDisplayedScalarRange()
{
   vprDEBUG(vesDBG,1) << "cfdDataSet::GetDisplayedScalarRange"
      << " activeScalar = " << this->activeScalar
      << std::endl << vprDEBUG_FLUSH;
   return this->displayedScalarRange[ this->activeScalar ];
}
////////////////////////////////////////////////////////////////////////
double * cfdDataSet::GetDisplayedScalarRange( int index )
{
   return this->displayedScalarRange[ index ];
}
/////////////////////////////////////////////////////////////////////////////////////
void cfdDataSet::SetDisplayedScalarRange( int index, double* range )
{
   this->displayedScalarRange[ index ][ 0 ] = range[ 0 ];
   this->displayedScalarRange[ index ][ 1 ] = range[ 1 ];
   //this->definedRange[ 0 ] = range[ 0 ];
   //this->definedRange[ 1 ] = range[ 1 ];
}
//////////////////////////////////////////////////////////////////
VE_SceneGraph::Switch* cfdDataSet::GetSwitchNode()
{
   if ( !switchNode )
   {
      switchNode = new VE_SceneGraph::Switch();
   }

   return switchNode.get();
}
/////////////////////////////////////////////////////
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
/////////////////////////////////////////////////////////////////////
void cfdDataSet::SetDCS( VE_SceneGraph::DCS* myDCS )
{
   if ( dcs == NULL )
      this->dcs = myDCS;
   else
      std::cerr << " ERROR: DCS is already set for this dataset " << std::endl;
}
/////////////////////////////////////////////////
int cfdDataSet::IsPartOfTransientSeries()
{
   return this->partOfTransientSeries;
}
////////////////////////////////////////////////////////
void cfdDataSet::SetAsPartOfTransientSeries()
{
   this->partOfTransientSeries = 1;
}
/////////////////////////////////////////
void cfdDataSet::StoreScalarInfo()
{
    //Get the scalar and vector information
    m_dataObjectHandler->SetDatasetOperatorCallback(m_dataObjectOps["Count Number Of Vectors And Scalars"]);
    m_dataObjectHandler->OperateOnAllDatasetsInObject(this->dataSet);
    
    m_dataObjectHandler->SetDatasetOperatorCallback(m_dataObjectOps["Count Number Of Vectors And Scalars"]);
    m_dataObjectHandler->OperateOnAllDatasetsInObject(this->dataSet);
    
    // count the number of vectors and store names ...
    this->numScalars = dynamic_cast<VE_Util::CountNumberOfParametersCallback*>
       (m_dataObjectOps["Count Number Of Vectors And Scalars"])->GetNumberOfParameters();

    vprDEBUG(vesDBG,1) << "\tStoreScalarInfo: numScalars = " 
                          << this->numScalars
                          << std::endl << vprDEBUG_FLUSH;

    if ( this->numScalars )
    {
        this->scalarName = dynamic_cast<VE_Util::CountNumberOfParametersCallback*>
        (m_dataObjectOps["Count Number Of Vectors And Scalars"])->GetParameterNames();

        this->actualScalarRange = new double * [ this->numScalars ];
        this->displayedScalarRange = new double * [ this->numScalars ];
        for ( int i=0; i<this->numScalars; i++ )
        {
           this->actualScalarRange[ i ]    = new double [ 2 ];
           this->displayedScalarRange[ i ] = new double [ 2 ];
        }
        VE_Util::ProcessScalarRangeCallback* processScalarRangeCbk =
            dynamic_cast<VE_Util::ProcessScalarRangeCallback*>
            (m_dataObjectOps["Scalar Range Information"]);
        m_dataObjectHandler->SetDatasetOperatorCallback(processScalarRangeCbk);
        m_dataObjectHandler->OperateOnAllDatasetsInObject(this->dataSet);
        
        //Add the scalar info to actual scalar range and displayed scalar range
        for(int i = 0; i < this->numScalars; ++i)
        {
            processScalarRangeCbk->GetScalarRange(GetScalarName(i),actualScalarRange[i]);
            this->AutoComputeUserRange( 
                              this->GetParent()->GetActualScalarRange( i ),
                              this->displayedScalarRange[ i ] );
        }
   }
}
//////////////////////////////////////////////////////////
double* cfdDataSet::GetScalarRange(std::string scalarName)
{
    for(int i = 0; i < this->numScalars; ++i)
    {
        if(this->scalarName[i] == scalarName)
        {
            return this->actualScalarRange[i];
        }
    }
}
/////////////////////////////
void cfdDataSet::Print()
{
   std::cout << "filename = " << this->fileName << std::endl;
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
    if( !m_visualBBox.valid() )
    {
       m_visualBBox = new VE_SceneGraph::Group();
    }
    VE_Util::CreateDataObjectBBoxActorsCallback* bboxActorsCbk = 
        dynamic_cast<VE_Util::CreateDataObjectBBoxActorsCallback*>
        (m_dataObjectOps["Create BBox Actors"]);

    m_dataObjectHandler->SetDatasetOperatorCallback(bboxActorsCbk);
    m_dataObjectHandler->OperateOnAllDatasetsInObject(this->dataSet);
    std::vector< vtkActor* > bboxActors = bboxActorsCbk->GetBBoxActors();
    size_t nBBoxActors = bboxActors.size();
    for(size_t i = 0; i < nBBoxActors; ++i)
    {
        osg::ref_ptr<VE_SceneGraph::Geode> bboxGeode = new VE_SceneGraph::Geode();
        bboxGeode->TranslateToGeode( bboxActors.at(i) );
        m_visualBBox->AddChild(bboxGeode.get());

    }
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
    if( !m_visualBBox.valid() )
    {
        CreateBoundingBoxGeode();
        GetDCS()->AddChild( m_visualBBox.get() );
    }
    m_visualBBox->setNodeMask((state==0)?0:1);
   /*if ( (state == 0) && bboxGeode.valid() )
   {
      //GetDCS()->RemoveChild( bboxGeode.get() );
       bboxGeode->setNodeMask(0);
   }
   else if ( state == 1 )
   {
      if ( bboxGeode == 0 )
      {
         
      }
      GetDCS()->RemoveChild( bboxGeode.get() );
     
   }*/
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
         dataSetAxes->SetBoundingBox( GetBounds() );
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
         dataSetScalarBar->SetBoundingBox( GetBounds() );
      }
      GetDCS()->RemoveChild( dataSetScalarBar->GetScalarBar() );
      dataSetScalarBar->AddScalarBarToGroup();
      GetDCS()->AddChild( dataSetScalarBar->GetScalarBar() );
   }
}
