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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdHDFtoVTK.h"
#include "vtkRectilinearGrid.h"
#include "vtkDoubleArray.h"
#include "vtkFloatArray.h"
#include "vtkCellData.h"
#include "vtkPointData.h"
#include "vtkCellDataToPointData.h"
#include "vtkRectilinearGridWriter.h"
#include "viewCells.h"
#include <iostream>
#include <cmath>
#include <vector>
//////////////////////////////
//Constructor               //
//////////////////////////////
cfdHDFToVTK::cfdHDFToVTK()
{
   _asciiOut = 0;
   _outFile = 0;
   _viewGridBeforeWriting = 1;
   _numCells = 0;
   _inHDFFile = 0;
   _outVTKDirectory = 0;
   _verbose = 0;;
   _dimensions[0] = 0;
   _dimensions[1] = 0;
   _dimensions[2] = 0;
   _rank = 0;
   _maxrank = 3;
   _ax = 0;
   _ay = 0;
   _az = 0;
   _bx = 0;
   _by = 0;
   _bz = 0;
   _deltaAX = 0;
   _deltaBX = 0;
   _deltaAY = 0;
   _deltaBY = 0;
   _deltaAZ = 0;
   _deltaBZ = 0;
   _vX = 0;
   _vY = 0;
   _vZ = 0;
   _b1 = 0;
   _b2 = 0;
   _b3 = 0;
   _pseudoGravPot = 0;
   _density = 0;
   _energyDensity = 0;
   _velocityMag = 0;
   _magneticMag =0;
   
}
///////////////////////////////////////////////
cfdHDFToVTK::cfdHDFToVTK(char* inHDFFileName)
{
   _asciiOut = 0;
   _outFile = 0;
    _velocityMag = 0;
   _magneticMag =0;
   _viewGridBeforeWriting = 1;
   _numCells = 0;
   _inHDFFile = 0;
   _outVTKDirectory = 0;
   _verbose = 0;
   _dimensions[0] = 0;
   _dimensions[1] = 0;
   _dimensions[2] = 0;
   _rank = 0;
   _maxrank = 3;
   _ax = 0;
   _ay = 0;
   _az = 0;
   _bx = 0;
   _by = 0;
   _bz = 0;
   _deltaAX = 0;
   _deltaBX = 0;
   _deltaAY = 0;
   _deltaBY = 0;
   _deltaAZ = 0;
   _deltaBZ = 0;
   _vX = 0;
   _vY = 0;
   _vZ = 0;
   _b1 = 0;
   _b2 = 0;
   _b3 = 0;
   _pseudoGravPot = 0;
   _density = 0;
   _energyDensity = 0;
   
   
   setInputHDFFile(inHDFFileName);
}
//////////////////////////////////////////////////////////////////
cfdHDFToVTK::cfdHDFToVTK(char* inHDFFileName,char* outputVTKDir)
{
   _asciiOut = 0;
   _outFile = 0;
    _velocityMag = 0;
   _magneticMag =0;
   _viewGridBeforeWriting = 1;
   _numCells = 0;
   _inHDFFile = 0;
   _outVTKDirectory = 0;
   _verbose = 0;
   _dimensions[0] = 0;
   _dimensions[1] = 0;
   _dimensions[2] = 0;
   _rank = 0;
   _maxrank = 3;
   _ax = 0;
   _ay = 0;
   _az = 0;
   _bx = 0;
   _by = 0;
   _bz = 0;
   _deltaAX = 0;
   _deltaBX = 0;
   _deltaAY = 0;
   _deltaBY = 0;
   _deltaAZ = 0;
   _deltaBZ = 0;
   _vX = 0;
   _vY = 0;
   _vZ = 0;
   _b1 = 0;
   _b2 = 0;
   _b3 = 0;
   _pseudoGravPot = 0;
   _density = 0;
   _energyDensity = 0;

   setInputHDFFile(inHDFFileName);
   setOutputVTKDirectory(outputVTKDir);
}
/////////////////////////////
cfdHDFToVTK::~cfdHDFToVTK()
{
   if(_outFile){
      delete [] _outFile;
      _outFile = 0;
   }
   if(_inHDFFile){
      delete [] _inHDFFile;
      _inHDFFile = 0;
   }
   if(_outVTKDirectory){
      delete [] _outVTKDirectory;
      _outVTKDirectory = 0;
   }
   if(_ax){
      delete [] _ax;
      _ax = 0;
   }
   if(_ay){
      delete [] _ay;
      _ay = 0;
   }
   if(_az){
      delete [] _az;
      _az = 0;
   }
   if(_deltaAX){
      delete [] _deltaAX;
      _deltaAX = 0;
   }
   if(_deltaAY){
      delete [] _deltaAY;
      _deltaAY = 0;
   }
   if(_deltaAZ){
      delete [] _deltaAZ;
      _deltaAZ = 0;
   }
   if(_deltaBX){
      delete [] _deltaBX;
      _deltaBX = 0;
   }
   if(_deltaBY){
      delete [] _deltaBY;
      _deltaBY = 0;
   }
   if(_deltaBZ){
      delete [] _deltaBZ;
      _deltaBZ = 0;
   }
   if(_vX){
      delete [] _vX;
      _vX = 0;
   }
   if(_vY){
      delete [] _vY;
      _vY = 0;
   }
   if(_vZ){
      delete [] _vZ;
      _vZ = 0;
   }
   if(_b1){
      delete [] _b1;
      _b1 = 0;
   }
   if(_b2){
      delete [] _b2;
      _b2 = 0;
   }
   if(_b3){
      delete [] _b3;
      _b3 = 0;
   }
   if(_bx){
      delete [] _bx;
      _bx = 0;
   }
   if(_by){
      delete [] _by;
      _by = 0;
   }
   if(_bz){
      delete [] _bz;
      _bz = 0;
   }
   if(_pseudoGravPot){
      delete [] _pseudoGravPot;
      _pseudoGravPot = 0;
   }
   if(_density){
      delete [] _density;
      _density = 0;
   }
   if(_energyDensity){
      delete [] _energyDensity;
      _energyDensity = 0;
   }
   if(_scalarNames.size()){
       _scalarNames.clear();
   }
   if(_vectorNames.size()){
       _vectorNames.clear();
   }
   if(_velocityMag){
      delete [] _velocityMag;
      _velocityMag = 0;
   }
   
}
////////////////////////////////////////////////
void cfdHDFToVTK::setInputHDFFile(char* inFile)
{
   if(!inFile){
      if(_verbose){
         std::cout<<"Invalid input filename!"<<std::endl;
         std::cout<<"cfdHDFToVTK::setInputHDFFile()"<<std::endl;
         return;
      }
   }
   if(_inHDFFile){
      delete [] _inHDFFile;
      _inHDFFile = 0;
   }

   _inHDFFile = new char[strlen(inFile) + 1];
   strcpy(_inHDFFile,inFile);
   if(_verbose){
      std::cout<<"Input HDF filename: "<<_inHDFFile<<std::endl;
   }
}
//////////////////////////////////////////////////////
void cfdHDFToVTK::setOutputVTKDirectory(char* outDir)
{
   if(!outDir){
      if(_verbose){
         std::cout<<"Invalid input filename!"<<std::endl;
         std::cout<<"cfdHDFToVTK::setOutputVTKDirectory()"<<std::endl;
         return;
      }
   }
   if(_outVTKDirectory){
      delete [] _outVTKDirectory;
      _outVTKDirectory = 0;
   }

   _outVTKDirectory = new char[strlen(outDir) + 1];
   strcpy(_outVTKDirectory,outDir);
   if(_verbose){
      std::cout<<"Output VTK file directory: "<<_outVTKDirectory<<std::endl;
   }
}
///////////////////////////////////////
int cfdHDFToVTK::translateFileToVTK()
{
   if(!_inHDFFile){
      if(_verbose){
         std::cout<<"ERROR!!!"<<std::endl;
         std::cout<<"Invalid HDF filename!"<<std::endl;
         std::cout<<"cfdHDFToVTK::translateFileToVTK()"<<std::endl;
      }
   
      return 0;
   }
   if(!_outVTKDirectory){
      if(_verbose){
         std::cout<<"Setting VTK output directory to ./"<<std::endl;
      }
      _outVTKDirectory = new char[128];
      strcpy(_outVTKDirectory,"./");
   }
   if(_type == DCLARKE){
      //read David Clarke's file and extract data
      if(_readDCHDFFile(_inHDFFile)){
      }else{
         _createVelocityMagnitudeScalars();
         _createMagneticMagnitudeScalars();
         _createRectilinearVTKGrid();
      }
   }else{
      //read in the hdf file
      hid_t fileID = -1;
      hid_t currentDatasetID = -1; 
      hid_t rootGroup = -1; 
   
      fileID = H5Fopen(_inHDFFile, H5F_ACC_RDONLY, H5P_DEFAULT);
   
      if(!fileID){
         if(_verbose){
            std::cout<<"Couldn't open HDF file: "<<_inHDFFile<<std::endl;
            return 0;
         }
      }

      rootGroup = H5Gopen(fileID,"/");
      if(!rootGroup){
         if(_verbose){
            std::cout<<"Couldn't open root level group! "<<std::endl;
            return 0;
        }
      }
      if(_verbose){
         std::cout<<"Traversing hdf file root level group!"<<std::endl;
      }   
      _traverseGroup(rootGroup,"/");

      if(_verbose){
         std::cout<<"Finished hdf file root level traversal."<<std::endl;
      }
   }
   return 1;
}
////////////////////////////////////////////////////////////////////
herr_t groupTraverserCallback(hid_t objectID, 
                           const char* name, 
                           void* translator)
{
   cfdHDFToVTK* xlatr = (cfdHDFToVTK*)(translator);
   H5G_stat_t statbuf;
   herr_t ret = 1;
   H5Gget_objinfo(objectID, name, 1, &statbuf);

   switch(statbuf.type){
      case H5G_LINK:
         if(xlatr->_verbose){
            std::cout<<"Link"<<std::endl;
         }
         break;
      case H5G_GROUP:
         if(xlatr->_verbose){
            std::cout<<"Group"<<std::endl;
         }
         xlatr->_traverseGroup(objectID,name);
         
         break;
      case H5G_DATASET:
          if(xlatr->_verbose){
             std::cout<<"Dataset"<<std::endl;
          }
          xlatr->_extractDatasetInfo(objectID,name);
          break;
      case H5G_TYPE:
         if(xlatr->_verbose){
            std::cout<<"Type"<<std::endl;
         }
         break;
       default:
          if(xlatr->_verbose){
             std::cout<<"Something else"<<std::endl;
          }
          ret = 0;
          break;
    };

   return ret;
}
/////////////////////////////////////////////////////
void cfdHDFToVTK::_extractDatasetInfo(int groupID,
                                   const char* name)
{
   if(_verbose){
      std::cout<<"=====New dataset.====="<<std::endl;
   }
   hid_t dSpaceID = -1;
   hid_t dTypeID = -1;
   hid_t pListID = -1; 
   H5T_class_t typeClass;                 
   size_t size = 0;                
   hsize_t* dims = 0;              
   herr_t status = -1; 
   int rank = 0;

   int datasetID = H5Dopen(groupID, name);

   dSpaceID = H5Dget_space(datasetID);
   dTypeID = H5Dget_type(datasetID);
   pListID = H5Dget_create_plist(datasetID);

   if(_verbose){
      std::cout<<"Extracting:"<<name<<" info:"<<std::endl;
      std::cout<<"Dataspace id: "<<dSpaceID<<std::endl;
      std::cout<<"Datatype id: "<<dTypeID<<std::endl;
      std::cout<<"Property list id: "<<pListID<<std::endl;
   }

   /*
     * Get datatype and dataspace handles and then query
     * dataset class, order, size, rank and dimensions.
     */

   
    rank = H5Sget_simple_extent_ndims(dSpaceID);
    dims = new hsize_t[rank];

    status = H5Sget_simple_extent_dims(dSpaceID, dims, NULL);
    printf("rank %d, dimensions %lu x %lu \n", rank,
	   (unsigned long)(dims[0]), (unsigned long)(dims[1]));
    
   typeClass = H5Tget_class(dTypeID);
    
   size = H5Tget_size(dTypeID);
   printf(" Data size is %d \n", (int)size);

   H5Tclose(dTypeID);
   H5Sclose(dSpaceID);
   H5Dclose(datasetID);
   if(dims){
      delete [] dims;
      dims = 0;
   }

}
///////////////////////////////////////////////
void cfdHDFToVTK::_traverseGroup(int groupID,
                              const char* name)
{
   int type = H5G_GROUP;
   hsize_t numObjectsInGroup = 0;
   H5Gget_num_objs(groupID,&numObjectsInGroup);

   H5Giterate(groupID, name, NULL,
            groupTraverserCallback, 
            (void*)this);
   H5Gclose(groupID);
}
///////////////////////////////////////////////
int cfdHDFToVTK::_readDCHDFFile(char* inDCFile)
{
   //this is actually HDF4!!!!!!!!
   int error = 0; 
   //read data array _dimensions
   error = DFSDgetdims(inDCFile,&_rank,_dimensions,_maxrank);

   if(error < 0){
      std::cout<<"ERROR!!readDCHDFFile()"<<std::endl;
      std::cout<<"Error reading: "<<inDCFile<<" !"<<std::endl;
      return 0;
   }

   if(_bx){
      delete [] _bx;
      _bx = 0;
   }
   if(_by){
      delete [] _by;
      _by = 0;
   }
   if(_bz){
      delete [] _bz;
      _bz = 0;
   }
   _bx = new float32[_dimensions[0]];
   _by = new float32[_dimensions[1]];
   _bz = new float32[_dimensions[2]];

   //read the grid coordinates
   error = DFSDgetdimscale(1,_dimensions[0],_bx);
   if(error){
      for(int i = 0; i < _dimensions[0]; i++){
         _bx[i] = i;
      }
   }
   error = DFSDgetdimscale(2,_dimensions[1],_by);
   if(error){
      for(int i = 0; i < _dimensions[1]; i++){
         _by[i] = i;
      }
   }
   error = DFSDgetdimscale(3,_dimensions[2],_bz);
   if(error){
      for(int i = 0; i < _dimensions[2]; i++){
         _bz[i] = i;
      }
   }
   if(_verbose){
      for(int i = 0;  i < _dimensions[0]; i++){
         std::cout<<_bx[i]<<std::endl;
      }
      for(int i = 0;  i < _dimensions[1]; i++){
         std::cout<<_by[i]<<std::endl;
      }
      for(int i = 0;  i < _dimensions[2]; i++){
         std::cout<<_bz[i]<<std::endl;
      }
   }
   //read data arrays
   int readingInput = 1;
   char label[256];
   char format[256];
   char coordsys[256];
   char unit[256];
  
   int dataSize = 0;
   dataSize = _dimensions[0]*_dimensions[1]*_dimensions[2];

   float32* gridData = 0;
   gridData = new float32[dataSize];

   if(_vX){
      delete [] _vX;
      _vX = 0;
   }
   if(_vY){
      delete [] _vY;
      _vY = 0;
   }
   if(_vZ){
      delete [] _vZ;
      _vZ = 0;
   }
   if(_b1){
      delete [] _b1;
      _b1 = 0;
   }
   if(_b2){
      delete [] _b2;
      _b2 = 0;
   }
   if(_b3){
      delete [] _b3;
      _b3 = 0;
   }
   if(_pseudoGravPot){
      delete [] _pseudoGravPot;
      _pseudoGravPot = 0;
   }
   if(_density){
      delete [] _density;
      _density = 0;
   }
   if(_energyDensity){
      delete [] _energyDensity;
      _energyDensity = 0;
   }
   _vX = new double[dataSize];
   _vY = new double[dataSize];
   _vZ = new double[dataSize];
   _b1 = new double[dataSize];
   _b2 = new double[dataSize];
   _b3 = new double[dataSize];
   _pseudoGravPot = new double[dataSize];
   _density = new double[dataSize];
   _energyDensity = new double[dataSize];
   
   while(readingInput){
      error = DFSDgetdatastrs(label,unit,format,coordsys);
      
      //velocity
      if(strstr(label,"v1")){
         char* vName = new char[256]; 
         //sName = strstr(label,":");
         strcpy(vName,label);
         _vectorNames.push_back(vName);
         error = DFSDgetdata(inDCFile,_rank,_dimensions,gridData);
	       _copyData(gridData,_vX,(int*)_dimensions);
      }else	if(strstr(label,"v2")){
         error = DFSDgetdata(inDCFile,_rank,_dimensions,gridData);
	       _copyData(gridData,_vY,(int*)_dimensions);
      }else	if(strstr(label,"v3")){
         error = DFSDgetdata(inDCFile,_rank,_dimensions,gridData);
	       _copyData(gridData,_vZ,(int*)_dimensions);
      }else if(strstr(label,"b1")){
         char* vName = new char[256]; 
         strcpy(vName,label);
         _vectorNames.push_back(vName);
         error = DFSDgetdata(inDCFile,_rank,_dimensions,gridData);
	       _copyData(gridData,_b1,(int*)_dimensions);
      }else	if(strstr(label,"b2")){
         error = DFSDgetdata(inDCFile,_rank,_dimensions,gridData);
	       _copyData(gridData,_b2,(int*)_dimensions);
      }else if(strstr(label,"b3")){
         error = DFSDgetdata(inDCFile,_rank,_dimensions,gridData);
	      _copyData(gridData,_b3,(int*)_dimensions);
      }else if(strstr(label,"gravity")){
         char* sName = new char[256]; 
         //sName = strstr(label,":");
         strcpy(sName,label);
         _scalarNames.push_back(sName);
         error = DFSDgetdata(inDCFile,_rank,_dimensions,gridData);
	      _copyData(gridData,_pseudoGravPot,(int*)_dimensions);
      }else if(strstr(label,"density")){
         char* sName = new char[256];
         strcpy(sName,label);
         _scalarNames.push_back(sName);
         error = DFSDgetdata(inDCFile,_rank,_dimensions,gridData);
	      _copyData(gridData,_density,(int*)_dimensions);
      }else if(strstr(label,"energy")){
         char* sName = new char[256];
         strcpy(sName,label);
         _scalarNames.push_back(sName);
         error = DFSDgetdata(inDCFile,_rank,_dimensions,gridData);
	      _copyData(gridData,_energyDensity,(int*)_dimensions);
      }		      
      error = DFSDgetdims(inDCFile,&_rank,_dimensions,_maxrank);
      if(error){
         readingInput = 0;
      }
   }

   //now compute "A" grid properties (deltaX/Y/Z)

   if(_ax){
      delete [] _ax;
      _ax = 0;
   }
   if(_ay){
      delete [] _ay;
      _ay = 0;
   }
   if(_az){
      delete [] _az;
      _az = 0;
   }
   if(_deltaAX){
      delete [] _deltaAX;
      _deltaAX = 0;
   }
   if(_deltaAY){
      delete [] _deltaAY;
      _deltaAY = 0;
   }
   if(_deltaAZ){
      delete [] _deltaAZ;
      _deltaAZ = 0;
   }
   if(_deltaBX){
      delete [] _deltaBX;
      _deltaBX = 0;
   }
   if(_deltaBY){
      delete [] _deltaBY;
      _deltaBY = 0;
   }
   if(_deltaBZ){
      delete [] _deltaBZ;
      _deltaBZ = 0;
   }
   _ax = new double[_dimensions[0]+1];
   _ay = new double[_dimensions[1]+1];
   _az = new double[_dimensions[2]+1];

   double xSquared = 0;
   for(int i = 1; i < _dimensions[0] -1;i++){
      xSquared = (_bx[i]-_bx[i-1]) * (_bx[i]-_bx[i-1]);
      _ax[i] = _bx[i-1] + xSquared/(_bx[i+1]-_bx[i-1]);
   }
   //need to check the array index conversion here from fortran to C
   _ax[_dimensions[0] -1] = 2.0*_bx[_dimensions[0] -2]
		        - _ax[_dimensions[0] -2];
   _ax[_dimensions[0]] = 2.0*_bx[_dimensions[0] -1]
		        - _ax[_dimensions[0] -1];
   _ax[0] = 2.0*_bx[0] - _ax[1];


   double ySquared = 0;
   for(int i = 1; i < _dimensions[1] -1;i++){
      ySquared = (_by[i]-_by[i-1]) * (_by[i]-_by[i-1]);
      _ay[i] = _by[i-1] +ySquared/(_by[i+1]-_by[i-1]);
   }
   //need to check the array index conversion here from fortran to C
   _ay[_dimensions[1] -1] = 2.0*_by[_dimensions[1] -2]
		        - _ay[_dimensions[1] -2];
   _ay[_dimensions[1]] = 2.0*_by[_dimensions[1] -1]
		        - _ay[_dimensions[1] -1];
   _ay[0] = 2.0*_by[0] - _ay[1];


   double zSquared = 0;
   for(int i = 1; i < _dimensions[2] -1;i++){
      zSquared = (_bz[i]-_bz[i-1]) * (_bz[i]-_bz[i-1]);
      _az[i] = _bz[i-1] + zSquared/(_bz[i+1]-_bz[i-1]);
   }

   //need to check the array index conversion here from fortran to C
   _az[_dimensions[2] -1] = 2.0*_bz[_dimensions[2] -2]
		        - _az[_dimensions[2] -2];
   _az[_dimensions[2]] = 2.0*_bz[_dimensions[2] -1]
		        - _az[_dimensions[2] -1];
   
   _az[0] = 2.0*_bz[0] - _az[1];


   if(gridData){
      delete [] gridData;
      gridData = 0;
   }
   return 0;	
}
/////////////////////////////////////////////////////////////////////////////
int cfdHDFToVTK::_copyData(float32* inData,double* outData, int* dimensions)
{
   int dataSize = 0;
   dataSize = dimensions[0]*dimensions[1]*dimensions[2];
   
   for(int i = 0; i < dataSize; i++){
      outData[i] = inData[i];
   }
  
   return 0;
}
//////////////////////////////////////////////
void cfdHDFToVTK::_createRectilinearVTKGrid()
{

   // Create a rectilinear grid by defining three arrays specifying the
   // coordinates in the x-y-z directions.
   _numCells = (_dimensions[0])*(_dimensions[1])*(_dimensions[2]);
   vtkFloatArray* aXCoords = vtkFloatArray::New();
   aXCoords->SetNumberOfValues(_dimensions[2]+1);
   for (int i=0; i<_dimensions[2]+1; i++){ 
     aXCoords->SetValue(i,_az[i]);
   }
   vtkFloatArray* aYCoords = vtkFloatArray::New();
   aYCoords->SetNumberOfValues(_dimensions[1]+1);
   for (int i=0; i<_dimensions[1]+1; i++){
     aYCoords->SetValue(i,_ay[i]);
   }  
   vtkFloatArray* aZCoords = vtkFloatArray::New();
   aZCoords->SetNumberOfValues(_dimensions[0]+1);
   for (int i=0; i<_dimensions[0]+1; i++){
     aZCoords->SetValue(i,_ax[i]);
   }  

   vtkRectilinearGrid* aGrid = vtkRectilinearGrid::New();
   aGrid->SetDimensions(_dimensions[2]+1,_dimensions[1]+1,_dimensions[0]+1);
   aGrid->SetXCoordinates(aXCoords);
   aGrid->SetYCoordinates(aYCoords);
   aGrid->SetZCoordinates(aZCoords);
   //aGrid->Print( std::cout );
   int nScalars = _scalarNames.size();
   std::vector<double*> cellData;
   
   for(int i = 0; i < nScalars; i++){
     if(strstr(_scalarNames.at(i),"pg")){
        cellData.push_back(_pseudoGravPot);
        _addCellDataToGrid(aGrid,_scalarNames.at(i),cellData,
                         _numCells,1);
        cellData.clear();
        
     }else if(strstr(_scalarNames.at(i),"density")){
        cellData.push_back(_density);
        _addCellDataToGrid(aGrid,_scalarNames.at(i),cellData,
                         _numCells,1);
        cellData.clear();
        
     }else if(strstr(_scalarNames.at(i),"energy")){
        cellData.push_back(_energyDensity);
        _addCellDataToGrid(aGrid,_scalarNames.at(i),cellData,
                         _numCells,1);
        cellData.clear();
     }	
   }
   //add vtk interface--should be easy!!!
   int nvectors = _vectorNames.size();
   if(!nvectors){
     std::cout<<"No vector info found!!"<<std::endl;
   }
  
   double tuple[3] = {0,0,0};
   for(int i = 0; i < nvectors; i++){

     if(strstr(_vectorNames.at(i),"velocity")){
        cellData.push_back(_vX);
        cellData.push_back(_vY);
        cellData.push_back(_vZ);
        _addCellDataToGrid(aGrid,_vectorNames.at(i),cellData,
                         _numCells,3);
        cellData.clear();
     }else if(strstr(_vectorNames.at(i),"magnetic")){
        cellData.push_back(_b1);
        cellData.push_back(_b2);
        cellData.push_back(_b3);
        _addCellDataToGrid(aGrid,_vectorNames.at(i),cellData,
                         _numCells,3);
        cellData.clear();
     } 
   }
  
   cellData.push_back(_velocityMag);
   _addCellDataToGrid(aGrid,"Velocity Magnitude",cellData,
                   _numCells,1);
   cellData.clear();
   cellData.push_back(_magneticMag);
   _addCellDataToGrid(aGrid,"Magnetic Magnitude",cellData,
                   _numCells,1);
   cellData.clear();

   char* aName = 0;
   //char* bName = 0;
   if(!_outVTKDirectory){
      _outVTKDirectory =  new char[256];
      strcpy(_outVTKDirectory,"./");
   }
   aName = new char[1028];
   strcpy(aName,_outVTKDirectory);
   if(!_outFile){
      setVTKOutFileName(0);
   }
   //strcat(aName,"a");
   strcat(aName,_outFile);
   strcat(aName,".vtk");
  
   _writeRectilinearCellDataToPointDataFile(aGrid,aName,_asciiOut);
   if(aName){
      delete [] aName;
      aName = 0;
   }
   aGrid->Delete();
   aXCoords->Delete();
   aYCoords->Delete();
   aZCoords->Delete();
}
/////////////////////////////////////////////////////////////////////////////////////////
void cfdHDFToVTK::_writeRectilinearCellDataToPointDataFile(vtkRectilinearGrid* cellGrid,
                                                     char* fileName, int asciiFile)
{
   vtkCellDataToPointData* dataConvertCellToPoint = vtkCellDataToPointData::New();
   vtkRectilinearGrid* rGridPt = vtkRectilinearGrid::New();
   dataConvertCellToPoint->SetInput( cellGrid );
   dataConvertCellToPoint->PassCellDataOff();
   dataConvertCellToPoint->Update();

   rGridPt->ShallowCopy(dataConvertCellToPoint->GetRectilinearGridOutput());
   
   if(_viewGridBeforeWriting){
     viewCells(rGridPt,0);
   }

   vtkRectilinearGridWriter* rGridWriter = vtkRectilinearGridWriter::New();
   rGridWriter->SetInput(rGridPt);
   if(asciiFile){
      rGridWriter->SetFileTypeToASCII();
   }else{
      rGridWriter->SetFileTypeToBinary();
   }
   rGridWriter->SetFileName(fileName);
   rGridWriter->Write();

   rGridWriter->Delete();
   dataConvertCellToPoint->Delete();
   rGridPt->Delete();


}
////////////////////////////////////////////////////
void cfdHDFToVTK::_addCellDataToGrid(vtkDataSet* dSet,
                                  char* name,
                                  std::vector<double*> cellData,
                                  int nTuples,
                                  int nComponents,
                                  int pointCellGrid)
{
   vtkFloatArray* data = vtkFloatArray::New();
   data->SetName( name );
   data->SetNumberOfComponents(nComponents);
   data->SetNumberOfTuples( nTuples );
   double* tuple = new double[nComponents];
   for( int j = 0; j < nTuples; j++ ){
      for(int i = 0; i < nComponents; i++){
         tuple[i] = cellData.at(i)[j];      
      }
      data->SetTuple(j,tuple);
   }
   if(pointCellGrid){
      dSet->GetPointData()->AddArray(data);
   }else{
      dSet->GetCellData()->AddArray(data);
   }
   data->Delete();
   if(tuple){
      delete [] tuple;
      tuple = 0;
   }
}
////////////////////////////////////////////////////
void cfdHDFToVTK::_createVelocityMagnitudeScalars()
{
   if((!_vX)||(!_vY)||(!_vZ)){
      std::cout<<"ERROR!!!"<<std::endl;
      std::cout<<"Velocity vectors not found!!"<<std::endl;
      std::cout<<"cfdHDFToVTK::_createVelocityMagnitudeScalars."<<std::endl;
      return;
   }

   if(_velocityMag){
      delete [] _velocityMag;
      _velocityMag = 0;
   }
   int nCells = (_dimensions[0])*(_dimensions[1])*(_dimensions[2]);
   _velocityMag = new double[nCells];
   for(int i = 0; i < nCells; i++){
      _velocityMag[i] = sqrt(_vX[i]*_vX[i] +
                          _vY[i]*_vY[i] +
                          _vZ[i]*_vZ[i]);
   }
}
////////////////////////////////////////////////////
void cfdHDFToVTK::_createMagneticMagnitudeScalars()
{
   if((!_b1)||(!_b2)||(!_b3)){
      std::cout<<"ERROR!!!"<<std::endl;
      std::cout<<"Magnetic Field vectors not found!!"<<std::endl;
      std::cout<<"cfdHDFToVTK::_createMagneticMagnituedScalars."<<std::endl;
      return;
   }

   if(_magneticMag){
      delete [] _magneticMag;
      _magneticMag = 0;
   }
   int nCells = (_dimensions[0])*(_dimensions[1])*(_dimensions[2]);
   _magneticMag = new double[nCells];
   for(int i = 0; i < nCells; i++){
      _magneticMag[i] = sqrt(_b1[i]*_b1[i] +
                          _b2[i]*_b2[i] +
                          _b3[i]*_b3[i]);
   }
}
//////////////////////////////////////////////////////
void cfdHDFToVTK::setVTKOutFileName(char* outfileName)
{
   if(!outfileName){
      if(!_outFile){
         _outFile = new char[256];
         strcpy(_outFile,"out");
      }
      std::cout<<"Invalid filename!"<<std::endl;
      std::cout<<"Cannot write file!"<<std::endl;
      std::cout<<"Setting file name to out."<<std::endl;
      return;
   }

   if(_outFile){
      delete [] _outFile;
      _outFile = 0;
   }
   _outFile = new char[strlen(outfileName)+1];
   strcpy(_outFile,outfileName);

}
