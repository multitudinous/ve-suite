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
 * File:          $RCSfile: textureCreator.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _BIV_VTK_TO_TEXTURE_H_
#define _BIV_VTK_TO_TEXTURE_H_

#include <vtkDataSet.h>
#include <vtkUnstructuredGridReader.h>
#include <vtkStructuredGridReader.h>
#include <vtkRectilinearGridReader.h>
#include <vtkCellLocator.h>
#include "flowTexture.h"
#include <string>

class TCFrame;
class vtkCellDataToPointData;

//////////////////////////////////////////////
//This class takes a vtkdataset and creates //
//an rgba file representing the velocity    //
//field (+/-) and the active scalar         //
//Basically, you give it a dataset          //
//it pumps out rgba files.                  //
//NOTE: The files contain the dimension     //
//as the first line so to read in a normal  //
//reader the first line can be ignored      //
//////////////////////////////////////////////
class VTKDataToTexture{
public:
   VTKDataToTexture();
   VTKDataToTexture(const VTKDataToTexture& v);
   ~VTKDataToTexture();
 
   ////////////////////////////////////////////
   //set the desired resolution of the       //    
   //ouput textures                          //
   //these need to be 2^n compliant for      //
   //textures                                //
   ////////////////////////////////////////////
   void setOutputResolution(int x,int y, int z)
   {
      _resolution[0] = x;
      _resolution[1] = y;
      _resolution[2] = z;
   }

   ///////////////////////////////////////////////////
   //set the file name                              //
   //example:                                       //
   //if vFileName == "vectors"
   //and outDir == "./"
   //output will be "./vectors_p.rgba"     //
   //               "./vectors_n.rgba"     // 
   //representing the positive and negative textures//
   ///////////////////////////////////////////////////
   void setVelocityFileName(const char* vFileName);

   //set the output directory
   void setOutputDirectory(const char* outDir);

   //set the dataset for this converter
   void setDataset(vtkDataSet* dSet);

   //create a dataset from a file
   void createDataSetFromFile(const std::string filename);
  
   //create the textures for this data set
   void createTextures();

   //write out the textures
   //files will be in ./textures
   void writeVelocityTexture(int index);
   void writeScalarTexture(int index);

   void setRectilinearGrid();
   void setStructuredGrid();
   void setUnstructuredGrid();

   //reset the translators internal data
   //used to cleanup between each file translation
   void reset();
   //initialize the translator session for each file set
   void reInit();
   void setParentGUI(TCFrame* parent){_parent = parent;}   
   void setBatchOn(){_isBatch = true;}
   void setBatchOff(){_isBatch = false;}

   ///For transient datasets, tell the translator whether or not
   ///we need to recalculate where valid grid samples are located
   void TurnOnDynamicGridResampling();
   void TurnOffDynamicGridResampling();

   //get the min and max of the velocity magnitude
   //float minVelocityMagnitude(){return _minMagVel;}
   //float maxVelocityMagnitude(){return _maxMagVel;}
   char** getParameterNames( const int numComponents, 
                    const int numParameters );
   int countNumberOfParameters(const int numComponents);
   //equal operator
   VTKDataToTexture& operator=(const VTKDataToTexture& rhs);
protected:
   bool _recreateValidityBetweenTimeSteps;
   bool _madeValidityStructure;
   bool _isBatch;
   bool _isRGrid;
   bool _isUGrid;
   bool _isSGrid;
   bool _initTranslation;
   std::vector<FlowTexture> _velocity;
   std::vector<FlowTexture> _curScalar;
   
   //check to make sure that grid type specified by the
   //user is the actual file type
   void _confirmFileType(const char* fileName);

   //store the info if sampled pts are 
   //inside the computational domain
   void _createValidityTexture();
   //resample the data into a cartesian
   //representation based on the desired
   //resolution and the bbox of the data
   void _resampleData(int dataValueIndex,int isScalar);
   //interpolate the data from the weights
   //returned by vtkCellLocator(Octtree)
   void _interpolateData(vtkGenericCell* cell,
		      double* weights,
		      float* vec,
		      float& scal);

   void _addOutSideCellDomainDataToFlowTexture(int index,int isScalar);
   void _interpolateDataInCell(vtkGenericCell* cell,
		                                    double* weights,
                                          int component,
                                          int scalar);
   void _interpolatePixelData(FlowPointData& data,
                      vtkDataArray* array,
                      double* weights, 
                      int npts,int whichValue);
   void _extractTuplesForVector(vtkIdList* ptIds,vtkDataArray* vector,
                                          int whichVector);
   void _extractTuplesForScalar(vtkIdList* ptIds,vtkDataArray* scalar,
                                          int whichScalar);
   void _updateTranslationStatus(const char* msg);

   char* _cleanUpFileNames();
   
   char* _vFileName;
   char* _outputDir;
   int _resolution[3];
   int _nScalars;
   int _nVectors;
   int _nPtDataArrays;
   unsigned int _curPt;
   std::vector<double*> _scalarRanges;
   std::vector<double*> _vectorRanges;
   std::vector<bool> _validPt;
   char** _scalarNames;
   char** _vectorNames;

   TCFrame* _parent;

   vtkDataSet* _dataSet;
   vtkCellLocator* _cLocator;
   vtkCellDataToPointData* _dataConvertCellToPoint;
   vtkUnstructuredGridReader* _usgrid;
   vtkStructuredGridReader* _sgrid;
   vtkRectilinearGridReader* _rgrid;
};
#endif// _BIV_VTK_TO_TEXTURE_H_
