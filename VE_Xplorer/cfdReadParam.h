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
 * File:          $RCSfile: cfdReadParam.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_READPARAM_H
#define CFD_READPARAM_H

#include <vector>
#include <string>
#include "cfdGlobalBase.h"

class fileInfo;
class cfdDataSet;
class vtkDataSet;
class cfdCommandArray;
class cfdDCS;

class cfdReadParam : public cfdGlobalBase
{
   public:
      cfdReadParam();
      ~cfdReadParam();

      // compare VjObs_i commandArray with its child's value
      virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );

      // in future, multi-threaded apps will make a copy of VjObs_i commandArray
      virtual void UpdateCommand();
      // Continues to read parameter file based on the current object type
      void ContinueRead( std::ifstream &, unsigned int );

      int   numGeoms;
      int   bmpFile;
      char  bmpFileName[ 100 ];
      char  quatCamFileName[ 100 ];
      double bmpPosition[ 3 ];
      int bmpOrientation;  // 0=X-plane, 1=Y-plane, and 2=Z-plane.
      int   soundFile;
      float isoScale;
      float delta;
      char  textLine[ 256 ];
      int   *guiVal;
      float diameter;

      float scalarBarPos[ 3 ];
      float scalarBarZRot;
      float scalarBarH;
      float scalarBarW;

      //BY YANG for ANIMATED IMG
      char basename[256];
      int frames;
      int ex_x, ex_y;
      int dim;
      double origin[3];
      double spacing[3];
      
      //END of YANG

      // IHCC Model - should be deleted at a later date
      bool ihccModel;
      bool changeGeometry;

      std::vector< fileInfo * > files;
      std::vector< fileInfo * > soundFiles;

      void CreateNewDataSet();
      int GetNumberOfDataSets();
      cfdDataSet * GetDataSet( int i );
      cfdDataSet * GetDataSetWithName( const char * );

      //std::vector< cfdTransientInfo * > transientInfo;
      //std::vector< cfd1DTextInput * > textInput;
   
      //!
      /*!
       Initialize the number of data to load and parallel process.
       By default, using the octree table.
      */

      //void data_read( char * );
     
      //void param_read( std::ifstream & );
     
      //void Vtk( std::ifstream & );
     
      //void Stl( std::ifstream & );
     
      //void set1DText( std::ifstream & );
     
      //void soundData( std::ifstream & );

      //void vrxprConfigFiles( std::ifstream & );

      //get the transient data params
      //void getTransientInfo( std::ifstream& infile );

      int  convertDecimalToBinary( long );
      void convertBinaryToDecimal( int );
      void convertBinaryToArray( int, int );
      //void BMPLoader( std::ifstream &inFile );
      //void readWorldDCS( std::ifstream &inFile );
      //void readScalarBar( std::ifstream &inFile );
      //void IMGReader( std::ifstream &inFile );
      //void quatCamFile( std::ifstream &inFile );

      char * readDirName( std::ifstream &inFile, char * description );
      int readID( std::ifstream &inFile );

      float worldScale[ 3 ];
      float worldTrans[ 3 ];
      float worldRot[ 3 ];

      float imageScale[ 3 ];
      float imageTrans[ 3 ];
      float imageRot[ 3 ];

      cfdDCS* dashBoardDCS;
      std::string dashboardFilename;

      static void read_pf_DCS_parameters( std::ifstream &inFile,
                     float*, float*, float* );

      static void SkipModuleBlock( std::ifstream &inFile, int );

   private:
      std::vector< cfdDataSet * > dataSets;
      std::vector< int > testBin;
      //void LoadSurfaceFiles( char * dir );
};

#endif
