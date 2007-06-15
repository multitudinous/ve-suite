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
#include <iostream>
#include <cstdlib>
#include <sstream>
#include <string>

#include <vtkPointSet.h>
#include <vtkStructuredGrid.h>
#include <vtkUnstructuredGrid.h>
#include <vtkTransform.h>
#include <vtkTransformFilter.h>

#include "VE_Xplorer/Utilities/fileIO.h"
#include "VE_Builder/Translator/viewCells.h"
#include "VE_Xplorer/Utilities/cleanVtk.h"
#include "VE_Xplorer/Utilities/readWriteVtkThings.h"
#include "VE_Xplorer/Utilities/cfdAccessoryFunctions.h"

#include "VE_Builder/Translator/fluentParticles.h"
#include "VE_Builder/Translator/reiParticles.h"
#include "VE_Builder/Translator/plot3dReader.h"
#include "VE_Builder/Translator/starReader.h"
#include "VE_Builder/Translator/jdMAPReader.h"
#include "VE_Builder/Translator/enSightGoldReader.h"
#include "VE_Builder/Translator/ansysReader.h"
#include "VE_Builder/Translator/ansysReader10.h"
#include "VE_Builder/Translator/tecplotReader.h"
using namespace VE_Util;

// function declarations
extern vtkUnstructuredGrid * avsReader( std::string fluentAVSFileName, int debug );
extern vtkStructuredGrid * reiReader( std::string reiFileName, int debug );
extern vtkUnstructuredGrid * en7Reader( std::string caseFileName, 
                            vtkTransform * transform, int number, int debug );

// AVL FIRE/SWIFT data translator
extern vtkUnstructuredGrid * fireReader( std::string geomFile, std::string dataFile, int debug );

extern vtkUnstructuredGrid * mfixReader( std::string mfixFileName, 
                  int nx, int ny, int nz, 
                  int retainEveryNthFrame, vtkTransform * transform, int debug );

int debug = 0;   // 0=no debug output, 1=some debug output, 2=more debug output

char  infilename[100];           // file name for converters requiring a single input file
char  fireGeomFileName[100];     // FIRE/SWIFT cell info
char  fireDataFileName[100];     // FIRE/SWIFT data info
int nx = 1, ny = 1, nz = 1;
int retainEveryNthFrame = 1;
plot3dReader   *plot3d;
starReader     *star;
ansysReader * reader = NULL;
ansysReader10* reader10 = NULL;
tecplotReader * _tecPlotReader = NULL;

std::string preprocess( int argc, char *argv[], 
                   int type, vtkTransform *aTransform, int & number ) 
{
   int B_fname = 0;

#ifndef SJK_TEST
   int arg = 3;
   if ( argc > 2 )
   {
      strcpy( infilename, argv[2] );
      std::cout << "Will read data from: " << infilename <<std::endl;
      if (type == 2) 
      {
         star = new starReader( infilename );
         star->ReadParameterFile();
         star->SetDebugLevel( debug );
      }
   }
   else
   {
#endif //SJK_TEST

#ifdef SJK_TEST
      // set the fluent and star-cd defaults...
      char fullname [256];
      std::string path = getenv("VE_SUITE_HOME");
      //cout << "path is \"" << path << "\"" <<std::endl;
      if(path != NULL)
      {
         strcpy(fullname, path);
         strcat(fullname,"/VE_Builder/Translator/testData/");
      }
      else
      {
         std::cerr << "ERROR: environment variable VE_SUITE_HOME is not defined" << std::endl;
         return NULL;
      }

      if (type == 1)
      {
         strcpy(infilename,"/home/users/sjk60/xlatrTestData/testData/2D.avs");
         //strcpy(infilename,"/home/users/sjk60/xlatr/testInputData/fluent/combineTest.avs");
         //strcpy(infilename,"/home/users/sjk60/xlatr/testInputData/fluent/2scalar_1vec_3d.avs");

         //strcpy(infilename,fullname);
         //strcat(infilename,"2D.avs");
      }
      else if (type == 2) 
      {
         B_fname = 1; // don't have a default star parameter file, set flag to ask for filename
         //strcpy(infilename,"/home/users/sjk60/xlatrTestData/star/star.param6cols");
      }
      else if (type == 3) 
      {
         strcpy(infilename,"/home/users/sjk60/xlatrTestData/testInputData/rei/banffdb_gasifier_raw_data");
      }
      else if (type == 4)
      {
         strcpy(infilename,"/home/users/sjk60/gm/deice/deice/ensight_300steps/Deice_transient_shortGeo.encas");
         //strcpy(infilename,"/home/users/sjk60/vtk/VTKData/Data/EnSight/naca.bin.case");
         //strcpy(infilename,"/home/users/sjk60/gm/engineSpray/en7/trial40-1.case");
      }
      else if ( type == 5 ) 
         B_fname = 1; // don't have a default FIRE/SWIFT input file, set flag to ask for filename
      else if ( type == 7 )
      {
         strcpy(infilename,"/home/users/sjk60/xlatr/mfix/netl/3D_JETS.SP1");
         nx = 30;
         ny = 60;
         nz = 30;
         retainEveryNthFrame = 1;
      }
#else
      if ( type == 1 ) 
         B_fname = 1; // don't have a default fluent AVS input file, set flag to ask for filename
      else if ( type == 2 ) 
      {
         B_fname = 1; // don't have a default star parameter file, set flag to ask for filename
         //strcpy(infilename,"star.param"); // default star parameter file
      }
      else if ( type == 3 ) 
         B_fname = 1; // don't have a default REI input file, set flag to ask for filename
      else if ( type == 4 ) 
         B_fname = 1; // don't have a default EnSight input file, set flag to ask for filename
      else if ( type == 5 ) 
         B_fname = 1; // don't have a default FIRE/SWIFT input file, set flag to ask for filename
      else if ( type == 7 ) 
         B_fname = 1; // don't have a default mfix input file, set flag to ask for filename
      else if ( type == 9 ) 
         B_fname = 1; // don't have a default plot3d input file, set flag to ask for filename
      else if ( type == 11 ) 
         B_fname = 1; // don't have a default ensight filename
      else if ( type == 12 ) 
         B_fname = 1; // don't have a default ANSYS *.rst filename
      else if ( type == 13 ) 
         B_fname = 1; // don't have a default ANSYS *.rst filename
      else if ( type == 14 ) 
         B_fname = 1; // don't have a default ANSYS 10 *.rst filename
#endif  //SJK_TEST

      if ( B_fname == 0 )
      {
         // Does the user want to use the default input filenames?
         if (type == 1)       // fluent AVS input
         {
            std::cout << "By default vertex, cell, and solution data read from " << infilename <<std::endl;
         }
         else if (type == 2)  // star-CD input
         {
            //std::cout << "By default vertex, cell, and solution data read from parameter file " << infilename <<std::endl;
         }
         else if (type == 3)  // REI input
         {
            std::cout << "By default vertex, cell, and solution data read from " << infilename <<std::endl;
         }
         else if (type == 4)  // EnSight
         {
            std::cout << "By default vertex, cell, and solution data read from " << infilename <<std::endl;
         }
         else if (type == 5)  // FIRE/SWIFT
         {
         }
         else if (type == 6)  // REI_Particles
         {
         }
         else if (type == 7)  // mfix input
         {
            std::cout << "By default vertex, cell, and solution data read from " << infilename <<std::endl;
         }
         else if (type == 9)  // REI_Particles
         {
         }
         else if (type == 12)  // ANSYS *.rst file
         {
         }
         else if (type == 13)  // tec plot ascii
         {
         }
         else if ( type == 14 )  // ANSYS *.rst file
         {
            //std::cout<<"Type is :"<<type<<std::endl;
         }
         else
         {
            std::cout << "Invalid type=" << type <<std::endl <<std::endl;
            return NULL;
         } 

         if (type != 2)    // star-cd input
         {
            std::cout << "Answer (0) Use default (1) Change" <<std::endl;
            B_fname = fileIO::getIntegerBetween( 0, 1 );
         }
      } 
      
      if ( B_fname )
      {
         if (type == 1)    // fluent input
         {
            do
            {
                std::cout << "\nFluent *.AVS file: \t";
                std::cout.flush();
                std::cin >> infilename;
                std::cin.ignore();
            }
            while ( !fileIO::isFileReadable( infilename ) );
         }
         else if (type == 2)    // star-cd input
         {
           std::string paramFile = fileIO::getReadableFileFromDefault( 
                                            "the parameter file", "star.param" );

           star = new starReader( paramFile );//star = new starReader( paramFile.c_str() );

           star->ReadParameterFile();

           star->SetDebugLevel( debug );

           //delete [] paramFile;

         }
         else if (type == 3)    // rei input
         {
            do
            {
                std::cout << "Input rei file: \t";
                std::cout.flush();
                std::cin >> infilename;
                std::cin.ignore();
            }
            while ( ! fileIO::isFileReadable( infilename ) );
         }

         else if (type == 4)    // EnSight input
         {
            do
            {
                std::cout << "Input EnSight case file: \t";
                std::cout.flush();
                std::cin >> infilename;
                std::cin.ignore();
            }
            while ( ! fileIO::isFileReadable( infilename ) );
         }
         else if (type == 5)    // FIRE/SWIFT 
         {
             do
             {
                 std::cout << "Geometry file name:   \t";
                 std::cout.flush();
                 std::cin >> fireGeomFileName;
                 std::cin.ignore();
                 //strcpy(fireGeomFileName,"/home/users/mccdo/jdeere/FIRE/avl-geo.dat");
                 
             }
             while ( ! fileIO::isFileReadable( fireGeomFileName ) );

             do
             {
                 std::cout << "Data file name:\t";
                 std::cout.flush();
                 std::cin >> fireDataFileName;
                 std::cin.ignore();
                 //strcpy(fireDataFileName,"/home/users/mccdo/jdeere/FIRE/avl_results.dat");
             }
             while ( ! fileIO::isFileReadable( fireDataFileName ) );
         }

        else if (type == 7)    // mfix input
        {
           do
           {
              std::cout << "mfix *.SP# file: \t";
              std::cout.flush();
              std::cin >> infilename;
              std::cin.ignore();
           }
           while ( ! fileIO::isFileReadable( infilename ) );
           std::cout << "input mesh cell dimensions nx, ny, nz: \t";
           std::cout.flush();
           std::cin >> nx >> ny >> nz;
           std::cin.ignore();

           std::cout << "input retainEveryNthFrame (1 to write every frame, 2 to write every other frame, etc.): \t";
           std::cout.flush();
           std::cin >> retainEveryNthFrame;
           std::cin.ignore();
        }
        else if (type == 9)    // PLOT3D
        {      
           plot3d = new plot3dReader();
           plot3d->GetFileNames();
        }
        else if (type == 11)    
        // John Deere EnSight
        {      
            do
            {
                std::cout << "Note: Be sure that the geo filename specified in \n";
                std::cout << "\t in the case file is not too long. \n";
                std::cout << "Input EnSight case file: \t";
                std::cout.flush();
                std::cin >> infilename;
                std::cin.ignore();
            }
            while ( !fileIO::isFileReadable( infilename ) );
         }
         else if (type == 12)    // ANSYS *.rst file
         {
            do
            {
                std::cout << "\nANSYS *.rst file: \t";
                std::cout.flush();
                std::cin >> infilename;
                std::cin.ignore();
            }
            while ( ! fileIO::isFileReadable( infilename ) );
         }
         else if (type == 13)    // tec plot file
         {
            do
            {
                std::cout << "\nTecPlot ASCII file: \t";
                std::cout.flush();
                std::cin >> infilename;
                std::cin.ignore();
            }
            while ( ! fileIO::isFileReadable( infilename ) );
         }
         else if (type == 14)    // ANSYS 10 *.rst file
         {
            do
            {
                std::cout << "\nANSYS 10 *.rst file: \t";
                std::cout.flush();
                std::cin >> infilename;
                std::cin.ignore();
            }
            while ( ! fileIO::isFileReadable( infilename ) );
         }
      }
#ifndef SJK_TEST
   }
#endif  //SJK_TEST

   unsigned int scaleIndex;

#ifndef SJK_TEST
   // read scale index from the command line
   if ( argc > arg )
   {
      scaleIndex = atoi( argv[arg] );
      std::cout << "\tscaleIndex = " << scaleIndex <<std::endl;
      arg++;
   }
   else
#endif  //SJK_TEST
   {
      if ( type == 2 )
      {
         scaleIndex = star->GetScaleIndex();
      }
      else
      {
/*         // Get factor to scale the geometry (and vectors)
         std::cout << "\nSelect the geometry/vector scaling:" << std::endl;
         std::cout << "(0) No scaling" << std::endl;
         std::cout << "(1) Custom scaling" << std::endl;
         std::cout << "(2) meters to feet" << std::endl;
         std::cout << "(3) millim to feet" << std::endl;
         std::cout << "(4) inches to feet" << std::endl;
         std::cout << "(5) meters (1:12 scale) to feet" << std::endl;
         std::cout << "(6) centimeters to feet" << std::endl;
         scaleIndex = fileIO::getIntegerBetween( 0, 6 );*/
      }
   }
   
    float geomScale [3] = {1.0f, 1.0f, 1.0f};    // default
    if      ( scaleIndex == 0 ) { /* do nothing */ }
/*    else if ( scaleIndex == 1 )
    {

#ifndef SJK_TEST
      // read scale factors from the command line
      if ( argc > arg )
      {
         geomScale[0] = atof( argv[arg] );
         std::cout << "\tgeomScale[0] = " << geomScale[0] <<std::endl;
         arg++;
         geomScale[1] = atof( argv[arg] );
         std::cout << "\tgeomScale[1] = " << geomScale[1] <<std::endl;
         arg++;
         geomScale[2] = atof( argv[arg] );
         std::cout << "\tgeomScale[2] = " << geomScale[2] <<std::endl;
         arg++;
      }
      else
#endif  //SJK_TEST

      {
         if ( type == 2 )
         {
            geomScale[ 0 ] = star->GetScaleFactor();
            geomScale[ 1 ] = geomScale[ 0 ];
            geomScale[ 2 ] = geomScale[ 0 ];
         }
         else
         {
            for (int i=0; i<3; i++)
            {
               std::cout << "input the scale factor for axis " << i << ": ";
               std::cout.flush();
               std::cin >> geomScale[i];
               std::cin.ignore();
            }
         }
      }
   }
   else if ( scaleIndex == 2 )
   {
      geomScale[0] = 3.28083989501f;
      geomScale[1] = 3.28083989501f;
      geomScale[2] = 3.28083989501f;
   }
   else if ( scaleIndex == 3 )
   {
      geomScale[0] = 3.28083989501e-3;
      geomScale[1] = 3.28083989501e-3;
      geomScale[2] = 3.28083989501e-3;
   }
   else if ( scaleIndex == 4 )
   {
      geomScale[0] = 1.0/12.0;
      geomScale[1] = 1.0/12.0;
      geomScale[2] = 1.0/12.0;
   }
   else if ( scaleIndex == 5 )
   {
      geomScale[0] = 12.0*3.28083989501;
      geomScale[1] = 12.0*3.28083989501;
      geomScale[2] = 12.0*3.28083989501;
   }
   else if ( scaleIndex == 6 )
   {
      geomScale[0] = 3.28083989501e-2;
      geomScale[1] = 3.28083989501e-2;
      geomScale[2] = 3.28083989501e-2;
   }
   else
      std::cerr << "\n!!! Invalid scaleIndex = " << scaleIndex 
                << ": will not scale geometry" << std::endl;*/

   //if ( debug ) 
      std::cout << "Using geomScale = " << geomScale[0] << std::endl;

   aTransform->Scale( geomScale[0], geomScale[1], geomScale[2] );

// We no longer allow transforming of datasets as vectors stored in general
// purpose point data field did not get transformed.  A BIG problem during
// rotations.  Solution: use the rotation/translation settings in the
// VE_Suite parameter file.
   
   std::string vtkFileName;// = NULL;

#ifndef SJK_TEST
   if ( argc > arg )
   {
      number = fileIO::extractIntegerBeforeExtension( infilename );
      std::ostringstream dirStringStream;
      dirStringStream << "flowdata_" << number << ".vtk";
      //vtkFileName = new char [ strlen( dirStringStream.str().c_str() ) + 1 ];
      //strcpy( vtkFileName, dirStringStream.str().c_str() );
      //std::string vtkFileName;
      vtkFileName.assign( dirStringStream.str() );
      arg++;
   }
   else
#endif  //SJK_TEST
   {
      if ( type != 2 )
      {
         // Set the output filename
         //std::string vtkFileName;
         vtkFileName.assign( fileIO::getWritableFile( "flowdata.vtk" ));
         number = -1;
      }
      else if ( type == 2 )
      {
         //std::string vtkFileName;
         vtkFileName.assign(star->GetVTKFileName());
      }
   }

   return vtkFileName;    //successfully executed
}

int main( int argc, char *argv[] )
{
   std::cout <<std::endl;
   std::cout << "~-~~-~~-~~-~~-~~-~~-~~-~~-~~-~~-~~-~~-~" <<std::endl;
   std::cout << "~-~   CFD-to-VTK CONVERTOR ROUTINE  ~-~" <<std::endl;
   std::cout << "~-~~-~~-~~-~~-~~-~~-~~-~~-~~-~~-~~-~~-~" <<std::endl;
   std::cout <<std::endl;

   enSightGoldReader* _enSightReader = NULL;
   
   int cfdType;
   if ( argc == 1 )
   {
      std::cout << "Which data type do you want to convert to VTK?\n" 
                << "\t(1)Fluent avs (2)Star-CD (3)REI "
                //<< "(4)EnSight "  //commented out
                << "(5)FIRE/SWIFT "
                << "(6)REI Particle\n"
                << "\t(7)mfix (8)Fluent Particle Data "
                << "(9)PLOT3D (10)John Deere MAP Data\n"
                << "\t(11)John Deere EnSight Data" << "  " 
                << "(12)ANSYS rst binary data\n"
                << "\t(13)Tecplot ASCII files\t"
                << "(14)ANSYS 10 rst binary data\n"
                << "\t(0)exit" <<std::endl;
      cfdType = fileIO::getIntegerBetween( 0, 14 );
   }
   else
   {
      cfdType = atoi( argv[1] );
      //cout << "cfdType = " << cfdType <<std::endl;
   }

   if (cfdType == 0)               
      return 0;

   // Get preprocess settings (CFD filenames, factors) ...
   int number;
   std::string vtkFileName;// = NULL;
   vtkTransform *aTransform;// = NULL;
   if ( (cfdType != 6) && (cfdType != 8) && (cfdType != 10) )
   {
      aTransform = vtkTransform::New();
      vtkFileName = preprocess( argc, argv, cfdType, aTransform, number );
      if ( vtkFileName.empty() )
         return 1;
   }

   // From CFD data, read in the points (vertices), cells, and solutions 
   // and output a vtkPointSet (structured and unstructured grids)
   vtkPointSet * pointset = NULL; 
   if (cfdType == 1)       // fluent
   {
      pointset = avsReader( infilename, debug);
   }
   else if (cfdType == 2)  // starCD
   {
      pointset = star->GetUnsGrid();
   }
   else if (cfdType == 3)  // rei
   {
      pointset = reiReader( infilename, debug );
   }
   else if (cfdType == 4)  // EnSight
   {
      pointset = en7Reader( infilename, aTransform, number, debug );
   }
   else if (cfdType == 5)  // FIRE/SWIFT Translator
   {
      pointset = fireReader( fireGeomFileName, fireDataFileName, debug );
   }
   else if (cfdType == 6)  // REI Particle Data
   {
      reiParticles *particles = new reiParticles();
      delete particles;
      return ( 1 );
   }
   else if (cfdType == 7)  // mfix Data
   {
      pointset = mfixReader( infilename, nx, ny, nz, retainEveryNthFrame,
                             aTransform, debug );
   }
   else if ( cfdType == 8 )
   {
      fluentParticles *particles = new fluentParticles();
      //std::cout<<"declare fluent particles finished"<<std::endl;
      delete particles;
      //std::cout<<"delete fluent particles finished"<<std::endl;
      return ( 1 );
   }
   else if (cfdType == 9)  // PLOT3D Data
   {
      pointset = plot3d->GetUnsGrid();
   }
   else if ( cfdType == 10 )
   {
      jdMAPReader *data = new jdMAPReader();
      //std::cout<<"declare fluent particles finished"<<std::endl;
      delete data;
      //std::cout<<"delete fluent particles finished"<<std::endl;
      return ( 1 );
   }
   else if ( cfdType == 11 )
   {
      _enSightReader = new enSightGoldReader();
      pointset = _enSightReader->GetUnstructuredGrid( infilename, debug );
   }
   else if ( cfdType == 12 )
   {
      reader = new ansysReader( infilename );
      pointset = reader->GetUGrid();
   }

   else if ( cfdType == 13 )
   {
      _tecPlotReader = new tecplotReader();
      pointset = _tecPlotReader->tecplotToVTK( infilename, debug ); 
   }
   else if ( cfdType == 14 )
   {
      reader10 = new ansysReader10( infilename );
      pointset = reader10->GetUGrid();
   }
/*
   else if (cfdType == 5)        // PIV
   {
      if (readPIV( v, vec )) exit(1);
   }
*/

   if ( debug > 1 ) 
   {
      //std::cout << "pointset = " << pointset << std::endl;
      //pointset->Print( std::cout );
      //aTransform->Print( std::cout );
   }
   
   if ( pointset == NULL )
   {
      std::cerr << "\nERROR: pointset is NULL, so exiting.\n" <<std::endl;
      aTransform->Delete();
      exit(1);
   }

   //pointset->Squeeze();     // Reclaim any extra memory used to store data. 
                              // vtk says: THIS METHOD IS NOT THREAD SAFE

   // see if the requested transform is an identity...
   vtkMatrix4x4 * identity = vtkMatrix4x4::New();
   int isIdentity = 1;
   for (int i=0; i<4; i++)
   {
      for (int j=0; j<4; j++)
      {
         if ( debug ) 
            std::cout << i << " : " << j << " : "
                 << aTransform->GetMatrix()->GetElement(i,j) << " : "
                 << identity->GetElement(i,j) << std::endl;

         //if ( fabs( aTransform->GetMatrix()->GetElement(i,j) - 
         //          identity->GetElement(i,j) ) > 0.00001 )
         if ( aTransform->GetMatrix()->GetElement(i,j) !=
              identity->GetElement(i,j) )
         {
            isIdentity = 0;
            break;
         }
      }
   }
   identity->Delete();

   // Only transform if the transform matrix is NOT the identity...
   vtkPointSet * transformedPointset = NULL;
   if ( ! isIdentity )
   {
      std::cout << "Transforming the dataset...\n" << std::endl;
      vtkTransformFilter *transFilter = vtkTransformFilter::New();
      if ( debug > 1 )
         transFilter->DebugOn();

      //transFilter->ReleaseDataFlagOn();
      transFilter->BreakOnError();
      transFilter->SetInput( pointset );
      transFilter->SetTransform( aTransform );
      transFilter->Update();

      // Originally, we used DeepCopy instead of ShallowCopy in the code below.
      // It greatly increased memory requirements and delivered same results.
      if ( pointset->IsA("vtkUnstructuredGrid") )
      {
         transformedPointset = vtkUnstructuredGrid::New();
         transformedPointset->ShallowCopy( transFilter->GetOutput() );
      }
      else if ( pointset->IsA("vtkStructuredGrid") )
      {
         transformedPointset = vtkStructuredGrid::New();
         transformedPointset->ShallowCopy( transFilter->GetOutput() );
      }
      else
      {
         std::cerr << "\nERROR: pointset is not vtkStructuredGrid or "
                   << "vtkUnstructuredGrid so exiting.\n" <<std::endl;
         aTransform->Delete();
         exit(1);
      }

      pointset->Delete();
      transFilter->Delete();
   }
   else
   {
      if ( debug )
         std::cout << "not transforming" << std::endl;
      transformedPointset = pointset;
   }

   aTransform->Delete();

   // The following is a time extensive computation that is better done here
   // rather than at runtime in the virtual environment
   double meanCellBBLength = cfdAccessoryFunctions::
                            ComputeMeanCellBBLength( transformedPointset );

   vtkFloatArray * array = vtkFloatArray::New();
   array->SetName( "meanCellBBLength" );
   array->SetNumberOfComponents( 1 );
   array->SetNumberOfTuples( 1 );
   array->SetTuple1( 0, meanCellBBLength );
   transformedPointset->GetFieldData()->AddArray( array );
   array->Delete();

   int answer;
   if ( cfdType == 2 )
   {
      answer = star->GetWriteOption();
   }
   else if ( transformedPointset->IsA("vtkUnstructuredGrid") )
   {
      std::cout << "\nDo you want to directly write your vtk file to disk?\n"
                << "   input 0 to let vtk write the file\n"
                << "   input 1 to write ascii file directly to disk " 
                << "(Recommended only for troublesome large files)"
                << std::endl;
      answer = fileIO::getIntegerBetween( 0, 1 );
      std::cout << std::endl;
   }
   else
   {
      answer = 0;
   }
   
   if ( answer == 0 )
   {
      if ( cfdType != 5 ) // NOT sure if it will work on cell centered data
      {
         // Clean up...
         VE_Util::dumpVerticesNotUsedByCells( transformedPointset );
      }

      // Now write to vtk format...
      writeVtkThing( transformedPointset, vtkFileName, !debug ); // 0=ascii
   }
   else
   {
      // Clean up and directly write to vtk ascii format...
      dumpVerticesNotUsedByCells( transformedPointset, vtkFileName );
   }

   //delete [] vtkFileName;
   transformedPointset->Delete();

   if ( plot3d != NULL )
   {
      delete plot3d;
   }
   else if ( star != NULL )
   {
      delete star;
   }
   else if ( _enSightReader != NULL )
   {
      delete _enSightReader;
   }
   else if ( reader != NULL  )
   {
      delete reader;
   }

   std::cout << "NOTE:\tIt is suggested that for all StarCD, PLOT3D, and REI"
             << "\n\tdata that you follow up this translation process with"
             << "\n\tthe mergeVertices utility in the VE-Builder bin directory."
             << "\n" << std::endl;

   return 0;
}

