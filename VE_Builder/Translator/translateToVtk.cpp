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
 * File:          $RCSfile: translateToVtk.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <cstdlib>

#include <vtkPointSet.h>
#include <vtkStructuredGrid.h>
#include <vtkUnstructuredGrid.h>
#include <vtkTransform.h>
#include <vtkTransformFilter.h>

#include "fileIO.h"
#include "viewCells.h"
#include "cleanVtk.h"
#include "readWriteVtkThings.h"

#include "fluentParticles.h"
#include "reiParticles.h"
#include "plot3dReader.h"
#include "starReader.h"
#include "jdMAPReader.h"
#include "enSightGoldReader.h"
#include "ansysReader.h"

// function declarations
extern vtkUnstructuredGrid * avsReader( char * fluentAVSFileName, int debug );
extern vtkStructuredGrid * reiReader( char * reiFileName, int debug );
extern vtkUnstructuredGrid * en7Reader( char * caseFileName, 
                            vtkTransform * transform, int number, int debug );

// AVL FIRE/SWIFT data translator
extern vtkUnstructuredGrid * fireReader( char * geomFile, char * dataFile, int debug );

extern vtkUnstructuredGrid * mfixReader( char * mfixFileName, 
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

char * preprocess( int argc, char *argv[], 
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
      char* path = getenv("VE_SUITE_HOME");
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
                cin >> infilename;
                cin.ignore();
            }
            while ( ! fileIO::isFileReadable( infilename ) );
         }
         else if (type == 2)    // star-cd input
         {
           char * paramFile = fileIO::getReadableFileFromDefault( 
                                            "the parameter file", "star.param" );
           star = new starReader( paramFile );
           star->ReadParameterFile();
           star->SetDebugLevel( debug );
           delete [] paramFile;
         }
         else if (type == 3)    // rei input
         {
            do
            {
                std::cout << "Input rei file: \t";
                std::cout.flush();
                cin >> infilename;
                cin.ignore();
            }
            while ( ! fileIO::isFileReadable( infilename ) );
         }

         else if (type == 4)    // EnSight input
         {
            do
            {
                std::cout << "Input EnSight case file: \t";
                std::cout.flush();
                cin >> infilename;
                cin.ignore();
            }
            while ( ! fileIO::isFileReadable( infilename ) );
         }
         else if (type == 5)    // FIRE/SWIFT 
         {
             do
             {
                 std::cout << "Geometry file name:   \t";
                 std::cout.flush();
                 cin >> fireGeomFileName;
                 cin.ignore();
                 //strcpy(fireGeomFileName,"/home/users/mccdo/jdeere/FIRE/avl-geo.dat");
                 
             }
             while ( ! fileIO::isFileReadable( fireGeomFileName ) );

             do
             {
                 std::cout << "Data file name:\t";
                 std::cout.flush();
                 cin >> fireDataFileName;
                 cin.ignore();
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
              cin >> infilename;
              cin.ignore();
           }
           while ( ! fileIO::isFileReadable( infilename ) );
           std::cout << "input mesh cell dimensions nx, ny, nz: \t";
           std::cout.flush();
           cin >> nx >> ny >> nz;
           cin.ignore();

           std::cout << "input retainEveryNthFrame (1 to write every frame, 2 to write every other frame, etc.): \t";
           std::cout.flush();
           cin >> retainEveryNthFrame;
           cin.ignore();
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
                cin >> infilename;
                cin.ignore();
            }
            while ( !fileIO::isFileReadable( infilename ) );
         }
         else if (type == 12)    // ANSYS *.rst file
         {
            do
            {
                std::cout << "\nANSYS *.rst file: \t";
                std::cout.flush();
                cin >> infilename;
                cin.ignore();
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
         // Get factor to scale the geometry (and vectors)
         std::cout << "\nSelect the geometry/vector scaling:" << std::endl;
         std::cout << "(0) No scaling" << std::endl;
         std::cout << "(1) Custom scaling" << std::endl;
         std::cout << "(2) meters to feet" << std::endl;
         std::cout << "(3) millim to feet" << std::endl;
         std::cout << "(4) inches to feet" << std::endl;
         std::cout << "(5) meters (1:12 scale) to feet" << std::endl;
         scaleIndex = fileIO::getIntegerBetween( 0, 5 );
      }
   }
   
    float geomScale [3] = {1.0f, 1.0f, 1.0f};    // default
    if      ( scaleIndex == 0 ) { /* do nothing */ }
    else if ( scaleIndex == 1 )
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
               cin >> geomScale[i];
               cin.ignore();
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
   else
      std::cerr << "\n!!! Invalid scaleIndex = " << scaleIndex 
                << ": will not scale geometry" << std::endl;

   //if ( debug ) 
      std::cout << "Using geomScale = " << geomScale[0] << std::endl;

   aTransform->Scale( geomScale[0], geomScale[1], geomScale[2] );
/*    
   // Get factor to scale the solution (i.e., velocity)
   std::cout << "\nSelect the solution conversion:" <<std::endl;
   std::cout << "(0) No convert" <<std::endl;
   std::cout << "(1) meter/sec to feet/sec" <<std::endl;
   std::cout << "(2) feet/sec to meter/sec" <<std::endl;
   std::cout << "(3) FORD cylinder" <<std::endl;
   cin >> scaleIndex;

   velConvert = 1.0f;    // default
   if      (scaleIndex==0) velConvert = 1.0f;
   else if (scaleIndex==1) velConvert = 3.28083989501f;
   else if (scaleIndex==2) velConvert = 0.3048f;
   else if (scaleIndex==3) velConvert = 1.0f;
   else    std::cout << "Invalid entry: will not scale the solution" <<std::endl;
*/

   // Transform the geometry 
   int B_trans;
   float rotX, rotY, rotZ;
   float transX, transY, transZ;

#ifndef SJK_TEST
   if ( argc > arg )
   {
      B_trans = atoi( argv[arg] );
      std::cout << "\tB_trans = " << B_trans <<std::endl;
      arg++;
      if ( B_trans )
      {
         rotX = atoi( argv[arg] );
         std::cout << "\trotX = " << rotX <<std::endl;
         arg++;
         rotY = atoi( argv[arg] );
         std::cout << "\trotY = " << rotY <<std::endl;
         arg++;
         rotZ = atoi( argv[arg] );
         std::cout << "\trotZ = " << rotZ <<std::endl;
         arg++;
         transX = atoi( argv[arg] );
         std::cout << "\ttransX = " << transX <<std::endl;
         arg++;
         transY = atoi( argv[arg] );
         std::cout << "\ttransY = " << transY <<std::endl;
         arg++;
         transZ = atoi( argv[arg] );
         std::cout << "\ttransZ = " << transZ <<std::endl;
         arg++;
      }
   }
   else
#endif  //SJK_TEST
   {
      if ( type != 2 )
      {
         std::cout << "\nTransform (translate/rotate) the geometry? (0) No (1) Yes " << std::endl;
         B_trans = fileIO::getIntegerBetween( 0, 1 );

         if ( B_trans )
         {  
            std::cout << "\nTo rotate X (degrees) : ";
            std::cout.flush();
            cin >> rotX;
            cin.ignore();

            std::cout << "To rotate Y (degrees) : ";
            std::cout.flush();
            cin >> rotY;
            cin.ignore();

            std::cout << "To rotate Z (degrees) : ";
            std::cout.flush();
            cin >> rotZ;
            cin.ignore();

            std::cout << "\nTo translate X value : ";
            std::cout.flush();
            cin >> transX;
            cin.ignore();

            std::cout << "To translate Y value : ";
            std::cout.flush();
            cin >> transY;
            cin.ignore();

            std::cout << "To translate Z value : ";
            std::cout.flush();
            cin >> transZ;
            cin.ignore();

            std::cout << std::endl;
         }
      }
      else if ( type == 2 )
      {
         float * rotations = star->GetRotations();
         float * translations = star->GetTranslations();

         rotX = rotations[ 0 ];
         rotY = rotations[ 1 ];
         rotZ = rotations[ 2 ];

         transX = translations[ 0 ];
         transY = translations[ 1 ];
         transZ = translations[ 2 ];
      }
   }

   if ( B_trans )
   {
      aTransform->RotateX( rotX );
      aTransform->RotateY( rotY );
      aTransform->RotateZ( rotZ );
      // Create a translation matrix and concatenate it with the current
      // transformation according to PreMultiply or PostMultiply semantics
      // aTransform->Translate( 0.0f, 0.0f, 5.5f ); // for FORD MODEL
      aTransform->Translate( transX / geomScale[ 0 ], 
                             transY / geomScale[ 1 ], 
                             transZ / geomScale[ 2 ] );
   }
   else 
   {
      aTransform->RotateX( 0.0f );
   }

   char * vtkFileName = NULL;

#ifndef SJK_TEST
   if ( argc > arg )
   {
      number = fileIO::extractIntegerBeforeExtension( infilename );
      char outFile [100];
      sprintf( outFile, "flowdata_%i.vtk", number );
      vtkFileName = new char [ strlen(outFile) + 1 ];
      strcpy( vtkFileName, outFile );
      arg++;
   }
   else
#endif  //SJK_TEST
   {
      if ( type != 2 )
      {
         // Set the output filename
         vtkFileName = fileIO::getWritableFile( "flowdata.vtk" );
         number = -1;
      }
      else if ( type == 2 )
      {
         vtkFileName = star->GetVTKFileName();
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
                << "\t(1)Fluent avs (2)Star-CD (3)REI (4)EnSight (5)FIRE/SWIFT "
                << "(6)REI Particle\n"
                << "\t(7)mfix (8)Fluent Particle Data "
                << "(9)PLOT3D (10)John Deere MAP Data\n"
                << "\t(11)John Deere EnSight Data" << "  " 
                << "(12)ANSYS rst binary data\n"
                << "\t(0)exit" <<std::endl;
      cfdType = fileIO::getIntegerBetween( 0, 12 );
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
   char * vtkFileName = NULL;
   vtkTransform *aTransform = NULL;
   if ( (cfdType != 6) && (cfdType != 8) && (cfdType != 10) )
   {
      aTransform = vtkTransform::New();
      vtkFileName = preprocess( argc, argv, cfdType, aTransform, number );
      if ( vtkFileName == NULL ) 
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
      reader->ReadHeader();
      reader->ReadRSTHeader();
      reader->ReadDOFBlock();
      reader->ReadNodalEquivalencyTable();
      reader->ReadElementEquivalencyTable();
      reader->ReadDataStepsIndexTable();
      reader->ReadTimeTable();
      reader->ReadGeometryTable();
      reader->ReadElementTypeIndexTable();
      reader->ReadNodalCoordinates();
      reader->ReadElementDescriptionIndexTable();
      reader->ReadSolutionDataHeader();
      reader->ReadNodalSolutions();
      pointset = reader->GetUGrid();
   }
/*
   else if (cfdType == 5)        // PIV
   {
      if (readPIV( v, vec )) exit(1);
   }
*/

   if ( debug > 1 ) 
   {
      std::cout << "pointset = " << pointset << std::endl;
      pointset->Print( cout );
      aTransform->Print( cout );
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
         if ( debug ) cout << i << " : " << j << " : "
                 << aTransform->GetMatrix()->GetElement(i,j) << " : "
                 << identity->GetElement(i,j) << endl;

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

      if ( pointset->IsA("vtkUnstructuredGrid") )
      {
         transformedPointset = vtkUnstructuredGrid::New();
         //transformedPointset->DeepCopy( transFilter->GetOutput() );
         transformedPointset->ShallowCopy( transFilter->GetOutput() );
      }
      else if ( pointset->IsA("vtkStructuredGrid") )
      {
         transformedPointset = vtkStructuredGrid::New();
         //transformedPointset->DeepCopy( transFilter->GetOutput() );
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
   }
   else
      answer = 0;
   
   if ( answer == 0 )
   {
      if ( cfdType != 5 ) // NOT sure if it will work on cell centered data
      {
         dumpVerticesNotUsedByCells( transformedPointset );
      }

      // Now write to vtk format...
      writeVtkThing( transformedPointset, vtkFileName, !debug ); // 0=ascii
   }
   else
   {
      dumpVerticesNotUsedByCells( transformedPointset, vtkFileName );
   }

   delete [] vtkFileName;
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

   std::cout << "NOTE : It is suggested that for all StarCD, PLOT3D, and REI" << std::endl;
   std::cout << "       data that you follow up this translation process with" << std::endl;
   std::cout << "       the mergeVertices utility in the VE-Builder bin directory.\n" << std::endl;

   return 0;
}


