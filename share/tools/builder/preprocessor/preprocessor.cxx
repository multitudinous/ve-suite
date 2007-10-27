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
// A program to preprocess the vtk files
//
// Also, decompose the vtk file format into octree-based 
// datasets for fast streamline integration.

#include <iostream>
#include <cassert>

#include "STAR_octree.h"
#include "STAR_isosurface.h"
#include "STAR_surface.h"
#include "multiPlaneVtkOutput.h"
#include <ves/xplorer/util/fileIO.h>
#include <ves/xplorer/util/readWriteVtkThings.h>

#ifdef _MPI
#include "mpi.h"
#endif   
using namespace ves::xplorer::util;

int main( int argc, char *argv[] )
{
   int  noOfCellsInOctant, isoNum, xCut, yCut, zCut;
   float deciVal, isoMin, isoMax;
   char surfFname[100], paramFile[100];

   bool B_octree = false, B_surface = false, B_iso = false, B_cont = false;
   bool B_surfFname = true, B_isoNum = true;

   int cutPlanesOption = 1;
   int multiPlaneOption;
   int myid;
#ifdef _MPI
   int xprocs, yprocs, zprocs, procs_remainder, numprocs;
   MPI_Init(&argc, &argv);
   MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
   MPI_Comm_rank(MPI_COMM_WORLD, &myid);
   //MPI_Status status;
   //std::cout << myid << " : " << numprocs << " : " << postDataDir << std::endl;
#else
   myid = 0;
#endif   

   std::string postFname;// = NULL;
   std::string postDataDir;// = NULL;

   if ( myid == 0 )
   {
      int arg = 1;
      if ( argc > arg )
      {
         //postFname = new char [ strlen(argv[ arg ])+1 ];
         postFname.assign( argv[ arg ] );//strcpy( postFname, argv[ arg ] );
         ++arg;
         
         if ( ! fileIO::isFileReadable( postFname ) )
         {
            std::cerr << "\nERROR: Can't read file \"" << postFname
                      << "\" " << std::endl;
            //delete [] postFname;
            postFname = fileIO::getReadableFileFromDefault( 
                           "the input vtk data file", "./flowdata.vtk" );
         }
      }
      else
      {
         postFname = fileIO::getReadableFileFromDefault( 
                           "the input vtk data file", "./flowdata.vtk" );
      }

      if ( argc > arg )
      {
         //postDataDir = new char [ strlen(argv[ arg ])+1 ];
         postDataDir.assign( argv[ arg ] );//strcpy( postDataDir, argv[ arg ] );
         arg++;

         if ( ! fileIO::isDirWritable( postDataDir ) )
         {
            std::cerr << "\nERROR: Can't write to \"" << postDataDir
                      << "\" directory" << std::endl;
            //delete [] postDataDir;
            std::string tmp = fileIO::getWritableDir();
            //postDataDir = new char [strlen(tmp)+1];
            postDataDir.assign( tmp );//strcpy( postDataDir, tmp );
         }
      }
      else
      {
         std::cout << "\nA directory is needed to hold precomputed data"
                   << std::endl;
         std::string tmp = fileIO::getWritableDir();
         //postDataDir = new char [strlen(tmp)+1];
         postDataDir.assign( tmp );//strcpy( postDataDir, tmp );
      }

      if ( argc > arg )
      {
		  B_octree = (atoi( argv[ arg ] ))?true:false;
         arg++;
      }
      else
      {
         std::cout << "\nDecompose octrees?  (0)No (1)Yes" << std::endl;
		 B_octree = (fileIO::getIntegerBetween( 0, 1 ))?true:false;
      }

      if ( B_octree )
      {
         std::cout << "Number of cells for each octant : ";
         std::cout.flush();
         std::cin >> noOfCellsInOctant;
      }

      if ( argc > arg )
      {
         B_surface = (atoi( argv[ arg ] ))?true:false;
         arg++;
      }
      else
      {
         std::cout << "\nExtract surfaces?  (0)No (1)Yes" << std::endl;
         B_surface = (fileIO::getIntegerBetween( 0, 1 ))?true:false;
      }

      if ( B_surface )
      {
         std::cout << "Decimation value (range from 0.0 to 1.0 ) : ";
         std::cout.flush();
         std::cin >> deciVal;

         std::cout << "\nFilename to output?" << std::endl;
         std::cout << "By default : " << postDataDir << "/surface.vtk" << std::endl;
         std::cout << "Answer (0) Use default (1) Change" << std::endl;
         B_surfFname = (fileIO::getIntegerBetween( 0, 1 ))?true:false;

         if ( B_surfFname )
         {
            std::cout << "Enter file name (including path if desired): ";
            std::cout.flush();
            std::cin >> surfFname;
         }
         else
         {
            strcpy( surfFname, postDataDir.c_str() );
            strcat( surfFname, "/surface.vtk" );
         }
         std::cout << "Will write to file " << surfFname << std::endl;
      }

      if ( argc > arg )
      {
         B_iso = (atoi( argv[ arg ] ))?true:false;
         arg++;
      }
      else
      {
         std::cout << "\nExtract isosurfaces? (0)No (1)Yes" << std::endl;
         B_iso = (fileIO::getIntegerBetween( 0, 1 ))?true:false;
      }

      if ( B_iso )
      {
         std::cout << "\nNumber of isosurface(s)?" << std::endl;
         std::cout << "Answer (0) Use default of 10 (1) Change" << std::endl;
         B_isoNum = (fileIO::getIntegerBetween( 0, 1 ))?true:false;

         if ( B_isoNum )
         {
            std::cout << "No. of isosurfaces : ";
            std::cout.flush();
            std::cin >> isoNum;
         }
         else 
         {
            isoNum = 10;
         }

         std::cout << "Range of isosurface?" << std::endl;
         std::cout << "Answer (0) Use default automatic range (1) Set" << std::endl;
         int B_range = fileIO::getIntegerBetween( 0, 1 );

         if ( B_range )
         {
            std::cout << "Min range value : ";
            std::cout.flush();
            std::cin >> isoMin;

            std::cout << "Max range value : ";
            std::cout.flush();
            std::cin >> isoMax;
         }
         else
         {
            isoMin = -0.1f;
            isoMax = -0.1f;
         }
      }

      if ( argc > arg )
      {
         B_cont = (atoi( argv[ arg ] ))?true:false;
         arg++;
      }
      else
      {
         std::cout << "\nExtract cutting planes? (0)No (1)Yes" << std::endl;
         B_cont = (fileIO::getIntegerBetween( 0, 1 ))?true:false;
      }

      if ( B_cont )
      {
         if ( argc > arg )
         {
            cutPlanesOption = atoi( argv[ arg ] );
            arg++;
         }
         else
         {
            std::cout << "\nSet the number of cuts?" << std::endl;
            std::cout << "By default : Number of X,Y,Z cuts = 20" << std::endl;
            std::cout << "Answer (0) Use default (1) Set (2) Specify text file" << std::endl;
            cutPlanesOption = fileIO::getIntegerBetween( 0, 2 );
         }
         if ( cutPlanesOption == 0 )
         {
            xCut = yCut = zCut = 20;
         }
         else if ( cutPlanesOption == 1 )
         {
            if ( argc > arg )
            {
               xCut = atoi( argv[ arg ] );
               arg++;
               yCut = atoi( argv[ arg ] );
               arg++;
               zCut = atoi( argv[ arg ] );
               arg++;
            }
            else
            {
               std::cout << "Number of X cut : ";
               std::cout.flush();
               std::cin >> xCut;

               std::cout << "Number of Y cut : ";
               std::cout.flush();
               std::cin >> yCut;

               std::cout << "Number of Z cut : ";
               std::cout.flush();
               std::cin >> zCut;
            }
         }
         else if ( cutPlanesOption == 2 )
         {
            if ( argc > arg )
            {
               strcpy( paramFile, argv[ arg ] );
               //std::cout << "Using parameter file: " << paramFile << std::endl;
               arg++;
            }
            else
            {
               std::cout << "Enter text file name: ";
               std::cout.flush();
               std::cin >> paramFile;
            }
         }
         else 
         {
            xCut = yCut = zCut = 20;
         }
      }

      multiPlaneOption = 0;
      if ( argc > arg )
      {
         multiPlaneOption = atoi( argv[ arg ] );
         arg++;
      }
      else if ( B_cont )
      {
         std::cout << "\nDo you want to concatenate multiple planes into "
            << "one polydata object (used with transient data): " << std::endl;
         std::cout << "Answer (0) No - Keep them separate" << std::endl;
         std::cout << "       (1) Yes - concatenate multiple planes into one polydata object" << std::endl;
         multiPlaneOption = fileIO::getIntegerBetween( 0, 1 );
      }
#ifdef _MPI
      int sumCuts = xCut + yCut + zCut;
      //float sum = (float(xCut)/(float( sumCuts )));
      //std::cout << sum << std::endl;
      //std::cout << sum << std::endl;
      xprocs = (float(xCut)/(float( sumCuts )))*float(numprocs);
      //sum = (float(yCut)/(float( sumCuts )));
      //std::cout << sum << std::endl;
      //std::cout << sum << std::endl;
      yprocs = (float(yCut)/(float( sumCuts )))*float(numprocs);
      //sum = (float(zCut)/(float( sumCuts )));
      //std::cout << sum << std::endl;
      zprocs = (float(zCut)/(float( sumCuts )))*float(numprocs);
      procs_remainder = numprocs - xprocs - yprocs - zprocs;
      //std::cout << " procs_remainder = " <<  procs_remainder << std::endl;
      if(procs_remainder == 2)
      {
         xprocs += 1;
         yprocs += 1;
      }
      else if(procs_remainder == 1)
         xprocs += 1;
      assert( sumCuts >= numprocs );
      assert( xCut > 0 && yCut > 0 && zCut > 0 );
//std::cout << xCut << " : " << yCut << " : " << zCut << std::endl 
//std::cout << xprocs << " : " << yprocs << " : " << zprocs << std::endl;
#endif

   }   

#ifdef _MPI
   std::cout << myid << " : " << numprocs << std::endl;
   MPI_Bcast(postFname,strlen(postFname),MPI_CHAR,0,MPI_COMM_WORLD);
   MPI_Bcast(postDataDir,strlen(postDataDir),MPI_CHAR,0,MPI_COMM_WORLD);
   MPI_Bcast(&surfFname[0],100,MPI_CHAR,0,MPI_COMM_WORLD);
   MPI_Bcast(&paramFile[0],100,MPI_CHAR,0,MPI_COMM_WORLD);
   MPI_Bcast(&B_cont,1,MPI_INT,0,MPI_COMM_WORLD);

   MPI_Bcast(&deciVal,1,MPI_FLOAT,0,MPI_COMM_WORLD);
   MPI_Bcast(&isoMin,1,MPI_FLOAT,0,MPI_COMM_WORLD);
   MPI_Bcast(&isoMax,1,MPI_FLOAT,0,MPI_COMM_WORLD);
   MPI_Bcast(&noOfCellsInOctant,1,MPI_INT,0,MPI_COMM_WORLD);
   MPI_Bcast(&isoNum,1,MPI_INT,0,MPI_COMM_WORLD);
   MPI_Bcast(&xCut,1,MPI_INT,0,MPI_COMM_WORLD);
   MPI_Bcast(&yCut,1,MPI_INT,0,MPI_COMM_WORLD);
   MPI_Bcast(&zCut,1,MPI_INT,0,MPI_COMM_WORLD);
   MPI_Bcast(&xprocs,1,MPI_INT,0,MPI_COMM_WORLD);
   MPI_Bcast(&yprocs,1,MPI_INT,0,MPI_COMM_WORLD);
   MPI_Bcast(&zprocs,1,MPI_INT,0,MPI_COMM_WORLD);
#endif

   // Reading flowdata for further processes
   std::cout << "Reading vtk data file \"" << postFname << "\"" << std::endl;
   //int number = fileIO::extractIntegerBeforeExtension( postFname );
   //std::cout << "number = " << number << std::endl;
   ///This will need to be changed to handle both vtkDataset and vtkMultigroupDataSet
   vtkDataSet* dataSet = dynamic_cast<vtkDataSet*>(readVtkThing( postFname, 0 ));
   
#ifdef _MPI
   MPI_Bcast(&number,1,MPI_INT,0,MPI_COMM_WORLD);
   MPI_Bcast(&multiPlaneOption,1,MPI_INT,0,MPI_COMM_WORLD);
   std::cout << myid << " : " << numprocs << " : " << xprocs << " : " << yprocs << " : " << zprocs << " : " << number << std::endl;
#endif


   if ( dataSet->GetNumberOfCells() > 0 )
   {
      if ( myid == 0 )
      {
         // Input flow data to be decomposed into octree-based datasets
         if ( B_octree )
         {
            // Needs to be converted to take any data set
            /*std::cout << "\nProcess.......................octree decomposition" << std::endl;
            std::cout << "Number of cells in octant = " 
                      << noOfCellsInOctant << std::endl;
            Octree *octree = new Octree;
            octree->InitOctreeDecomposition( dataSet, 
                                             noOfCellsInOctant );*/
         }

         // Extracting surface geometry for display
         if ( B_surface )
         {
            std::cout << "\nProcess.........................surface extraction" << std::endl;
            std::cout << "Decimation value = " << deciVal << std::endl;
            std::cout << "Output filename = " << surfFname << std::endl;
            surfaceVtkOutput *surfaceOutput = new surfaceVtkOutput;
            surfaceOutput->writeSurface( dataSet, 
                                         deciVal, surfFname );
         }

         // Extracting isosurface
         if ( B_iso )
         {
            std::cout << "\nProcess......................isosurface extraction" << std::endl;
            //std::cout << "Number of isosurfaces = " << isoNum << std::endl;
            //std::cout << "Output to " << postDataDir << " directory" << std::endl;
            isosurfaceVtkOutput *isoOutput = new isosurfaceVtkOutput;
            isoOutput->writeIsosurface( dataSet, postDataDir.c_str(), 
                                        isoNum, isoMin, isoMax );
         }
      }
      
      // Extracting precalculated multi-plane data
      if ( B_cont )
      {
         if ( cutPlanesOption != 2 ) 
         {
            std::cout << "\nProcess................multi-plane data extraction" << std::endl;
            std::cout << "Number of X cut = " << xCut << std::endl;
            std::cout << "Number of Y cut = " << yCut << std::endl;
            std::cout << "Number of Z cut = " << zCut << std::endl;
            std::cout << "Output to " << postDataDir << " directory" << std::endl;
            multiPlaneVtkOutput * contOutput = new multiPlaneVtkOutput( postDataDir );
#ifdef _MPI
            std::cout << myid << " : " << numprocs << " : " << xprocs << " : " << yprocs << " : " << zprocs << " : " << number << std::endl;
            contOutput->writeMultiPlanes( dataSet, 
                                          xCut, yCut, zCut, 
                                          multiPlaneOption, number, myid, 
                                          xprocs, yprocs, zprocs, numprocs );
#else
            contOutput->writeMultiPlanes( dataSet, 
                                          xCut, yCut, zCut,
                                          multiPlaneOption, 0 );
                                          //multiPlaneOption, number );number was always == to 0 previously
#endif
         }
         else
         {
            multiPlaneVtkOutput * contOutput = new multiPlaneVtkOutput( postDataDir );
            contOutput->readParamFileandWriteMultiPlanes( 
                                        dataSet, paramFile, 
                                        multiPlaneOption, 0 );
                                        //multiPlaneOption, number );number was always == to 0 previously
         }
      }
      //delete [] postDataDir;
   }
   else
   {
      std::cerr << "Error!!!! No cells in " << postFname << std::endl; 
   }
   dataSet->Delete();
   //delete [] postFname; 
#ifdef _MPI
   MPI_Finalize();
#endif
   return 0;
}
