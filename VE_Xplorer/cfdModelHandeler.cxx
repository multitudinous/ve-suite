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
 * File:          $RCSfile: cfdModel.cxx,v $
 * Date modified: $Date: 2004-05-18 13:44:18 -0700 (Tue, 18 May 2004) $
 * Version:       $Rev: 382 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdModelHandeler.h"

#include "cfdDCS.h"
#include "cfdGroup.h"
#include "cfdObjects.h"
#include "cfdDataSet.h"
#include "fileIO.h"
#include "cfdModel.h"
#include "cfdVectorBase.h"
#include "cfdCommandArray.h"
#include "cfdEnum.h"
#include "cfdReadParam.h"
#include "cfdFILE.h"
#include "cfdScalarBarActor.h"

#ifndef _WIN32 // not windows
#include <unistd.h>
#include <sys/types.h>
#include <sys/dir.h>
#else // it is windows
#include <windows.h>
#include <direct.h>
#endif

#include <string>
#include <cstdio>

#include <vtkPolyDataReader.h>
#include <vtkPolyData.h>

#include <vrj/Util/Debug.h>

cfdModelHandler::cfdModelHandler( char* input, cfdDCS* dcs)
{
   _param = input;
   worldNode = dcs;
   activeDataset = NULL;
   _scalarBar = NULL;
   _readParam = new cfdReadParam( NULL );
   commandArray = NULL;
   // worldnode getting passed in to model
   // model will then add its own node to the tree
   if ( worldNode == NULL )
   {
      cerr << " ERROR : Must intialize worldDCS for creating cfdModelHandler. " << endl;
      exit( 1 );
   }
   _modelList.push_back( new cfdModel( worldNode ) );
   this->CreateObjects();
}

cfdModelHandler::~cfdModelHandler()
{
   delete _scalarBar;
}

///////////////////////
// Helper functions
///////////////////////
void cfdModelHandler::SetCommandArray( cfdCommandArray* input )
{
   commandArray = input;
}

cfdObjects* cfdModelHandler::GetActiveSequence( void )
{
   return activeSequenceObject;
}

cfdDataSet* cfdModelHandler::GetActiveDataSet( void )
{
   return activeDataset;
}
///////////////////////

void cfdModelHandler::InitScene( void )
{
   // Locate and load the arrow file...
   char * arrowFile = fileIO::GetFile( "arrow", "/VE_Xplorer/data/arrow.vtk" );
   if ( arrowFile == NULL )
   {
      std::cout << " ERROR : The vtkPolyData arrow file could not be found "
                << "        Please create one and put it in the data dir "
                << std::endl;
      exit( 1 );
   }

   vtkPolyDataReader * arrowReader = vtkPolyDataReader::New();
   arrowReader->SetFileName( arrowFile );
   arrowReader->Update();

   this->arrow = vtkPolyData::New();
   this->arrow->ShallowCopy( arrowReader->GetOutput() );
   arrowReader->Delete();
   delete [] arrowFile;

   for ( unsigned int j = 0; j < _modelList.size(); j++ )
      for ( unsigned int i = 0; i < _modelList.at( j )->GetNumberOfCfdDataSets(); i++)
      {
         std::cout << "|   Loading data for file " 
                << _modelList.at( j )->GetCfdDataSet( i )->GetFileName()
                << std::endl;
         _modelList.at( j )->GetCfdDataSet( i )->LoadData();
         _modelList.at( j )->GetCfdDataSet( i )->SetArrow( this->arrow );
      }

   // set default active dataset to be the meshed volume
   if ( _modelList.at( 0 )->GetNumberOfCfdDataSets() > 0 )
      activeDataset = _modelList.at( 0 )->GetCfdDataSet( 0 );

   if ( activeDataset != NULL )
   {
      // Fix this later - we need to check and see if this is already 
      // done in cfdDataSet upon initialization
      // set first scalar active
      activeDataset->SetActiveScalar( 0 );
      strcpy( oldDatasetName, activeDataset->GetFileName() );

      cfdVectorBase::SetThreshHoldPercentages( 0, 100 );
      cfdVectorBase::UpdateThreshHoldValues();
      cfdVectorBase::SetVectorRatioFactor( 1 );
   }

   // Create Scalar bar
   _scalarBar = new cfdScalarBarActor( _param, (cfdGroup*)worldNode->GetParent( 0 ) );
   _scalarBar->RefreshScalarBar();
}

void cfdModelHandler::PreFrameUpdate( void )
{
   bool updateScalarRange = false;
   
   if ( commandArray == NULL )
   {
      cerr << " ERROR : commandArray not set fot cfdModelHandler " << endl;
      exit( 1 );
   }

   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_STEADYSTATE_DATASET )
   {
      unsigned int i = commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      vprDEBUG(vprDBG_ALL,1) 
         << "CHANGE_STEADYSTATE_DATASET " << i
         //<< ", scalarIndex = " << this->cfdSc
         //<< ", min = " << this->cfdMin 
         //<< ", max = " << this->cfdMax
         << std::endl << vprDEBUG_FLUSH;
   // Need to add some sort of model reference in later fix later
   //for ( unsigned int j = 0; j < _modelList.size(); j++ )
      unsigned int j = 0;
      
      if ( i < _modelList.at( j )->GetNumberOfCfdDataSets() )
      {
         vprDEBUG(vprDBG_ALL,0) << " dataset = "
            << _modelList.at( j )->GetCfdDataSet( i )->GetFileName()
            << ", dcs = " << _modelList.at( j )->GetCfdDataSet( i )->GetDCS()
            << std::endl << vprDEBUG_FLUSH;

         int cfdType = _modelList.at( j )->GetCfdDataSet( i )->GetType();
         vprDEBUG(vprDBG_ALL,1) << " cfdType: " << cfdType
                                << std::endl << vprDEBUG_FLUSH;

         // set the dataset as the appropriate dastaset type
         // (and the active dataset as well)
         activeDataset = _modelList.at( j )->GetCfdDataSet( i );         
         
         vprDEBUG(vprDBG_ALL,1) << "last active dataset name = " 
                                << oldDatasetName
                                << std::endl << vprDEBUG_FLUSH;

         vprDEBUG(vprDBG_ALL,1) << "Activating steady state file " 
                   << activeDataset->GetFileName()
                   << std::endl << vprDEBUG_FLUSH;

         // make sure that the user did not just hit same dataset button
         // (or change scalar since that is routed through here too)
         if ( strcmp( oldDatasetName, activeDataset->GetFileName() ) )
         {
            vprDEBUG(vprDBG_ALL,1) << " setting dataset as newly activated" 
                                   << std::endl << vprDEBUG_FLUSH;
            activeDataset->SetNewlyActivated();
            strcpy( oldDatasetName, activeDataset->GetFileName() );
         }

         // update scalar bar for possible new scalar name
         updateScalarRange = true;
      }
      else
      {
         std::cerr << "ERROR: requested steady state dataset " 
                   << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) << " must be less than " 
                   << _modelList.at( j )->GetNumberOfCfdDataSets()
                   << std::endl;
      }
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_VECTOR )
   { 
      int vectorIndex = commandArray->GetCommandValue( cfdCommandArray::CFD_SC );
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_VECTOR, vectorIndex = " << vectorIndex
                             << std::endl << vprDEBUG_FLUSH;

      activeDataset->SetActiveVector( vectorIndex );
      activeDataset->GetParent()->SetActiveVector( vectorIndex );
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == UPDATE_GEOMETRY )
   {
      vprDEBUG(vprDBG_ALL,1)
         << commandArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE ) << std::endl << vprDEBUG_FLUSH;

      long int test = this->_readParam->convertDecimalToBinary( 
                        commandArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE ) );
      vprDEBUG(vprDBG_ALL,1)
         << " test : " << test << std::endl << vprDEBUG_FLUSH;

      this->_readParam->convertBinaryToArray( test, _modelList.at( 0 )->GetNumberOfGeomDataSets() );
      // Update Scene Graph with selected or deselected geometry
      vprDEBUG(vprDBG_ALL,0) << " Change Geometry in Scene Graph."
                             << std::endl << vprDEBUG_FLUSH;

      for( unsigned int i = 0; i < _modelList.at( 0 )->GetNumberOfGeomDataSets(); i++ )
      {
         int temp = 0;
         //vprDEBUG(vprDBG_ALL,1)
         //   << "guiVal[ " << q << " ] = " << this->_readParam->guiVal[ q ] 
         //   << std::endl
         //   << vprDEBUG_FLUSH;

         if ( ( this->_readParam->guiVal[ i ] == 1 ) && 
              ( this->worldNode->SearchChild( (cfdSceneNode*)_modelList.at( 0 )->GetGeomDataSet( i )->getpfDCS() ) == -1 ) )
         {
            temp = this->worldNode->AddChild( (cfdSceneNode*)_modelList.at( 0 )->GetGeomDataSet( i )->getpfDCS() );
         }
         else if ( ( this->_readParam->guiVal[ i ] == 0 ) &&
                   ( this->worldNode->SearchChild( (cfdSceneNode*)_modelList.at( 0 )->GetGeomDataSet( i )->getpfDCS() ) != -1 ) )
         {
            temp = this->worldNode->RemoveChild( (cfdSceneNode*)_modelList.at( 0 )->GetGeomDataSet( i )->getpfDCS() );
         }
         vprDEBUG(vprDBG_ALL,1) << "|   Add Child Error  " << temp 
                              << std::endl << vprDEBUG_FLUSH;
      }
   }
   // This represents all viz options. If more viz options are added these enums probably need to be changed. 
   // We should probably come up with a better way to do this.
   else if ( ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) >= CONTOUR ) && 
             ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) <= PARTICLE_TRANSIENT ) )
   {
      // for the active model, change opaque geometries to transparent
      for ( unsigned int i = 0; i < _modelList.at( 0 )->GetNumberOfGeomDataSets(); i++ )
      {
         if ( _modelList.at( 0 )->GetGeomDataSet( i )->GetTransparentFlag() == 1 )
         {
            vprDEBUG(vprDBG_ALL,2) << "Changing Transparency for geom : "
                                   << i << std::endl << vprDEBUG_FLUSH;
            _modelList.at( 0 )->GetGeomDataSet( i )->setOpac( 0.2 );
         }
      }
   }  //change the model's property
   
   // Can't be an else if because may have to update if dataset has changed beforehand
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SCALAR || 
        commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SCALAR_RANGE ||
        updateScalarRange
      )
   { 
      int scalarIndex = commandArray->GetCommandValue( cfdCommandArray::CFD_SC );
      vprDEBUG(vprDBG_ALL,1) << "CHANGE_SCALAR || CHANGE_SCALAR_RANGE"
         << ", scalarIndex = " << scalarIndex
         << ", min = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MIN )
         << ", max = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MAX )
         << std::endl << vprDEBUG_FLUSH;

      activeDataset->SetActiveScalar( scalarIndex );
      activeDataset->GetParent()->SetActiveScalar( scalarIndex );

      activeDataset->ResetScalarBarRange( 
                           commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ), 
                           commandArray->GetCommandValue( cfdCommandArray::CFD_MAX ) );
      activeDataset->GetParent()->ResetScalarBarRange( 
                           commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ), 
                           commandArray->GetCommandValue( cfdCommandArray::CFD_MAX ) );
      // Fix this if we update scalar we need to update the scalar bar
      // very important
   }

   // Check and see if we need to refresh the scalar bar
   _scalarBar->CheckCommandId( commandArray );
   // May use in the future
   //_scalarBar->UpdateCommand();
}

///////////////////////////////////////////////
// Used to initialize data for the simulation
///////////////////////////////////////////////
void cfdModelHandler::CreateObjects( void )
{  
   int numObjects;
   char text[ 256 ];
   char textLine[ 256 ];
   std::ifstream input;
   input.open( this->_param );
   input >> numObjects; 
   input.getline( text, 256 );   //skip past remainder of line

   vprDEBUG(vprDBG_ALL,1) << " Number of Obejcts in Interactive Geometry : " << numObjects << std::endl  << vprDEBUG_FLUSH;
   for( int i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      vprDEBUG(vprDBG_ALL,1) << "Id of object in Interactive Geometry : " << id << std::endl << vprDEBUG_FLUSH;
      input.getline( text, 256 );   //skip past remainder of line
      if ( id == 8 )
      {
         // Assume only one model for now
         // Flexibilty to have multiply models
         _modelList.at( 0 )->CreateCfdDataSet();

         vprDEBUG(vprDBG_ALL,0) << " ************************************* "
                          << std::endl << vprDEBUG_FLUSH;

         vprDEBUG(vprDBG_ALL,0) << " vtk DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;

         float scale[3], trans[3], rotate[3];   // pfDCS stuff
         this->_readParam->read_pf_DCS_parameters( input, scale, trans, rotate);

         // Pass in -1 to GetCfdDataSet to get the last dataset added
         _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()->SetScaleArray( scale );
         _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()->SetTranslationArray( trans );
         _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()->SetRotationArray( rotate );

         // get vtk data set name...
         char vtk_filein[ 256 ];
         input >> vtk_filein;
         input.getline( textLine, 256 );   //skip past remainder of line

         if (fileIO::isFileReadable( vtk_filein ) ) 
         {
            vprDEBUG(vprDBG_ALL,0) << " vtk file = " << vtk_filein 
                             << ", dcs = "  << _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()
                             << std::endl << vprDEBUG_FLUSH;
            _modelList.at( 0 )->GetCfdDataSet( -1 )->SetFileName( vtk_filein );
         }
         else
         {
            std::cerr << "ERROR: unreadable vtk file = " << vtk_filein 
                        << ".  You may need to correct your param file."
                        << std::endl;
            exit(1);
         }

         char * precomputedDataSliceDir = _readParam->readDirName( input, "precomputedDataSliceDir" );
         _modelList.at( 0 )->GetCfdDataSet( -1 )->SetPrecomputedDataSliceDir( precomputedDataSliceDir );
         delete [] precomputedDataSliceDir;
         precomputedDataSliceDir = NULL;

         char * precomputedSurfaceDir = _readParam->readDirName( input, "precomputedSurfaceDir" );
         _modelList.at( 0 )->GetCfdDataSet( -1 )->SetPrecomputedSurfaceDir( precomputedSurfaceDir );
         delete [] precomputedSurfaceDir;
         precomputedSurfaceDir = NULL;

         this->LoadSurfaceFiles( _modelList.at( 0 )->GetCfdDataSet( -1 )->GetPrecomputedSurfaceDir() );
      }
      else if ( id == 9 ) // if it is an geom file
      {
         char fileName[100];
         float stlColor[3];
         int color;
         int transFlag;

         input >> transFlag;
         input.getline( textLine, 256 );   //skip past remainder of line
         vprDEBUG(vprDBG_ALL,0) << " geometry transparency flag = "
                                 << transFlag
                                 << std::endl << vprDEBUG_FLUSH;

         // read color flag
         input >> color;
         vprDEBUG(vprDBG_ALL,0) << " stl color flag = " << color
                          << std::endl << vprDEBUG_FLUSH;

         // read color if color flag = 1
         if( color == 1)
         {
            for(int i=0;i<3;i++)
            {
               input >> stlColor[ i ];
            }
            vprDEBUG(vprDBG_ALL,0) << "\tcolor: " << stlColor[ 0 ] << " : " << stlColor[ 1 ] << " : "
                                    << stlColor[ 2 ]
                                    << std::endl << vprDEBUG_FLUSH;
         }
         input.getline( textLine, 256 );   //skip past remainder of line

         vprDEBUG(vprDBG_ALL,0) << " geometry DCS parameters:" 
                          << std::endl << vprDEBUG_FLUSH;
         float scale[3], trans[3], rotate[3];   // pfDCS stuff
         this->_readParam->read_pf_DCS_parameters( input, scale, trans, rotate);


         input >> fileName;
         input.getline( textLine, 256 );   //skip past remainder of line

         int test1 = fileIO::isFileReadable( fileName );
         if ( test1 == 1 )
         { 
            vprDEBUG(vprDBG_ALL,0) << " geometry fileName = "
                                    << fileName
                                    << std::endl << vprDEBUG_FLUSH;
         }
         else
         {
            std::cerr << "ERROR: unreadable geometry file = " 
                        << fileName 
                        << ".  You may need to correct your param file." << std::endl;
            exit(1);
         }
         _modelList.at( 0 )->CreateGeomDataSet( fileName );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->getpfDCS()->SetScaleArray( scale );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->getpfDCS()->SetTranslationArray( trans );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->getpfDCS()->SetRotationArray( rotate );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->SetFILEProperties( color, transFlag, stlColor );
      }
      else
      {
         // Skip past block
         _readParam->ContinueRead( input, id );
      }
   }
}  

void cfdModelHandler::LoadSurfaceFiles( char * precomputedSurfaceDir )
{
    if ( precomputedSurfaceDir == NULL )
   {
      vprDEBUG(vprDBG_ALL,1) << "precomputedSurfaceDir == NULL" 
                             << std::endl << vprDEBUG_FLUSH;
      return;
   }

   vprDEBUG(vprDBG_ALL,1) << "Loading surface files from " 
      << precomputedSurfaceDir << std::endl << vprDEBUG_FLUSH;

   //store the current directory so we can change back to it
   char *cwd;
#ifndef WIN32
   if ((cwd = getcwd(NULL, 100)) == NULL)
   {
      std::cerr << "Couldn't get the current working directory!" << std::endl;
      exit(1);
   }

   //open the directory (we already know that it is valid)

   DIR* dir = opendir( precomputedSurfaceDir );
   //change into this directory so that vtk can find the files
   chdir( precomputedSurfaceDir );
   
   //get the name of each file
   direct* file = 0;
   while( (file = readdir(dir)) != NULL )
   {
      //assume all vtk files in this directory are to be loaded
      if(strstr(file->d_name, ".vtk"))
      {
         char* pathAndFileName = new char[strlen(precomputedSurfaceDir)+
                                          strlen(file->d_name)+2];
         strcpy(pathAndFileName,precomputedSurfaceDir);
         strcat(pathAndFileName,"/");
         strcat(pathAndFileName,file->d_name);

         if ( fileIO::isFileReadable( file->d_name ) ) 
         {
            vprDEBUG(vprDBG_ALL,0) << "\tsurface file = " << pathAndFileName
                                   << std::endl << vprDEBUG_FLUSH;

            _modelList.at( 0 )->CreateCfdDataSet();
            unsigned int numDataSets = _modelList.at( 0 )->GetNumberOfCfdDataSets();
            // subtract 1 because this number was 1 base not 0 base
            numDataSets -= 1;
            _modelList.at( 0 )->GetCfdDataSet( -1 )->SetFileName( pathAndFileName );

            // set the dcs matrix the same as the last file
            _modelList.at( 0 )->GetCfdDataSet( -1 )->SetDCS( 
                        _modelList.at( 0 )->GetCfdDataSet( (int)(numDataSets-1) )->GetDCS() ); 

            // precomputed data that descends from a flowdata.vtk should
            // automatically have the same color mapping as the "parent" 
            _modelList.at( 0 )->GetCfdDataSet( -1 )->SetParent( 
                        _modelList.at( 0 )->GetCfdDataSet( (int)(numDataSets-1) )->GetParent() );
         }
         else
         {
            std::cerr << "ERROR: unreadable file = " << pathAndFileName
                      << ".  You may need to correct your param file."
                      << std::endl;
            exit(1);
         }
      }
   };
   //close the directory
   closedir( dir );
   dir = 0;
   file = 0;

   //change back to the original directory
   chdir( cwd );
#else
   //biv--this code will need testing
   //BIGTIME!!!!!!!
   char buffer[_MAX_PATH];
   BOOL finished;
   HANDLE hList;
   TCHAR directory[MAX_PATH+1];
   WIN32_FIND_DATA fileData;

   //windows compatibility
   //get the current working directory
   if ((cwd = _getcwd(buffer, _MAX_PATH)) == NULL){
      std::cerr << "Couldn't get the current working directory!" << std::endl;
      return ;
   }

   // Get the proper directory path for transient files
   sprintf(directory, "%s\\*", precomputedSurfaceDir);

   //get the first file
   hList = FindFirstFile(directory, &fileData);
  
   //check to see if directory is valid
   if(hList == INVALID_HANDLE_VALUE){ 
	   std::cerr<<"No precomputed surface files found in: "<<precomputedSurfaceDir<<std::endl;
      return ;
   }
   else
   {
      // Traverse through the directory structure
      finished = FALSE;
      while (!finished)
      {
         //add the file name to our data list
		 //assume all vtk files in this directory are part of the sequence
		 //assume all vtk files in this directory are to be loaded
         if(strstr(fileData.cFileName, ".vtk"))
         {
            char* pathAndFileName = new char[strlen(precomputedSurfaceDir)+
                                          strlen(fileData.cFileName)+2];
            strcpy(pathAndFileName,precomputedSurfaceDir);
            strcat(pathAndFileName,"/");
            strcat(pathAndFileName,fileData.cFileName);

            if ( fileIO::isFileReadable( pathAndFileName ) ) {
            vprDEBUG(vprDBG_ALL,0) << "\tsurface file = " << pathAndFileName
                                   << std::endl << vprDEBUG_FLUSH;

            _modelList.at( 0 )->CreateCfdDataSet();
            unsigned int numDataSets = _modelList.at( 0 )->GetNumberOfCfdDataSets();
            // subtract 1 because this number was 1 base not 0 base
            numDataSets -= 1;
            _modelList.at( 0 )->GetCfdDataSet( -1 )->SetFileName( pathAndFileName );

            // set the dcs matrix the same as the last file
            _modelList.at( 0 )->GetCfdDataSet( -1 )->SetDCS( 
                        _modelList.at( 0 )->GetCfdDataSet( (int)(numDataSets-1) )->GetDCS() ); 

            // precomputed data that descends from a flowdata.vtk should
            // automatically have the same color mapping as the "parent" 
            _modelList.at( 0 )->GetCfdDataSet( -1 )->SetParent( 
                        _modelList.at( 0 )->GetCfdDataSet( (int)(numDataSets-1) )->GetParent() );
			}
         else
         {
               std::cerr << "ERROR: unreadable file = " << pathAndFileName
                         << ".  You may need to correct your param file."
                         << std::endl;
               exit(1);
			}
		 }
		 //check to see if this is the last file
		 if(!FindNextFile(hList, &fileData)){
            if(GetLastError() == ERROR_NO_MORE_FILES){
               finished = TRUE;
			}
		 }
	  }
   }
   //close the handle
   FindClose(hList);
   //make sure we are in the correct directory
   chdir(cwd);
#endif

}
