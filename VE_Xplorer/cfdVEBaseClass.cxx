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
 * File:          $RCSfile: cfdVEBaseClass.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdVEBaseClass.h"
#include "cfdModuleGeometry.h"
#include "cfdGroup.h"
#include "cfdModel.h"
#include "cfdReadParam.h"
#include "fileIO.h"
#include "cfdFILE.h"
#include "cfdDataSet.h"
#include <fstream>
#include <cstdlib>
#include <string>
#include <map>

#include <vrj/Util/Debug.h>

#ifndef _WIN32 // not windows
#include <sys/dir.h>
#else // it is windows
#include <windows.h>
#include <direct.h>
#endif

using namespace std;

IMPLEMENT_DYNAMIC_CLASS( cfdVEBaseClass, wxObject )

// Constructor
cfdVEBaseClass::cfdVEBaseClass( void ) 
{
   _onSceneGraph = false;
}


/*cfdVEBaseClass::cfdVEBaseClass( cfdDCS* veworldDCS )
{
   this->groupNode = new cfdGroup();
   this->_dcs = new cfdDCS();
   //this->dataRepresentation = new cfdObjects();
   this->geometryNode = new cfdModuleGeometry( groupNode );
   this->worldDCS = veworldDCS;
   this->_model = new cfdModel( _dcs );
   this->_readParam = new cfdReadParam();
   //this->worldDCS->addChild( this->geometryNode->GetPfDCS() );
   
}*/

// Destructor
cfdVEBaseClass::~cfdVEBaseClass( void )
{
   //delete this->dataRepresentation;
}

void cfdVEBaseClass::InitializeNode( cfdDCS* veworldDCS )
{
   this->groupNode = new cfdGroup();
   this->_dcs = new cfdDCS();
   //this->dataRepresentation = new cfdObjects();
   this->geometryNode = new cfdModuleGeometry( groupNode );
   this->worldDCS = veworldDCS;
   this->_model = new cfdModel( _dcs );
   this->_readParam = new cfdReadParam();
}

// Methods to do scene graph manipulations
// New methods may have to be added later
void cfdVEBaseClass::AddSelfToSG( void )
{
   _onSceneGraph = true;
   this->worldDCS->AddChild( this->_dcs );
}

void cfdVEBaseClass::RemoveSelfFromSG( void )
{
   _onSceneGraph = false;
   this->worldDCS->RemoveChild( this->_dcs );
}

// Change state information for geometric representation
void cfdVEBaseClass::MakeTransparent( void )
{
   this->geometryNode->SetOpacity( 0.7 );
   this->geometryNode->Update();
}

void cfdVEBaseClass::SetColor( double* color )
{
   this->geometryNode->SetRGBAColorArray( color );
   this->geometryNode->Update();
}
      
// transform object based 
void cfdVEBaseClass::SetTransforms( float* scale, float* rot, float* trans)
{
   this->_dcs->SetTranslationArray( trans );
   this->_dcs->SetScaleArray( scale );
   this->_dcs->SetRotationArray( rot );
}

// Implement Gengxun's work by using socket
// stuff from vtk. This will be used in parallel
// with implementation of a unit connected to the 
// computational engine.
void cfdVEBaseClass::GetDataFromUnit( void )
{
   // Need to get Gengxun's work
   /*std::cout << "this->cfdId = " << geodeEnumToString(this->cfdId) << std::endl;
   this-> sock = vtkSocketCommunicator::New();
   this-> sock->WaitForConnection(33000);

   std::cout << "[DBG] VE_Xplorer is connected to the port 33000 "<< std::endl;
   

   vprDEBUG(vprDBG_ALL,1)
         <<" UPDATE_INTERACTIVE_DESIGN " << this->Interactive_state;
   
   vtkUnstructuredGrid* ugrid = vtkUnstructuredGrid::New();
   
   if (!this->sock->Receive(ugrid,1,9))
   {
      std::cerr << " cfdCalculator side error :: Error receiving data." << std::endl;
      if (this->sock)
      {
         this->sock->CloseConnection();
         this->sock->Delete();
         this->sock = NULL;

      }

      ugrid->Delete();

   }

   std::cout << "[DBG] Receiving ugrid data..." << std::endl;
   
   
   
   if( this -> sock)
   {
      std::cout << "[DBG] testing if the sock is still connected" << std::endl;
      this->sock->CloseConnection();
      this->sock->Delete();
      this->sock = NULL;

   }*/
}
// Basically uses vtkActorToPF to create a geode and 
// add it to the scene graph. Probably use cfdObject.
void cfdVEBaseClass::MakeGeodeByUserRequest( int )
{
   //this->dataRepresentation->UpdatecfdGeode();
}

//This returns the name of the module
wxString cfdVEBaseClass::GetName( void )
{
   return this->_objectName;
}

//This returns the description of the module, This should be a short description
wxString cfdVEBaseClass::GetDesc( void )
{
   return this->_objectDescription;
}


//This is the load function of the module, unpack the input string and fill up the UI according to this
void cfdVEBaseClass::UnPack(Interface* intf)
{
   vector<string> vars;
  
  map<string, long *>::iterator iteri;
  map<string, double *>::iterator iterd;
  map<string, string *>::iterator iters;
  map<string, vector<long> *>::iterator itervi;
  map<string, vector<double> *>::iterator itervd;
  map<string, vector<string> *>::iterator itervs;
  
  unsigned int i;
  long temp;

  mod_pack = *intf;
  vars = mod_pack.getInts();
  for (i=0; i<vars.size(); i++)
    {
      iteri =_int.find(vars[i]);
      if (iteri!=_int.end())
	mod_pack.getVal(vars[i], *(iteri->second));
      else if (vars[i]=="XPOS")
	{
	  mod_pack.getVal("XPOS", temp);
	  //	  printf("xpos %ld\n", temp);
	  //pos.x = temp;
     pos_x = temp;
	}
      else if (vars[i]=="YPOS")
	{
	  //	  printf("ypos %ld\n", temp);
	  mod_pack.getVal("YPOS", temp);
	  //pos.y = temp;
     pos_y = temp;
	}
    }

  vars = mod_pack.getDoubles();
  for (i=0; i<vars.size(); i++)
    {
      iterd =_double.find(vars[i]);
      if (iterd!=_double.end())
	mod_pack.getVal(vars[i], *(iterd->second));
    }  
  
  vars = mod_pack.getStrings();
  for (i=0; i<vars.size(); i++)
    {
      iters =_string.find(vars[i]);
      if (iters!=_string.end())
	mod_pack.getVal(vars[i], *(iters->second));
    }

  vars = mod_pack.getInts1D();
  for (i=0; i<vars.size(); i++)
    {
      itervi =_int1D.find(vars[i]);
      if (itervi!=_int1D.end())
	mod_pack.getVal(vars[i], *(itervi->second));
    }

   vars = mod_pack.getDoubles1D();
   for (i=0; i<vars.size(); i++)
   {
      itervd =_double1D.find(vars[i]);
      if (itervd!=_double1D.end())
	      mod_pack.getVal(vars[i], *(itervd->second));
   }

   vars = mod_pack.getStrings1D();
   for (i=0; i<vars.size(); i++)
   {
      itervs =_string1D.find(vars[i]);
      if (itervs!=_string1D.end())
         mod_pack.getVal(vars[i], *(itervs->second));
   }
}

Interface* cfdVEBaseClass::Pack()
{  
   string result;
  
   map<string, long *>::iterator iteri;
   map<string, double *>::iterator iterd;
   map<string, string *>::iterator iters;
   map<string, vector<long> *>::iterator itervi;
   map<string, vector<double> *>::iterator itervd;
   map<string, vector<string> *>::iterator itervs;


   //printf("mod id : %d\n", mod_pack._id);
   //mod_pack.setVal("XPOS",long (pos.x));
   //mod_pack.setVal("YPOS",long (pos.y));
   mod_pack.setVal("XPOS",long (pos_x));
   mod_pack.setVal("YPOS",long (pos_y));
  
   for(iteri=_int.begin(); iteri!=_int.end(); iteri++)
      mod_pack.setVal(iteri->first, *(iteri->second));

   for(iterd=_double.begin(); iterd!=_double.end(); iterd++)
      mod_pack.setVal(iterd->first, *(iterd->second));

   for(iters=_string.begin(); iters!=_string.end(); iters++)
      mod_pack.setVal(iters->first, *(iters->second));

   for(itervi=_int1D.begin(); itervi!=_int1D.end(); itervi++)
      mod_pack.setVal(itervi->first, *(itervi->second));

   for(itervd=_double1D.begin(); itervd!=_double1D.end(); itervd++)
      mod_pack.setVal(itervd->first, *(itervd->second));

   for(itervs=_string1D.begin(); itervs!=_string1D.end(); itervs++)
      mod_pack.setVal(itervs->first, *(itervs->second));

   //mod_pack.pack(result);
  
   //wxString wxstr = result.c_str();
   return &mod_pack ;//wxstr;
}

//This is to unpack the result from the 
void cfdVEBaseClass::UnPackResult(Interface * intf)
{
}

// Set the id for a particular module
void cfdVEBaseClass::SetID(int id)
{
   _modID = id;
}

cfdModel* cfdVEBaseClass::GetCFDModel( void )
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
         _model->CreateCfdDataSet();

         vprDEBUG(vprDBG_ALL,0) << " ************************************* "
                          << std::endl << vprDEBUG_FLUSH;

         vprDEBUG(vprDBG_ALL,0) << " vtk DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;

         float scale[3], trans[3], rotate[3];   // pfDCS stuff
         this->_readParam->read_pf_DCS_parameters( input, scale, trans, rotate);

         // Pass in -1 to GetCfdDataSet to get the last dataset added
         _model->GetCfdDataSet( -1 )->GetDCS()->SetScaleArray( scale );
         _model->GetCfdDataSet( -1 )->GetDCS()->SetTranslationArray( trans );
         _model->GetCfdDataSet( -1 )->GetDCS()->SetRotationArray( rotate );

         // get vtk data set name...
         char vtk_filein[ 256 ];
         input >> vtk_filein;
         input.getline( textLine, 256 );   //skip past remainder of line

         if (fileIO::isFileReadable( vtk_filein ) ) 
         {
            vprDEBUG(vprDBG_ALL,0) << " vtk file = " << vtk_filein 
                             << ", dcs = "  << _model->GetCfdDataSet( -1 )->GetDCS()
                             << std::endl << vprDEBUG_FLUSH;
            _model->GetCfdDataSet( -1 )->SetFileName( vtk_filein );
         }
         else
         {
            std::cerr << "ERROR: unreadable vtk file = " << vtk_filein 
                        << ".  You may need to correct your param file."
                        << std::endl;
            exit(1);
         }

         char * precomputedDataSliceDir = _readParam->readDirName( input, "precomputedDataSliceDir" );
         _model->GetCfdDataSet( -1 )->SetPrecomputedDataSliceDir( precomputedDataSliceDir );
         delete [] precomputedDataSliceDir;
         precomputedDataSliceDir = NULL;

         char * precomputedSurfaceDir = _readParam->readDirName( input, "precomputedSurfaceDir" );
         _model->GetCfdDataSet( -1 )->SetPrecomputedSurfaceDir( precomputedSurfaceDir );
         delete [] precomputedSurfaceDir;
         precomputedSurfaceDir = NULL;

         this->LoadSurfaceFiles( _model->GetCfdDataSet( -1 )->GetPrecomputedSurfaceDir() );
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

         cout << scale[0] << " : " << scale[1] << " : " << scale[2] << " : " << endl;
         _model->CreateGeomDataSet( fileName );
         _model->GetGeomDataSet( -1 )->getpfDCS()->SetScaleArray( scale );
         _model->GetGeomDataSet( -1 )->getpfDCS()->SetTranslationArray( trans );
         _model->GetGeomDataSet( -1 )->getpfDCS()->SetRotationArray( rotate );
         _model->GetGeomDataSet( -1 )->SetFILEProperties( color, transFlag, stlColor );
      }
      else
      {
         // Skip past block
         _readParam->ContinueRead( input, id );
      }
   }
   return _model;
}

void cfdVEBaseClass::LoadSurfaceFiles( char * precomputedSurfaceDir )
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

            _model->CreateCfdDataSet();
            unsigned int numDataSets = _model->GetNumberOfCfdDataSets();
            // subtract 1 because this number was 1 base not 0 base
            numDataSets -= 1;
            _model->GetCfdDataSet( -1 )->SetFileName( pathAndFileName );

            // set the dcs matrix the same as the last file
            _model->GetCfdDataSet( -1 )->SetDCS( 
                        _model->GetCfdDataSet( (int)(numDataSets-1) )->GetDCS() ); 

            // precomputed data that descends from a flowdata.vtk should
            // automatically have the same color mapping as the "parent" 
            _model->GetCfdDataSet( -1 )->SetParent( 
                        _model->GetCfdDataSet( (int)(numDataSets-1) )->GetParent() );
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

            _model->CreateCfdDataSet();
            unsigned int numDataSets = _model->GetNumberOfCfdDataSets();
            // subtract 1 because this number was 1 base not 0 base
            numDataSets -= 1;
            _model->GetCfdDataSet( -1 )->SetFileName( pathAndFileName );

            // set the dcs matrix the same as the last file
            _model->GetCfdDataSet( -1 )->SetDCS( 
                        _model->GetCfdDataSet( (int)(numDataSets-1) )->GetDCS() ); 

            // precomputed data that descends from a flowdata.vtk should
            // automatically have the same color mapping as the "parent" 
            _model->GetCfdDataSet( -1 )->SetParent( 
                        _model->GetCfdDataSet( (int)(numDataSets-1) )->GetParent() );
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
   
// Stuff taken from Plugin_base.h
// All of Yang's work (REI)
/////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::RegistVar(string vname, long *var)
{
  _int[vname]=var;
}

void cfdVEBaseClass::RegistVar(string vname, double *var)
{
  _double[vname]=var;
}

void cfdVEBaseClass::RegistVar(string vname, string *var)
{
  _string[vname]=var;
}

void cfdVEBaseClass::RegistVar(string vname, vector<long> *var)
{
  _int1D[vname]=var;
}

void cfdVEBaseClass::RegistVar(string vname, vector<double> *var)
{
  _double1D[vname]=var;
}

void cfdVEBaseClass::RegistVar(string vname, vector<string> *var)
{
  _string1D[vname]=var;
}
