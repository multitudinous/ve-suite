/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdQuatCamHandler.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/cfdQuatCamHandler.h"
#include "VE_Xplorer/cfdFileInfo.h"
#include "VE_Xplorer/fileIO.h"
#include "VE_Xplorer/cfdQuatCam.h"
#include "VE_Xplorer/cfdNavigate.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Xplorer/cfdCommandArray.h"
#include "VE_Xplorer/cfdReadParam.h"

#include "VE_SceneGraph/cfdDCS.h"
#include <cstdio>
#include <iostream>
#include <cstdlib>
#include <fstream>
#include <ostream>
#include <string>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

using namespace gmtl;
using namespace VE_Xplorer;
using namespace VE_Util;
using namespace VE_SceneGraph;

cfdPoints::cfdPoints(double* worldPos, Matrix44f& mat)
{
   for (int i=0; i<3; i++)
      trans[i] = worldPos[i];
   m = mat;
}


cfdQuatCamHandler::cfdQuatCamHandler( VE_SceneGraph::cfdDCS* worldDCS, 
                                     cfdNavigate* nav, char* param )
{
   flyThroughList.clear();
   thisQuatCam = NULL;
   nextPoint = NULL;
   _worldDCS = NULL;
   _nav = NULL;
   _readParam = NULL;
   _param = NULL;
   t = 0.0f;
   numQuatCams = 0;
   activecam = false;
   cam_id = 0;
   activeFlyThrough = -1;
   
   _worldDCS = worldDCS;
   _nav = nav;
   _param = param;
   _readParam = new cfdReadParam();
   CreateObjects();

}

cfdQuatCamHandler::~cfdQuatCamHandler( void )
{
}
void cfdQuatCamHandler::LoadData(double* worldPos, VE_SceneGraph::cfdDCS* worldDCS)
{
   Matrix44f vjm;
   float recordrot[4];

   vjm = worldDCS->GetMat();
   
   recordrot[0] = vjm[0][0];
   recordrot[1] = vjm[0][1];
   recordrot[2] = vjm[1][0];
   recordrot[3] = vjm[1][1];

   QuatCams.push_back(new cfdQuatCam(vjm, worldPos, recordrot));

   //nextPoint = new cfdPoints(worldPos, vjm);
   //cfdPointsVec.push_back(nextPoint);
}


void cfdQuatCamHandler::WriteToFile(char* fileName)
{
   std::ofstream inFile( fileName, std::ios::out );

   if ( fileIO::isFileReadable( fileName ) )
   {  
      printf("QuatCam File Was Opened Successfully for writing\n"); 
      if ( QuatCams.size() > 0 )
      {
         inFile << "*Quaternion_Cameras" << std::endl;
         //inFile << cfdPointsVec.size() << std::endl;
         inFile << QuatCams.size() << std::endl;
      
         std::cout<< QuatCams.size() << " view points are being written" << std::endl;
      
         Matrix44f temp;
         for (unsigned int i=0; i<QuatCams.size(); i++)
         {
            temp = QuatCams[i]->GetMatrix();
            for ( unsigned int j=0; j<4; j++)
            {
               for ( unsigned int k=0; k<4; k++)
               {
                  inFile << temp[ j ][ k ] << " ";
               }            
            }

            inFile << std::endl;

            gmtl::Vec3f temptrans;
            for (int k=0; k<3; k++)
            {
               temptrans = QuatCams[i]->GetTrans();
               inFile << temptrans[k] << " ";
            }
 
            inFile << std::endl;
         }

         if ( flyThroughList.size() > 0 )
         {
            inFile << "#Stored_FlyThroughs" << std::endl;
            inFile << flyThroughList.size() << std::endl;
            for (unsigned int n=0; n<flyThroughList.size(); n++)
            {
               inFile << numPointsInFlyThrough[n] << std::endl;
               for ( unsigned int l=0; l<numPointsInFlyThrough[n]; l++)
               {
                  inFile << flyThroughList.at(n).at(l) << " ";
               }
               inFile << std::endl;   
            }
         }
      }
      inFile.close();
   }
   else 
      printf( "Could Not Open QuatCam File\n" );
}

void cfdQuatCamHandler::LoadFromFile( char* fileName)
{ 
   char textLine [ 256 ];
   double transpts[3];
   float recordrot[4];
   Matrix44f temp;

   if ( !QuatCams.empty() )
   {
      QuatCams.clear();
   }

   if ( !flyThroughList.empty() )
   {
      flyThroughList.clear();
   }
   
   std::ifstream inFile( fileName, std::ios::in ); 

   if ( fileIO::isFileReadable( fileName ) )
   { 
      printf("QuatCam File Was Opened Successfully\n");

      //while ( !inFile.eof() )
      for (int i=0; i<2; i++)
      {
         if ( (char)inFile.peek() == '*' )
         {
            inFile.getline( textLine, 256 );   //skip past remainder of line
            inFile >> numQuatCams;
            inFile.getline( textLine, 256 );   //skip past remainder of line      
            std::cout << "Number of QuatCams: " << numQuatCams << std::endl;

            for ( int i=0; i<numQuatCams; i++ )
            {
               for ( int j=0; j<4; j++ )
               {
                  for ( unsigned int k=0; k<4; k++ )
                     inFile >> temp[ j ][ k ];
               }
               inFile.getline( textLine, 256 );   //skip past remainder of line            

               for ( int k=0; k<3; k++ )
               {
                  inFile >> transpts[k];
               }
               inFile.getline( textLine, 256 );   //skip past remainder of line      

               recordrot[0] = temp[0][0];
               recordrot[1] = temp[0][1];
               recordrot[2] = temp[1][0];
               recordrot[3] = temp[1][1];

               QuatCams.push_back(new cfdQuatCam(temp, transpts, recordrot));
            } 
         }
         else if ( (char)inFile.peek() == '#' )
         {
            inFile.getline( textLine, 256 );   //skip past remainder of line
            inFile >> numFlyThroughs;
            inFile.getline( textLine, 256 );   //skip past remainder of line      
            std::cout<<"Number of FlyThroughs: " << numFlyThroughs<<std::endl;
            numPointsInFlyThrough = new int[numFlyThroughs];
            std::vector<int> tempPts;
            int dummy;
            for ( int i=0; i<numFlyThroughs; i++ )
            {
               inFile >> numPointsInFlyThrough[i];
            std::cout<<"Number of points in FlyThrough " << i << " :" << numPointsInFlyThrough[i]<<std::endl;

               for ( int j=0; j<numPointsInFlyThrough[i]; j++)
               {
                  inFile >> dummy;
                  tempPts.push_back(dummy);
               }
               flyThroughList.push_back(tempPts);
               tempPts.clear();
               std::cout<<"At the end" <<std::endl;
            }
         }
      }
      inFile.close();
      std::cout<<"After Closed" <<std::endl;
   }
   
   else 
      printf( "Could Not Open QuatCam File\n" ); 
          
}

void cfdQuatCamHandler::Relocate( VE_SceneGraph::cfdDCS* worldDCS,  cfdNavigate* nav )
{
   Matrix44f vjm;
   //run = cfdId;

   if ( t == 0.0f )
      QuatCams[cam_id]->SetCamPos( nav->worldTrans, worldDCS );

   if ( t < 1.0f )
   {
      t += 0.05f;
      QuatCams[cam_id]->MoveCam( nav->worldTrans, t, worldDCS );
      QuatCams[cam_id]->UpdateTrans( nav );
      QuatCams[cam_id]->UpdateRotation( nav, worldDCS );

      //vjm = worldDCS->GetMat();

      //for ( int i=0; i<3; i++)
      //{
         //rotvec[i] = makeRot<EulerAngleXYZf>(vjm)[i];
      /*rotvec[0] = makeZRot(vjm);
      std::cout<<"z "<<rotvec[0]<<std::endl;
      rotvec[1] = makeXRot(vjm);
      std::cout<<"x "<<rotvec[1]<<std::endl;
      rotvec[2] = makeYRot(vjm);
      std::cout<<"y "<<rotvec[2]<<std::endl;
      //}
      //std::cout<<"angle "<<QuatCams[cfdIso_value]->angle<<std::endl;
      //nav->worldRot[0] = QuatCams[cam_id]->angle;
      nav->worldRot[0] = rotvec[0];
      nav->worldRot[1] = rotvec[1];
      nav->worldRot[2] = rotvec[2];*/
   }
   else
   {
      t = 0.0f;
      //run = -1;
      activecam = false;
   }      
}

void cfdQuatCamHandler::RemoveViewPt( void )
{
   delete QuatCams.at( cam_id );
   QuatCams.erase( QuatCams.begin() + cam_id );
}

bool cfdQuatCamHandler::CheckCommandId( cfdCommandArray* commandArray )
{
   bool flag = false;
   
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == LOAD_NEW_VIEWPT )
   {
      this->LoadData( this->_nav->worldTrans, _worldDCS );
      this->WriteToFile( this->quatCamFileName );
      this->LoadFromFile( this->quatCamFileName );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == MOVE_TO_SELECTED_LOCATION )
   {
      activecam = true;
      cam_id = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      //this->Relocate( _worldDCS, _nav);
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == REMOVE_SELECTED_VIEWPT )
   {
      cam_id = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      this->RemoveViewPt();
      this->WriteToFile( this->quatCamFileName );
      this->LoadFromFile( this->quatCamFileName );
      return true;
   }

   return flag;
}


// If a quat is active this will move the cam to the next location
void cfdQuatCamHandler::PreFrameUpdate( void )
{
   /*static int pointCounter = 0;
       if ( _nav->flyThrough[ 0 ]->getData() == gadget::Digital::ON )
      {
         activeFlyThrough = 0;
         activecam = true;
		 pointCounter = 0;
         cam_id = activeFlyThroughArray[ activeFlyThrough ].at( 0 );
      }
      else if ( _nav->flyThrough[ 1 ]->getData() == gadget::Digital::ON )
      {
         activeFlyThrough = 1;
         activecam = true;
		 pointCounter = 0;
         cam_id = activeFlyThroughArray[ activeFlyThrough ].at( 0 );
      }
      else if ( _nav->flyThrough[ 2 ]->getData() == gadget::Digital::ON )
      {
         activeFlyThrough = 2;
         activecam = true;
		 pointCounter = 0;
         cam_id = activeFlyThroughArray[ activeFlyThrough ].at( 0 );
      }
      else if ( _nav->flyThrough[ 3 ]->getData() == gadget::Digital::ON )
      {
         activeFlyThrough = 3;
         activecam = true;
		 pointCounter = 0;
         cam_id = activeFlyThroughArray[ activeFlyThrough ].at( 0 );
      }
      else if ( (activeFlyThrough != -1) && (!activecam) )
      {
         pointCounter = ( pointCounter == ((int)activeFlyThroughArray[ activeFlyThrough ].size()-1) ) ? 0 : ++pointCounter; 
         cam_id = activeFlyThroughArray[ activeFlyThrough ].at( pointCounter );
	     activecam = true;
      }*/
 
   if ( activecam )
   {
      this->Relocate( _worldDCS, _nav);    
   }
}

void cfdQuatCamHandler::UpdateCommand()
{
   std::cerr << "doing nothing in cfdQuatCamHandler::UpdateCommand()" << std::endl;
}

void cfdQuatCamHandler::CreateObjects( void )
{
   int numObjects;
   char text[ 256 ];
   char textLine[ 256 ];

   quatCamDirName = "./STORED_VIEWPTS";

   boost::filesystem::path dir_path( quatCamDirName );
   try
   {
      ( boost::filesystem::is_directory( dir_path ) );
   }
   catch ( const std::exception& ex )
	{
	   std::cout << ex.what() << std::endl;
      boost::filesystem::create_directory(dir_path);
	   std::cout << "...so we made it for you..." << std::endl;
	}

   quatCamFileName = "./STORED_VIEWPTS/stored_viewpts_flythroughs.dat";

   this->LoadFromFile( this->quatCamFileName );

   /*vprDEBUG(vprDBG_ALL,1) << " Number of Obejcts in Interactive Geometry : " << numObjects << std::endl  << vprDEBUG_FLUSH;
   for( int i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      vprDEBUG(vprDBG_ALL,1) << "Id of object in Interactive Geometry : " << id << std::endl << vprDEBUG_FLUSH;
      input.getline( text, 256 );   //skip past remainder of line
      if ( id == 14 )
      {
         input >> quatCamFileName;
         input.getline( textLine, 256 );   //skip past remainder of line
         std::cout<< " QuatCam file = " << quatCamFileName << std::endl;

         if (fileIO::isFileReadable( quatCamFileName ) ) 
         {
            vprDEBUG(vprDBG_ALL,0) << " QuatCam file = " << quatCamFileName 
                             << std::endl << vprDEBUG_FLUSH;

            LoadFromFile( quatCamFileName );
         }
         else
         {
            std::cerr << "ERROR: unreadable QuatCam file = " << quatCamFileName 
                     << ".  You may need to correct your param file."
                     << std::endl;
            exit(1);
         }   
      }
      else
      {
         // Skip past block
         _readParam->ContinueRead( input, id );
      }
   }*/
}

int cfdQuatCamHandler::getNumLocs()
{
   return this->numQuatCams;
}

std::vector< std::vector <int> > cfdQuatCamHandler::getFlyThroughs()
{
   /*if ( flyThroughList.empty() )
   {
      std::vector<int> temp;
      temp.push_back( 0 );
      flyThroughList.push_back( temp );
   }*/
   return this->flyThroughList;
}
