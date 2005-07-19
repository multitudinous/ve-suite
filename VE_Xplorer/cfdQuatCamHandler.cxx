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



#include <cmath>
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

//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
//NOTE: ALL USEFUL COUT'S SHOULD BE MADE VPR:DEBUG STATEMENTS, LEVEL 2
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////

cfdQuatCamHandler::cfdQuatCamHandler( VE_SceneGraph::cfdDCS* worldDCS, 
                                     cfdNavigate* nav, char* param )
{
   flyThroughList.clear();
   thisQuatCam = NULL;
   _worldDCS = NULL;
   _nav = NULL;
   _param = NULL;
   t = 0.0f;
   numQuatCams = 0;
   numPointsInFlyThrough = 0;
   activecam = false;
   _runFlyThrough = false;
   writeReadComplete = false;
   cam_id = 0;
   activeFlyThrough = -1;
   
   _worldDCS = worldDCS;
   _nav = nav;
   _param = param;
   completionTest.push_back( 0 );
   pointCounter = 0;
   movementIntervalCalc = 0.01;
   frameTimer = new vpr::Timer();
   //this->frameTimer->startTiming();
   movementSpeed = 10.0f;
   
   CreateObjects();
   
}

cfdQuatCamHandler::~cfdQuatCamHandler( void )
{
}
void cfdQuatCamHandler::LoadData(double* worldPos, VE_SceneGraph::cfdDCS* worldDCS)
{
   Matrix44f vjm;

   vjm = worldDCS->GetMat();

   QuatCams.push_back(new cfdQuatCam(vjm, worldPos));
}


void cfdQuatCamHandler::WriteToFile(char* fileName)
{
   std::ofstream inFile( fileName, std::ios::out );

   if ( fileIO::isFileReadable( fileName ) )
   {  
      //std::cout<<"QuatCam File Was Opened Successfully for writing"<<std::endl; 
      if ( QuatCams.size() > 0 )
      {
         inFile << "*Quaternion_Cameras" << std::endl;
         inFile << QuatCams.size() << std::endl;
      
         //std::cout<< QuatCams.size() << " view points are being written" << std::endl;
      
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
               inFile << flyThroughList.at( n ).size() << std::endl;
               for ( unsigned int l=0; l<flyThroughList.at( n ).size(); l++)
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
      std::cout<<"Could Not Open QuatCam File"<<std::endl;
}

void cfdQuatCamHandler::LoadFromFile( char* fileName)
{ 
   char textLine [ 256 ];
   double transpts[3];
   Matrix44f temp;

   if ( !QuatCams.empty() )
   {
      for ( unsigned int i=0; i<QuatCams.size(); i++ )
      {
         delete QuatCams.at(i);
      } 
      QuatCams.clear();
   }

   if ( !flyThroughList.empty() )
   {
      flyThroughList.clear();
   }
   
   std::ifstream inFile( fileName, std::ios::in ); 

   if ( fileIO::isFileReadable( fileName ) )
   { 
      std::cout<<"QuatCam File Was Opened Successfully"<<std::endl;

      if ( (char)inFile.peek() != '*' )
      {
         numQuatCams = 0;
         return;
      }
      else if ( (char)inFile.peek() == '*' )
      {
         inFile.getline( textLine, 256 );   //skip past remainder of line
         inFile >> numQuatCams;
         inFile.getline( textLine, 256 );   //skip past remainder of line      
         //std::cout << "Number of QuatCams: " << numQuatCams << std::endl;

         for ( unsigned int i=0; i<numQuatCams; i++ )
         {
            for ( unsigned int j=0; j<4; j++ )
            {
               for ( unsigned int k=0; k<4; k++ )
                  inFile >> temp[ j ][ k ];
            }
            inFile.getline( textLine, 256 );   //skip past remainder of line            

            for ( unsigned int k=0; k<3; k++ )
            {
               inFile >> transpts[k];
            }
            inFile.getline( textLine, 256 );   //skip past remainder of line      

            QuatCams.push_back(new cfdQuatCam(temp, transpts));
         } 

         if ( (char)inFile.peek() == '#' )
         {
            inFile.getline( textLine, 256 );   //skip past remainder of line
            inFile >> numFlyThroughs;
            inFile.getline( textLine, 256 );   //skip past remainder of line      
            //std::cout<<"Number of FlyThroughs: " << numFlyThroughs<<std::endl;

            numPointsInFlyThrough = new unsigned int[numFlyThroughs];
            std::vector<int> tempPts;
            int dummy;
            for ( unsigned int i=0; i<numFlyThroughs; i++ )
            {
               inFile >> numPointsInFlyThrough[i];
               //std::cout<<"Number of points in FlyThrough " << i << " :" << numPointsInFlyThrough[i]<<std::endl;

               for ( unsigned int j=0; j<numPointsInFlyThrough[i]; j++)
               {
                  inFile >> dummy;
                  tempPts.push_back(dummy);
               }
               flyThroughList.push_back(tempPts);
               tempPts.clear();
            }
            delete [] numPointsInFlyThrough;
         }
      }   
      inFile.close();  
   }
   
   else 
   {
      std::ofstream newFile( fileName, std::ios::out ); 
      newFile.open( fileName, std::ios::out );
      newFile.close();
   } 
       
}

void cfdQuatCamHandler::Relocate( VE_SceneGraph::cfdDCS* worldDCS,  cfdNavigate* nav )
{
   Matrix44f vjm;

   if ( t == 0.0f )
      QuatCams.at( cam_id )->SetCamPos( nav->worldTrans, worldDCS );

   if ( ( t < 1.0f ) && ( t < ( 1.0f - movementIntervalCalc ) ) )
   {
      t += movementIntervalCalc;
      QuatCams.at( cam_id )->MoveCam( t );
      QuatCams.at( cam_id )->UpdateTrans( nav );
      QuatCams.at( cam_id )->UpdateRotation( worldDCS );
   }
   else if ( ( t < 1.0f ) && ( t >= ( 1.0f - movementIntervalCalc ) ) )
   {
      //t += ( 1.0f - t );
      t = 1.0f;
      QuatCams.at( cam_id )->MoveCam( t );
      QuatCams.at( cam_id )->UpdateTrans( nav );
      QuatCams.at( cam_id )->UpdateRotation( worldDCS );
   }
   else
   {
      t = 0.0f;
      activecam = false;
   } 
   //std::cout<<"MovementCalc: "<< movementIntervalCalc <<std::endl;    
}

void cfdQuatCamHandler::RemoveViewPt( void )
{
   delete QuatCams.at( cam_id );
   QuatCams.erase( QuatCams.begin() + cam_id );

   for ( unsigned int i=0; i<flyThroughList.size(); i++ )
   {
      for ( unsigned int j=0; j<flyThroughList.at( i ).size(); j++ )
      {
         if ( flyThroughList.at( i ).at( j ) == cam_id )
         {
            RemoveFlythroughPt( i, j );
         }
      }
      for ( unsigned int k=0; k<flyThroughList.at( i ).size(); k++ )
      {
         if ( flyThroughList.at( i ).at( k ) > cam_id )
         {
            flyThroughList.at( i ).at( k ) -= 1;
         }
      }
   }
}

void cfdQuatCamHandler::RemoveFlythroughPt( unsigned int flyindex, unsigned int ptindex )
{
   flyThroughList.at( flyindex ).erase( flyThroughList.at( flyindex ).begin() + ptindex );
}

void cfdQuatCamHandler::AddViewPtToFlyThrough( unsigned int flyindex, unsigned int ptindex )
{
   flyThroughList.at( flyindex ).push_back( ptindex );
}

void cfdQuatCamHandler::InsertViewPtInFlyThrough( unsigned int flyindex, unsigned int beforept, unsigned int ptindex )
{
   flyThroughList.at( flyindex ).insert( flyThroughList.at( flyindex ).begin() + beforept, ptindex );
}

void cfdQuatCamHandler::DeleteEntireFlythrough( unsigned int flyindex )
{
   flyThroughList.erase( flyThroughList.begin() + flyindex );
}

void cfdQuatCamHandler::TurnOffMovement( void )
{
   _runFlyThrough = false;
   activecam = false;
   pointCounter = 0;
   t = 0.0f;
}

void cfdQuatCamHandler::AddNewFlythrough( void )
{
   std::vector< int > temp;
   temp.clear();
   flyThroughList.push_back( temp );
}

bool cfdQuatCamHandler::CheckCommandId( cfdCommandArray* commandArray )
{
   bool flag = false;

   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == LOAD_NEW_VIEWPT )
   {
      this->TurnOffMovement();
      this->LoadData( this->_nav->worldTrans, _worldDCS );
      this->WriteToFile( this->quatCamFileName );
      this->LoadFromFile( this->quatCamFileName );
      this->writeReadComplete = true;
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == MOVE_TO_SELECTED_LOCATION )
   {
      //this->frameTimer->startTiming();
      this->activecam = true;
      this->cam_id = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == REMOVE_SELECTED_VIEWPT )
   {
      this->TurnOffMovement();
      this->cam_id = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      this->RemoveViewPt();
      this->WriteToFile( this->quatCamFileName );
      this->LoadFromFile( this->quatCamFileName );
      this->writeReadComplete = true;
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == ADD_NEW_POINT_TO_FLYTHROUGH )
   {
      this->TurnOffMovement();
      this->AddViewPtToFlyThrough( (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ), 
                                   (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC ) );
      this->WriteToFile( this->quatCamFileName );
      this->LoadFromFile( this->quatCamFileName );
      this->writeReadComplete = true;
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == INSERT_NEW_POINT_IN_FLYTHROUGH )
   {
      this->TurnOffMovement();
      this->InsertViewPtInFlyThrough( (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ), 
                                      (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC ),
                                      (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ) );
      this->WriteToFile( this->quatCamFileName );
      this->LoadFromFile( this->quatCamFileName );
      this->writeReadComplete = true;
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == REMOVE_POINT_FROM_FLYTHROUGH )
   {
      this->TurnOffMovement();
      this->RemoveFlythroughPt( (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ), 
                                (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC ) );
      this->WriteToFile( this->quatCamFileName );
      this->LoadFromFile( this->quatCamFileName );
      this->writeReadComplete = true;
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == DELETE_ENTIRE_FLYTHROUGH )
   {
      this->TurnOffMovement();
      this->DeleteEntireFlythrough( (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
      this->WriteToFile( this->quatCamFileName );
      this->LoadFromFile( this->quatCamFileName );
      this->writeReadComplete = true;
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == ADD_NEW_FLYTHROUGH )
   {
      this->TurnOffMovement();
      this->AddNewFlythrough();
      this->WriteToFile( this->quatCamFileName );
      this->LoadFromFile( this->quatCamFileName );
      this->writeReadComplete = true;
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == RUN_ACTIVE_FLYTHROUGH )
   {
      this->activeFlyThrough = (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      this->_runFlyThrough = true;
      this->activecam = true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == STOP_ACTIVE_FLYTHROUGH )
   {
      this->TurnOffMovement();
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_MOVEMENT_SPEED )
   {
      this->movementSpeed = (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      return true;
   }



   return flag;
}


// If a quat is active this will move the cam to the next location
void cfdQuatCamHandler::PreFrameUpdate( void )
{
   frameTimer->stopTiming();
   if ( _runFlyThrough )
   {
      if ( (!activecam) )
      {      
         activecam = true;
         if ( pointCounter < (unsigned int)flyThroughList.at( activeFlyThrough ).size()-1 )
         {
            pointCounter += 1;
         }
         else
         {
            pointCounter = 0;
         }
         //pointCounter = ( pointCounter == ((int)flyThroughList.at( activeFlyThrough ).size()-1) ) ? 0 : ++pointCounter;
      }     
      cam_id = flyThroughList.at( activeFlyThrough ).at( pointCounter );
   }
   
   if ( activecam )
   {
      double vecDistance;
      
      if ( t == 0.0f )
      {
         gmtl::Vec3f vjVecTemp;

         for ( int i=0; i<3; i++ )
         {
            vjVecTemp[i] = this->_nav->worldTrans[i];
         } 
         vecDistance = getLinearDistance( vjVecTemp, QuatCams.at( cam_id )->GetTrans() );
      }
      else
      {
         vecDistance = getLinearDistance( QuatCams.at( cam_id )->GetLastTrans(), 
                                          QuatCams.at( cam_id )->GetTrans() );         
      }

      movementIntervalCalc = 1 / ( vecDistance / ( this->movementSpeed * ( frameTimer->getTiming() ) ) );

      this->Relocate( this->_worldDCS, this->_nav);
          
   }
   frameTimer->reset();
   frameTimer->startTiming();
}

void cfdQuatCamHandler::UpdateCommand()
{
   std::cerr << "doing nothing in cfdQuatCamHandler::UpdateCommand()" << std::endl;
}

void cfdQuatCamHandler::CreateObjects( void )
{
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

}

double cfdQuatCamHandler::getLinearDistance( gmtl::Vec3f vjVecLast, gmtl::Vec3f vjVecNext )
{
   double distance;
   gmtl::Vec3f temp;

   temp = vjVecNext - vjVecLast; 

   distance = gmtl::length( temp );

   return distance;
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

std::vector < int > cfdQuatCamHandler::getCompletionTest()
{
   if ( writeReadComplete )
   {
      completionTest.at( 0 ) = 1;
      writeReadComplete = false;
   }
   else
   {
      completionTest.at( 0 ) = 0;
   }
   return this->completionTest;
}
