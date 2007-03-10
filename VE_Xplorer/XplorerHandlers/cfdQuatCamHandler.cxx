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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/XplorerHandlers/cfdQuatCamHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"
#include "VE_Xplorer/XplorerHandlers/QCClearDataEH.h"
#include "VE_Xplorer/XplorerHandlers/QCLoadFileEH.h"
#include "VE_Xplorer/SceneGraph/cfdFileInfo.h"
#include "VE_Xplorer/Utilities/fileIO.h"
#include "VE_Xplorer/XplorerHandlers/cfdQuatCam.h"
#include "VE_Xplorer/XplorerHandlers/cfdNavigate.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/XplorerHandlers/cfdCommandArray.h"
#include "VE_Xplorer/XplorerHandlers/cfdReadParam.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/OneDIntArray.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include <cmath>
#include <iostream>
#include <cstdlib>
#include <fstream>
#include <ostream>
#include <string>

#include <vpr/System.h>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

using namespace gmtl;
using namespace VE_Xplorer;
using namespace VE_Util;
using namespace VE_SceneGraph;

vprSingletonImp( VE_Xplorer::cfdQuatCamHandler );
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
//NOTE: ALL USEFUL COUT'S SHOULD BE MADE VPR:DEBUG STATEMENTS, LEVEL 2
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////

cfdQuatCamHandler::cfdQuatCamHandler( /*osg::ref_ptr< VE_SceneGraph::DCS > worldDCS, 
                                     cfdNavigate* nav, std::string param*/ )
{
   flyThroughList.clear();
   thisQuatCam = NULL;
   //_worldDCS = NULL;
   _nav = NULL;
   _param.empty();// = NULL;
   t = 0.0f;
   numQuatCams = 0;
   numPointsInFlyThrough = 0;
   activecam = false;
   _runFlyThrough = false;
   writeReadComplete = false;
   cam_id = 0;
   activeFlyThrough = -1;
   
   //_worldDCS = 0;
   _nav = 0;
   _param = "";//param;
   completionTest.push_back( 0 );
   pointCounter = 0;
   movementIntervalCalc = 0.01;
   frameTimer = new vpr::Timer();
   movementSpeed = 10.0f;
   lastCommandId = 0;
   currentFrame = 0;
   writeFrame = 0;

   
   command = 0;

   //CreateObjects();
   quatCamDirName = "./";

   quatCamFileName = "stored_viewpts_flythroughs.vel";
   
   _eventHandlers[std::string("QC_LOAD_STORED_POINTS")] = new VE_EVENTS::QuatCamLoadFileEventHandler();
   _eventHandlers[std::string("QC_CLEAR_QUAT_DATA")] = new VE_EVENTS::QuatCamClearDataEventHandler();
 
   ///more hacking to initialize the flythroughlist
   ///This forces us to only have one flythrought per ves file
   if(flyThroughList.empty())
   {
      AddNewFlythrough();
   }
}
///////////////////////////////////////////////////////////////
void cfdQuatCamHandler::SetDCS(VE_SceneGraph::DCS* worldDCS)
{
   _worldDCS = worldDCS;
}
///////////////////////////////////////////////////////////////////
void cfdQuatCamHandler::SetNavigation(VE_Xplorer::cfdNavigate* nav)
{
   _nav = nav;
}
///////////////////////////////////////
void cfdQuatCamHandler::CleanUp( void )
{
   ///empty destrutor?
}
void cfdQuatCamHandler::LoadData(double* worldPos, VE_SceneGraph::DCS* worldDCS)
{
   Matrix44f vjm;

   vjm = worldDCS->GetMat();

   QuatCams.push_back(new cfdQuatCam(vjm, worldPos));
}


void cfdQuatCamHandler::WriteToFile(std::string fileName)
{
   if ( !onMasterNode )
   {
	   return;
   }

      boost::filesystem::path dir_path( quatCamDirName,boost::filesystem::no_check );
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

      std::ofstream inFile( fileName.c_str(), std::ios::out );

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
	  {
		  std::cout<<"Could Not Open QuatCam File"<<std::endl;
	  }
}

void cfdQuatCamHandler::LoadFromFile( std::string fileName)
{
   // this is for cluster mode so that the master has a chance to write
   // the quat cam file and then one frame later all the slaves will
   // read that file
   if ( !onMasterNode  )
   {
      return;
   }
   quatCamDirName = fileName;
   boost::filesystem::path dir_path( quatCamDirName,boost::filesystem::no_check  );
   try
   {
      ( boost::filesystem::is_directory( dir_path ) );
   }
   catch ( const std::exception& ex )
	{
	   std::cout << ex.what() << std::endl;    
	   return;
	} 

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
   
   std::ifstream inFile( fileName.c_str(), std::ios::in ); 

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
      this->writeReadComplete = true;
   }
   else 
   {
     std::ofstream newFile( fileName.c_str(), std::ios::out ); 
     newFile.open( fileName.c_str(), std::ios::out );
      newFile.close();
   }
}
/////////////////////////////////////////////
void cfdQuatCamHandler::ClearQuaternionData()
{
   //if ( !QuatCams.empty() )
   {
      for ( size_t i=0; i<QuatCams.size(); i++ )
      {
         delete QuatCams.at(i);
      } 
      QuatCams.clear();
   }

   //if ( !flyThroughList.empty() )
   {
      flyThroughList.clear();
   }
}
//////////////////////////////////////////////////////////////////
void cfdQuatCamHandler::Relocate( VE_SceneGraph::DCS* worldDCS, cfdNavigate* nav )
{
   Matrix44f vjm;

   if ( t == 0.0f )
   {
      QuatCams.at( cam_id )->SetCamPos( nav->worldTrans, worldDCS );
   }
   float temp = this->GetQuatCamIncrementor();

   if ( ( t < 1.0f ) )
   {
      QuatCams.at( cam_id )->MoveCam( temp );
      //QuatCams.at( cam_id )->UpdateTrans( nav );
      QuatCams.at( cam_id )->UpdateRotation( worldDCS );
   }
   else
   {
      activecam = false;
      t = 0.0f;
   }   
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
   if ( !onMasterNode )
   {
      return true;
   }

   bool flag = false;
   std::string commandType;
   if ( command )
   {
      commandType = command->GetCommandName();
   }
   else
   {
      commandType = "wait";
   }


   if ( !commandType.compare( "ViewLoc_Data" ) )
   {
      VE_XML::DataValuePair* commandData = command->GetDataValuePair( 0 );
      //this->cfdIso_value = commandData->GetDataValue();
      
      ///Change this to grab a OneDIntArray via GetDataXMLObject() from DataValuePair---biv
      std::vector< long > commandIds;
      //VE_XML::OneDIntArray *tempArray = commandData->GetDataXMLObject(); 
      //commandIds = tempArray->GetArray();
      commandData->GetData( commandIds );
      std::string newCommand = commandData->GetDataName();

      //if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == LOAD_NEW_VIEWPT )
      if ( !newCommand.compare( "LOAD_NEW_VIEWPT" ) ) 
      {
         writeFrame = currentFrame;
         this->TurnOffMovement();
         this->LoadData( this->_nav->worldTrans, _worldDCS.get() );
         AddViewPtToFlyThrough(0,QuatCams.size()-1);
         this->WriteToFile( this->quatCamFileName );
         //this->LoadFromFile( this->quatCamFileName );
         this->writeReadComplete = true;
         this->lastCommandId = LOAD_NEW_VIEWPT;
         return true;
      }
      //else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == MOVE_TO_SELECTED_LOCATION )
      else if ( !newCommand.compare( "MOVE_TO_SELECTED_LOCATION" ) )      
      {
         //this->frameTimer->startTiming();
         this->activecam = true;
         //this->cam_id = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         this->cam_id = (unsigned int)commandIds.at( 0 );
         return true;
      }
      //else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == REMOVE_SELECTED_VIEWPT )
      else if ( !newCommand.compare( "REMOVE_SELECTED_VIEWPT" ) )      
      {
         writeFrame = currentFrame;
         this->TurnOffMovement();
         //this->cam_id = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         this->cam_id = (unsigned int)commandIds.at( 0 );
         this->RemoveViewPt();
         this->WriteToFile( this->quatCamFileName );
         //this->LoadFromFile( this->quatCamFileName );
         this->writeReadComplete = true;
         this->lastCommandId = REMOVE_SELECTED_VIEWPT;
         return true;
      }
      //else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == ADD_NEW_POINT_TO_FLYTHROUGH )
      else if ( !newCommand.compare( "ADD_NEW_POINT_TO_FLYTHROUGH" ) )      
      {
         writeFrame = currentFrame;
         this->TurnOffMovement();
         //this->AddViewPtToFlyThrough( (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ), 
         //                             (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC ) );
         this->AddViewPtToFlyThrough( (unsigned int)commandIds.at( 0 ), 
                                      (unsigned int)commandIds.at( 1 ) );
         this->WriteToFile( this->quatCamFileName );
         //this->LoadFromFile( this->quatCamFileName );
         this->writeReadComplete = true;
         this->lastCommandId = ADD_NEW_POINT_TO_FLYTHROUGH;
         return true;
      }
      //else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == INSERT_NEW_POINT_IN_FLYTHROUGH )
      else if ( !newCommand.compare( "INSERT_NEW_POINT_IN_FLYTHROUGH" ) )      
      {
         writeFrame = currentFrame;
         this->TurnOffMovement();
         //this->InsertViewPtInFlyThrough( (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ), 
         //                                (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC ),
         //                                (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ) );
         this->InsertViewPtInFlyThrough( (unsigned int)commandIds.at( 0 ), 
                                         (unsigned int)commandIds.at( 1 ),
                                         (unsigned int)commandIds.at( 2 ) );
         this->WriteToFile( this->quatCamFileName );
         //this->LoadFromFile( this->quatCamFileName );
         this->writeReadComplete = true;
         this->lastCommandId = INSERT_NEW_POINT_IN_FLYTHROUGH;
         return true;
      }
      //else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == REMOVE_POINT_FROM_FLYTHROUGH )
      else if ( !newCommand.compare( "REMOVE_POINT_FROM_FLYTHROUGH" ) )      
      {
         writeFrame = currentFrame;
         this->TurnOffMovement();
         //this->RemoveFlythroughPt( (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ), 
         //                          (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC ) );
         this->RemoveFlythroughPt( (unsigned int)commandIds.at( 0 ), 
                                   (unsigned int)commandIds.at( 1 ) );
         this->WriteToFile( this->quatCamFileName );
         //this->LoadFromFile( this->quatCamFileName );
         this->writeReadComplete = true;
         this->lastCommandId = REMOVE_POINT_FROM_FLYTHROUGH;
         return true;
      }
      //else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == DELETE_ENTIRE_FLYTHROUGH )
      else if ( !newCommand.compare( "DELETE_ENTIRE_FLYTHROUGH" ) )      
      {
         writeFrame = currentFrame;
         this->TurnOffMovement();
         //this->DeleteEntireFlythrough( (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
         this->DeleteEntireFlythrough( (unsigned int)commandIds.at( 0 ) );
         this->WriteToFile( this->quatCamFileName );
         //this->LoadFromFile( this->quatCamFileName );
         this->writeReadComplete = true;
         this->lastCommandId = DELETE_ENTIRE_FLYTHROUGH;
         return true;
      }
      //else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == ADD_NEW_FLYTHROUGH )
      else if ( !newCommand.compare( "ADD_NEW_FLYTHROUGH" ) )      
      {
         writeFrame = currentFrame;
         this->TurnOffMovement();
         this->AddNewFlythrough();
         this->WriteToFile( this->quatCamFileName );
         //this->LoadFromFile( this->quatCamFileName );
         this->writeReadComplete = true;
         this->lastCommandId = ADD_NEW_FLYTHROUGH;
         return true;
      }
      //else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == RUN_ACTIVE_FLYTHROUGH )
      else if ( !newCommand.compare( "RUN_ACTIVE_FLYTHROUGH" ) )      
      {
         //this->activeFlyThrough = (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         this->activeFlyThrough = (unsigned int)commandIds.at( 0 );
         this->_runFlyThrough = true;
         this->activecam = true;
      }
      //else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == STOP_ACTIVE_FLYTHROUGH )
      else if ( !newCommand.compare( "STOP_ACTIVE_FLYTHROUGH" ) )      
      {
         this->TurnOffMovement();
         return true;
      }
      //else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_MOVEMENT_SPEED )
      else if ( !newCommand.compare( "CHANGE_MOVEMENT_SPEED" ) )      
      {
         //this->movementSpeed = (unsigned int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         this->movementSpeed = commandIds.at( 0 );
         return true;
      }
   }
   return flag;
}


// If a quat is active this will move the cam to the next location
void cfdQuatCamHandler::PreFrameUpdate( void )
{
   currentFrame += 1;

   std::map<std::string,VE_EVENTS::EventHandler*>::iterator currentEventHandler;
   if( cfdModelHandler::instance()->GetXMLCommand() )
   {
      vprDEBUG(vesDBG,3) << "Command Name : "
                           << cfdModelHandler::instance()->GetXMLCommand()->GetCommandName() 
                           << std::endl<< vprDEBUG_FLUSH;
      currentEventHandler = _eventHandlers.find( cfdModelHandler::instance()->GetXMLCommand()->GetCommandName() );
      if(currentEventHandler != _eventHandlers.end())
      {
         vprDEBUG(vesDBG,1) << "Executing: "
                              << cfdModelHandler::instance()->GetXMLCommand()->GetCommandName() 
                              << std::endl<< vprDEBUG_FLUSH;
         currentEventHandler->second->SetGlobalBaseObject();
         currentEventHandler->second->Execute( cfdModelHandler::instance()->GetXMLCommand() );
      }
   }
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

      this->Relocate( this->_worldDCS.get(), this->_nav);
          
   }
   frameTimer->reset();
   frameTimer->startTiming();
}

void cfdQuatCamHandler::UpdateCommand()
{
   std::cerr << "doing nothing in cfdQuatCamHandler::UpdateCommand()" << std::endl;
}

/*void cfdQuatCamHandler::CreateObjects( void )
{
   quatCamDirName = "./STORED_VIEWPTS";

   quatCamFileName = "./STORED_VIEWPTS/stored_viewpts_flythroughs.dat";

   this->LoadFromFile( this->quatCamFileName );

}*/

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
   //this assumes there is only one flythrough!!!!--biv
   return QuatCams.size();//this->numQuatCams;
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
////////////////////////////////////////////////////////////////////////////////
float cfdQuatCamHandler::GetQuatCamIncrementor( void )
{
   ////////////////////////////////////////////////////////////////////////
   //When in cluster mode this function is only called by the Master Node//
   ////////////////////////////////////////////////////////////////////////

   if ( ( t < 1.0f ) && ( t < ( 1.0f - movementIntervalCalc ) ) )
   {
      t += movementIntervalCalc;
   }
   else if ( ( t < 1.0f ) && ( t >= ( 1.0f - movementIntervalCalc ) ) )
   {
      t = 1.0f;
   }

   return t;   
}
////////////////////////////////////////////////////////////////////////////////
bool cfdQuatCamHandler::IsActive( void )
{
   return activecam;
}
////////////////////////////////////////////////////////////////////////////////
void cfdQuatCamHandler::SetVECommand( VE_XML::Command* veCommand )
{
   command = veCommand;
}
////////////////////////////////////////////////////////////////////////////////
void cfdQuatCamHandler::SetMasterNode( bool masterNode )
{
   onMasterNode = masterNode;
}
