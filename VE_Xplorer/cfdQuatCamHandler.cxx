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
 * File:          $RCSfile: cfdQuatCamHandler.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdQuatCamHandler.h"
#include "cfdFileInfo.h"
#include "fileIO.h"
#include "cfdDCS.h"
#include "cfdQuatCam.h"
#include "cfdNavigate.h"
#include "cfdEnum.h"
#include "cfdCommandArray.h"
#include "cfdReadParam.h"

#include <cstdio>
#include <iostream>
#include <cstdlib>
#include <fstream>
#include <string>

using namespace std;
using namespace gmtl;

cfdPoints::cfdPoints(double* worldPos, Matrix44f& mat)
{
   for (int i=0; i<3; i++)
      trans[i] = worldPos[i];
   m = mat;
}


cfdQuatCamHandler::cfdQuatCamHandler( cfdDCS* worldDCS, cfdNavigate* nav, char* param )
{
   thisQuatCam = NULL;
   nextPoint = NULL;
   _worldDCS = NULL;
   _nav = NULL;
   _readParam = NULL;
   _param = NULL;
   t = 0.0;
   
   _worldDCS = worldDCS;
   _nav = nav;
   _param = param;
   _readParam = new cfdReadParam();
   CreateObjects();
}

cfdQuatCamHandler::~cfdQuatCamHandler( void )
{
}
void cfdQuatCamHandler::LoadData(double* worldPos, cfdDCS* worldDCS)
{
   Matrix44f vjm;
   vjm = worldDCS->GetMat();;
   nextPoint = new cfdPoints(worldPos, vjm);
   cfdPointsVec.push_back(nextPoint);
}


void cfdQuatCamHandler::WriteToFile(char* fileName)
{

   //float* matpts = new float [16];

   FILE* ptsFile = fopen(fileName, "w");

   fprintf(ptsFile, "%d", cfdPointsVec.size());
   fprintf(ptsFile, "\n");

   Matrix44f temp;
   for (unsigned int i=0; i<cfdPointsVec.size(); i++)
   {
      temp = cfdPointsVec[i]->matrix();
      for ( unsigned int j=0; j<4; j++)
      {
         for ( unsigned int k=0; k<4; k++)
            fprintf(ptsFile, "%f  ", temp[ j ][ k ]); 
      }

      fprintf(ptsFile, "\n");

      for (int k=0; k<3; k++)
      {
         fprintf(ptsFile, "%f  ", cfdPointsVec[i]->ptrans()[k]);
      }
      fprintf(ptsFile, "\n");   
   }
   fclose(ptsFile);
}

void cfdQuatCamHandler::LoadFromFile(char* fileName)
{ 
   char textLine [ 256 ];
   int numQuatCams;
   double transpts[3];
   float recordrot[4];
   Matrix44f temp;
   
   std::ifstream inFile( fileName, std::ios::in ); 

   if ( fileIO::isFileReadable( fileName ) )
   { 
      printf("QuatCam File Was Opened Successfully\n");

      inFile >> numQuatCams;
      inFile.getline( textLine, 256 );   //skip past remainder of line      
      std::cout<<numQuatCams<<std::endl;

      for(int i=0; i<numQuatCams; i++)
      {
         for (int j=0; j<4; j++)
         {
            for ( unsigned int k=0; k<4; k++)
               inFile >> temp[ j ][ k ];
         }
         inFile.getline( textLine, 256 );   //skip past remainder of line            

         for (int k=0; k<3; k++)
         {
            inFile >> transpts[k];
         }
         inFile.getline( textLine, 256 );   //skip past remainder of line      

         recordrot[0] = temp[0][0];
         recordrot[1] = temp[0][1];
         recordrot[2] = temp[1][0];
         recordrot[3] = temp[1][1];
         
         thisQuatCam = new cfdQuatCam(temp, transpts, recordrot);
         QuatCams.push_back(thisQuatCam);
      } 
   }
   else 
      printf( "Could Not Open QuatCam File\n" ); 
          
}

void cfdQuatCamHandler::Relocate(int cfdId, cfdDCS* worldDCS, int cfdIso_value, cfdNavigate* nav)
{
   Matrix44f vjm;
   run = cfdId;

   if ( t == 0.0 )
      QuatCams[cfdIso_value]->SetCamPos(nav->worldTrans, worldDCS);

   if ( t < 1.0 )
   {
      t += 0.01f;
      QuatCams[cfdIso_value]->MoveCam(nav->worldTrans, t, worldDCS);
      QuatCams[cfdIso_value]->UpdateTrans(nav);
      QuatCams[cfdIso_value]->UpdateRotation();

      vjm = worldDCS->GetMat();

      for ( int i=0; i<3; i++)
      {
         rotvec[i] = makeRot<EulerAngleXYZf>(vjm)[i];
      }
      std::cout<<"angle "<<QuatCams[cfdIso_value]->angle<<std::endl;
      nav->worldRot[0] = QuatCams[cfdIso_value]->angle;
      nav->worldRot[1] = rotvec[1];
      nav->worldRot[2] = rotvec[2];
   }
   else
   {
      t = 0.0;
      run = -1;
   }      
}

bool cfdQuatCamHandler::CheckCommandId( cfdCommandArray* commandArray )
{
   bool flag = false;
   
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == LOAD_POINT )
   {
      this->LoadData( this->_nav->worldTrans, _worldDCS );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == WRITE_POINTS_TO_FILE )
   {
      this->WriteToFile( this->quatCamFileName ); 
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == READ_POINTS_FROM_FILE )
   {
      this->LoadFromFile( this->quatCamFileName );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == MOVE_TO_SELECTED_LOCATION )
   {
      this->Relocate( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ), 
                        _worldDCS, commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ), _nav);
      // Need to fix this
      //this->setId( this->run );
   }

   return flag;
}

void cfdQuatCamHandler::UpdateCommand()
{
   cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << endl;
}

void cfdQuatCamHandler::CreateObjects( void )
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
      if ( id == 14 )
      {
         input >> quatCamFileName;
         input.getline( textLine, 256 );   //skip past remainder of line

         if (fileIO::isFileReadable( quatCamFileName ) ) 
         {
            vprDEBUG(vprDBG_ALL,0) << " QuatCam file = " << quatCamFileName 
                             << std::endl << vprDEBUG_FLUSH;
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
   }
}


