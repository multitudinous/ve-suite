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

#include <cfdQuatCamHandler.h>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include "cfdFileInfo.h"
#include "fileIO.h"
#ifdef _CFDCOMMANDARRAY
#include "cfdEnum.h"
#include "cfdCommandArray.h"
#endif //_CFDCOMMANDARRAY


using namespace gmtl;
using namespace vrj;


cfdPoints::cfdPoints(double* worldPos, Matrix44f& mat)
{
   for (int i=0; i<3; i++)
      trans[i] = worldPos[i];
   m = mat;
}


cfdQuatCamHandler::cfdQuatCamHandler()
{
   t = 0.0;
}


void cfdQuatCamHandler::LoadData(double* worldPos, pfDCS* worldDCS)
{
   Matrix44f vjm;
   pfMatrix m;

   worldDCS->getMat(m);
   vjm = GetVjMatrix(m);
   nextPoint = new cfdPoints(worldPos, vjm);
   cfdPointsVec.push_back(nextPoint);
}


void cfdQuatCamHandler::WriteToFile(char* fileName)
{

   float* matpts = new float [16];

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
   pfMatrix mat;
   
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

void cfdQuatCamHandler::Relocate(int cfdId, pfDCS* worldDCS, int cfdIso_value, cfdNavigate* nav)
{
   pfMatrix pfm;
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

      worldDCS->getMat(pfm);

      vjm = GetVjMatrix(pfm);
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

#ifdef _CFDCOMMANDARRAY
bool cfdQuatCamHandler::CheckCommandId( cfdCommandArray* commandArray )
{
   // This is here because Dr. K. has code in 
   // cfdObjects that doesn't belong there
   bool flag = cfdObjects::CheckCommandId( commandArray );
   
   else if ( commandArray->GetCommandValue( CFD_ID ) == LOAD_POINT )
   {
      this->LoadData( this->nav->worldTrans, worldDCS );
      return true;
   }
   else if ( commandArray->GetCommandValue( CFD_ID ) == WRITE_POINTS_TO_FILE )
   {
      this->WriteToFile( this->paramReader->quatCamFileName ); 
      return true;
   }
   else if ( commandArray->GetCommandValue( CFD_ID ) == READ_POINTS_FROM_FILE )
   {
      this->LoadFromFile( this->paramReader->quatCamFileName );
      return true;
   }
   else if ( commandArray->GetCommandValue( CFD_ID ) == MOVE_TO_SELECTED_LOCATION )
   {
      this->Relocate( commandArray->GetCommandValue( CFD_ID ), 
                        worldDCS, commandArray->GetCommandValue( CFD_ISOVALUE ), nav);
      this->setId( this->run );
   }

   return flag;
}

void cfdQuatCamHandler::UpdateCommand()
{
   cfdObjects::UpdateCommand();
   cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << endl;
}
#endif //_CFDCOMMANDARRAY

