#include <cfdQuatCamHandler.h>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include "cfdFileInfo.h"
#include "fileIO.h"

using namespace gmtl;
using namespace vrj;


cfdPoints::cfdPoints(double* worldPos, pfMatrix mat)
{
   for (int i=0; i<3; i++)
      ptrans[i] = worldPos[i];
   m = mat;
}


cfdQuatCamHandler::cfdQuatCamHandler()
{
   t = 0.0;
}


void cfdQuatCamHandler::LoadData(double* worldPos, pfDCS* worldDCS)
{
   worldDCS->getMat(m);
   nextPoint = new cfdPoints(worldPos, m);
   cfdPointsVec.push_back(nextPoint);
}


void cfdQuatCamHandler::WriteToFile(char* fileName)
{

   float* matpts = new float [16];

   FILE* ptsFile = fopen(fileName, "w");

   fprintf(ptsFile, "%d", cfdPointsVec.size());
   fprintf(ptsFile, "\n");

   for (int i=0; i<cfdPointsVec.size(); i++)
   {
      cfdPointsVec[i]->m.getCol(0, &matpts[0], &matpts[1], &matpts[2], &matpts[3]);
      cfdPointsVec[i]->m.getCol(1, &matpts[4], &matpts[5], &matpts[6], &matpts[7]);
      cfdPointsVec[i]->m.getCol(2, &matpts[8], &matpts[9], &matpts[10], &matpts[11]);
      cfdPointsVec[i]->m.getCol(3, &matpts[12], &matpts[13], &matpts[14], &matpts[15]);
      
      for (int j=0; j<16; j++)
      {
         fprintf(ptsFile, "%f  ", matpts[j]); 
      }
      fprintf(ptsFile, "\n");

      for (int k=0; k<3; k++)
      {
         fprintf(ptsFile, "%f  ", cfdPointsVec[i]->ptrans[k]);
      }
      fprintf(ptsFile, "\n");   
   }
   fclose(ptsFile);
}

void cfdQuatCamHandler::LoadFromFile(char* fileName)
{ 
   char textLine [ 256 ];
   int numQuatCams;
   float matpts[16];
   double transpts[3];
   float recordrot[4];
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
         for (int j=0; j<16; j++)
         {
            inFile >> matpts[j];
         }
         inFile.getline( textLine, 256 );   //skip past remainder of line            

         for (int k=0; k<3; k++)
         {
            inFile >> transpts[k];
         }
         inFile.getline( textLine, 256 );   //skip past remainder of line      

         mat.setCol(0, matpts[0], matpts[1], matpts[2], matpts[3]);
         mat.setCol(1, matpts[4], matpts[5], matpts[6], matpts[7]);
         mat.setCol(2, matpts[8], matpts[9], matpts[10], matpts[11]);
         mat.setCol(3, matpts[12], matpts[13], matpts[14], matpts[15]);

         recordrot[0] = matpts[0];
         recordrot[1] = matpts[1];
         recordrot[2] = matpts[4];
         recordrot[3] = matpts[5];
         
         thisQuatCam = new cfdQuatCam(mat, transpts, recordrot);
         QuatCams.push_back(thisQuatCam);
      } 
   }
   else 
      printf( "Could Not Open QuatCam File\n" ); 
          
}

pfDCS* cfdQuatCamHandler::Relocate(int cfdId, pfDCS* worldDCS, int cfdIso_value, cfdNavigate* nav)
{
   pfMatrix pfm;
   Matrix44f vjm;
   run = cfdId;

   if ( t == 0.0 )
      QuatCams[cfdIso_value]->SetCamPos(nav->worldTrans, worldDCS);

   if ( t < 1.0 )
   {
      t += 0.01;
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
   
   return worldDCS;      
}

