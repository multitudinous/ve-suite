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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "fluentParticles.h"
#include <fstream>
#include "converter.h"
#include <cmath>
#include <ctime>

fluentParticles::fluentParticles( void ) 
{
   std::cout << "[DBG] Inside Ctor..." << std::endl;
   writer      = vtkPolyDataWriter::New();
   writer->SetFileTypeToBinary();
   //polydata    = vtkPolyData::New();
   //points      = vtkPoints::New();
   //parameterData = vtkFloatArray::New();
   translateFluentPartToVTK();   
}

fluentParticles::~fluentParticles( void ) 
{
   writer->Delete();
   writer=NULL;
   //polydata->Delete();
   polydata=NULL;
   //points->Delete();
   points=NULL;
   //parameterData->Delete();
   //transform->Delete();
   //transFilter->Delete();
}



void fluentParticles::translateFluentPartToVTK( void ) 

{
   std::cout << "[DBG] Translating Fluent Part To VTK" << std::endl;   
   fluentParticle mfluentParticle;
   int i,k, partCount=0,timeStep=0,maxtimeStep=0,icount=0,fileID=0;
   //int numParticles;
   
   bool repeated=0;
     
   srand( time(NULL));
   
            
   std::ifstream inPartfile;
   char textLine[256];
   
   inPartfile.open("Pipe_CEH_DPM.part");
   for(i=0;i<19;i++)
   {
      inPartfile.getline(textLine,256);
   }

   while(!inPartfile.eof())
   {
      inPartfile>>mfluentParticle.mTime;
      if(mfluentParticle.mTime==0.0)
      {
         ++partCount;
         if(maxtimeStep<timeStep)
            maxtimeStep=timeStep;
         timeStep=0;
         icount=0;
      }
      else
      {  
         ++timeStep;
         
      }

      mfluentParticle.mID=partCount;
      mfluentParticle.mTimeSteps=timeStep;
      inPartfile>>mfluentParticle.mPosX>>mfluentParticle.mPosY>>mfluentParticle.mPosZ;
      inPartfile>>mfluentParticle.mVelocityU>>mfluentParticle.mVelocityV>>mfluentParticle.mVelocityW;
      inPartfile>>mfluentParticle.mDiameter;
      inPartfile>>mfluentParticle.mTemp;
      inPartfile>>mfluentParticle.mDensity;
      inPartfile>>mfluentParticle.mMass;

      if(timeStep%50==0)
      {  
         ++icount;
         mfluentParticles.insert(std::make_pair(icount, mfluentParticle));  
      }

   }



   std::cout<<"max timestep= "<<maxtimeStep<<std::endl;
   std::cout<<"number of files= "<<icount<<std::endl;
   inPartfile.close();
   
   for(i=0;i<150;i++)
   {
      do
      {
         repeated=0;
         particleID[i]=rand()%partCount;
         for(k=1;k<i;k++)
         {
            if(particleID[i]==particleID[k])
               repeated=1;
         }
      }while(repeated==1);

   }              
   
   
   for(fileID=1;fileID<=icount;fileID++)
   {
      
     
      //std::cout<<"numParticles= "<<numParticles<<" in the timestep "<< (fileID-1)*100<<std::endl;
      allocatePolydata();
      //std::cout<<"allocate finished"<<std::endl;
      addToPolydata(fileID);
      //std::cout<<"addToPolydata finished"<<std::endl;     
      deallocatePolydata();
      //std::cout<<"deallocate finished"<<std::endl;      
   }

   
  
}


void fluentParticles::allocatePolydata( void )
{

   int i;
   int numParameters=8;
   
   
   polydata = vtkPolyData::New();
   this->points = vtkPoints::New();   
        // set up arrays to store scalar and vector data over entire mesh...
   parameterData = NULL;
   parameterData = new vtkFloatArray * [numParameters];
   this->transform   = vtkTransform::New();
   this->transFilter = vtkTransformPolyDataFilter::New();

   for (i=0; i < numParameters; i++)
   {
      parameterData[i] = vtkFloatArray::New();
      parameterData[i]->SetNumberOfComponents( 1 );
      if (parameterData[i] == NULL)
      {
         std::cerr << "ERROR: can't get memory for parameterData, so exiting" << std::endl;
         exit( 1 );
      }
   }
   parameterData[ 0 ]->SetName( "U_Velocity" );
   parameterData[ 1 ]->SetName( "V_Velocity" );
   parameterData[ 2 ]->SetName( "W_Veloctiy" );
   parameterData[ 3 ]->SetName( "Velocity_Magnitude" );
   parameterData[ 4 ]->SetName( "Particle_Diameter" );
   parameterData[ 5 ]->SetName( "Particle_Temperature" );
   parameterData[ 6 ]->SetName( "Particle_Density" );
   parameterData[ 7 ]->SetName( "Particle_Mass" );

   polydata->Allocate();
   //polydata->Allocate(numParticles);   
}

void fluentParticles::deallocatePolydata( void )
{
   int i;
   int numParameters=8;
   points->Delete();
   polydata->Delete();
   transform->Delete();
   transFilter->Delete();
   //delete parameterData
   for (i=0; i < numParameters; i++) 
      parameterData[i]->Delete();
   
   delete [] parameterData;      
   parameterData = NULL;

}

void fluentParticles::addToPolydata( int timestep)
{
   
   //scalar->SetName("Gas_temp");
   float metertofoot=3.28083;
   float Velocity_Magnitude;
   int IDinVTKFile=0;
   int i;
  
   std::multimap<int, fluentParticle>::iterator j;

   for(j=mfluentParticles.begin();j!=mfluentParticles.end();j++)
     {
        
        if(j->first==timestep)
        {  
           // A particle a timestep fileIO
           // Add to polydata current particles info
            for(i=0;i<250;i++)
            {
               if((j->second).mID==particleID[i])
                  {
                     IDinVTKFile++;
                     Velocity_Magnitude=((j->second).mVelocityU)*((j->second).mVelocityU)
                                       +((j->second).mVelocityV)*((j->second).mVelocityV)
                                       +((j->second).mVelocityW)*((j->second).mVelocityW);

                     Velocity_Magnitude=sqrt(Velocity_Magnitude);
                     points->InsertNextPoint((j->second).mPosX*metertofoot,
                                             (j->second).mPosY*metertofoot,
                                             (j->second).mPosZ*metertofoot);

                     parameterData[ 0 ]->InsertNextValue( (j->second).mVelocityU);
                     parameterData[ 1 ]->InsertNextValue( (j->second).mVelocityV);
                     parameterData[ 2 ]->InsertNextValue( (j->second).mVelocityW);
                     parameterData[ 3 ]->InsertNextValue( Velocity_Magnitude );
                     parameterData[ 4 ]->InsertNextValue( (j->second).mDiameter*metertofoot);
                     parameterData[ 5 ]->InsertNextValue( (j->second).mDensity);
                     parameterData[ 6 ]->InsertNextValue( (j->second).mTemp);
                     parameterData[ 7 ]->InsertNextValue( (j->second).mMass);
                     //std::cout << (j->second).mID << "  "<<std::endl;
                     //vtkIdType id = ((j->second).mID)-1;
                     vtkIdType id=IDinVTKFile-1;
                     polydata->InsertNextCell(VTK_VERTEX, 1, (vtkIdType *)&id);
      
                     break;
                  }
         }        
      }
   }
   std::cout<<std::endl;
   //writeVtkThing( transFilter->GetOutput(), currentFilename, 1 ); //0=ascii
   writePolydata( timestep);
   std::cout<<"end of addToPolydaat"<<std::endl;
}

void fluentParticles::writePolydata( int timestep )
{
   int numParameters=7;
   // int i,j,k;
   // int counter;
   // int coordinateFilter;
   float   xRot,   yRot,   zRot;
   float xTrans, yTrans, zTrans;
   float xScale, yScale, zScale;
   // char textLine[ 256 ];
   
   std::ifstream paramFile( "./particle.param", std::ios::in );
   std::ostringstream file_name;
   std::string outVtkFileName="finalVtkFile_000.vtk";
   file_name<<"finalVtkFile_0"<<timestep<<".vtk";
   outVtkFileName=file_name.str();
   //outVtkfile.open((outVtkFileName).c_str(),std::ios::app||std::ios::ate);
   
   polydata->SetPoints(points);
   letUsersAddParamsToField( numParameters, parameterData, polydata->GetPointData(), 0 );
   polydata->Update();

   xTrans = yTrans = zTrans = 0.0f;
   yScale = zScale = xScale = 1;
   xRot=0;yRot=0;zRot=0;
   /*std::cout << xRot   << " : " << yRot   << " : " << zRot   << " : "
        << xTrans << " : " << yTrans << " : " << zTrans << " : "
        << xScale << " : " << coordinateFilter << std::endl;*/
   transform->Scale( xScale, yScale, zScale );
   transform->RotateX( xRot );
   transform->RotateY( yRot );
   transform->RotateZ( zRot );
   transform->Translate( xTrans, yTrans, zTrans );
   transform->Update();

   transFilter->SetInput( polydata );
   transFilter->SetTransform( transform );
   transFilter->Update();

   writer->SetInput( transFilter->GetOutput() );
   writer->SetFileName( (outVtkFileName).c_str() );
   writer->Write();
   //outVtkfile.close();
}




