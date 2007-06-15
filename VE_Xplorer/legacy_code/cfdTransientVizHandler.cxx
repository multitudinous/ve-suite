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
#include "cfdTransientVizHandler.h"

#include "cfdTransientFlowManager.h"
#include "cfdObjects.h"
#include "cfdAnimation.h"
#include "cfdTransientInfo.h"
#include "cfdReadParam.h"
#include "cfdFrame.h"
#include "cfdEnum.h"
#include "cfdTransientSet.h"
#include "cfdDCS.h"
#include "cfdDataSet.h"
#include "cfdCommandArray.h"
#include "cfdTempAnimation.h"

#include <vpr/Util/Debug.h>

#include <fstream>

cfdTransientVizHandler::cfdTransientVizHandler( char* filename )
{
   this->transientSequence = NULL;
   this->_cfdTFM_X_Contour[0] = NULL;
   this->_cfdTFM_X_Contour[1] = NULL;
   this->_cfdTFM_Y_Contour = NULL;
   this->_cfdTFM_Z_Contour = NULL;
   this->_cfdTFM_X_Vector = NULL;
   this->_cfdTFM_Y_Vector = NULL;
   this->_cfdTFM_Z_Vector = NULL;
   this->_cfdTFM_Particle = NULL;
   this->_cfdTFM_Geometry[0] = NULL;
   this->_cfdTFM_Geometry[1] = NULL;
   this->lastSource = NULL;
   _readParam = new cfdReadParam();
   _param = filename;
}

cfdTransientVizHandler::~cfdTransientVizHandler( void )
{
   if ( this->transientSequence != NULL )
   {
      vprDEBUG(vprDBG_ALL,2) 
         << "deleting this->transientSequence" << std::endl << vprDEBUG_FLUSH;
      delete this->transientSequence;
   }
}

///////////////////////
// Helper functions
///////////////////////
void cfdTransientVizHandler::SetCommandArray( cfdCommandArray* input )
{
   _commandArray = input;
}
//////////////////////

void cfdTransientVizHandler::InitScene( void )
{
   //
   // Initiate Transient cfdAnimation
   //
   if ( this->transientInfo.size() > 0 ) 
   {   
      std::cout << "| 42. Initializing.................................... cfdAnimation |" << std::endl;
      this->transientSequence = new cfdAnimation();
      this->transientSequence->SetDuration( this->transientInfo[ 0 ]->GetDuration() );

      int numFrames;

      //
      // Initiate Transient X Contour Data
      //
      // windshield hack follows
      if ( this->transientInfo[ 0 ]->Get_X_planeTransSet() &&
           this->transientInfo[ 0 ]->Get_X_planeTransSet()->GetNumberOfScalars() ) 
      {
         int count = 0;
         for ( int i = 0; i < (int)this->transientInfo.size(); i++ )
         {
            std::cout << "| 43. Initializing........................ Transient X Contour Data |" << std::endl;
            this->_cfdTFM_X_Contour[ count ] = new cfdTransientFlowManager();
            this->_cfdTFM_X_Contour[ count ]->SetParameterFile( this, i );
            this->_cfdTFM_X_Contour[ count ]->SetDirectory( this->transientInfo[ 0 ]->Get_X_planeTransSet()->GetDirectory() );
            this->_cfdTFM_X_Contour[ count ]->SetFrameDataType( cfdFrame::VTK_SCALAR );
            numFrames = this->_cfdTFM_X_Contour[ count ]->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_X_Contour[ count ] );     
            this->dataList.back()->SetObjectType( X_TRANSIENT_CONTOUR_AND_VECTOR );
            //this->dataList.back()->SetSequence( this->transientSequence->GetSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_X_Contour[ count ] );
            count++;
            
            if ( count == 2 )
               break;
         }
      } 

      //
      // Initiate Transient Y Contour Data
      //
      if ( this->transientInfo[ 0 ]->Get_Y_planeTransSet() &&
           this->transientInfo[ 0 ]->Get_Y_planeTransSet()->GetNumberOfScalars() ) 
      {
         for ( int i = 0; i < (int)this->transientInfo.size(); i++ )
         {
            std::cout << "| 44. Initializing........................ Transient Y Contour Data |" << std::endl;
            this->_cfdTFM_Y_Contour = new cfdTransientFlowManager();
            this->_cfdTFM_Y_Contour->SetParameterFile( this, i );
            this->_cfdTFM_Y_Contour->SetDirectory( this->transientInfo[ 0 ]->Get_Y_planeTransSet()->GetDirectory() );
            this->_cfdTFM_Y_Contour->SetFrameDataType( cfdFrame::VTK_SCALAR );
            numFrames = this->_cfdTFM_Y_Contour->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_Y_Contour );     
            this->dataList.back()->SetObjectType( Y_TRANSIENT_CONTOUR_AND_VECTOR );
            //this->dataList.back()->SetSequence( this->transientSequence->GetSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_Y_Contour );
            break;
         }
      } 

      //
      // Initiate Transient Z Contour Data
      //
      if ( this->transientInfo[ 0 ]->Get_Z_planeTransSet() &&
           this->transientInfo[ 0 ]->Get_Z_planeTransSet()->GetNumberOfScalars() ) 
      {
         for ( int i = 0; i < (int)this->transientInfo.size(); i++ )
         {
            std::cout << "| 45. Initializing........................ Transient Z Contour Data |" << std::endl;
            this->_cfdTFM_Z_Contour = new cfdTransientFlowManager();
            this->_cfdTFM_Z_Contour->SetParameterFile( this, i );
            this->_cfdTFM_Z_Contour->SetDirectory( this->transientInfo[ 0 ]->Get_Z_planeTransSet()->GetDirectory() );
            this->_cfdTFM_Z_Contour->SetFrameDataType( cfdFrame::VTK_SCALAR );
            numFrames = this->_cfdTFM_Z_Contour->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_Z_Contour );     
            this->dataList.back()->SetObjectType( Z_TRANSIENT_CONTOUR_AND_VECTOR );
            //this->dataList.back()->SetSequence( this->transientSequence->GetSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_Z_Contour );
            break;
         }
      } 

      //
      // Initiate Transient Vector Data
      //
      if ( this->transientInfo[ 0 ]->Get_X_planeTransSet() &&
           this->transientInfo[ 0 ]->Get_X_planeTransSet()->GetNumberOfVectors() ) 
      {
         for ( int i = 0; i < (int)this->transientInfo.size(); i++ )
         {
            std::cout << "| 46. Initializing......................... Transient X Vector Data |" << std::endl;
            this->_cfdTFM_X_Vector = new cfdTransientFlowManager();
            this->_cfdTFM_X_Vector->SetParameterFile( this, i );
            this->_cfdTFM_X_Vector->SetDirectory( this->transientInfo[ 0 ]->Get_X_planeTransSet()->GetDirectory() );
            this->_cfdTFM_X_Vector->SetFrameDataType( cfdFrame::VTK_VECTOR );
            numFrames = this->_cfdTFM_X_Vector->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_X_Vector );     
            this->dataList.back()->SetObjectType( X_TRANSIENT_CONTOUR_AND_VECTOR );
            //this->dataList.back()->SetSequence( this->transientSequence->GetSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_X_Vector );
            break;
         }
      }

      //
      // Initiate Transient Vector Data
      //
      if ( this->transientInfo[ 0 ]->Get_Y_planeTransSet() &&
           this->transientInfo[ 0 ]->Get_Y_planeTransSet()->GetNumberOfVectors() ) 
      {
         for ( int i = 0; i < (int)this->transientInfo.size(); i++ )
         {
            std::cout << "| 47. Initializing......................... Transient Y Vector Data |" << std::endl;
            this->_cfdTFM_Y_Vector = new cfdTransientFlowManager();
            this->_cfdTFM_Y_Vector->SetParameterFile( this, i );
            this->_cfdTFM_Y_Vector->SetDirectory( this->transientInfo[ 0 ]->Get_Y_planeTransSet()->GetDirectory() );
            this->_cfdTFM_Y_Vector->SetFrameDataType( cfdFrame::VTK_VECTOR );
            numFrames = this->_cfdTFM_Y_Vector->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_Y_Vector );     
            this->dataList.back()->SetObjectType( Y_TRANSIENT_CONTOUR_AND_VECTOR );
            //this->dataList.back()->SetSequence( this->transientSequence->GetSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_Y_Vector );
            break;
         }
      }

      //
      // Initiate Transient Vector Data
      //
      if ( this->transientInfo[ 0 ]->Get_Z_planeTransSet() &&
           this->transientInfo[ 0 ]->Get_Z_planeTransSet()->GetNumberOfVectors() ) 
      {
         for ( int i = 0; i < (int)this->transientInfo.size(); i++ )
         {
            std::cout << "| 48. Initializing......................... Transient Z_Vector Data |" << std::endl;
            this->_cfdTFM_Z_Vector = new cfdTransientFlowManager();
            this->_cfdTFM_Z_Vector->SetParameterFile( this, i );
            this->_cfdTFM_Z_Vector->SetDirectory( this->transientInfo[ 0 ]->Get_Z_planeTransSet()->GetDirectory() );
            this->_cfdTFM_Z_Vector->SetFrameDataType( cfdFrame::VTK_VECTOR );
            numFrames = this->_cfdTFM_Z_Vector->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_Z_Vector );     
            this->dataList.back()->SetObjectType( Z_TRANSIENT_CONTOUR_AND_VECTOR );
            //this->dataList.back()->SetSequence( this->transientSequence->GetSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_Z_Vector );
            break;
         }
      }

      //
      // Initiate Transient Geometry Data
      //
      if ( this->transientInfo[ 0 ]->GetGeometryDir() ) 
      {
         int count = 0;
         for ( int i = 0; i < (int)this->transientInfo.size(); i++ )
         {
            std::cout << "| 49. Initializing......................... Transient Geometry Data |" << std::endl;
            this->_cfdTFM_Geometry[ count ] = new cfdTransientFlowManager();
            this->_cfdTFM_Geometry[ count ]->SetParameterFile( this, i );
            this->_cfdTFM_Geometry[ count ]->SetDirectory( this->transientInfo[ 0 ]->GetGeometryDir() );
            this->_cfdTFM_Geometry[ count ]->SetFrameDataType( cfdFrame::GEOM );
            numFrames = this->_cfdTFM_Geometry[ count ]->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->_cfdTFM_Geometry[ count ]->SetObjectType( TRANS_GEOM );
            //this->_cfdTFM_Geometry[ count ]->SetSequence( this->transientSequence->GetSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_Geometry[ count ] );
            this->dataList.push_back( this->_cfdTFM_Geometry[ count ] ); 

            this->_cfdTFM_Geometry[ count ]->LoadFrames();

            count++;
            if ( count == 2 )
               break;
         }
      } 

      //
      // Initiate Transient Particle Data
      //
      if ( this->transientInfo[ 0 ]->GetDropletTransSet() ) 
      {
         for ( int i = 0; i < (int)this->transientInfo.size(); i++ )
         {
            std::cout << "| 50. Initializing......................... Transient Particle Data |" << std::endl;
            this->_cfdTFM_Particle = new cfdTransientFlowManager();
            this->_cfdTFM_Particle->SetParameterFile( this, i );
            this->_cfdTFM_Particle->SetDirectory( this->transientInfo[ 0 ]->GetDropletTransSet()->GetDirectory() );
            this->_cfdTFM_Particle->SetFrameDataType( cfdFrame::VTK_PARTICLE );
            numFrames = this->_cfdTFM_Particle->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_Particle ); 
            this->dataList.back()->SetObjectType( PARTICLE_TRANSIENT );
            //this->dataList.back()->SetSequence( this->transientSequence->GetSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_Particle );
            break;
         }
      } 

      // Add the appropriate number of pfGroups to the sequence node...
      this->transientSequence->SetpfGroups();
   } 

}

void cfdTransientVizHandler::PreFrameUpdate( void )
{
   // request to overide default scalar and vector option
   if ( _commandArray->GetCommandValue(  cfdCommandArray::CFD_ID ) == SET_TRANSIENT_OPTIONS )
   {
      int objectType = _commandArray->GetCommandValue(  cfdCommandArray::CFD_ISO_VALUE );
      vprDEBUG(vprDBG_ALL,1)
         << " Changing transient object type to: " << objectType
         << std::endl << vprDEBUG_FLUSH;

      // The gui should block out unauthorized access to the button. 
      // But use guards just in case...
      if ( objectType == X_TRANSIENT_CONTOUR ||
           objectType == X_TRANSIENT_CONTOUR_AND_VECTOR )
      {
         if ( this->_cfdTFM_X_Contour[0] ) 
            this->_cfdTFM_X_Contour[0]->SetObjectType( objectType );

         if ( this->_cfdTFM_X_Contour[1] )
            this->_cfdTFM_X_Contour[1]->SetObjectType( objectType );
      }

      if ( objectType == Y_TRANSIENT_CONTOUR ||
           objectType == Y_TRANSIENT_CONTOUR_AND_VECTOR )
      {
         if ( this->_cfdTFM_Y_Contour )
            this->_cfdTFM_Y_Contour->SetObjectType( objectType );
      }

      if ( objectType == Z_TRANSIENT_CONTOUR ||
           objectType == Z_TRANSIENT_CONTOUR_AND_VECTOR )
      {
         if ( this->_cfdTFM_Z_Contour  )
            this->_cfdTFM_Z_Contour->SetObjectType( objectType );
      }

      if ( objectType == X_TRANSIENT_VECTOR ||
           objectType == X_TRANSIENT_CONTOUR_AND_VECTOR )
      {
         if ( this->_cfdTFM_X_Vector ) 
            this->_cfdTFM_X_Vector->SetObjectType( objectType );
      }

      if ( objectType == Y_TRANSIENT_VECTOR ||
           objectType == Y_TRANSIENT_CONTOUR_AND_VECTOR )
      {
         if ( this->_cfdTFM_Y_Vector ) 
            this->_cfdTFM_Y_Vector->SetObjectType( objectType );
      }

      if ( objectType == Z_TRANSIENT_VECTOR ||
           objectType == Z_TRANSIENT_CONTOUR_AND_VECTOR )
      {
         if ( this->_cfdTFM_Z_Vector ) 
            this->_cfdTFM_Z_Vector->SetObjectType( objectType );
      }

      _commandArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );
   }
   else if ( _commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_RESET )
   { 
      //delete all transient objects in the scene
      if ( this->_activeSequenceObject )
      {
         //vprDEBUG(vprDBG_ALL,1) << "Sequence address : " 
           //<< this->_activeSequenceObject->GetSequence()
           //<< std::endl << vprDEBUG_FLUSH;

         // Stop the sequence and set the active sequence to NULL
         // so that the progress bar will not try to update
         //this->_activeSequenceObject->GetSequence()->StopSequence();
         this->_activeSequenceObject = NULL;  

         for ( int i = 0; i < (int)this->dataList.size(); i++ )
         {
            // remove geodes from sequence groups and clear cfdObjects geodes list
            {
               //this->dataList[ i ]->GetSequence()->ClearSequence();
               ((cfdTransientFlowManager*)this->dataList[ i ])->isTimeToUpdateSequence = 1;
            }
         }
      }
      else
      {
         vprDEBUG(vprDBG_ALL,1) << " Don't have an active sequence object" 
                                << std::endl << vprDEBUG_FLUSH;
      }

      _commandArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );
   }
   else if ( _commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_STOP )   // request to pause the sequence
   {
      if ( this->_activeSequenceObject )
      {
         //vprDEBUG(vprDBG_ALL,1) << "Sequence address : " 
           //        << this->_activeSequenceObject->GetSequence()
             //      << std::endl << vprDEBUG_FLUSH;
                   
         //pausing the transient flow to the graph
         vprDEBUG(vprDBG_ALL,1) << "pausing active sequence" 
                                << std::endl << vprDEBUG_FLUSH;

         //this->_activeSequenceObject->GetSequence()->PauseSequence();         
         // There is only one sequence for all transient 
         // models so we can break after one has been found
      }
      _commandArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );
   }
   else if ( _commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_BACKWARD )   // request to step back in the sequence
   {
      if ( this->_activeSequenceObject )
      {  
         //this->_activeSequenceObject->GetSequence()->ReverseSequence();
      }
      _commandArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );
   }
   else if ( _commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_FORWARD )   // request to step forrward in the sequence
   {
      if ( this->_activeSequenceObject )
      {  
         //this->_activeSequenceObject->GetSequence()->ForwardSequence();
      }
      _commandArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );
   }

   if ( _commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_VIS_ACTIVE ) 
   {
      // Get dataset name from modelHandler as to which trans model is active
      // create list of names of datasets that are to be used
      // for the number of datasets
         // set dataset active
         // after the dataset is active set viz feature active
      // end for
      // Need to do frame counting above because can only set viz feature after dataset is active
      // once geode is created we need to copy geode into holder for tempanimation
   }

   //if ( command map vector is not empty )
   {
      // set vjobs command array accordingly
   }

   // Scene update stuff for transient
   // Need to fix this for transient
               //add the transient flow to the graph

/*            // verify that if a transient sequence is desired, 
            // an appropriate DCS is active...
            if ( ( ( X_TRANSIENT_CONTOUR <= this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) ) &&
                                          ( this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID )<= PARTICLE_TRANSIENT ) ) &&
                 ! cfdObjects::GetActiveDataSet()->IsPartOfTransientSeries() )
            {
               std::cerr << "\nERROR: You must activate an appropriate transient "
                         << "dataset before proceeding\n" << std::endl;
               this->_activeObject = NULL;
               this->computeActorsAndGeodes = false;
               break;
            }

               {
                  int i;
                  // Need to fix this stuff
                  //tfmTest->CreateNodeList();

                  this->dataList[ i ]->GetSequence()->AddToSequence( 
                                       this->dataList[ i ]->GetObjectType());
            
                  if ( this->_activeSequenceObject )
                  {
                     vprDEBUG(vprDBG_ALL,1) << " ResumeSequence"
                                            << std::endl << vprDEBUG_FLUSH;
                     this->dataList[ i ]->GetSequence()->ResumeSequence();
                  }
                  else
                  {
                     vprDEBUG(vprDBG_ALL,1) << " StartSequence"
                                            << std::endl << vprDEBUG_FLUSH;
                     this->dataList[ i ]->GetSequence()->StartSequence();
                  }

                  // Set active sequence to current sequence
                  this->_activeSequenceObject = this->dataList[ i ];
               }*/
}

void cfdTransientVizHandler::CreateActors( void )
{
   //if ( tfmTest != NULL )
   {
      vprDEBUG(vprDBG_ALL,1)
        << "transient object." << std::endl << vprDEBUG_FLUSH;

      vprDEBUG(vprDBG_ALL,1) << "_activeObject->GetObjectType() = "
                             << this->_activeObject->GetObjectType()
                             << std::endl << vprDEBUG_FLUSH;

      for ( int i = 0; i < (int)this->dataList.size(); i++ )
      {
         // Transient Vector, Contour, and/or Particle Update
         if ( this->_activeObject->GetObjectType() == 
              this->dataList[ i ]->GetObjectType() )
         {
            //this->dataList[ i ]->SetDCS( this->_activeDataSetDCS );
            vprDEBUG(vprDBG_ALL,1) << "Trans Data : Object : " << i  
                                   << std::endl << vprDEBUG_FLUSH;

            this->dataList[ i ]->Update();
         }
         // Transient Geometry Update
         else if ( this->dataList[ i ]->GetObjectType() == TRANS_GEOM )
         {
            //this->dataList[ i ]->SetDCS( this->_activeDataSetDCS );
            vprDEBUG(vprDBG_ALL,1) << "Trans Geom : Object : " << i
                                   << std::endl << vprDEBUG_FLUSH;

            this->dataList[ i ]->Update();
         }
      }
      this->_activeObject = NULL;
      this->computeActorsAndGeodes = false;
   }
}

void cfdTransientVizHandler::CreateObjects( void )
{
   int numObjects;
   std::ifstream input;
   char text[ 256 ];
   char textLine[ 256 ];
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
      if ( id == 10 )
      {
         // how many directories contain transient vtk data?
         int numTransientSets;
         input >> numTransientSets;
         input.getline( textLine, 256 );   //skip past remainder of line

         int ii = transientInfo.size();
         this->transientInfo.push_back( new cfdTransientInfo() );

         vprDEBUG(vprDBG_ALL,0) << " transient DCS parameters:"
                                 << std::endl << vprDEBUG_FLUSH;

         float scale[3], trans[3], rotate[3];   // pfDCS stuff
         this->_readParam->read_pf_DCS_parameters( input, scale, trans, rotate);

         // This needs to be moved to cfdModel
         cfdDCS* dcs = new cfdDCS;
         /*dcs->SetScaleArray( scale );
         dcs->SetTranslationArray( trans );
         dcs->SetRotationArray( rotate );
         this->transientInfo[ ii ]->SetDCS( dcs );*/

         // read the directories...
         char ** transientDataDir = new char * [ numTransientSets ];
         for( int i = 0; i < numTransientSets; i++ )
         {
            transientDataDir[ i ] = _readParam->readDirName( input, "transientDataDir" );
            int id = _readParam->readID( input );
            vprDEBUG(vprDBG_ALL,0) << "\tbutton id = " << id
                             << std::endl << vprDEBUG_FLUSH;
            if ( transientDataDir[ i ] )
            {
               cfdTransientSet * cfdtransientset = new cfdTransientSet( 
                                              transientDataDir[ i ], id, dcs );
               cfdtransientset->SetParameterFile( this );
               this->transientInfo[ ii ]->LoadTransientSet( cfdtransientset );
            }
            else
            {
               vprDEBUG(vprDBG_ALL,0) << " did not find transient data directory "
                                       << i << std::endl << vprDEBUG_FLUSH;
            }
         }

         char * geometryDir = _readParam->readDirName( input, "geometryDir" );
         this->transientInfo[ ii ]->SetGeometryDir( geometryDir );

         vprDEBUG(vprDBG_ALL,0) << " transient geometry DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;

         this->_readParam->read_pf_DCS_parameters( input, scale, trans, rotate);

         cfdDCS* geometryDcs = new cfdDCS;
         geometryDcs->SetScaleArray( scale );
         geometryDcs->SetTranslationArray( trans );
         geometryDcs->SetRotationArray( rotate );
         this->transientInfo[ ii ]->SetGeometryDCS( geometryDcs );

         // read geometry transparency flag
         input >> this->transientInfo[ ii ]->trans;
         vprDEBUG(vprDBG_ALL,0) << " trans flag = " 
                          << this->transientInfo[ ii ]->trans
                          << std::endl << vprDEBUG_FLUSH;

         // read geometry color flag and color if flag = 1
         input >> this->transientInfo[ ii ]->color;
         vprDEBUG(vprDBG_ALL,0) << " color flag = " 
                          << this->transientInfo[ ii ]->color
                          << std::endl << vprDEBUG_FLUSH;

         this->transientInfo[ ii ]->stlColor[ 0 ] = -1.0;
         this->transientInfo[ ii ]->stlColor[ 1 ] = -1.0;
         this->transientInfo[ ii ]->stlColor[ 2 ] = -1.0;
         if( this->transientInfo[ ii ]->color )
         {
            for(int i=0; i<3; i++)
            {
               input >> this->transientInfo[ ii ]->stlColor[ i ];
            }
            vprDEBUG(vprDBG_ALL,0) << "   stlColor: " 
                       << this->transientInfo[ ii ]->stlColor[ 0 ] << " : "
                       << this->transientInfo[ ii ]->stlColor[ 1 ] << " : "
                       << this->transientInfo[ ii ]->stlColor[ 2 ]
                       << std::endl << vprDEBUG_FLUSH;
         }
         input.getline( textLine, 256 );   //skip past remainder of line
      
         double dur;
         input >> dur;
         input.getline( textLine, 256 );   //skip past remainder of line
         this->transientInfo[ ii ]->SetDuration( dur );
         vprDEBUG(vprDBG_ALL,0) << " duration = " << dur
                          << std::endl << vprDEBUG_FLUSH;
      }
      else
      {
         // Skip past block
         _readParam->ContinueRead( input, id );
      }
   }
}

void cfdTransientVizHandler::CreateNewDataSet()
{
   this->dataSets.push_back( new cfdDataSet() );
}

int cfdTransientVizHandler::GetNumberOfDataSets()
{
   return (int)this->dataSets.size();
}

cfdDataSet * cfdTransientVizHandler::GetDataSet( int i )
{
   if ( 0 <= i && i < (int)this->dataSets.size() )
      return this->dataSets[ i ];
   else
      return NULL;
}

cfdDataSet * cfdTransientVizHandler::GetDataSetWithName( const char * vtkFilename )
{
   int numDataSets = this->GetNumberOfDataSets();
   for ( int i = 0; i < numDataSets; i++ )
   {
      //std::cout << this->GetDataSet( i )->GetFileName() << std::endl;
      if ( ! strcmp(this->GetDataSet( i )->GetFileName(),vtkFilename) )
      {
         return this->GetDataSet( i );
         break;
      }
   }
   return NULL;
}

