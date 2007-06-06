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
#include "VE_Xplorer/XplorerHandlers/cfdNavigate.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#include "VE_Xplorer/SceneGraph/SceneManager.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

// --- VR Juggler Stuff --- //
#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>

using namespace gmtl;
using namespace gadget;
using namespace VE_Xplorer;
using namespace VE_SceneGraph;

cfdNavigate::cfdNavigate( )
{
   wand.init("VJWand");
   head.init("VJHead");
   this->digital[0].init("VJButton0");   // trigger (and top right button) TODO: I think this is unused ?
   this->digital[1].init("VJButton1");   // top left button -- toggle cursor mode: laser, streamlines, box, & arrow
   this->digital[2].init("VJButton2");   // 12 o'clock -- forward navigation
   this->digital[3].init("VJButton3");   // 3 o'clock -- not used at present
   this->digital[4].init("VJButton4");   // 6 o'clock -- reset
   this->digital[5].init("VJButton5");   // 9 o'clock -- exit streamer while loop

   this->IHdigital[0].init("VJMovementZ0");  //press "8" for forward navigation
   this->IHdigital[1].init("VJMovementZ1");  //press "2" for backward navigation
   this->IHdigital[2].init("VJMovementX0");  //press "6" for rightward navigation
   this->IHdigital[3].init("VJMovementX1");  //press "4" for leftward navigation
   this->IHdigital[4].init("VJMovementY0");  //press "9" for upward navigation
   this->IHdigital[5].init("VJMovementY1");  //press "7" for downward navigation
   this->IHdigital[6].init("VJRotateX");  //press "right_arrow" for CW rotation
   this->IHdigital[7].init("VJRotateY");  //press "left_arrow" for CCW rotation
   //this->IHdigital[8].init("IHVJButton8");  //press "up_arrow" for upward rotation
   //this->IHdigital[9].init("IHVJButton9");  //press "down_arrow" for downward rotation
   // inputs for flythrough paths
   flyThrough[0].init("Path_1");
   flyThrough[1].init("Path_2");
   flyThrough[2].init("Path_3");
   flyThrough[3].init("Path_4");

   command = 0;
   rotationFlag = 1;
   subzeroFlag = 0;
}

cfdNavigate::~cfdNavigate( )
{
}

void cfdNavigate::Initialize( VE_SceneGraph::DCS* worldDCS )
{
   this->worldDCS = worldDCS;
   this->cursorLen = 2.0f;
   //this->dObj = 0.05f;
   this->UpdateDir( );
   this->UpdateLoc( );

   this->translationStepSize = 0.25f;
   this->rotationStepSize = 1.0f;
   this->worldLoc[0] = this->worldLoc[1] = this->worldLoc[2] = 0.0f;
   this->worldTrans[0] = 0;
   this->worldTrans[1] = 0;
   this->worldTrans[2] = 0;
   for ( int i=0; i<3; i++ )
   {
      this->cursorLoc[i] = this->loc[i] + this->dir[i]*this->cursorLen;
      this->objLoc[i] = this->cursorLoc[i] + this->worldLoc[i];
      this->LastVec[i] = 0;
   }
}

double * cfdNavigate::GetDirection( )
{
  this->UpdateDir( );

  return this->dir;
}

double * cfdNavigate::GetLocation( )
{
  this->UpdateLoc( );

  return this->loc;
}

void cfdNavigate::GetObjLocation( double xyzO[3] )
{
  this->GetObjLocation( xyzO[0], xyzO[1], xyzO[2] );
}

void cfdNavigate::GetObjLocation( double &xO, double &yO, double &zO )
{
  xO = this->objLoc[0];
  yO = this->objLoc[1];
  zO = this->objLoc[2];
}

double * cfdNavigate::GetObjLocation( )
{
  return this->objLoc;
}

double * cfdNavigate::GetCurObjLocation( )
{
 this->CursorTranslate(); 
 return this->objLoc;
}

double * cfdNavigate::GetCursorLocation( )
{
  this->CursorTranslate( );
  return this->cursorLoc;
}

void cfdNavigate::SetWorldLocation( double xyzS[3] )
{
   // Initialize worldloc in initscene
   this->worldLoc[0] = xyzS[0];
   this->worldLoc[1] = xyzS[1]; 
   this->worldLoc[2] = xyzS[2];
}


void cfdNavigate::GetWorldLocation( double xyzS[3] )
{
  this->GetWorldLocation( xyzS[0], xyzS[1], xyzS[2] );
}

void cfdNavigate::GetWorldLocation( double &xS, double &yS, double &zS )
{
   xS = this->worldLoc[0];// + xS;
   yS = this->worldLoc[1];// + yS;
   zS = this->worldLoc[2];// + zS;
   //cout << " Current X Loc : " << xS << " Current Y Loc : " << yS 
   //      << " Current Z Loc : " << zS << endl;
}

double * cfdNavigate::GetWorldLocation( )
{
  return this->worldLoc;
}

void cfdNavigate::UpdateDir( )      //Changed by Gengxun
{ 
   // get the normalized direction relative to the juggler frame
   vjVec.set( 0.0f, 0.0f, -1.0f );
   //vjMat = wand->getData( );
   for( size_t i = 0; i < 16; ++i )
   {
       vjMat.mData[ i ] = static_cast< double >( wand->getData().mData[i] );
   }
   vjVec = gmtl::xform( vjVec,vjMat,vjVec);
   gmtl::normalize(vjVec);

   // transform from juggler to performer...
   dir[0] =  vjVec[0];
   dir[1] = -vjVec[2];
   dir[2] =  vjVec[1];
}

void cfdNavigate::UpdateLoc( )      //Changed by Gengxun
{
   // get the location relative to the juggler frame
   gmtl::Vec3d loc_temp;
   gmtl::setTrans(loc_temp,vjMat);

   // transform from juggler to performer...
   loc[0] =  loc_temp[0];
   loc[1] = -loc_temp[2];
   loc[2] =  loc_temp[1];
}  

void cfdNavigate::UpdateLoc( double* tempTrans )  //Added by Dave
{
   // used to get cfdNavigate caught up with moving done by quatcams
   this->worldLoc[0] = tempTrans[0];
   this->worldLoc[1] = tempTrans[1];
   this->worldLoc[2] = tempTrans[2];
}

void cfdNavigate::FwdTranslate( )
{
   this->UpdateDir( );

   for ( int i=0; i<3; i++ )
   {
      // Update the translation movement for the objects
      // How much object should move
      this->worldLoc[i] += this->dir[i]*this->translationStepSize;

      // How much the cursor movement are needed to trace back
      // to the object after each movement of the object
      this->objLoc[i] = this->cursorLoc[i] + this->worldLoc[i];
   }
}

void cfdNavigate::AftTranslate( )
{
   this->UpdateDir( );

   for ( int i=0; i<3; i++ )
   {
      // Update the translation movement for the objects
      // How much object should move
      this->worldLoc[i] -= this->dir[i]*this->dObj;

      // How much the cursor movement are needed to trace back
      // to the object after each movement of the object
      this->objLoc[i] = this->cursorLoc[i] + this->worldLoc[i];
   }
}

void cfdNavigate::CursorTranslate( )
{
  this->UpdateDir( );
  this->UpdateLoc( );

  for ( int i=0; i<3; i++ )
  {
    this->cursorLoc[i] = this->loc[i] + this->dir[i]*this->cursorLen;
    this->objLoc[i] = this->cursorLoc[i] + this->worldLoc[i];
  }
}
////////////////////////////////////////////////////////////////////////////////
void cfdNavigate::updateNavigationFromGUI()
{
   this->buttonData[ 1 ] = this->digital[ 1 ]->getData();
   this->buttonData[ 2 ] = this->digital[ 2 ]->getData();

   double* tempWorldRot = this->worldDCS->GetRotationArray();
   this->worldRot[ 0 ] = tempWorldRot[ 0 ];
   this->worldRot[ 1 ] = tempWorldRot[ 1 ];
   this->worldRot[ 2 ] = tempWorldRot[ 2 ];

	double* tempWorldTrans = this->worldDCS->GetVETranslationArray();
	this->worldTrans[ 0 ] = -tempWorldTrans[ 0 ];
	this->worldTrans[ 1 ] = -tempWorldTrans[ 1 ];
	this->worldTrans[ 2 ] = -tempWorldTrans[ 2 ];
   
   // This is NOT how we should do things
   // Command should allowed to be null but because we always
   // have to have a command due to our command structure
   // this hack must be in here
   // This should be changed once our complete command structure is
   // in place
   std::string commandType;
   std::string newCommand;
   if ( command )
   {
      commandType = command->GetCommandName();
   }
   else
   {
      commandType = "wait";
      newCommand = "wait";
   }

   if ( !commandType.compare( "Navigation_Data" ) )
   {
      VE_XML::DataValuePair* commandData = command->GetDataValuePair( 0 );
      this->cfdIso_value = commandData->GetDataValue();
      newCommand = commandData->GetDataName();
   }

   if ( !newCommand.compare( "ROTATE_ABOUT_HEAD" ) )         
   {
      this->SetHeadRotationFlag( this->cfdIso_value );
   }
   else if ( !newCommand.compare( "Z_ZERO_PLANE" ) )         
   {
      this->SetSubZeroFlag( this->cfdIso_value );
   }
   else if ( !newCommand.compare( "RESET_NAVIGATION_POSITION" ) )         
   {
      for ( unsigned int i = 0; i < 3; i++ )
	   {
         this->worldTrans[ i ] = initialTranslate[ i ];
         this->worldRot[ i ] = initialRotate[ i ];
	   }
   }
   else if ( !newCommand.compare( "CHANGE_TRANSLATION_STEP_SIZE" ) )         
   {
      // This equation returns a range of ~ 0.01' -> 220'
      // the equation is 1/100 * e ^ ( x / 10 ) where 1 < x < 100
      this->translationStepSize = 0.01f * exp( cfdIso_value*0.10f );   
   }

   else if ( !newCommand.compare( "CHANGE_ROTATION_STEP_SIZE" ) )         
   {
      // This equation returns a range of ~ 0.00029' -> 1.586' NOTE: These are in degrees
      // This equation is 1 / 750 * ( x / 2 ) ^ 2.2 where 1 < x < 50 
      this->rotationStepSize = (0.001333f) * powf( (cfdIso_value * 0.5f), 2.2f);
   }
   else if ( !newCommand.compare( "GUI_NAV" ) )
   {
      if ( this->cfdIso_value == NAV_FWD ||
            this->IHdigital[0]->getData() == gadget::Digital::ON ) 
      //forward translate
      { 
         this->worldTrans[1] += translationStepSize;
      }
      else if ( this->cfdIso_value == NAV_BKWD ||
                  this->IHdigital[1]->getData() == gadget::Digital::ON ) 
      //backward translate
      { 
         this->worldTrans[1] -= translationStepSize;
      }
      else if ( this->cfdIso_value == NAV_RIGHT || 
         this->IHdigital[2]->getData() == gadget::Digital::ON ) 
      //right translate
      { 
           this->worldTrans[0] += translationStepSize;
      }
      else if ( this->cfdIso_value == NAV_LEFT || 
                  this->IHdigital[3]->getData() == gadget::Digital::ON ) 
      //left translate
      { 
           this->worldTrans[0] -= translationStepSize;
      }
      else if ( this->cfdIso_value == NAV_UP ||
                  this->IHdigital[4]->getData() == gadget::Digital::ON ) 
      //upward translate
      { 
         this->worldTrans[2] += translationStepSize;  
      }
      else if ( this->cfdIso_value == NAV_DOWN ||
                  this->IHdigital[5]->getData() == gadget::Digital::ON ) 
      //downward translate
      { 
         this->worldTrans[2] -= translationStepSize;  
      } 
      else if ( this->cfdIso_value == YAW_CCW ||
                  this->IHdigital[6]->getData() == gadget::Digital::ON )        
      //CW rotation
      {
         this->worldRot[ 0 ] -= rotationStepSize;

         if ( rotationFlag )
         {
            //vjHeadMat = head->getData();
            for( size_t i = 0; i < 16; ++i )
            {
                vjHeadMat.mData[ i ] = static_cast< double >( head->getData().mData[i] );
            }
            // get juggler Matrix of worldDCS
            // Note:: for pf we are in juggler land
            //        for osg we are in z up land
            Matrix44d worldMat;
            worldMat = this->worldDCS->GetMat();

            gmtl::Point3d jugglerHeadPoint, jugglerHeadPointTemp;
            jugglerHeadPoint = gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );
         #ifdef _OSG
            jugglerHeadPointTemp[ 0 ] = jugglerHeadPoint[ 0 ];
            jugglerHeadPointTemp[ 1 ] = -jugglerHeadPoint[ 2 ];
            jugglerHeadPointTemp[ 2 ] = 0;
         #else
            jugglerHeadPointTemp[ 0 ] = jugglerHeadPoint[ 0 ];
            jugglerHeadPointTemp[ 1 ] = 0;
            jugglerHeadPointTemp[ 2 ] = jugglerHeadPoint[ 2 ];
         #endif

            // translate world dcs by distance that the head
            // is away from the origin
            gmtl::Matrix44d transMat = gmtl::makeTrans< gmtl::Matrix44d >( -jugglerHeadPointTemp );
            gmtl::Matrix44d worldMatTrans = transMat * worldMat;
            gmtl::Point3d newJugglerHeadPoint;
            // get the position of the head in the new world space
            // as if the head is on the origin
            gmtl::Point3d newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;

            // Create rotation matrix and juggler head vector
         #ifdef _OSG
            gmtl::EulerAngleXYZd worldRotVecTemp(0,0, gmtl::Math::deg2Rad(-rotationStepSize));
         #else
            gmtl::EulerAngleXYZd worldRotVecTemp(0, gmtl::Math::deg2Rad(-rotationStepSize), 0);
         #endif
            gmtl::Matrix44d rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >(worldRotVecTemp);
            gmtl::Vec4d newGlobalHeadPointVec;
            newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
            newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
            newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];
            // roate the head vector by the rotation increment
            gmtl::Vec4d rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;

            // create translation from new rotated point
            // and add original head off set to the newly found location
            // set world translation accordingly
            this->worldTrans[0] = -(rotateJugglerHeadVec[ 0 ] + jugglerHeadPointTemp[ 0 ] );
         #ifdef _OSG
            this->worldTrans[1] = -(rotateJugglerHeadVec[ 1 ] + jugglerHeadPointTemp[ 1 ] );
         #else
            this->worldTrans[1] = (rotateJugglerHeadVec[ 2 ] + jugglerHeadPointTemp[ 2 ] );
         #endif
         }
      }
      else if ( this->cfdIso_value == YAW_CW ||
                  this->IHdigital[7]->getData() == gadget::Digital::ON )         
      //CCWrotation
      {
         this->worldRot[ 0 ] += rotationStepSize;

         if ( rotationFlag )
         {
            //vjHeadMat = head->getData();
            for( size_t i = 0; i < 16; ++i )
            {
                vjHeadMat.mData[ i ] = static_cast< double >( head->getData().mData[i] );
            }
            // get juggler Matrix of worldDCS
            // Note:: for pf we are in juggler land
            //        for osg we are in z up land
            Matrix44d worldMat;
            worldMat = this->worldDCS->GetMat();

            gmtl::Point3d jugglerHeadPoint, jugglerHeadPointTemp;
            jugglerHeadPoint = gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );
         #ifdef _OSG
            jugglerHeadPointTemp[ 0 ] = jugglerHeadPoint[ 0 ];
            jugglerHeadPointTemp[ 1 ] = -jugglerHeadPoint[ 2 ];
            jugglerHeadPointTemp[ 2 ] = 0;
         #else
            jugglerHeadPointTemp[ 0 ] = jugglerHeadPoint[ 0 ];
            jugglerHeadPointTemp[ 1 ] = 0;
            jugglerHeadPointTemp[ 2 ] = jugglerHeadPoint[ 2 ];
         #endif

            // translate world dcs by distance that the head
            // is away from the origin
            gmtl::Matrix44d transMat = gmtl::makeTrans< gmtl::Matrix44d >( -jugglerHeadPointTemp );
            gmtl::Matrix44d worldMatTrans = transMat * worldMat;
            gmtl::Point3d newJugglerHeadPoint;
            // get the position of the head in the new world space
            // as if the head is on the origin
            gmtl::Point3d newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;

            // Create rotation matrix and juggler head vector
         #ifdef _OSG
            gmtl::EulerAngleXYZd worldRotVecTemp(0,0, gmtl::Math::deg2Rad(rotationStepSize));
         #else
            gmtl::EulerAngleXYZd worldRotVecTemp(0, gmtl::Math::deg2Rad(rotationStepSize), 0);
         #endif
            gmtl::Matrix44d rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >(worldRotVecTemp);
            gmtl::Vec4d newGlobalHeadPointVec;
            newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
            newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
            newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];
            // roate the head vector by the rotation increment
            gmtl::Vec4d rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;

            // create translation from new rotated point
            // and add original head off set to the newly found location
            // set world translation accordingly
            this->worldTrans[0] = -(rotateJugglerHeadVec[ 0 ] + jugglerHeadPointTemp[ 0 ] );
         #ifdef _OSG
            this->worldTrans[1] = -(rotateJugglerHeadVec[ 1 ] + jugglerHeadPointTemp[ 1 ] );
         #else
            this->worldTrans[1] = (rotateJugglerHeadVec[ 2 ] + jugglerHeadPointTemp[ 2 ] );
         #endif
         }
      }
      else if ( this->cfdIso_value == PITCH_DOWN )         
      {
            this->worldRot[ 1 ] += rotationStepSize;
      }
      else if ( this->cfdIso_value == PITCH_UP )         
      {
            this->worldRot[ 1 ] -= rotationStepSize;
      }
      else if ( this->cfdIso_value == ROLL_CW )         
      {
            this->worldRot[ 2 ] -= rotationStepSize;
      }
      else if ( this->cfdIso_value == ROLL_CCW )         
      {
            this->worldRot[ 2 ] += rotationStepSize;
      }
   }

   // navigate with buttons now
   if ( ( this->buttonData[1] == gadget::Digital::TOGGLE_ON ) || 
        ( this->buttonData[1] == gadget::Digital::ON ) )
   {
      this->currentWandDirection = this->GetDirection();

      vprDEBUG(vesDBG,1) << this->currentWandDirection[0] << " : "
                             << this->currentWandDirection[1] << " : "
                             << this->currentWandDirection[2]
                             << std::endl << vprDEBUG_FLUSH;
      if ( this->currentWandDirection[ 0 ] > 0.0f )
      {
         this->worldRot[ 0 ] -= rotationStepSize;
      }
      else 
      {
         this->worldRot[ 0 ] += rotationStepSize;
      }
      
      if ( rotationFlag )
      {
         //vjHeadMat = head->getData();

         for( size_t i = 0; i < 16; ++i )
         {
             vjHeadMat.mData[ i ] = static_cast< double >( head->getData().mData[i] );
         }
         // get juggler Matrix of worldDCS
         // Note:: for pf we are in juggler land
         //        for osg we are in z up land
         Matrix44d worldMat;
         worldMat = this->worldDCS->GetMat();

         gmtl::Point3d jugglerHeadPoint, jugglerHeadPointTemp;
         jugglerHeadPoint = gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );
      #ifdef _OSG
         jugglerHeadPointTemp[ 0 ] = jugglerHeadPoint[ 0 ];
         jugglerHeadPointTemp[ 1 ] = -jugglerHeadPoint[ 2 ];
         jugglerHeadPointTemp[ 2 ] = 0;
      #else
         jugglerHeadPointTemp[ 0 ] = jugglerHeadPoint[ 0 ];
         jugglerHeadPointTemp[ 1 ] = 0;
         jugglerHeadPointTemp[ 2 ] = jugglerHeadPoint[ 2 ];
      #endif

         // translate world dcs by distance that the head
         // is away from the origin
         gmtl::Matrix44d transMat = gmtl::makeTrans< gmtl::Matrix44d >( -jugglerHeadPointTemp );
         gmtl::Matrix44d worldMatTrans = transMat * worldMat;
         gmtl::Point3d newJugglerHeadPoint;
         // get the position of the head in the new world space
         // as if the head is on the origin
         gmtl::Point3d newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;

         // Create rotation matrix and juggler head vector
         gmtl::Matrix44d rotMatTemp;
         if ( this->currentWandDirection[ 0 ] > 0.0f )
         {
         #ifdef _OSG
            gmtl::EulerAngleXYZd worldRotVecTemp(0,0, gmtl::Math::deg2Rad(-rotationStepSize));
         #else
            gmtl::EulerAngleXYZd worldRotVecTemp(0, gmtl::Math::deg2Rad(-rotationStepSize), 0);
         #endif
            rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >(worldRotVecTemp);
         }
         else 
         {
         #ifdef _OSG
            gmtl::EulerAngleXYZd worldRotVecTemp(0,0, gmtl::Math::deg2Rad(rotationStepSize));
         #else
            gmtl::EulerAngleXYZd worldRotVecTemp(0, gmtl::Math::deg2Rad(rotationStepSize), 0);
         #endif
            rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >(worldRotVecTemp);
         }
         gmtl::Vec4d newGlobalHeadPointVec;
         newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
         newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
         newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];
         // roate the head vector by the rotation increment
         gmtl::Vec4d rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;

         // create translation from new rotated point
         // and add original head off set to the newly found location
         // set world translation accordingly
         this->worldTrans[0] = -(rotateJugglerHeadVec[ 0 ] + jugglerHeadPointTemp[ 0 ] );
      #ifdef _OSG
         this->worldTrans[1] = -(rotateJugglerHeadVec[ 1 ] + jugglerHeadPointTemp[ 1 ] );
      #else
         this->worldTrans[1] = (rotateJugglerHeadVec[ 2 ] + jugglerHeadPointTemp[ 2 ] );
      #endif
      }
   }
   else if ( this->buttonData[2] == gadget::Digital::TOGGLE_ON ||
             this->buttonData[2] == gadget::Digital::ON )
   // Navigation based on current wand direction
   { 
      this->FwdTranslate();
      this->GetWorldLocation( this->worldTrans );
   }

   // Set the DCS postion based off of previous 
   // manipulation of the worldTrans array
   double tempArray[ 3 ];
   for ( unsigned int i = 0; i < 3; i++ )
      tempArray[ i ] = -this->worldTrans[ i ];

   //Do not allow translation below z = 0 plane
   if( subzeroFlag ){
      if( tempArray[2] > 0 )
      {
         tempArray[2] = 0;
      }
   }

   vprDEBUG(vesDBG,3) << " Navigate" << std::endl << vprDEBUG_FLUSH;
   this->worldDCS->SetTranslationArray( tempArray );
   this->worldDCS->SetRotationArray( this->worldRot );   
   this->UpdateLoc( this->worldTrans );
}
//////////////////////////////////////////
double* cfdNavigate::GetWorldTranslation()
{
   return worldTrans;
}
//////////////////////////////////////////
double* cfdNavigate::GetWorldRotation()
{
   return worldRot;
}
//////////////////////////////////////////
void cfdNavigate::SetHeadRotationFlag( int input )
{
   rotationFlag = input;
}
//////////////////////////////////////////
void cfdNavigate::SetSubZeroFlag( int input )
{
   subzeroFlag = input;
}
//////////////////////////////////////////
void cfdNavigate::SetVECommand( VE_XML::Command* veCommand )
{
   command = veCommand;
}
