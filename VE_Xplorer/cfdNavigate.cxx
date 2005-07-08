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
 * File:          $RCSfile: cfdNavigate.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/cfdNavigate.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_SceneGraph/cfdDCS.h"

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
}

cfdNavigate::~cfdNavigate( )
{
}

void cfdNavigate::Initialize( VE_SceneGraph::cfdDCS* worldDCS )
{
   this->worldDCS = worldDCS;
   this->cursorLen = 2.0f;
   this->dObj = 0.05f;
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

void cfdNavigate::GetObjLocation( float xyzO[3] )
{
  this->GetObjLocation( xyzO[0], xyzO[1], xyzO[2] );
}

void cfdNavigate::GetObjLocation( float &xO, float &yO, float &zO )
{
  xO = this->objLoc[0];
  yO = this->objLoc[1];
  zO = this->objLoc[2];
}

float * cfdNavigate::GetObjLocation( )
{
  return this->objLoc;
}

float * cfdNavigate::GetCurObjLocation( )
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
   vjMat = wand->getData( );
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
   gmtl::Vec3f loc_temp;
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
    this->worldLoc[i] += this->dir[i]*this->dObj;

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

void cfdNavigate::SetDataValues( int id, int iso )
{
   this->cfdId = id;
   this->cfdIso_value = iso;
}

void cfdNavigate::updateNavigationFromGUI()
{
   this->buttonData[ 1 ] = this->digital[ 1 ]->getData();
   this->buttonData[ 2 ] = this->digital[ 2 ]->getData();
   
   if ( (this->cfdId == GUI_NAV && this->cfdIso_value == NAV_FWD) ||
         this->IHdigital[0]->getData() == gadget::Digital::ON ) 
   //forward translate
   { 
      this->worldTrans[1] += translationStepSize;
   }
   else if ( (this->cfdId == GUI_NAV && this->cfdIso_value == NAV_BKWD) ||
               this->IHdigital[1]->getData() == gadget::Digital::ON ) 
   //backward translate
   { 
      this->worldTrans[1] -= translationStepSize;
   }
   else if ( (this->cfdId == GUI_NAV && this->cfdIso_value == NAV_RIGHT) || 
      this->IHdigital[2]->getData() == gadget::Digital::ON ) 
   //right translate
   { 
        this->worldTrans[0] += translationStepSize;
   }
   else if ( (this->cfdId == GUI_NAV && this->cfdIso_value == NAV_LEFT) || 
               this->IHdigital[3]->getData() == gadget::Digital::ON ) 
   //left translate
   { 
        this->worldTrans[0] -= translationStepSize;
   }
   else if ( (this->cfdId == GUI_NAV && this->cfdIso_value == NAV_UP) ||
               this->IHdigital[4]->getData() == gadget::Digital::ON ) 
   //upward translate
   { 
      this->worldTrans[2] += translationStepSize;  
   }
   else if ( (this->cfdId == GUI_NAV && this->cfdIso_value == NAV_DOWN) ||
               this->IHdigital[5]->getData() == gadget::Digital::ON ) 
   //downward translate
   { 
      this->worldTrans[2] -= translationStepSize;  
   } 
   else if ( (this->cfdId == GUI_NAV && this->cfdIso_value == YAW_CCW) ||
               this->IHdigital[6]->getData() == gadget::Digital::ON )        
   //CW rotation
   {
      float oldAng = worldRot[ 0 ];
         this->worldRot[ 0 ] -= rotationStepSize;
      gmtl::Vec3f  worldVec, wandVec, tempVec, worldPosVec(this->worldTrans[0]- this->LastVec[0], 0,-this->worldTrans[1]-this->LastVec[2]);
      gmtl::Matrix44f rotMat;
      gmtl::EulerAngleXYZf myEuler( 0, -gmtl::Math::deg2Rad(oldAng),0), worldRotVec(0, gmtl::Math::deg2Rad(this->worldRot[0]),0);
     
      vjHeadMat = head->getData();
      gmtl::setTrans(wandVec, vjHeadMat);

      rotMat = gmtl::makeRot<gmtl::Matrix44f>(myEuler);
      gmtl::xform(worldVec, rotMat, worldPosVec);

      rotMat = gmtl::makeRot<gmtl::Matrix44f>(worldRotVec);
      gmtl::xform(worldVec, rotMat, worldVec);
      gmtl::xform(tempVec, rotMat, wandVec);

      LastVec= tempVec - wandVec;
      
      this->worldTrans[0] = worldVec[ 0 ] + LastVec[ 0 ];
      this->worldTrans[1] = -(worldVec[ 2 ] + LastVec[ 2 ]);
   }
   else if ( (this->cfdId == GUI_NAV && this->cfdIso_value == YAW_CW) ||
               this->IHdigital[7]->getData() == gadget::Digital::ON )         
   //CCWrotation
   {
      float oldAng = worldRot[ 0 ];
         this->worldRot[ 0 ] += rotationStepSize;
      gmtl::Vec3f  worldVec, wandVec, tempVec, worldPosVec(this->worldTrans[0]- this->LastVec[0], 0,-this->worldTrans[1]-this->LastVec[2]);
      gmtl::Matrix44f rotMat;
      gmtl::EulerAngleXYZf myEuler( 0, -gmtl::Math::deg2Rad(oldAng),0), worldRotVec(0, gmtl::Math::deg2Rad(this->worldRot[0]),0);
     
      vjHeadMat = head->getData();
      gmtl::setTrans(wandVec, vjHeadMat);

      rotMat = gmtl::makeRot<gmtl::Matrix44f>(myEuler);
      gmtl::xform(worldVec, rotMat, worldPosVec);

      rotMat = gmtl::makeRot<gmtl::Matrix44f>(worldRotVec);
      gmtl::xform(worldVec, rotMat, worldVec);
      gmtl::xform(tempVec, rotMat, wandVec);

      LastVec= tempVec - wandVec;
      
      this->worldTrans[0] = worldVec[ 0 ] + LastVec[ 0 ];
      this->worldTrans[1] = -(worldVec[ 2 ] + LastVec[ 2 ]);
   }
   else if ( (this->cfdId == GUI_NAV && this->cfdIso_value == PITCH_DOWN) )         
   {
         this->worldRot[ 1 ] += rotationStepSize;
   }
   else if ( (this->cfdId == GUI_NAV && this->cfdIso_value == PITCH_UP) )         
   {
         this->worldRot[ 1 ] -= rotationStepSize;
   }
   else if ( (this->cfdId == GUI_NAV && this->cfdIso_value == ROLL_CW) )         
   {
         this->worldRot[ 2 ] -= rotationStepSize;
   }
   else if ( (this->cfdId == GUI_NAV && this->cfdIso_value == ROLL_CCW) )         
   {
         this->worldRot[ 2 ] += rotationStepSize;
   }


   if ( this->buttonData[1] == gadget::Digital::TOGGLE_ON ) //|| (this->cfdId == GUI_NAV && (this->cfdIso_value == YAW_CCW || this->cfdIso_value == YAW_CW)) ||
        //this->buttonData[1] == gadget::Digital::ON )
   {
      this->currentWandDirection = this->GetDirection();

      vprDEBUG(vprDBG_ALL,1) << this->currentWandDirection[0] << " : "
                             << this->currentWandDirection[1] << " : "
                             << this->currentWandDirection[2]
                             << std::endl << vprDEBUG_FLUSH;
      float oldAng = worldRot[ 0 ];
      if ( this->currentWandDirection[ 0 ] > 0.0f )
      {
         this->worldRot[ 0 ] -= rotationStepSize;
      }
      else 
      {
         this->worldRot[ 0 ] += rotationStepSize;
      }

/*      gmtl::Matrix44f worldMat = this->worldDCS->GetMat();
      gmtl::Matrix44f vjHeadMat = head->getData();
      gmtl::Vec3f vjHeadVec = makeTrans< gmtl::Vec3f >( vjHeadMat );
std::cout << vjHeadVec << std::endl;
      gmtl::Vec3f vjWorldHeadVec = worldMat * vjHeadVec;
std::cout << vjWorldHeadVec << std::endl;
      
      gmtl::Vec3f y_axis( 0.0f, 1.0f, 0.0f );
      
      gmtl::Matrix44f yAxisMatrix = gmtl::makeRot<gmtl::Matrix44f>( gmtl::AxisAnglef( gmtl::Math::deg2Rad( this->worldRot[ 0 ] - oldAng ), y_axis ) );
      gmtl::Vec3f vjWorldRotHeadVec = yAxisMatrix * vjWorldHeadVec;
std::cout << vjWorldRotHeadVec << std::endl;

      gmtl::Vec3f newTrans = vjWorldHeadVec - vjWorldRotHeadVec;
std::cout << newTrans << std::endl;
      this->worldTrans[0] += newTrans[0];
      this->worldTrans[1] += (-newTrans[2]); 
*/
      
      gmtl::Vec3f  worldVec, wandVec, tempVec, worldPosVec(this->worldTrans[0]- this->LastVec[0], 0,-this->worldTrans[1]-this->LastVec[2]);
      gmtl::Matrix44f rotMat;
      gmtl::EulerAngleXYZf myEuler( 0, -gmtl::Math::deg2Rad(oldAng),0), worldRotVec(0, gmtl::Math::deg2Rad(this->worldRot[0]),0);
     
      vjHeadMat = head->getData();
      gmtl::setTrans(wandVec, vjHeadMat);

      rotMat = gmtl::makeRot<gmtl::Matrix44f>(myEuler);
      gmtl::xform(worldVec, rotMat, worldPosVec);

      rotMat = gmtl::makeRot<gmtl::Matrix44f>(worldRotVec);
      gmtl::xform(worldVec, rotMat, worldVec);
      gmtl::xform(tempVec, rotMat, wandVec);

      LastVec= tempVec - wandVec;
      this->worldTrans[0] = worldVec[ 0 ] + LastVec[ 0 ];
      this->worldTrans[1] = -(worldVec[ 2 ] + LastVec[ 2 ]);
   }
   else if ( this->buttonData[2] == gadget::Digital::TOGGLE_ON ||
             this->buttonData[2] == gadget::Digital::ON )
   // Navigation based on current wand direction
   { 
      this->FwdTranslate();
      this->GetWorldLocation( this->worldTrans );
   }
   else if ( this->cfdId == CHANGE_TRANSLATION_STEP_SIZE )         
   {
      this->translationStepSize = cfdIso_value * (0.25f/50.0f);
   }
   else if ( this->cfdId == CHANGE_ROTATION_STEP_SIZE )         
   {
      this->rotationStepSize = cfdIso_value * (1.0f/50.0f);
   }

   // Set the DCS postion based off of previous 
   // manipulation of the worldTrans array
   float tempArray[ 3 ];
   for ( unsigned int i = 0; i < 3; i++ )
      tempArray[ i ] = -this->worldTrans[ i ];

   vprDEBUG(vprDBG_ALL,3) << " Navigate" << std::endl << vprDEBUG_FLUSH;
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
float* cfdNavigate::GetWorldRotation()
{
   return worldRot;
}
