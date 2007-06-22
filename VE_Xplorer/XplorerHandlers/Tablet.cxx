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
#include "VE_Xplorer/XplorerHandlers/Tablet.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#include "VE_Xplorer/XplorerHandlers/DeviceHandler.h"
#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"
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
////////////////////////////////////////////////////////////////////////////////
Tablet::Tablet( )
{
   head.init("VJHead");

   command = 0;
   rotationFlag = 1;
   subzeroFlag = 0;
   Initialize();
}
////////////////////////////////////////////////////////////////////////////////
Tablet::~Tablet()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Tablet::Initialize( void )
{
   translationStepSize = 0.25f;
   rotationStepSize = 1.0f;
}
////////////////////////////////////////////////////////////////////////////////
void Tablet::UpdateNavigation()
{
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
      cfdIso_value = commandData->GetDataValue();
      newCommand = commandData->GetDataName();
   }
   else
   {
      return;
   }

   osg::Quat rot_quat;
   osg::Quat world_quat = activeDCS->getAttitude();
   
   double worldTrans[3];
	double* tempWorldTrans = activeDCS->GetVETranslationArray();
	worldTrans[0] = -tempWorldTrans[0];
	worldTrans[1] = -tempWorldTrans[1];
	worldTrans[2] = -tempWorldTrans[2];
   
   if ( !newCommand.compare( "ROTATE_ABOUT_HEAD" ) )         
   {
      SetHeadRotationFlag( cfdIso_value );
   }
   else if ( !newCommand.compare( "Z_ZERO_PLANE" ) )         
   {
      SetSubZeroFlag( cfdIso_value );
   }
   else if ( !newCommand.compare( "RESET_NAVIGATION_POSITION" ) )         
   {
      for ( unsigned int i = 0; i < 3; i++ )
	   {
         worldTrans[i] = 0.0f;
         world_quat.set( 0, 0, 0, 1 );
         center_point->mData[i] = 0.0f;
	   }

      center_point->mData[1] = worldTrans[1] = 2.0f;
   }
   else if ( !newCommand.compare( "CHANGE_TRANSLATION_STEP_SIZE" ) )         
   {
      // This equation returns a range of ~ 0.01' -> 220'
      // the equation is 1/100 * e ^ ( x / 10 ) where 1 < x < 100
      translationStepSize = 0.01f * exp( cfdIso_value*0.10f );   
   }

   else if ( !newCommand.compare( "CHANGE_ROTATION_STEP_SIZE" ) )         
   {
      // This equation returns a range of ~ 0.00029' -> 1.586' NOTE: These are in degrees
      // This equation is 1 / 750 * ( x / 2 ) ^ 2.2 where 1 < x < 50 
      rotationStepSize = (0.001333f) * powf( (cfdIso_value * 0.5f), 2.2f);
   }
   else if ( !newCommand.compare( "GUI_NAV" ) )
   {
      if ( cfdIso_value == NAV_FWD ) 
      //forward translate
      { 
         worldTrans[1] += translationStepSize;
         center_point->mData[1] -= translationStepSize;
      }
      else if ( cfdIso_value == NAV_BKWD ) 
      //backward translate
      { 
         worldTrans[1] -= translationStepSize;
         center_point->mData[1] += translationStepSize;
      }
      else if ( cfdIso_value == NAV_RIGHT ) 
      //right translate
      { 
         worldTrans[0] += translationStepSize;
         center_point->mData[0] -= translationStepSize;
      }
      else if ( cfdIso_value == NAV_LEFT ) 
      //left translate
      { 
         worldTrans[0] -= translationStepSize;
         center_point->mData[0] += translationStepSize;
      }
      else if ( cfdIso_value == NAV_UP ) 
      //upward translate
      { 
         worldTrans[2] += translationStepSize;
         center_point->mData[2] -= translationStepSize;
      }
      else if ( cfdIso_value == NAV_DOWN ) 
      //downward translate
      { 
         worldTrans[2] -= translationStepSize;
         center_point->mData[2] += translationStepSize;
      } 
      else if ( cfdIso_value == YAW_CCW )        
      //CW rotation
      {
         rot_quat = osg::Quat( osg::DegreesToRadians( rotationStepSize ), osg::Vec3d( 0, 0, 1 ) );

         if ( rotationFlag )
         {
            //vjHeadMat = head->getData();
            vjHeadMat = convertTo< double >( head->getData() );
            /*for( size_t i = 0; i < 16; ++i )
            {
                vjHeadMat.mData[ i ] = static_cast< double >( head->getData().mData[i] );
            }*/
            // get juggler Matrix of worldDCS
            // Note:: for pf we are in juggler land
            //        for osg we are in z up land
            Matrix44d worldMat;
            worldMat = activeDCS->GetMat();

            gmtl::Point3d jugglerHeadPoint, jugglerHeadPointTemp;
            jugglerHeadPoint = gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );
            jugglerHeadPointTemp[0] = jugglerHeadPoint[0];
            jugglerHeadPointTemp[1] = -jugglerHeadPoint[2];
            jugglerHeadPointTemp[2] = 0;

            // translate world dcs by distance that the head
            // is away from the origin
            gmtl::Matrix44d transMat = gmtl::makeTrans< gmtl::Matrix44d >( -jugglerHeadPointTemp );
            gmtl::Matrix44d worldMatTrans = transMat * worldMat;
            gmtl::Point3d newJugglerHeadPoint;
            // get the position of the head in the new world space
            // as if the head is on the origin
            gmtl::Point3d newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;

            // Create rotation matrix and juggler head vector
            gmtl::EulerAngleXYZd worldRotVecTemp( 0, 0, gmtl::Math::deg2Rad( rotationStepSize ) );
            gmtl::Matrix44d rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >( worldRotVecTemp );
            gmtl::Vec4d newGlobalHeadPointVec;
            newGlobalHeadPointVec[0] = newGlobalHeadPointTemp[0];
            newGlobalHeadPointVec[1] = newGlobalHeadPointTemp[1];
            newGlobalHeadPointVec[2] = newGlobalHeadPointTemp[2];
            // roate the head vector by the rotation increment
            gmtl::Vec4d rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;

            // create translation from new rotated point
            // and add original head off set to the newly found location
            // set world translation accordingly

            worldTrans[0] = -(rotateJugglerHeadVec[0] + jugglerHeadPointTemp[0] );
            worldTrans[1] = -(rotateJugglerHeadVec[1] + jugglerHeadPointTemp[1] );

            center_point->mData[0] = -worldTrans[0];
            center_point->mData[1] = -worldTrans[1];
         }
      }
      else if ( cfdIso_value == YAW_CW  )         
      //CCWrotation
      {
         rot_quat = osg::Quat( osg::DegreesToRadians( -rotationStepSize ), osg::Vec3d( 0, 0, 1 ) );

         if ( rotationFlag )
         {
            //vjHeadMat = head->getData();
            vjHeadMat = convertTo< double >( head->getData() );
            /*for( size_t i = 0; i < 16; ++i )
            {
                vjHeadMat.mData[ i ] = static_cast< double >( head->getData().mData[i] );
            }*/
            // get juggler Matrix of worldDCS
            // Note:: for pf we are in juggler land
            //        for osg we are in z up land
            Matrix44d worldMat;
            worldMat = activeDCS->GetMat();

            gmtl::Point3d jugglerHeadPoint, jugglerHeadPointTemp;
            jugglerHeadPoint = gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );
         #ifdef _OSG
            jugglerHeadPointTemp[0] = jugglerHeadPoint[0];
            jugglerHeadPointTemp[1] = -jugglerHeadPoint[2];
            jugglerHeadPointTemp[2] = 0;
         #else
            jugglerHeadPointTemp[0] = jugglerHeadPoint[0];
            jugglerHeadPointTemp[1] = 0;
            jugglerHeadPointTemp[2] = jugglerHeadPoint[2];
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
            gmtl::EulerAngleXYZd worldRotVecTemp( 0, 0, gmtl::Math::deg2Rad( -rotationStepSize ) );
         #else
            gmtl::EulerAngleXYZd worldRotVecTemp( 0, gmtl::Math::deg2Rad( -rotationStepSize ), 0 );
         #endif
            gmtl::Matrix44d rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >( worldRotVecTemp );
            gmtl::Vec4d newGlobalHeadPointVec;
            newGlobalHeadPointVec[0] = newGlobalHeadPointTemp[0];
            newGlobalHeadPointVec[1] = newGlobalHeadPointTemp[1];
            newGlobalHeadPointVec[2] = newGlobalHeadPointTemp[2];
            // roate the head vector by the rotation increment
            gmtl::Vec4d rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;

            // create translation from new rotated point
            // and add original head off set to the newly found location
            // set world translation accordingly
            worldTrans[0] = -(rotateJugglerHeadVec[0] + jugglerHeadPointTemp[0] );
         #ifdef _OSG
            worldTrans[1] = -(rotateJugglerHeadVec[1] + jugglerHeadPointTemp[1] );
         #else
            worldTrans[1] = (rotateJugglerHeadVec[2] + jugglerHeadPointTemp[2] );
         #endif

            center_point->mData[0] = -worldTrans[0];
            center_point->mData[1] = -worldTrans[1];
         }
      }

      //yes
      else if ( cfdIso_value == PITCH_DOWN )         
      {
         rot_quat = osg::Quat( osg::DegreesToRadians( rotationStepSize ), osg::Vec3d( 1, 0, 0 ) );
      }
      else if ( cfdIso_value == PITCH_UP )         
      {
         rot_quat = osg::Quat( osg::DegreesToRadians( -rotationStepSize ), osg::Vec3d( 1, 0, 0 ) );
      }
      else if ( cfdIso_value == ROLL_CW )         
      {
         rot_quat = osg::Quat( osg::DegreesToRadians( rotationStepSize ), osg::Vec3d( 0, 1, 0 ) );
      }
      else if ( cfdIso_value == ROLL_CCW )         
      {
         rot_quat = osg::Quat( osg::DegreesToRadians( -rotationStepSize ), osg::Vec3d( 0, 1, 0 ) );
      }
   }

   // Set the DCS postion based off of previous 
   // manipulation of the worldTrans array
   for ( unsigned int i = 0; i < 3; i++ )
   {   
      worldTrans[ i ] = -worldTrans[ i ];
   }

   //Do not allow translation below z = 0 plane
   if( subzeroFlag )
   {
      if( worldTrans[2] > 0 )
      {
         worldTrans[2] = 0;
      }
   }

   activeDCS->SetTranslationArray( worldTrans );

   world_quat *= rot_quat;
   activeDCS->SetQuat( world_quat );

   vprDEBUG(vesDBG,3) << "|\tEnd Tablet Navigate" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void Tablet::SetHeadRotationFlag( int input )
{
   rotationFlag = input;
}
////////////////////////////////////////////////////////////////////////////////
void Tablet::SetSubZeroFlag( int input )
{
   subzeroFlag = input;
}
////////////////////////////////////////////////////////////////////////////////
void Tablet::SetVECommand( VE_XML::Command* veCommand )
{
   command = veCommand;
}
