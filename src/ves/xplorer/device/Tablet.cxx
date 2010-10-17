/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/device/Tablet.h>
#include <ves/xplorer/device/KeyboardMouse.h>

#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/Debug.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

// --- VR Juggler Includes --- //
#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

#ifdef MINERVA_GIS_SUPPORT
//These includes must be below the gmlt matrix convert header
#include <ves/xplorer/minerva/MinervaManager.h>
#include <Minerva/Core/TileEngine/Body.h>
#include <Minerva/Core/TileEngine/LandModel.h>
#endif

using namespace gmtl;
using namespace gadget;
using namespace ves::xplorer::device;
using namespace ves::xplorer::scenegraph;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
Tablet::Tablet( )
    :
    Device( TABLET )
{
    head.init( "VJHead" );

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
Tablet* Tablet::AsTablet()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
void Tablet::Initialize()
{
    translationStepSize = 0.25f;
    rotationStepSize = 1.0f;
}
////////////////////////////////////////////////////////////////////////////////
void Tablet::ProcessEvents( ves::open::xml::CommandPtr command )
{
    vprDEBUG( vesDBG, 3 ) << "|\tTablet Navigate ProcessEvents" 
        << std::endl << vprDEBUG_FLUSH;

    if( !command )
    {
        return;
    }

    const std::string commandType = command->GetCommandName();

    std::string newCommand;
    if( !commandType.compare( "Navigation_Data" ) )
    {
        DataValuePairPtr commandData = command->GetDataValuePair( 0 );
        cfdIso_value = commandData->GetDataValue();
        newCommand = commandData->GetDataName();
    }
    else
    {
        return;
    }

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > world =
        ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS();

    osg::Quat rot_quat;
    osg::Quat world_quat = world->getAttitude();

    double worldTrans[ 3 ];
    double* tempWorldTrans = world->GetVETranslationArray();
    worldTrans[ 0 ] = -tempWorldTrans[ 0 ];
    worldTrans[ 1 ] = -tempWorldTrans[ 1 ];
    worldTrans[ 2 ] = -tempWorldTrans[ 2 ];

    if( !newCommand.compare( "ROTATE_ABOUT_HEAD" ) )
    {
        SetHeadRotationFlag( cfdIso_value );
    }
    else if( !newCommand.compare( "Z_ZERO_PLANE" ) )
    {
        SetSubZeroFlag( cfdIso_value );
    }
    else if( !newCommand.compare( "RESET_NAVIGATION_POSITION" ) )
    {
        world_quat = *mResetAxis;
        for( unsigned int i = 0; i < 3; ++i )
        {
            worldTrans[ i ] = -mResetPosition->at( i );
            //world_quat[ i ] = 0.0f;
            mCenterPoint->mData[ i ] = 0.0f;
        }
    }
    else if( !newCommand.compare( "CHANGE_TRANSLATION_STEP_SIZE" ) )
    {
        //This equation returns a range of ~ 0.01' -> 220'
        //the equation is 1/100 * e ^ ( x / 10 ) where 1 < x < 100
        translationStepSize = 0.01f * exp( cfdIso_value * 0.10f );
    }
    else if( !newCommand.compare( "CHANGE_ROTATION_STEP_SIZE" ) )
    {
        //This equation returns a range of ~ 0.00029' -> 1.586' NOTE: These are in degrees
        //This equation is 1 / 750 * ( x / 2 ) ^ 2.2 where 1 < x < 50
        rotationStepSize = ( 0.001333f ) * powf(( cfdIso_value * 0.5f ), 2.2f );
    }
    else if( !newCommand.compare( "GUI_NAV" ) )
    {
        //Forward translate
        if( cfdIso_value == NAV_FWD )
        {
            worldTrans[ 1 ] += translationStepSize;
            mCenterPoint->mData[ 1 ] -= translationStepSize;
        }
        //Backward translate
        else if( cfdIso_value == NAV_BKWD )
        {
            worldTrans[ 1 ] -= translationStepSize;
            mCenterPoint->mData[ 1 ] += translationStepSize;
        }
        //Right translate
        else if( cfdIso_value == NAV_RIGHT )
        {
            worldTrans[ 0 ] += translationStepSize;
            mCenterPoint->mData[ 0 ] -= translationStepSize;
        }
        //Left translate
        else if( cfdIso_value == NAV_LEFT )
        {
            worldTrans[ 0 ] -= translationStepSize;
            mCenterPoint->mData[ 0 ] += translationStepSize;
        }
        //Upward translate
        else if( cfdIso_value == NAV_UP )
        {
            worldTrans[ 2 ] += translationStepSize;
            mCenterPoint->mData[ 2 ] -= translationStepSize;
        }
        //Downward translate
        else if( cfdIso_value == NAV_DOWN )
        {
            worldTrans[ 2 ] -= translationStepSize;
            mCenterPoint->mData[ 2 ] += translationStepSize;
        }
        //CCW rotation
        else if( cfdIso_value == YAW_CCW )
        {
            rot_quat = osg::Quat( osg::DegreesToRadians( rotationStepSize ), osg::Vec3d( 0, 0, 1 ) );

            if( rotationFlag )
            {
                vjHeadMat = gmtl::convertTo< double >( head->getData() );

                //Get juggler Matrix of worldDCS
                //Note:: for pf we are in juggler land
                //       for osg we are in z up land
                Matrix44d worldMat;
                worldMat = world->GetMat();

                gmtl::Point3d jugglerHeadPoint, jugglerHeadPointTemp;
                jugglerHeadPoint = gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );
                jugglerHeadPointTemp[ 0 ] = jugglerHeadPoint[ 0 ];
                jugglerHeadPointTemp[ 1 ] = -jugglerHeadPoint[ 2 ];
                jugglerHeadPointTemp[ 2 ] = 0;

                //Translate world dcs by distance that the head is away from the origin
                gmtl::Matrix44d transMat = gmtl::makeTrans< gmtl::Matrix44d >( -jugglerHeadPointTemp );
                gmtl::Matrix44d worldMatTrans = transMat * worldMat;
                gmtl::Point3d newJugglerHeadPoint;

                //Get the position of the head in the new world space as if the head is on the origin
                gmtl::Point3d newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;

                //Create rotation matrix and juggler head vector
                gmtl::EulerAngleXYZd worldRotVecTemp( 0, 0, gmtl::Math::deg2Rad( rotationStepSize ) );
                gmtl::Matrix44d rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >( worldRotVecTemp );
                gmtl::Vec4d newGlobalHeadPointVec;
                newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
                newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
                newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];

                //Rotate the head vector by the rotation increment
                gmtl::Vec4d rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;

                //Create translation from new rotated point and add original head offset to the newly found location
                //Set world translation accordingly
                worldTrans[ 0 ] = -( rotateJugglerHeadVec[ 0 ] + jugglerHeadPointTemp[ 0 ] );
                worldTrans[ 1 ] = -( rotateJugglerHeadVec[ 1 ] + jugglerHeadPointTemp[ 1 ] );

                //Calculate the new mCenterPoint position based off the swept rotation
                // --- If the head ever moves off of z = 0, that distance will have to be taken into account --- //
                double theta = osg::DegreesToRadians( rotationStepSize );
                double pos[ 2 ] = { mCenterPoint->mData[ 0 ], mCenterPoint->mData[ 1 ] };
                mCenterPoint->mData[ 0 ] = ( pos[ 0 ] * cos( theta ) ) - ( pos[ 1 ] * sin( theta ) );
                mCenterPoint->mData[ 1 ] = ( pos[ 0 ] * sin( theta ) ) + ( pos[ 1 ] * cos( theta ) );
            }
            else
            {
                //Get juggler Matrix of worldDCS
                //Note:: for pf we are in juggler land
                //       for osg we are in z up land
                Matrix44d worldMat;
                worldMat = world->GetMat();

                //Translate world dcs by distance that the head is away from the origin
                gmtl::Matrix44d transMat = gmtl::makeTrans< gmtl::Matrix44d >( -*mCenterPoint );
                gmtl::Matrix44d worldMatTrans = transMat * worldMat;

                //Get the position of the m_head in the new world space as if the m_head is on the origin
                gmtl::Point3d newJugglerHeadPoint;
                gmtl::Point3d newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;
                gmtl::Vec4d newGlobalHeadPointVec;
                newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
                newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
                newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];

                //Create rotation matrix and juggler head vector
                gmtl::EulerAngleXYZd worldRotVecTemp( 0, 0, gmtl::Math::deg2Rad( rotationStepSize ) );
                gmtl::Matrix44d rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >( worldRotVecTemp );

                //Rotate the head vector by the rotation increment
                gmtl::Vec4d rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;

                //Create translation from new rotated point and add original head offset to the newly found location
                //Set world translation accordingly
                worldTrans[ 0 ] = -( rotateJugglerHeadVec[ 0 ] + mCenterPoint->mData[ 0 ] );
                worldTrans[ 1 ] = -( rotateJugglerHeadVec[ 1 ] + mCenterPoint->mData[ 1 ] );
            }
        }
        //CW rotation
        else if( cfdIso_value == YAW_CW )
        {
            rot_quat = osg::Quat( osg::DegreesToRadians( -rotationStepSize ), osg::Vec3d( 0, 0, 1 ) );

            if( rotationFlag )
            {
                vjHeadMat = gmtl::convertTo< double >( head->getData() );

                //Get juggler Matrix of worldDCS
                //Note:: for pf we are in juggler land
                //       for osg we are in z up land
                Matrix44d worldMat;
                worldMat = world->GetMat();

                gmtl::Point3d jugglerHeadPoint, jugglerHeadPointTemp;
                jugglerHeadPoint = gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );

                jugglerHeadPointTemp[ 0 ] = jugglerHeadPoint[ 0 ];
                jugglerHeadPointTemp[ 1 ] = -jugglerHeadPoint[ 2 ];
                jugglerHeadPointTemp[ 2 ] = 0;

                //Translate world dcs by distance that the head is away from the origin
                gmtl::Matrix44d transMat = gmtl::makeTrans< gmtl::Matrix44d >( -jugglerHeadPointTemp );
                gmtl::Matrix44d worldMatTrans = transMat * worldMat;
                gmtl::Point3d newJugglerHeadPoint;

                //Get the position of the head in the new world space as if the head is on the origin
                gmtl::Point3d newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;

                //Create rotation matrix and juggler head vector
                gmtl::EulerAngleXYZd worldRotVecTemp( 0, 0, gmtl::Math::deg2Rad( -rotationStepSize ) );

                gmtl::Matrix44d rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >( worldRotVecTemp );
                gmtl::Vec4d newGlobalHeadPointVec;
                newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
                newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
                newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];

                //Rotate the head vector by the rotation increment
                gmtl::Vec4d rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;

                //Create translation from new rotated point and add original head offset to the newly found location
                //Set world translation accordingly
                worldTrans[ 0 ] = -( rotateJugglerHeadVec[ 0 ] + jugglerHeadPointTemp[ 0 ] );
                worldTrans[ 1 ] = -( rotateJugglerHeadVec[ 1 ] + jugglerHeadPointTemp[ 1 ] );

                //Calculate the new mCenterPoint position based off the swept rotation
                // --- If the head ever moves off of z = 0, that distance will have to be taken into account --- //
                double theta = osg::DegreesToRadians( -rotationStepSize );
                double pos[ 2 ] = { mCenterPoint->mData[ 0 ], mCenterPoint->mData[ 1 ] };
                mCenterPoint->mData[ 0 ] = ( pos[ 0 ] * cos( theta ) ) - ( pos[ 1 ] * sin( theta ) );
                mCenterPoint->mData[ 1 ] = ( pos[ 0 ] * sin( theta ) ) + ( pos[ 1 ] * cos( theta ) );
            }
            else
            {
                //Get juggler Matrix of worldDCS
                //Note:: for pf we are in juggler land
                //       for osg we are in z up land
                Matrix44d worldMat;
                worldMat = world->GetMat();

                //Translate world dcs by distance that the head is away from the origin
                gmtl::Matrix44d transMat = gmtl::makeTrans< gmtl::Matrix44d >( -*mCenterPoint );
                gmtl::Matrix44d worldMatTrans = transMat * worldMat;

                //Get the position of the m_head in the new world space as if the m_head is on the origin
                gmtl::Point3d newJugglerHeadPoint;
                gmtl::Point3d newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;
                gmtl::Vec4d newGlobalHeadPointVec;
                newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
                newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
                newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];

                //Create rotation matrix and juggler head vector
                gmtl::EulerAngleXYZd worldRotVecTemp( 0, 0, gmtl::Math::deg2Rad( -rotationStepSize ) );
                gmtl::Matrix44d rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >( worldRotVecTemp );

                //Rotate the head vector by the rotation increment
                gmtl::Vec4d rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;

                //Create translation from new rotated point and add original head offset to the newly found location
                //Set world translation accordingly
                worldTrans[ 0 ] = -( rotateJugglerHeadVec[ 0 ] + mCenterPoint->mData[ 0 ] );
                worldTrans[ 1 ] = -( rotateJugglerHeadVec[ 1 ] + mCenterPoint->mData[ 1 ] );
            }
        }
        else if( cfdIso_value == PITCH_DOWN )
        {
            rot_quat = osg::Quat( osg::DegreesToRadians( rotationStepSize ), osg::Vec3d( 1, 0, 0 ) );

            //Get juggler Matrix of worldDCS
            //Note:: for pf we are in juggler land
            //       for osg we are in z up land
            Matrix44d worldMat;
            worldMat = world->GetMat();

            //Translate world dcs by distance that the head is away from the origin
            gmtl::Matrix44d transMat = gmtl::makeTrans< gmtl::Matrix44d >( -*mCenterPoint );
            gmtl::Matrix44d worldMatTrans = transMat * worldMat;

            //Get the position of the m_head in the new world space as if the m_head is on the origin
            gmtl::Point3d newJugglerHeadPoint;
            gmtl::Point3d newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;
            gmtl::Vec4d newGlobalHeadPointVec;
            newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
            newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
            newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];

            //Create rotation matrix and juggler head vector
            gmtl::EulerAngleXYZd worldRotVecTemp( gmtl::Math::deg2Rad( rotationStepSize ), 0, 0 );
            gmtl::Matrix44d rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >( worldRotVecTemp );

            //Rotate the head vector by the rotation increment
            gmtl::Vec4d rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;

            //Create translation from new rotated point and add original head offset to the newly found location
            //Set world translation accordingly
            worldTrans[ 1 ] = -( rotateJugglerHeadVec[ 1 ] + mCenterPoint->mData[ 1 ] );
            worldTrans[ 2 ] = -( rotateJugglerHeadVec[ 2 ] + mCenterPoint->mData[ 2 ] );
        }
        else if( cfdIso_value == PITCH_UP )
        {
            rot_quat = osg::Quat( osg::DegreesToRadians( -rotationStepSize ), osg::Vec3d( 1, 0, 0 ) );

            //Get juggler Matrix of worldDCS
            //Note:: for pf we are in juggler land
            //       for osg we are in z up land
            Matrix44d worldMat;
            worldMat = world->GetMat();

            //Translate world dcs by distance that the head is away from the origin
            gmtl::Matrix44d transMat = gmtl::makeTrans< gmtl::Matrix44d >( -*mCenterPoint );
            gmtl::Matrix44d worldMatTrans = transMat * worldMat;

            //Get the position of the m_head in the new world space as if the m_head is on the origin
            gmtl::Point3d newJugglerHeadPoint;
            gmtl::Point3d newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;
            gmtl::Vec4d newGlobalHeadPointVec;
            newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
            newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
            newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];

            //Create rotation matrix and juggler head vector
            gmtl::EulerAngleXYZd worldRotVecTemp( gmtl::Math::deg2Rad( -rotationStepSize ), 0, 0 );
            gmtl::Matrix44d rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >( worldRotVecTemp );

            //Rotate the head vector by the rotation increment
            gmtl::Vec4d rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;

            //Create translation from new rotated point and add original head offset to the newly found location
            //Set world translation accordingly
            worldTrans[ 1 ] = -( rotateJugglerHeadVec[ 1 ] + mCenterPoint->mData[ 1 ] );
            worldTrans[ 2 ] = -( rotateJugglerHeadVec[ 2 ] + mCenterPoint->mData[ 2 ] );
        }
        else if( cfdIso_value == ROLL_CW )
        {
            rot_quat = osg::Quat( osg::DegreesToRadians( rotationStepSize ), osg::Vec3d( 0, 1, 0 ) );

            //Get juggler Matrix of worldDCS
            //Note:: for pf we are in juggler land
            //       for osg we are in z up land
            Matrix44d worldMat;
            worldMat = world->GetMat();

            //Translate world dcs by distance that the head is away from the origin
            gmtl::Matrix44d transMat = gmtl::makeTrans< gmtl::Matrix44d >( -*mCenterPoint );
            gmtl::Matrix44d worldMatTrans = transMat * worldMat;

            //Get the position of the m_head in the new world space as if the m_head is on the origin
            gmtl::Point3d newJugglerHeadPoint;
            gmtl::Point3d newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;
            gmtl::Vec4d newGlobalHeadPointVec;
            newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
            newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
            newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];

            //Create rotation matrix and juggler head vector
            gmtl::EulerAngleXYZd worldRotVecTemp( 0, gmtl::Math::deg2Rad( rotationStepSize ), 0 );
            gmtl::Matrix44d rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >( worldRotVecTemp );

            //Rotate the head vector by the rotation increment
            gmtl::Vec4d rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;

            //Create translation from new rotated point and add original head offset to the newly found location
            //Set world translation accordingly
            worldTrans[ 0 ] = -( rotateJugglerHeadVec[ 0 ] + mCenterPoint->mData[ 0 ] );
            worldTrans[ 2 ] = -( rotateJugglerHeadVec[ 2 ] + mCenterPoint->mData[ 2 ] );
        }
        else if( cfdIso_value == ROLL_CCW )
        {
            rot_quat = osg::Quat( osg::DegreesToRadians( -rotationStepSize ), osg::Vec3d( 0, 1, 0 ) );

            //Get juggler Matrix of worldDCS
            //Note:: for pf we are in juggler land
            //       for osg we are in z up land
            Matrix44d worldMat;
            worldMat = world->GetMat();

            //Translate world dcs by distance that the head is away from the origin
            gmtl::Matrix44d transMat = gmtl::makeTrans< gmtl::Matrix44d >( -*mCenterPoint );
            gmtl::Matrix44d worldMatTrans = transMat * worldMat;

            //Get the position of the m_head in the new world space as if the m_head is on the origin
            gmtl::Point3d newJugglerHeadPoint;
            gmtl::Point3d newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;
            gmtl::Vec4d newGlobalHeadPointVec;
            newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
            newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
            newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];

            //Create rotation matrix and juggler head vector
            gmtl::EulerAngleXYZd worldRotVecTemp( 0, gmtl::Math::deg2Rad( -rotationStepSize ), 0 );
            gmtl::Matrix44d rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >( worldRotVecTemp );

            //Rotate the head vector by the rotation increment
            gmtl::Vec4d rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;

            //Create translation from new rotated point and add original head offset to the newly found location
            //Set world translation accordingly
            worldTrans[ 0 ] = -( rotateJugglerHeadVec[ 0 ] + mCenterPoint->mData[ 0 ] );
            worldTrans[ 2 ] = -( rotateJugglerHeadVec[ 2 ] + mCenterPoint->mData[ 2 ] );
        }
    }

    //Set the DCS postion based off of previous manipulation of the worldTrans array
    for( unsigned int i = 0; i < 3; ++i )
    {
        worldTrans[ i ] = -worldTrans[ i ];
    }

    world_quat *= rot_quat;

    gmtl::Matrix44d vjHeadMat = gmtl::convertTo< double >( head->getData() );
    Device::EnsureCameraStaysAboveGround ( vjHeadMat, worldTrans, world_quat, subzeroFlag );

    world->SetTranslationArray( worldTrans );
    world->SetQuat( world_quat );

    if( m_characterController.IsEnabled() )
    {
        if( rot_quat[ 3 ] > 0 )
        {
            m_characterController.Rotate( 0., osg::DegreesToRadians( -rotationStepSize ) );
        }
        else
        {
            m_characterController.Rotate( 0., osg::DegreesToRadians( rotationStepSize ) );
        }        
    }
	
    vprDEBUG( vesDBG, 3 ) << "|\tEnd Tablet Navigate" << std::endl << vprDEBUG_FLUSH;
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
