/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/device/Wand.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/DeviceHandler.h>

#include <ves/xplorer/behavior/WandEvents.h>

#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/environment/NavigationAnimationEngine.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>
#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>
#include <ves/xplorer/scenegraph/Select.h>

#include <ves/xplorer/scenegraph/manipulator/RotateTwist.h>
#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>

#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/scenegraph/camera/CameraManager.h>
#include <ves/xplorer/scenegraph/camera/CameraObject.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SignalWrapper.h>
#include <ves/xplorer/eventmanager/EventFactory.h>

#include <ves/conductor/qt/UIManager.h>

// --- vrJuggler Includes --- //
#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

// --- OSG Includes --- //
#include <osgUtil/LineSegmentIntersector>
#include <osg/Group>
#include <osg/Geometry>
#include <osg/Array>
#include <osg/NodeVisitor>
#include <osg/Matrix>
#include <osg/io_utils>

// --- C/C++ Libraries --- //
#include <iostream>

// --- Boost Includes --- //
#include <boost/concept_check.hpp>

// --- POCO Includes --- //
#include <Poco/Util/TimerTaskAdapter.h>

// --- OSG Includes --- //
#include <osgwMx/MxCore.h>

using namespace gmtl;
using namespace gadget;
using namespace ves::xplorer::device;
using namespace ves::xplorer::scenegraph;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
Wand::Wand()
    :
    Device( WAND ),
    cursorLen( 1.0 ),
    translationStepSize( 0.15 ),
    rotationStepSize( 1.0 ),
    rotationFlag( 1 ),
    m_distance( 1000 ),
    m_buttonPushed( false ),
    m_cadSelectionMode( false ),
    m_wandEvents( new ves::xplorer::behavior::WandEvents ),
    m_triggerWandMove( false ),
    m_shutdown( false ),
    m_logger( Poco::Logger::get("xplorer.wand.EventDebug") ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    m_wand.init( "VJWand" );
    head.init( "VJHead" );
    
    // Connect to Juggler's new event handling interface
    m_wandButton0DoubleClickEventInterface.setClickTime(300);
    m_wandButton0DoubleClickEventInterface.init("VJButton0");
    m_wandButton0DoubleClickEventInterface.addCallback<gadget::event::double_click_tag>(boost::bind(&Wand::OnWandButton0DoubleClick, this, _1));
    
    m_wandButton1DoubleClickEventInterface.setClickTime(300);
    m_wandButton1DoubleClickEventInterface.init("VJButton1");
    m_wandButton1DoubleClickEventInterface.addCallback<gadget::event::double_click_tag>(boost::bind(&Wand::OnWandButton1DoubleClick, this, _1));

    m_wandButton2DoubleClickEventInterface.setClickTime(300);
    m_wandButton2DoubleClickEventInterface.init("VJButton2");
    m_wandButton2DoubleClickEventInterface.addCallback<gadget::event::double_click_tag>(boost::bind(&Wand::OnWandButton2DoubleClick, this, _1));

    m_wandButton3DoubleClickEventInterface.setClickTime(300);
    m_wandButton3DoubleClickEventInterface.init("VJButton3");
    m_wandButton3DoubleClickEventInterface.addCallback<gadget::event::double_click_tag>(boost::bind(&Wand::OnWandButton3DoubleClick, this, _1));

    m_wandButton4DoubleClickEventInterface.setClickTime(300);
    m_wandButton4DoubleClickEventInterface.init("VJButton4");
    m_wandButton4DoubleClickEventInterface.addCallback<gadget::event::double_click_tag>(boost::bind(&Wand::OnWandButton4DoubleClick, this, _1));

    m_wandButton5DoubleClickEventInterface.setClickTime(300);
    m_wandButton5DoubleClickEventInterface.init("VJButton5");
    m_wandButton5DoubleClickEventInterface.addCallback<gadget::event::double_click_tag>(boost::bind(&Wand::OnWandButton5DoubleClick, this, _1));

    
    m_wandButton0EventInterface.init("VJButton0");
    m_wandButton0EventInterface.addCallback(boost::bind(&Wand::OnWandButton0Event, this, _1));

    m_wandButton1EventInterface.init("VJButton1");
    m_wandButton1EventInterface.addCallback(boost::bind(&Wand::OnWandButton1Event, this, _1));

    m_wandButton2EventInterface.init("VJButton2");
    m_wandButton2EventInterface.addCallback(boost::bind(&Wand::OnWandButton2Event, this, _1));

    m_wandButton3EventInterface.init("VJButton3");
    m_wandButton3EventInterface.addCallback(boost::bind(&Wand::OnWandButton3Event, this, _1));

    m_wandButton4EventInterface.init("VJButton4");
    m_wandButton4EventInterface.addCallback(boost::bind(&Wand::OnWandButton4Event, this, _1));

    m_wandButton5EventInterface.init("VJButton5");
    m_wandButton5EventInterface.addCallback(boost::bind(&Wand::OnWandButton5Event, this, _1));

    m_wandButton6EventInterface.init("VJButton6");
    m_wandButton6EventInterface.addCallback(boost::bind(&Wand::OnWandButton6Event, this, _1));
    
    ///Setup the ves signals generated from the VR Juggler events
    m_wandButtonPressSignalMap["Wand.ButtonPress0"] = new WandButtonPressSignal_type;
    m_wandButtonReleaseSignalMap["Wand.ButtonRelease0"] = new WandButtonReleaseSignal_type;
    m_wandButtonOnSignalMap["Wand.ButtonOn0"] = new WandButtonOnSignal_type;
    m_wandDoubleClickSignalMap["Wand.ButtonDoubleClick0"] = new WandDoubleClickSignal_type;
    
    m_wandButtonPressSignalMap["Wand.ButtonPress1"] = new WandButtonPressSignal_type;
    m_wandButtonReleaseSignalMap["Wand.ButtonRelease1"] = new WandButtonReleaseSignal_type;
    m_wandButtonOnSignalMap["Wand.ButtonOn1"] = new WandButtonOnSignal_type;
    m_wandDoubleClickSignalMap["Wand.ButtonDoubleClick1"] = new WandDoubleClickSignal_type;

    m_wandButtonPressSignalMap["Wand.ButtonPress2"] = new WandButtonPressSignal_type;
    m_wandButtonReleaseSignalMap["Wand.ButtonRelease2"] = new WandButtonReleaseSignal_type;
    m_wandButtonOnSignalMap["Wand.ButtonOn2"] = new WandButtonOnSignal_type;
    m_wandDoubleClickSignalMap["Wand.ButtonDoubleClick2"] = new WandDoubleClickSignal_type;

    m_wandButtonPressSignalMap["Wand.ButtonPress3"] = new WandButtonPressSignal_type;
    m_wandButtonReleaseSignalMap["Wand.ButtonRelease3"] = new WandButtonReleaseSignal_type;
    m_wandButtonOnSignalMap["Wand.ButtonOn3"] = new WandButtonOnSignal_type;
    m_wandDoubleClickSignalMap["Wand.ButtonDoubleClick3"] = new WandDoubleClickSignal_type;

    m_wandButtonPressSignalMap["Wand.ButtonPress4"] = new WandButtonPressSignal_type;
    m_wandButtonReleaseSignalMap["Wand.ButtonRelease4"] = new WandButtonReleaseSignal_type;
    m_wandButtonOnSignalMap["Wand.ButtonOn4"] = new WandButtonOnSignal_type;
    m_wandDoubleClickSignalMap["Wand.ButtonDoubleClick4"] = new WandDoubleClickSignal_type;

    m_wandButtonPressSignalMap["Wand.ButtonPress5"] = new WandButtonPressSignal_type;
    m_wandButtonReleaseSignalMap["Wand.ButtonRelease5"] = new WandButtonReleaseSignal_type;
    m_wandButtonOnSignalMap["Wand.ButtonOn5"] = new WandButtonOnSignal_type;
    m_wandDoubleClickSignalMap["Wand.ButtonDoubleClick5"] = new WandDoubleClickSignal_type;

    m_wandButtonPressSignalMap["Wand.ButtonPress6"] = new WandButtonPressSignal_type;
    m_wandButtonReleaseSignalMap["Wand.ButtonRelease6"] = new WandButtonReleaseSignal_type;
    m_wandButtonOnSignalMap["Wand.ButtonOn6"] = new WandButtonOnSignal_type;
    m_wandDoubleClickSignalMap["Wand.ButtonDoubleClick6"] = new WandDoubleClickSignal_type;

    eventmanager::EventManager* evm = eventmanager::EventManager::instance();
    using eventmanager::SignalWrapper;
    
    ///Setup Button Press
    for( WandButtonPressSignalMapType::const_iterator 
        iter = m_wandButtonPressSignalMap.begin(); 
        iter != m_wandButtonPressSignalMap.end(); ++iter)
    {
        evm->RegisterSignal(
            new SignalWrapper< WandButtonPressSignal_type >( iter->second ),
            iter->first, 
            eventmanager::EventManager::button_SignalType );
    }

    ///Setup Button Release
    for( WandButtonReleaseSignalMapType::const_iterator 
        iter = m_wandButtonReleaseSignalMap.begin(); 
        iter != m_wandButtonReleaseSignalMap.end(); ++iter)
    {
        evm->RegisterSignal(
            new SignalWrapper< WandButtonReleaseSignal_type >( iter->second ),
            iter->first, 
            eventmanager::EventManager::button_SignalType );
    }
    
    ///Setup Button On
    for( WandButtonOnSignalMapType::const_iterator 
        iter = m_wandButtonOnSignalMap.begin(); 
        iter != m_wandButtonOnSignalMap.end(); ++iter)
    {
        evm->RegisterSignal(
            new SignalWrapper< WandButtonOnSignal_type >( iter->second ),
            iter->first, 
            eventmanager::EventManager::button_SignalType );
    }
    
    ///Setup Button Double Click
    for( WandDoubleClickSignalMapType::const_iterator 
        iter = m_wandDoubleClickSignalMap.begin(); 
        iter != m_wandDoubleClickSignalMap.end(); ++iter)
    {
        evm->RegisterSignal(
            new SignalWrapper< WandDoubleClickSignal_type >( iter->second ),
            iter->first, 
            eventmanager::EventManager::button_SignalType );
    }

    evm->RegisterSignal(
        new SignalWrapper< StartEndPointSignal_type >( &m_startEndPointSignal ),
        "Wand.StartEndPoint", eventmanager::EventManager::unspecified_SignalType );
    
    evm->RegisterSignal(
        new SignalWrapper< WandMoveSignal_type >( &m_wandMove ),
        "Wand.WandMove", eventmanager::EventManager::mouse_SignalType );
    
    // Register signal(s) with EventManager
    eventmanager::EventManager::instance()->RegisterSignal(
        new eventmanager::SignalWrapper< ves::util::BoolSignal_type >( &m_updateData ),
        "Wand.UpdateData");
    
    CONNECTSIGNAL_0( "App.LatePreFrame", void(), &Wand::LatePreFrameUpdate,
                    m_connections, highest_Priority );

    //m_hideShowUI =
    //    reinterpret_cast< eventmanager::SignalWrapper< ves::util::VoidSignal_type >* >
    //    ( eventmanager::EventFactory::instance()->GetSignal( "EventMapper.HideShowUI" ) )
    //    ->mSignal;
    eventmanager::EventManager::instance()->RegisterSignal(
        new eventmanager::SignalWrapper< ves::util::VoidSignal_type >( &m_hideShowUI ),
        "Wand.HideShowUI");
    
    
    // trigger (and top right button) used for the selection line
    digital[ 0 ].init( "VJButton0" );
    // top left button -- toggle cursor mode: laser, streamlines, box, & arrow
    digital[ 1 ].init( "VJButton1" );
    // 12 o'clock -- forward navigation
    digital[ 2 ].init( "VJButton2" );
    // 3 o'clock -- not used at present
    digital[ 3 ].init( "VJButton3" );
    // 6 o'clock -- reset
    digital[ 4 ].init( "VJButton4" );
    // 9 o'clock -- exit streamer while loop
    digital[ 5 ].init( "VJButton5" );

    m_beamLineSegment = new osgUtil::LineSegmentIntersector(
        osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0, 0.0, 0.0 ) );

    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
Wand* Wand::AsWand()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::Initialize()
{
    for( int i = 0; i < 3; ++i )
    {
        cursorLoc[ i ] = 0;
        //loc[ i ] + dir[ i ] * cursorLen;
        objLoc[ i ] = cursorLoc[ i ];
    }
    
    MakeWandLine();

    if( !m_sceneManager.IsDesktopMode() )
    {
        Poco::Util::TimerTask::Ptr moveTask = 
            new Poco::Util::TimerTaskAdapter<Wand>(*this, &Wand::OnWandMoveTimer);
        m_wandMoveTimer.schedule( moveTask, 500, 250 );
    }
}
////////////////////////////////////////////////////////////////////////////////
Wand::~Wand()
{
    m_shutdown = true;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::ProcessEvents( ves::open::xml::CommandPtr command )
{
    vprDEBUG( vesDBG, 3 ) << "|\tWand::ProcessEvents" 
        << std::endl << vprDEBUG_FLUSH;
    ves::xplorer::scenegraph::DCS* const activeDCS =
        ves::xplorer::DeviceHandler::instance()->GetActiveDCS();
    if( !activeDCS )
    {
        return;
    }

    //If the wand does not exist
    if( m_wand->isStupefied() )
    {
        return;
    }

    m_buttonPushed = false;

    //Update the wand direction every frame
    UpdateWandLocalDirection();
    UpdateWandGlobalLocation();
    
    buttonData[ 0 ] = digital[ 0 ]->getData();
    buttonData[ 1 ] = digital[ 1 ]->getData();
    buttonData[ 2 ] = digital[ 2 ]->getData();
    buttonData[ 3 ] = digital[ 3 ]->getData();
    buttonData[ 4 ] = digital[ 4 ]->getData();
    buttonData[ 5 ] = digital[ 5 ]->getData();

    m_rotIncrement.set( 0, 0, 0, 1 );
    m_worldQuat = activeDCS->getAttitude();

    double* tempWorldTrans = activeDCS->GetVETranslationArray();
    m_worldTrans[ 0 ] = -tempWorldTrans[ 0 ];
    m_worldTrans[ 1 ] = -tempWorldTrans[ 1 ];
    m_worldTrans[ 2 ] = -tempWorldTrans[ 2 ];

    //This is NOT how we should do things
    //Command should allowed to be null but because we always
    //have to have a command due to our command structure
    //this hack must be in here
    //This should be changed once our complete command structure is in place
    std::string commandType;
    std::string newCommand;

    if( command )
    {
        commandType = command->GetCommandName();
    }
    else
    {
        commandType = "wait";
        newCommand = "wait";
    }

    if( !commandType.compare( "Navigation_Data" ) )
    {
        DataValuePairPtr commandData = command->GetDataValuePair( 0 );
        cfdIso_value = int( commandData->GetDataValue() );
        newCommand = commandData->GetDataName();
    }

    if( !newCommand.compare( "ROTATE_ABOUT_HEAD" ) )
    {
        SetHeadRotationFlag( cfdIso_value );
    }
    else if( !newCommand.compare( "Z_GREATER_THAN_ZERO" ) )
    {
		Device::SetSubZeroFlag( cfdIso_value );
    }
	else if( !newCommand.compare( "Z_EQUALS_ZERO" ) )
    {
		Device::SetZEqualsZeroFlag( cfdIso_value );
    }
    /*else if( !newCommand.compare( "RESET_NAVIGATION_POSITION" ) )
    {
        for( unsigned int i = 0; i < 3; ++i )
        {
            m_worldTrans[ i ] = 0.0f;
            world_quat[ i ] = 0.0f;
            mCenterPoint->mData[ i ] = 0.0f;
        }

        world_quat[ 3 ] = 1.0f;
    }*/
    else if( !newCommand.compare( "CHANGE_TRANSLATION_STEP_SIZE" ) )
    {
        //This equation returns a range of ~ 0.01' -> 220'
        //the equation is 1/100 * e ^ ( x / 10 ) where 1 < x < 100
        translationStepSize = 0.01f * exp( cfdIso_value * 0.10f );
    }
    else if( !newCommand.compare( "CHANGE_ROTATION_STEP_SIZE" ) )
    {
        //This equation returns a range of ~ 0.00029' -> 1.586'
        //NOTE: These are in degrees
        //This equation is 1 / 750 * ( x / 2 ) ^ 2.2 where 1 < x < 50
        rotationStepSize = 0.001333f * powf(( cfdIso_value * 0.5f ), 2.2f );
    }

    UpdateObjectHandler();

    ///Check and see if the cpt is enabled so that we can handle
    ///button events differently
    ves::xplorer::scenegraph::camera::CameraManager& cameraManager = 
        m_sceneManager.GetCameraManager();
    bool cptEnabled = cameraManager.IsCPTEnabled();

    //Process a selection event from a toggle off event just like in KM
    if( ( buttonData[ 5 ] == gadget::Digital::TOGGLE_ON ) ||
        ( buttonData[ 5 ] == gadget::Digital::ON ) )
    {
        if( cptEnabled && ( buttonData[ 5 ] == gadget::Digital::TOGGLE_ON ) )
        {
            if( cameraManager.getNumChildren() == 0 )
            {
                return;
            }
            ves::xplorer::scenegraph::camera::CameraObject* activeCamera =
                cameraManager.GetActiveCameraObject();
            unsigned int activeNum = 0;
            if( activeCamera )
            {
                activeNum = cameraManager.getChildIndex( 
                    static_cast< osg::Group* >( activeCamera ) );
            }
            
            ves::xplorer::DeviceHandler::instance()->UnselectObjects();

            if( activeNum + 1 < cameraManager.getNumChildren() )
            {
                activeNum += 1;
            }
            else
            {
                activeNum = 0;
            }
            
            ves::xplorer::scenegraph::camera::CameraObject* newCameraObject = 
                static_cast< ves::xplorer::scenegraph::camera::CameraObject* >( 
                cameraManager.getChild( activeNum ) );
            cameraManager.SetActiveCameraObject( newCameraObject, true );
            
            //Right now we are saying you must have a DCS
            scenegraph::DCS& selectedDCS = newCameraObject->GetDCS();
            gmtl::Matrix44d selectedMatrix = selectedDCS.GetMat();
            
            //Set the connection between the scene manipulator and the selected dcs
            //sceneManipulator->Connect( &selectedDCS );
            
            //If dcs is from a camera object, we want to rotate about local zero point
            //osg::Vec3d center( 0.0, 0.0, 0.0 );
            //center = center * osg::Matrixd( selectedMatrix.mData );
            //sceneManipulator->SetPosition( center );
            
            //We need to transform center point into camera space
            //In the future the center point will be in world coordinates
            //center = center * osg::Matrixd( sceneManager.GetNavDCS()->GetMat().mData );
            //gmtl::Point3d tempCenter( center.x(), center.y(), center.z() );
            //deviceHandler.SetCenterPoint( &tempCenter );
            
            //Set the selected DCS
            ves::xplorer::DeviceHandler::instance()->SetSelectedDCS( &selectedDCS );
            
            //Need to do this for multi-pass techniques
            if( m_sceneManager.IsRTTOn() )
            {
                selectedDCS.SetTechnique( "Glow" );
            }
            else
            {
                selectedDCS.SetTechnique( "Select" );
            }
            
            //Hand the node we are interested in off to the animation engine
            NavigationAnimationEngine& nae =
                *(NavigationAnimationEngine::instance());
            nae.SetDCS( m_sceneManager.GetNavDCS() );
            
            //Hand our created end points off to the animation engine
            selectedMatrix = gmtl::invert( selectedMatrix );
            const gmtl::Matrix44d tempHeadMatrix = m_sceneManager.GetHeadMatrix();
            const gmtl::AxisAngled myAxisAngle( gmtl::Math::deg2Rad( double( -90 ) ), 1, 0, 0 );
            gmtl::Matrix44d myMat = gmtl::make< gmtl::Matrix44d >( myAxisAngle );
            selectedMatrix = tempHeadMatrix * myMat * selectedMatrix;
            
            gmtl::Vec3d navToPoint =
                gmtl::makeTrans< gmtl::Vec3d >( selectedMatrix );
            gmtl::Quatd rotationPoint =
                gmtl::makeRot< gmtl::Quatd >( selectedMatrix );
            nae.SetAnimationEndPoints( navToPoint, rotationPoint );
            return;
        }
        
        if( !cptEnabled )
        {
            if( ves::xplorer::NavigationAnimationEngine::instance()->IsActive() )
            {
                ves::xplorer::NavigationAnimationEngine::instance()->
                    IncrementAnimationSpeed( 0.5 );
            }
            else
            {
                DeviceHandler::instance()->UnselectObjects();
            }
            return;
        }
    }
    //Free rotation
    else if(( buttonData[ 1 ] == gadget::Digital::TOGGLE_ON ) ||
            ( buttonData[ 1 ] == gadget::Digital::ON ) )
    {
        if( cptEnabled && ( buttonData[ 1 ] == gadget::Digital::TOGGLE_ON ) )
        {
            if( cameraManager.IsPictureMode() )
            {
                cameraManager.GetActiveCameraObject()->
                    SetPictureFrameProjection( true );
                cameraManager.DisplayProjectionEffect( true, true );
                return;
            }

            if( !m_sceneManager.IsMasterNode() )
            {
                return;
            }

            open::xml::DataValuePairPtr dvp( new open::xml::DataValuePair() );
            unsigned int addFlag = 1;
            dvp->SetData( "AddCameraObject", addFlag );
            cameraManager.UpdateConductorData( dvp );
        }

        if( !cptEnabled )
        {
            if( ves::xplorer::NavigationAnimationEngine::instance()->IsActive() )
            {
                ves::xplorer::NavigationAnimationEngine::instance()->
                    IncrementAnimationSpeed( -0.5 );
                return;
            }
            else
            {
                m_buttonPushed = true;
                FreeRotateAboutWand();
            }
        }
    }
    else if( buttonData[ 1 ] == gadget::Digital::TOGGLE_OFF )
    {
        if( cptEnabled )
        {
            if( cameraManager.IsPictureMode() )
            {
                cameraManager.DisplayProjectionEffect( false, true );
                cameraManager.GetActiveCameraObject()->
                    SetPictureFrameProjection( false );
            }

            if( !m_sceneManager.IsMasterNode() )
            {
                return;
            }
        
            if( cameraManager.IsPictureMode() )
            {
                std::string tempDir;
                cameraManager.WriteActiveCameraImageFile( tempDir );
                return;
            }
        }
    }
    //Navigate about z up axis
    else if (( buttonData[ 3 ] == gadget::Digital::TOGGLE_ON ) ||
        ( buttonData[ 3 ] == gadget::Digital::ON ) )
    {
        m_buttonPushed = true;
        FreeRotateAboutWand( false );
    }
    //Navigation based on current wand direction
    else if( buttonData[ 2 ] == gadget::Digital::TOGGLE_ON ||
              buttonData[ 2 ] == gadget::Digital::ON )
    {
        m_buttonPushed = true;
        for( int i = 0; i < 3; ++i )
        {
            //Update the translation movement for the objects
            //How much object should move
            m_worldTrans[ i ] += m_dir[ i ] * translationStepSize;
        }
    }
    //Reset back to 0, 0, 0
    else if( buttonData[ 4 ] == gadget::Digital::TOGGLE_ON ||
              buttonData[ 4 ] == gadget::Digital::ON )
    {
        if( m_cadSelectionMode && 
           ( buttonData[ 4 ] == gadget::Digital::TOGGLE_ON ) )
        {
            if( m_unselectedCADFiles.size() )
            {
                m_unselectedCADFiles.back()->setNodeMask( 1 );
                m_unselectedCADFiles.pop_back();
            }
            return;
        }
        
        if( !m_cadSelectionMode )
        {
            m_buttonPushed = true;
            m_worldQuat = *mResetAxis;
            for( unsigned int i = 0; i < 3; ++i )
            {
                m_worldTrans[ i ] = -mResetPosition->at( i );
                //world_quat[ i ] = 0.0f;
                mCenterPoint->mData[ i ] = 0.0f;
            }
        }
    }
    else if( buttonData[ 2 ] == gadget::Digital::TOGGLE_OFF )
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StepForward( false );
        }
    }
    
    ///If we actually pushed a button then move things
    if( m_buttonPushed && !m_characterController.IsEnabled() )
    {
        //Set the DCS postion based off of previous
        //manipulation of the worldTrans array
        for( unsigned int i = 0; i < 3; ++i )
        {
            m_worldTrans[ i ] = -m_worldTrans[ i ];
        }

        m_worldQuat *= m_rotIncrement;

        gmtl::Matrix44d vjHeadMat = gmtl::convertTo< double >( head->getData() );
        Device::EnsureCameraStaysAboveGround( vjHeadMat, m_worldTrans, 
            m_worldQuat, m_subzeroFlag, m_zEqualsZeroFlag );

        activeDCS->SetTranslationArray( m_worldTrans );
        activeDCS->SetQuat( m_worldQuat );
    }
    else if( m_characterController.IsEnabled() && m_buttonPushed )
    {
        //Set the DCS postion based off of previous
        //manipulation of the worldTrans array
        for( unsigned int i = 0; i < 3; ++i )
        {
            m_worldTrans[ i ] = -m_worldTrans[ i ];
        }
        
        m_worldQuat *= m_rotIncrement;
        
        gmtl::Matrix44d vjHeadMat = gmtl::convertTo< double >( head->getData() );
        Device::EnsureCameraStaysAboveGround ( vjHeadMat, m_worldTrans, m_worldQuat, m_subzeroFlag, m_zEqualsZeroFlag );

        activeDCS->SetTranslationArray( m_worldTrans );
        activeDCS->SetQuat( m_worldQuat );
        //If the z axis is positive then rotate by a specific dz
        if( m_rotIncrement[ 3 ] > 0 )
        {
            m_characterController.Rotate( 0., osg::DegreesToRadians( -rotationStepSize ) );
        }
        else
        {
            m_characterController.Rotate( 0., osg::DegreesToRadians( rotationStepSize ) );
        }
        
        if( buttonData[ 2 ] == gadget::Digital::TOGGLE_ON ||
           buttonData[ 2 ] == gadget::Digital::ON )
        {
            m_characterController.StepForward( true );
        }
    }

    vprDEBUG( vesDBG, 3 ) << "|\tEnd Wand::ProcessEvents" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetStartEndPoint( osg::Vec3d*, osg::Vec3d* )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetHeadRotationFlag( int input )
{
    rotationFlag = input;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateSelectionLine( bool drawLine )
{
    /*osg::Vec3d startPoint, endPoint;
    SetupStartEndPoint( startPoint, endPoint );
    m_beamLineSegment->reset();
    m_beamLineSegment->setStart( startPoint );
    m_beamLineSegment->setEnd( endPoint );*/
    
    if( drawLine )
    {
        m_wandPAT->setNodeMask( 1 );
        DrawLine( m_startPoint, m_endPoint );
    }
    else
    {
        m_wandPAT->setNodeMask( 0 );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Wand::ProcessHit()
{
    osgUtil::LineSegmentIntersector::Intersections& intersections =
        scenegraph::TestForIntersections( *m_beamLineSegment.get(),
                                     *m_sceneManager.GetModelRoot() );
    
    //Unselect the previous selected DCS
    DeviceHandler::instance()->UnselectObjects();
    
    //Now find the new selected DCS
    if( intersections.empty() )
    {
        vprDEBUG( vesDBG, 1 )
        << "|\tWand::ProcessHit No object selected"
        << std::endl << vprDEBUG_FLUSH;
        
        return;
    }

    //Search for first item that is not the laser
    osg::Node* objectHit( NULL );
    for( osgUtil::LineSegmentIntersector::Intersections::iterator itr = 
        intersections.begin(); itr != intersections.end(); ++itr )
    {
        objectHit = *( itr->nodePath.rbegin() );
        if( objectHit->getName() != "Laser" &&
           objectHit->getName() != "Root Node" )
        {
            break;
        }
    }        
    
    if( !objectHit )
    {
        vprDEBUG( vesDBG, 1 ) << "|\tWand::ProcessHit No object selected"
        << std::endl << vprDEBUG_FLUSH;

        ves::xplorer::DeviceHandler::instance()->SetActiveDCS(
            ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS() );

        return;
    }

    ves::xplorer::scenegraph::camera::CameraManager& cameraManager = 
        ves::xplorer::scenegraph::SceneManager::instance()->GetCameraManager();
    bool cptEnabled = cameraManager.IsCPTEnabled();
    if( cptEnabled )
    {
        scenegraph::highlight::HighlightManager& highlightManager =
            m_sceneManager.GetHighlightManager();
        if( m_cameraManager.Handle( scenegraph::camera::Event::RELEASE,
            *m_beamLineSegment.get() ) )
        {
            vprDEBUG( vesDBG, 1 ) << "|\tWand::ProcessHit Selected a Camera"
                << std::endl << vprDEBUG_FLUSH;
        }
        else if( highlightManager.IsToggled() )
        {            
            //If we found a low level node
            osg::NodePath nodePath = intersections.begin()->nodePath;
            osg::Node* node = nodePath[ nodePath.size() - 1 ];
            
            highlightManager.CreateHighlightCircle( node, nodePath );
            return;
        }
    }    

    if( m_cadSelectionMode )
    {
        osg::NodePath nodePath = intersections.begin()->nodePath;
        osg::Node* node = nodePath[ nodePath.size() - 1 ];
        node->setNodeMask( 0 );
        m_unselectedCADFiles.push_back( node );
        return;
    }

    //Now find the id for the cad
    ves::xplorer::scenegraph::FindParentsVisitor parentVisitor( objectHit );
    osg::ref_ptr< osg::Node > parentNode = parentVisitor.GetParentNode();
    if( parentNode.valid() )
    {
        scenegraph::DCS* newSelectedDCS =
            static_cast< scenegraph::DCS* >( parentNode.get() );
        
        vprDEBUG( vesDBG, 1 ) << "|\tObjects has name "
            << parentNode->getName() << std::endl << vprDEBUG_FLUSH;

        vprDEBUG( vesDBG, 1 ) << "|\tObjects descriptors "
            << parentNode->getDescriptions().at( 1 )
            << std::endl << vprDEBUG_FLUSH;
            
        if( ves::xplorer::scenegraph::SceneManager::instance()->IsRTTOn() )
        {
            newSelectedDCS->SetTechnique( "Glow" );
        }
        else
        {
            newSelectedDCS->SetTechnique( "Select" );
        }
        DeviceHandler::instance()->SetSelectedDCS( newSelectedDCS );
        
        //Set the connection between the scene manipulator and the selected dcs
        scenegraph::manipulator::TransformManipulator* sceneManipulator =
            m_manipulatorManager.GetSceneManipulator();
        sceneManipulator->Connect( newSelectedDCS );
        
        //Move the scene manipulator to the center point
        scenegraph::LocalToWorldNodePath nodePath(
            newSelectedDCS, m_sceneManager.GetModelRoot() );
        scenegraph::LocalToWorldNodePath::NodeAndPathList npl =
        nodePath.GetLocalToWorldNodePath();
        scenegraph::LocalToWorldNodePath::NodeAndPath nap = npl.at( 0 );
        osg::Matrixd localToWorld = osg::computeLocalToWorld( nap.second );
        osg::Vec3d newCenter = newSelectedDCS->getBound().center() * localToWorld;
        sceneManipulator->SetPosition( newCenter );
        vprDEBUG( vesDBG, 1 ) << "|\tEnd Selection "
            << std::endl << vprDEBUG_FLUSH;
    }
    else
    {
        vprDEBUG( vesDBG, 1 ) << "|\tObject does not have name parent name "
            << objectHit->getParents().front()->getName()
            << std::endl << vprDEBUG_FLUSH;

        ves::xplorer::DeviceHandler::instance()->SetActiveDCS(
            ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Wand::DrawLine( const osg::Vec3d&, const osg::Vec3d& )
{
    gmtl::Matrix44d vrjWandMat = gmtl::convertTo< double >( m_wand->getData() );
    const gmtl::AxisAngled myAxisAngle( osg::DegreesToRadians( double( 90 ) ), 1, 0, 0 );
    const gmtl::Matrix44d myMat = gmtl::make< gmtl::Matrix44d >( myAxisAngle );
    gmtl::Vec3d x_axis( 1.0, 0.0, 0.0 );
    gmtl::Matrix44d zUpMatrix = gmtl::makeRot< gmtl::Matrix44d >(
        gmtl::AxisAngled( gmtl::Math::deg2Rad( -90.0 ), x_axis ) );
    
    ///Transform from juggler space to world space
    vrjWandMat = 
        ves::xplorer::scenegraph::SceneManager::instance()->
        GetInvertedNavMatrix() * myMat * vrjWandMat * zUpMatrix;

    const osg::Matrixd tempOsgMatrix( vrjWandMat.getData() );
    m_wandPAT->setMatrix( tempOsgMatrix );
    
    //UpdateForwardAndUp();
}
////////////////////////////////////////////////////////////////////////////////
//The current implemention uses VJButton0 to select an object then if VJButton3
//is pressed the object is translated with the users motion.  Later the
//selection method will occur from voice commands and translation will be done
//using gestures from a glove.
void Wand::UpdateObjectHandler()
{
    //Update the juggler location of the wand
    if( digital[ 0 ]->getData() == gadget::Digital::ON )
    {        
        UpdateSelectionLine( true );

        ///Push the FOCUS event if we are using manipulators and a dragger is not
        ///active
        if( m_manipulatorManager.IsEnabled() && 
            !m_manipulatorManager.LeafDraggerIsActive() )
        {
            osg::ref_ptr< osg::StateSet > stateset = 
                m_wandPAT->getOrCreateStateSet();
            if( m_manipulatorManager.Handle( scenegraph::manipulator::Event::FOCUS,
                m_beamLineSegment.get() ) )
            {
                
                stateset->setRenderBinDetails( 11, std::string( "RenderBin" ) );                
                stateset->setMode(
                                  GL_DEPTH_TEST,
                                  osg::StateAttribute::OFF |
                                  osg::StateAttribute::OVERRIDE );
                m_wandPAT->setCullingActive( false );
            }
            else
            {
                stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
                stateset->setMode(
                                  GL_DEPTH_TEST,
                                  osg::StateAttribute::ON |
                                  osg::StateAttribute::OVERRIDE );
                m_wandPAT->setCullingActive( true );
            }
        }

        if( m_manipulatorManager.IsEnabled() && 
           m_manipulatorManager.LeafDraggerIsActive() )
        {
            if( m_manipulatorManager.Handle(
                scenegraph::manipulator::Event::DRAG ) )
            {
                return;
            }
        }
    }
    //Now select and object based on the new wand location
    else if( digital[ 0 ]->getData() == gadget::Digital::TOGGLE_OFF )
    {
        UpdateSelectionLine( false );
        if( m_manipulatorManager.IsEnabled() && 
           m_manipulatorManager.LeafDraggerIsActive() )
        {
            bool success = m_manipulatorManager.Handle( 
                scenegraph::manipulator::Event::RELEASE );
            if( success )
            {
                 vprDEBUG( vesDBG, 2 ) << "|\tSuccessfully released a dragger." 
                    << std::endl << vprDEBUG_FLUSH;
                return;
            }
        }

        if( m_manipulatorManager.IsEnabled() )
        {
            bool success = m_manipulatorManager.Handle(
                                        scenegraph::manipulator::Event::PUSH,
                                        m_beamLineSegment.get() );
            vprDEBUG( vesDBG, 2 ) << "|\tTrying to push a dragger " 
                << success << "." 
                << std::endl << vprDEBUG_FLUSH;
            if( m_manipulatorManager.LeafDraggerIsActive() )
            {
                vprDEBUG( vesDBG, 2 ) << "|\tSuccessfully pushed a dragger." 
                    << std::endl << vprDEBUG_FLUSH;
                return;
            }
        }

        ProcessHit();
    }
    /*
    //Now we can move the object if the button
    int buttonData = digital[ 0 ]->getData();
    if( ( buttonData == gadget::Digital::ON ) && selectedGeometry.valid() )
    {
        TranslateObject();
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetupStartEndPoint( osg::Vec3d& startPoint, osg::Vec3d& endPoint )
{
    double* wandPosition = GetObjLocation();
    double wandEndPoint[ 3 ];

    for( int i = 0; i < 3; ++i )
    {
        wandEndPoint[ i ] = ( m_dir[ i ] * m_distance );
    }

    startPoint.set( wandPosition[ 0 ], wandPosition[ 1 ], wandPosition[ 2 ] );
    endPoint.set( wandEndPoint[ 0 ], wandEndPoint[ 1 ], wandEndPoint[ 2 ] );
    
    //Need to negate the the camera transform that is multiplied into the view
    {
        osg::Matrixd inverseCameraTransform(
            ves::xplorer::scenegraph::SceneManager::instance()->
            GetInvertedNavMatrix().getData() );
        
        startPoint = startPoint * inverseCameraTransform;
        endPoint = endPoint * inverseCameraTransform;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateWandLocalDirection()
{
    //Get the normalized direction relative to the juggler frame
    gmtl::Vec3d vjVec;
    vjVec.set( 0.0f, 0.0f, -1.0f );
    Matrix44d vjMat = gmtl::convertTo< double >( m_wand->getData() );

    gmtl::xform( vjVec, vjMat, vjVec );
    gmtl::normalize( vjVec );

    //Transform from juggler to osg...
    m_dir[0] =  vjVec[ 0 ];
    m_dir[1] = -vjVec[ 2 ];
    m_dir[2] =  vjVec[ 1 ];
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateWandGlobalLocation()
{
    //Transform wand point into global space get juggler Matrix of worldDCS
    //Note:: for osg we are in z up land
    gmtl::Point3d loc_temp, osgPointLoc;
    Matrix44d vjMat = gmtl::convertTo< double >( m_wand->getData() );

    gmtl::setTrans( loc_temp, vjMat );
    osgPointLoc[ 0 ] =  loc_temp[ 0 ];
    osgPointLoc[ 1 ] = -loc_temp[ 2 ];
    osgPointLoc[ 2 ] =  loc_temp[ 1 ];

    for( size_t i = 0; i < 3; ++i )
    {
        //LastWandPosition[ i ] = objLoc[ i ];
        //cursorLoc[ i ] = this->loc[ i ];// + this->dir[ i ] * this->cursorLen;
        objLoc[ i ] = osgPointLoc[ i ];
    }
}
////////////////////////////////////////////////////////////////////////////////
double* Wand::GetObjLocation()
{
    return objLoc;
}
////////////////////////////////////////////////////////////////////////////////
/*void Wand::UpdateDeltaWandPosition()
{
    for( size_t i = 0; i < 3; ++i )
    {
        deltaTrans[ i ] = objLoc[ i ] - LastWandPosition[ i ];
    }
}*/
////////////////////////////////////////////////////////////////////////////////
void Wand::FreeRotateAboutWand( const bool freeRotate )
{
    //gmtl::Quatd tempQuat;
    //tempQuat.set( m_rotIncrement[ 0 ], m_rotIncrement[ 1 ], m_rotIncrement[ 2 ], m_rotIncrement[ 3 ] );
    //gmtl::AxisAngled axisAngle = gmtl::makeRot< gmtl::AxisAngled >( tempQuat );
    
    m_sceneManager.GetMxCoreViewMatrix().rotateOrbit( osg::DegreesToRadians( -rotationStepSize ), 
        osg::Vec3d( 0.0, 0.0, 1.0 ) );
    return;

    ves::xplorer::scenegraph::DCS* const activeDCS =
        ves::xplorer::DeviceHandler::instance()->GetActiveDCS();

    gmtl::Matrix44d vrjWandMat = gmtl::convertTo< double >( m_wand->getData() );
    gmtl::Quatd wandQuat = gmtl::make< gmtl::Quatd >( vrjWandMat );

    osg::Vec3d tempVec( 0., 0., wandQuat[ 1 ] );
    m_rotIncrement =
        osg::Quat( osg::DegreesToRadians( -rotationStepSize ), tempVec );

    //If we are rotating about the users position
    //if( !rotationFlag )
    {
        return;
    }

    vjHeadMat = gmtl::convertTo< double >( head->getData() );

    //Get juggler Matrix of worldDCS
    //Note:: for osg we are in z up land
    Matrix44d worldMat;
    worldMat = activeDCS->GetMat();

    gmtl::Point3d jugglerHeadPoint, jugglerHeadPointTemp;
    jugglerHeadPoint = gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );
    jugglerHeadPointTemp[ 0 ] = jugglerHeadPoint[ 0 ];
    jugglerHeadPointTemp[ 1 ] = -jugglerHeadPoint[ 2 ];
    jugglerHeadPointTemp[ 2 ] = 0;//jugglerHeadPoint[ 1 ];

    //Translate world dcs by distance that the head
    // is away from the origin
    gmtl::Matrix44d transMat =
        gmtl::makeTrans< gmtl::Matrix44d >( -jugglerHeadPointTemp );
    gmtl::Matrix44d worldMatTrans = transMat * worldMat;
    gmtl::Point3d newJugglerHeadPoint;

    //Get the position of the head in the new world space
    //as if the head is on the origin
    gmtl::Point3d newGlobalHeadPointTemp =
        worldMatTrans * newJugglerHeadPoint;

    //Create rotation matrix and juggler head vector
    if( freeRotate )
    {
        //Rotate about arbitrary axis
        tempVec.set( wandQuat[ 0 ], -wandQuat[ 2 ], wandQuat[ 1 ] );
    }
    else
    {
        //Rotate about z-up axis
        tempVec.set( 0, 0, wandQuat[ 1 ] );
    }
    
    tempVec.normalize();
    //Create rotation increment
    m_rotIncrement =
        osg::Quat( osg::DegreesToRadians( -rotationStepSize ), tempVec );
    //Now make it a 4 x 4
    gmtl::Matrix44d rotMatTemp = gmtl::make< gmtl::Matrix44d >
                                 ( gmtl::Quat< double >( m_rotIncrement[ 0 ],
                                                         m_rotIncrement[ 1 ], m_rotIncrement[ 2 ],
                                                         m_rotIncrement[ 3 ] ) );

    gmtl::Vec4d newGlobalHeadPointVec;
    newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
    newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
    newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];

    //Rotate the head vector by the rotation increment
    gmtl::Vec4d rotateJugglerHeadVec =
        rotMatTemp * newGlobalHeadPointVec;

    //Create translation from new rotated point
    //and add original head off set to the newly found location
    //Set world translation accordingly
    m_worldTrans[ 0 ] =
        -( rotateJugglerHeadVec[ 0 ] + jugglerHeadPointTemp[ 0 ] );
    m_worldTrans[ 1 ] =
        -( rotateJugglerHeadVec[ 1 ] + jugglerHeadPointTemp[ 1 ] );
    m_worldTrans[ 2 ] =
        -( rotateJugglerHeadVec[ 2 ] + jugglerHeadPointTemp[ 2 ] );
}
////////////////////////////////////////////////////////////////////////////////
double* Wand::GetPlaneEquationConstantsNormalToWand()
{
    ///Get wand pointing vector
    Matrix44d vjMat = gmtl::convertTo< double >( m_wand->getData() );
    ///Transform from juggler space to world space
    Matrix44d worldWandMat =
        ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS()->GetMat() * vjMat;
    ///Normalize vector
    gmtl::Vec3d vjVec;
    vjVec.set( 0.0f, 0.0f, -1.0f );
    gmtl::xform( vjVec, worldWandMat, vjVec );
    gmtl::normalize( vjVec );
    ///Transform from juggler to osg...
    m_planeConstants[0] =  vjVec[ 0 ];
    m_planeConstants[1] = -vjVec[ 2 ];
    m_planeConstants[2] =  vjVec[ 1 ];
    m_planeConstants[3] =  4;
    return m_planeConstants;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::MakeWandLine()
{
    if( m_wandPAT.valid() )
    {
        return;
    }

    osg::Vec3 start( 0.0, 0.0, 0.0 );
    osg::Vec3 end( 0.0, m_distance, 0. );

    osg::ref_ptr< osg::Geode > beamGeode = new osg::Geode();
    beamGeode->setName( "Wand Beam Geode" );
    osg::ref_ptr< osg::Geometry > beamGeometry = new osg::Geometry();
    beamGeode->addDrawable( beamGeometry.get() );

    osg::ref_ptr< osg::Vec3Array > beamVertices = new osg::Vec3Array;
    ///Bottom
    beamVertices->push_back(
        osg::Vec3( start[ 0 ] - 0.05, start[ 1 ], start[ 2 ] - 0.05 ) );
    beamVertices->push_back(
        osg::Vec3( start[ 0 ] + 0.05, start[ 1 ], start[ 2 ] - 0.05 ) );
    beamVertices->push_back(
        osg::Vec3(   end[ 0 ] + 0.05,   end[ 1 ],   end[ 2 ] - 0.05 ) );
    beamVertices->push_back(
        osg::Vec3(   end[ 0 ] - 0.05,   end[ 1 ],   end[ 2 ] - 0.05 ) );
    ///Top
    beamVertices->push_back(
        osg::Vec3( start[ 0 ] - 0.05, start[ 1 ], start[ 2 ] + 0.05 ) );
    beamVertices->push_back(
        osg::Vec3( start[ 0 ] + 0.05, start[ 1 ], start[ 2 ] + 0.05 ) );
    beamVertices->push_back(
        osg::Vec3(   end[ 0 ] + 0.05,   end[ 1 ],   end[ 2 ] + 0.05 ) );
    beamVertices->push_back(
        osg::Vec3(   end[ 0 ] - 0.05,   end[ 1 ],   end[ 2 ] + 0.05 ) );

    beamGeometry->setVertexArray( beamVertices.get() );

    osg::ref_ptr< osg::DrawElementsUInt > beamTop =
        new osg::DrawElementsUInt( osg::PrimitiveSet::QUADS, 0 );
    beamTop->push_back( 3 );
    beamTop->push_back( 2 );
    beamTop->push_back( 1 );
    beamTop->push_back( 0 );
    beamGeometry->addPrimitiveSet( beamTop.get() );

    osg::ref_ptr< osg::DrawElementsUInt > beamBottom =
        new osg::DrawElementsUInt( osg::PrimitiveSet::QUADS, 0 );
    beamBottom->push_back( 4 );
    beamBottom->push_back( 5 );
    beamBottom->push_back( 6 );
    beamBottom->push_back( 7 );
    beamGeometry->addPrimitiveSet( beamBottom.get() );

    osg::ref_ptr< osg::DrawElementsUInt > beamLeft =
        new osg::DrawElementsUInt( osg::PrimitiveSet::QUADS, 0 );
    beamLeft->push_back( 4 );
    beamLeft->push_back( 7 );
    beamLeft->push_back( 3 );
    beamLeft->push_back( 0 );
    beamGeometry->addPrimitiveSet( beamLeft.get() );

    osg::ref_ptr< osg::DrawElementsUInt > beamRight =
        new osg::DrawElementsUInt( osg::PrimitiveSet::QUADS, 0 );
    beamRight->push_back( 1 );
    beamRight->push_back( 2 );
    beamRight->push_back( 6 );
    beamRight->push_back( 5 );
    beamGeometry->addPrimitiveSet( beamRight.get() );

    osg::ref_ptr< osg::DrawElementsUInt > beamBack =
        new osg::DrawElementsUInt( osg::PrimitiveSet::QUADS, 0 );
    beamBack->push_back( 5 );
    beamBack->push_back( 4 );
    beamBack->push_back( 0 );
    beamBack->push_back( 1 );
    beamGeometry->addPrimitiveSet( beamBack.get() );

    osg::ref_ptr< osg::DrawElementsUInt > beamFront =
        new osg::DrawElementsUInt( osg::PrimitiveSet::QUADS, 0 );
    beamFront->push_back( 7 );
    beamFront->push_back( 6 );
    beamFront->push_back( 2 );
    beamFront->push_back( 3 );
    beamGeometry->addPrimitiveSet( beamFront.get() );

    osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array();
    colors->push_back( osg::Vec4( 1.0f, 0.0f, 1.0f, 1.0f ) );

    osg::ref_ptr< osg::UIntArray > cfdColorIndexArray = new osg::UIntArray();
    cfdColorIndexArray->push_back( 0 );

    beamGeometry->setColorArray( colors.get() );
    beamGeometry->setColorIndices( cfdColorIndexArray.get() );
    beamGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );

    m_wandPAT = new osg::MatrixTransform();
    m_wandPAT->setNodeMask( 0 );
    m_wandPAT->addChild( beamGeode.get() );
    ves::xplorer::DeviceHandler::instance()->
        GetDeviceGroup()->addChild( m_wandPAT );
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetCADSelectionMode( bool cadSelectionMode )
{
    m_cadSelectionMode = cadSelectionMode;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandButton0Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        //LOG_INFO( "OnWandButton0Event: DigitalState::OFF" );
        return;
    }

    m_triggerWandMove = false;

    PreProcessNav();
    
    //SetupStartEndPoint( m_startPoint, m_endPoint );
    m_startEndPointSignal( m_startPoint, m_endPoint );

    ///For now we are going to map Wand button 0 to Mouse button 1
    switch(event) 
    {
    case gadget::DigitalState::ON:
    {
        //LOG_INFO( "OnWandButton0Event: DigitalState::ON" );
        UpdateSelectionLine( true );

        (*(m_wandButtonOnSignalMap["Wand.ButtonOn0"]))( gadget::MBUTTON1, 0, 0, gadget::KEY_DOWN|gadget::BUTTON1_MASK );

        m_wandMove( 0, 0, 0, gadget::KEY_DOWN|gadget::BUTTON1_MASK );
        
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        //LOG_INFO( "OnWandButton0Event: DigitalState::TOGGLE_ON" );
        (*(m_wandButtonPressSignalMap["Wand.ButtonPress0"]))( gadget::MBUTTON1, 0, 0, gadget::KEY_DOWN|gadget::BUTTON1_MASK );
        break;
    }
    case gadget::DigitalState::TOGGLE_OFF:
    {
        //LOG_INFO( "OnWandButton0Event: DigitalState::TOGGLE_OFF" );
        UpdateSelectionLine( false );

        (*(m_wandButtonReleaseSignalMap["Wand.ButtonRelease0"]))( gadget::MBUTTON1, 0, 0, gadget::KEY_UP );
        break;
    }
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandButton1Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    m_triggerWandMove = false;

    PreProcessNav();
    
    ///For now we are going to map Wand button 1 to Mouse button 2
    switch(event) 
    {
        case gadget::DigitalState::ON:
        {
            (*(m_wandButtonOnSignalMap["Wand.ButtonOn1"]))( gadget::MBUTTON2, 0, 0, gadget::KEY_DOWN|gadget::BUTTON2_MASK );
            break;
        }
        case gadget::DigitalState::TOGGLE_ON:
        {
            (*(m_wandButtonPressSignalMap["Wand.ButtonPress1"]))( gadget::MBUTTON2, 0, 0, gadget::KEY_DOWN|gadget::BUTTON2_MASK );
            break;
        }
        case gadget::DigitalState::TOGGLE_OFF:
        {
            (*(m_wandButtonReleaseSignalMap["Wand.ButtonRelease1"]))( gadget::MBUTTON2, 0, 0, gadget::KEY_UP );
            break;
        }
        default:
            break;
    }
    

    ///Check and see if the cpt is enabled so that we can handle
    ///button events differently
    ves::xplorer::scenegraph::camera::CameraManager& cameraManager = 
    m_sceneManager.GetCameraManager();
    bool cptEnabled = cameraManager.IsCPTEnabled();
    if(( event == gadget::DigitalState::TOGGLE_ON ) ||
       ( event == gadget::DigitalState::ON ) )
    {
        if( cptEnabled && ( event == gadget::DigitalState::TOGGLE_ON ) )
        {
            if( cameraManager.IsPictureMode() )
            {
                cameraManager.GetActiveCameraObject()->
                SetPictureFrameProjection( true );
                cameraManager.DisplayProjectionEffect( true, true );
                return;
            }
            
            if( !m_sceneManager.IsMasterNode() )
            {
                return;
            }
            
            open::xml::DataValuePairPtr dvp( new open::xml::DataValuePair() );
            unsigned int addFlag = 1;
            dvp->SetData( "AddCameraObject", addFlag );
            cameraManager.UpdateConductorData( dvp );
        }
        
        if( !cptEnabled )
        {
            if( ves::xplorer::NavigationAnimationEngine::instance()->IsActive() )
            {
                ves::xplorer::NavigationAnimationEngine::instance()->
                IncrementAnimationSpeed( -0.5 );
                return;
            }
            else
            {
                m_buttonPushed = true;
                FreeRotateAboutWand();
            }
        }
    }
    else if( event == gadget::DigitalState::TOGGLE_OFF )
    {
        if( cptEnabled )
        {
            if( cameraManager.IsPictureMode() )
            {
                cameraManager.DisplayProjectionEffect( false, true );
                cameraManager.GetActiveCameraObject()->
                SetPictureFrameProjection( false );
            }
            
            if( !m_sceneManager.IsMasterNode() )
            {
                return;
            }
            
            if( cameraManager.IsPictureMode() )
            {
                std::string tempDir( "." );
                cameraManager.WriteActiveCameraImageFile( tempDir );
                return;
            }
        }
    }
    
    PostProcessNav();
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandButton2Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }

    m_triggerWandMove = false;

    PreProcessNav();
    
    ///For now we are going to map Wand button 2 to Mouse button 3
    switch(event) 
    {
        case gadget::DigitalState::ON:
        {
            (*(m_wandButtonOnSignalMap["Wand.ButtonOn2"]))( gadget::MBUTTON3, 0, 0, gadget::KEY_DOWN|gadget::BUTTON3_MASK );
            break;
        }
        case gadget::DigitalState::TOGGLE_ON:
        {
            (*(m_wandButtonPressSignalMap["Wand.ButtonPress2"]))( gadget::MBUTTON3, 0, 0, gadget::KEY_DOWN|gadget::BUTTON3_MASK );
            break;
        }
        case gadget::DigitalState::TOGGLE_OFF:
        {
            (*(m_wandButtonReleaseSignalMap["Wand.ButtonRelease2"]))( gadget::MBUTTON3, 0, 0, gadget::KEY_UP );
            break;
        }
        default:
            break;
    }
    
    if( event == gadget::DigitalState::TOGGLE_ON ||
       event == gadget::DigitalState::ON )
    {        
        m_buttonPushed = true;
        m_sceneManager.GetMxCoreViewMatrix().moveLiteral( 
            osg::Vec3d( m_worldTrans[ 0 ], m_worldTrans[ 1 ], m_worldTrans[ 2 ] ) );
    }
    
    PostProcessNav();
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandButton3Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }

    m_triggerWandMove = false;

    PreProcessNav();
    
    switch(event) 
    {
        case gadget::DigitalState::ON:
        {
            (*(m_wandButtonOnSignalMap["Wand.ButtonOn3"]))( gadget::KEY_NONE, 0, 0, gadget::DigitalState::ON );
            break;
        }
        case gadget::DigitalState::TOGGLE_ON:
        {
            (*(m_wandButtonPressSignalMap["Wand.ButtonPress3"]))( gadget::KEY_NONE, 0, 0, gadget::DigitalState::TOGGLE_ON );
            break;
        }
        case gadget::DigitalState::TOGGLE_OFF:
        {
            (*(m_wandButtonReleaseSignalMap["Wand.ButtonRelease3"]))( gadget::KEY_NONE, 0, 0, gadget::DigitalState::TOGGLE_OFF );
            break;
        }
        default:
            break;
    }
    
    if (( event == gadget::DigitalState::TOGGLE_ON ) ||
        ( event == gadget::DigitalState::ON ) )
    {
        m_buttonPushed = true;
        FreeRotateAboutWand( false );
    }
    
    PostProcessNav();
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandButton4Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }

    m_triggerWandMove = false;

    PreProcessNav();
    
    switch(event) 
    {
        case gadget::DigitalState::ON:
        {
            (*(m_wandButtonOnSignalMap["Wand.ButtonOn4"]))( gadget::KEY_NONE, 0, 0, gadget::DigitalState::ON );
            break;
        }
        case gadget::DigitalState::TOGGLE_ON:
        {
            (*(m_wandButtonPressSignalMap["Wand.ButtonPress4"]))( gadget::KEY_NONE, 0, 0, gadget::DigitalState::TOGGLE_ON );
            break;
        }
        case gadget::DigitalState::TOGGLE_OFF:
        {
            (*(m_wandButtonReleaseSignalMap["Wand.ButtonRelease4"]))( gadget::KEY_NONE, 0, 0, gadget::DigitalState::TOGGLE_OFF );
            break;
        }
        default:
            break;
    }
    
    if( event == gadget::DigitalState::TOGGLE_ON ||
            event == gadget::DigitalState::ON )
    {
        if( m_cadSelectionMode && 
           ( event == gadget::DigitalState::TOGGLE_ON ) )
        {
            if( m_unselectedCADFiles.size() )
            {
                m_unselectedCADFiles.back()->setNodeMask( 1 );
                m_unselectedCADFiles.pop_back();
            }
            return;
        }
        
        if( !m_cadSelectionMode )
        {
            m_buttonPushed = true;
            m_worldQuat = *mResetAxis;
            m_sceneManager.GetMxCoreViewMatrix().reset();

            for( unsigned int i = 0; i < 3; ++i )
            {
                //m_worldTrans[ i ] = -mResetPosition->at( i );
                //world_quat[ i ] = 0.0f;
                mCenterPoint->mData[ i ] = 0.0f;
            }
        }
    }
    
    PostProcessNav();
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandButton5Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    m_triggerWandMove = false;

    PreProcessNav();
    
    switch(event) 
    {
        case gadget::DigitalState::ON:
        {
            (*(m_wandButtonOnSignalMap["Wand.ButtonOn5"]))( gadget::KEY_NONE, 0, 0, gadget::DigitalState::ON );
            break;
        }
        case gadget::DigitalState::TOGGLE_ON:
        {
            (*(m_wandButtonPressSignalMap["Wand.ButtonPress5"]))( gadget::KEY_NONE, 0, 0, gadget::DigitalState::TOGGLE_ON );
            break;
        }
        case gadget::DigitalState::TOGGLE_OFF:
        {
            (*(m_wandButtonReleaseSignalMap["Wand.ButtonRelease5"]))( gadget::KEY_NONE, 0, 0, gadget::DigitalState::TOGGLE_OFF );
            break;
        }
        default:
            break;
    }
    

    ///Check and see if the cpt is enabled so that we can handle
    ///button events differently
    ves::xplorer::scenegraph::camera::CameraManager& cameraManager = 
    m_sceneManager.GetCameraManager();
    bool cptEnabled = cameraManager.IsCPTEnabled();
    if( ( event == gadget::DigitalState::TOGGLE_ON ) ||
       ( event == gadget::DigitalState::ON ) )
    {
        if( cptEnabled && ( event == gadget::DigitalState::TOGGLE_ON ) )
        {
            if( cameraManager.getNumChildren() == 0 )
            {
                return;
            }
            ves::xplorer::scenegraph::camera::CameraObject* activeCamera =
            cameraManager.GetActiveCameraObject();
            unsigned int activeNum = 0;
            if( activeCamera )
            {
                activeNum = cameraManager.getChildIndex( 
                                                        static_cast< osg::Group* >( activeCamera ) );
            }
            
            ves::xplorer::DeviceHandler::instance()->UnselectObjects();
            
            if( activeNum + 1 < cameraManager.getNumChildren() )
            {
                activeNum += 1;
            }
            else
            {
                activeNum = 0;
            }
            
            ves::xplorer::scenegraph::camera::CameraObject* newCameraObject = 
            static_cast< ves::xplorer::scenegraph::camera::CameraObject* >( 
                                                                           cameraManager.getChild( activeNum ) );
            cameraManager.SetActiveCameraObject( newCameraObject, true );
            
            //Right now we are saying you must have a DCS
            scenegraph::DCS& selectedDCS = newCameraObject->GetDCS();
            gmtl::Matrix44d selectedMatrix = selectedDCS.GetMat();
            
            //Set the connection between the scene manipulator and the selected dcs
            //sceneManipulator->Connect( &selectedDCS );
            
            //If dcs is from a camera object, we want to rotate about local zero point
            //osg::Vec3d center( 0.0, 0.0, 0.0 );
            //center = center * osg::Matrixd( selectedMatrix.mData );
            //sceneManipulator->SetPosition( center );
            
            //We need to transform center point into camera space
            //In the future the center point will be in world coordinates
            //center = center * osg::Matrixd( sceneManager.GetNavDCS()->GetMat().mData );
            //gmtl::Point3d tempCenter( center.x(), center.y(), center.z() );
            //deviceHandler.SetCenterPoint( &tempCenter );
            
            //Set the selected DCS
            ves::xplorer::DeviceHandler::instance()->SetSelectedDCS( &selectedDCS );
            
            //Need to do this for multi-pass techniques
            if( m_sceneManager.IsRTTOn() )
            {
                selectedDCS.SetTechnique( "Glow" );
            }
            else
            {
                selectedDCS.SetTechnique( "Select" );
            }
            
            //Hand the node we are interested in off to the animation engine
            NavigationAnimationEngine& nae =
            *(NavigationAnimationEngine::instance());
            nae.SetDCS( m_sceneManager.GetNavDCS() );
            
            //Hand our created end points off to the animation engine
            selectedMatrix = gmtl::invert( selectedMatrix );
            const gmtl::Matrix44d tempHeadMatrix = m_sceneManager.GetHeadMatrix();
            const gmtl::AxisAngled myAxisAngle( gmtl::Math::deg2Rad( double( -90 ) ), 1, 0, 0 );
            gmtl::Matrix44d myMat = gmtl::make< gmtl::Matrix44d >( myAxisAngle );
            selectedMatrix = tempHeadMatrix * myMat * selectedMatrix;
            
            gmtl::Vec3d navToPoint =
            gmtl::makeTrans< gmtl::Vec3d >( selectedMatrix );
            gmtl::Quatd rotationPoint =
            gmtl::makeRot< gmtl::Quatd >( selectedMatrix );
            nae.SetAnimationEndPoints( navToPoint, rotationPoint );
            return;
        }
        
        if( !cptEnabled )
        {
            if( ves::xplorer::NavigationAnimationEngine::instance()->IsActive() )
            {
                ves::xplorer::NavigationAnimationEngine::instance()->
                IncrementAnimationSpeed( 0.5 );
            }
            else
            {
                DeviceHandler::instance()->UnselectObjects();
            }
            return;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandButton6Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    m_triggerWandMove = false;

    switch(event) 
    {
        case gadget::DigitalState::ON:
        {
            (*(m_wandButtonOnSignalMap["Wand.ButtonOn6"]))( gadget::KEY_NONE, 0, 0, gadget::DigitalState::ON );
            break;
        }
        case gadget::DigitalState::TOGGLE_ON:
        {
            m_hideShowUI();
            
            (*(m_wandButtonPressSignalMap["Wand.ButtonPress6"]))( gadget::KEY_NONE, 0, 0, gadget::DigitalState::TOGGLE_ON );
            break;
        }
        case gadget::DigitalState::TOGGLE_OFF:
        {
            (*(m_wandButtonReleaseSignalMap["Wand.ButtonRelease6"]))( gadget::KEY_NONE, 0, 0, gadget::DigitalState::TOGGLE_OFF );
            break;
        }
        default:
            break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandButton0DoubleClick( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    //LOG_INFO( "OnWandButton0DoubleClick: DigitalState::ON" );
    (*(m_wandDoubleClickSignalMap["Wand.ButtonDoubleClick0"]))( gadget::MBUTTON1, 0, 0, 0, gadget::KEY_DOWN|gadget::BUTTON1_MASK );
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandButton1DoubleClick( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }

    (*(m_wandDoubleClickSignalMap["Wand.ButtonDoubleClick1"]))( gadget::MBUTTON2, 0, 0, 0, gadget::KEY_DOWN|gadget::BUTTON2_MASK );
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandButton2DoubleClick( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }

    (*(m_wandDoubleClickSignalMap["Wand.ButtonDoubleClick2"]))( gadget::MBUTTON3, 0, 0, 0, gadget::KEY_DOWN|gadget::BUTTON3_MASK );
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandButton3DoubleClick( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }

    (*(m_wandDoubleClickSignalMap["Wand.ButtonDoubleClick3"]))( gadget::MBUTTON4, 0, 0, 0, gadget::KEY_DOWN|gadget::BUTTON4_MASK );
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandButton4DoubleClick( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }

    (*(m_wandDoubleClickSignalMap["Wand.ButtonDoubleClick4"]))( gadget::MBUTTON5, 0, 0, 0, gadget::KEY_DOWN|gadget::BUTTON5_MASK );
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandButton5DoubleClick( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }

    (*(m_wandDoubleClickSignalMap["Wand.ButtonDoubleClick5"]))( gadget::MBUTTON6, 0, 0, 0, gadget::KEY_DOWN|gadget::BUTTON6_MASK );
}
////////////////////////////////////////////////////////////////////////////////
void Wand::PreProcessNav()
{
    //If the wand does not exist
    if( m_wand->isStupefied() )
    {
        return;
    }
    
    m_buttonPushed = false;
    
    //Update the wand direction every frame
    //UpdateWandLocalDirection();
    //UpdateWandGlobalLocation();

    /*m_activeDCS =
        ves::xplorer::DeviceHandler::instance()->GetActiveDCS();
    if( !m_activeDCS )
    {
        return;
    }*/

    buttonData[ 0 ] = digital[ 0 ]->getData();
    buttonData[ 1 ] = digital[ 1 ]->getData();
    buttonData[ 2 ] = digital[ 2 ]->getData();
    buttonData[ 3 ] = digital[ 3 ]->getData();
    buttonData[ 4 ] = digital[ 4 ]->getData();
    buttonData[ 5 ] = digital[ 5 ]->getData();
    
    m_rotIncrement.set( 0, 0, 0, 1 );
    m_worldTrans[ 0 ] = 0.;
    m_worldTrans[ 1 ] = 0.;
    m_worldTrans[ 2 ] = 0.;
    
    UpdateForwardAndUp();
    
    //const gmtl::Matrix44d tempHeadMatrix = m_sceneManager.GetGlobalViewMatrix();
    //std::cout << " head " << std::endl << m_sceneManager.GetHeadMatrix() << std::endl << m_sceneManager.GetInvertedNavMatrix() << std::endl;
    const gmtl::Point4d tempHeadMatrix = m_sceneManager.GetInvertedNavMatrix() * gmtl::makeTrans< gmtl::Point4d >( m_sceneManager.GetHeadMatrix() );
    //std::cout << tempHeadMatrix << std::endl;
    m_sceneManager.GetMxCoreViewMatrix().setOrbitCenterPoint( 
        osg::Vec3d( tempHeadMatrix.mData[0], tempHeadMatrix.mData[1], tempHeadMatrix.mData[2] ) );
}
////////////////////////////////////////////////////////////////////////////////
void Wand::PostProcessNav()
{
    if( !m_buttonPushed )
    {
        return;
    }

    ///If we actually pushed a button then move things
    //if( !m_characterController.IsEnabled() )
    {
        //Set the DCS postion based off of previous
        //manipulation of the worldTrans array
        /*for( unsigned int i = 0; i < 3; ++i )
        {
            m_worldTrans[ i ] = -m_worldTrans[ i ];
        }*/
        //m_sceneManager.GetMxCoreViewMatrix().setPosition( osg::Vec3d( m_worldTrans[ 0 ], m_worldTrans[ 1 ], m_worldTrans[ 2 ] ) );

        /*gmtl::Quatd tempQuat;
        tempQuat.set( m_rotIncrement[ 0 ], m_rotIncrement[ 1 ], m_rotIncrement[ 2 ], m_rotIncrement[ 3 ] );
        gmtl::AxisAngled axisAngle = gmtl::makeRot< gmtl::AxisAngled >( tempQuat );

        m_sceneManager.GetMxCoreViewMatrix().rotateOrbit( axisAngle[ 0 ], 
            osg::Vec3d( axisAngle[ 1 ], axisAngle[ 2 ], axisAngle[ 3 ] ) );*/

        m_updateData( true );

        //m_worldQuat *= m_rotIncrement;
        
        //gmtl::Matrix44d vjHeadMat = gmtl::convertTo< double >( head->getData() );
        //Device::EnsureCameraStaysAboveGround( vjHeadMat, m_worldTrans, 
        //    m_worldQuat, m_subzeroFlag, m_zEqualsZeroFlag );
    }
    /*else if( m_characterController.IsEnabled() )
    {
        //Set the DCS postion based off of previous
        //manipulation of the worldTrans array
        for( unsigned int i = 0; i < 3; ++i )
        {
            m_worldTrans[ i ] = -m_worldTrans[ i ];
        }
        
        m_worldQuat *= m_rotIncrement;
        
        gmtl::Matrix44d vjHeadMat = gmtl::convertTo< double >( head->getData() );
        Device::EnsureCameraStaysAboveGround ( vjHeadMat, m_worldTrans, 
            m_worldQuat, m_subzeroFlag, m_zEqualsZeroFlag );
        
        m_activeDCS->SetTranslationArray( m_worldTrans );
        m_activeDCS->SetQuat( m_worldQuat );
        //If the z axis is positive then rotate by a specific dz
        if( m_rotIncrement[ 3 ] > 0 )
        {
            m_characterController.Rotate( 0., 
                osg::DegreesToRadians( -rotationStepSize ) );
        }
        else
        {
            m_characterController.Rotate( 0., 
                osg::DegreesToRadians( rotationStepSize ) );
        }
        
        if( buttonData[ 2 ] == gadget::Digital::TOGGLE_ON ||
           buttonData[ 2 ] == gadget::Digital::ON )
        {
            m_characterController.StepForward( true );
        }
    }*/
}
////////////////////////////////////////////////////////////////////////////////
osg::MatrixTransform& Wand::GetWandTransform()
{
    return *(m_wandPAT.get());
}
////////////////////////////////////////////////////////////////////////////////
void Wand::LatePreFrameUpdate()
{
    //If the wand does not exist
    if( m_wand->isStupefied() )
    {
        return;
    }
    
    if( !m_triggerWandMove )
    {
        return;
    }

    m_triggerWandMove = false;

    PreProcessNav();
    //SetupStartEndPoint( m_startPoint, m_endPoint );
    m_startEndPointSignal( m_startPoint, m_endPoint );
    m_wandMove( 0, 0, 0, 0 );
}
////////////////////////////////////////////////////////////////////////////////
void Wand::OnWandMoveTimer( Poco::Util::TimerTask& task )
{
    if( m_shutdown )
    {
        task.cancel();
        return;
    }

    gmtl::Matrix44d vrjWandMat = gmtl::convertTo< double >( m_wand->getData() );
    ///If our wand is not moving let the keyboard potentially interface with 
    ///the ui. We may need a better way to handle this.
    if( m_previousWandMat != vrjWandMat )
    {
        m_triggerWandMove = true;
        m_previousWandMat = vrjWandMat;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateForwardAndUp()
{
    if( m_wand->isStupefied() )
    {
        return;
    }
    
    gmtl::Matrix44d vrjWandMat = gmtl::convertTo< double >( m_wand->getData() );
    const gmtl::AxisAngled myAxisAngle( osg::DegreesToRadians( double( 90 ) ), 1, 0, 0 );
    const gmtl::Matrix44d myMat = gmtl::make< gmtl::Matrix44d >( myAxisAngle );
    gmtl::Vec3d x_axis( 1.0, 0.0, 0.0 );
    gmtl::Matrix44d zUpMatrix = gmtl::makeRot< gmtl::Matrix44d >(
        gmtl::AxisAngled( gmtl::Math::deg2Rad( -90.0 ), x_axis ) );
    
    vrjWandMat = myMat * vrjWandMat * zUpMatrix;
    
    const gmtl::Point4d tempWandPoint = m_sceneManager.GetInvertedNavMatrix() * 
        gmtl::makeTrans< gmtl::Point4d >( vrjWandMat );
    
    m_startPoint.set( tempWandPoint.mData[ 0 ], tempWandPoint.mData[ 1 ], tempWandPoint.mData[ 2 ] );
    
    gmtl::Vec3d vjVec;
    vjVec.set( 0.0f, 0.0f, 1.0f );
    gmtl::xform( vjVec, vrjWandMat, vjVec );
    gmtl::normalize( vjVec );
    osg::Vec3d upVec( vjVec.mData[ 0 ], vjVec.mData[ 1 ], vjVec.mData[ 2 ] );
    //m_sceneManager.GetMxCoreViewMatrix().setUp( upVec );
    
    vjVec.set( m_sceneManager.GetMxCoreViewMatrix().getDir().ptr() );
    gmtl::xform( vjVec, vrjWandMat, vjVec );
    gmtl::normalize( vjVec );
    osg::Vec3d dirVec( vjVec.mData[ 0 ], vjVec.mData[ 1 ], vjVec.mData[ 2 ] );
    //m_sceneManager.GetMxCoreViewMatrix().setDir( dirVec );
    //std::cout << " wand dir " << vjVec << std::endl;
    m_worldTrans[ 0 ] = vjVec.mData[ 0 ] * translationStepSize;
    m_worldTrans[ 1 ] = vjVec.mData[ 1 ] * translationStepSize;
    m_worldTrans[ 2 ] = vjVec.mData[ 2 ] * translationStepSize;
    
    m_endPoint.set( vjVec.mData[ 0 ] * m_distance, vjVec.mData[ 1 ] * m_distance, vjVec.mData[ 2 ] * m_distance );
}
////////////////////////////////////////////////////////////////////////////////
