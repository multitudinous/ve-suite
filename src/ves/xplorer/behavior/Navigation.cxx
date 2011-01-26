/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include <ves/xplorer/behavior/Navigation.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/Model.h>

#include <ves/xplorer/plugin/PluginBase.h>

#include <ves/xplorer/network/GraphicalPluginManager.h>

#include <ves/xplorer/environment/cfdDisplaySettings.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/BooleanPropagationCombiner.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/scenegraph/camera/CameraManager.h>

#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>

#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>

#include <ves/xplorer/scenegraph/FindParentsVisitor.h>
#include <ves/xplorer/scenegraph/CoordinateSystemTransform.h>
//#include <ves/xplorer/scenegraph/SetStateOnNURBSNodeVisitor.h>
#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>
#include <ves/xplorer/scenegraph/Select.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

#include <ves/xplorer/Debug.h>

#include <ves/open/xml/model/Model.h>

//OSG
#include <osg/BoundingSphere>
#include <osg/Vec3d>
#include <osg/Matrix>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/AutoTransform>
#include <osg/io_utils>

//GMTL
#include <gmtl/Matrix.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletDynamics/ConstraintSolver/btPoint2PointConstraint.h>

namespace ves
{
namespace xplorer
{
namespace behavior
{
////////////////////////////////////////////////////////////////////////////////
Navigation::Navigation()
    :
    m_physicsSimulator( *ves::xplorer::scenegraph::PhysicsSimulator::instance() ),
    m_sceneManager( *ves::xplorer::scenegraph::SceneManager::instance() ),
    m_characterController( m_sceneManager.GetCharacterController() ),
    m_manipulatorManager( m_sceneManager.GetManipulatorManager() ),
    m_cameraManager( m_sceneManager.GetCameraManager() ),
    m_lineSegmentIntersector( new osgUtil::LineSegmentIntersector(
        osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0, 0.0, 0.0 ) ) ),
    m_currX( 0 ),
    m_currY( 0 ),
    m_pickedBody( 0 ),
    m_pickConstraint( 0 ),
    mDeltaRotation( 0.0, 0.0, 0.0, 1.0 ),
    mDeltaTranslation( 0.0, 0.0, 0.0 ),
    mCenterPointThreshold( 0.1 ),
    mCenterPointJump( 10.0 )
{    
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.MouseMove", bool( int, int, int, int ),
                      eventmanager::BooleanPropagationCombiner, &Navigation::ProcessNavigation,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonPress1%", bool( gadget::Keys, int, int, int ),
                     eventmanager::BooleanPropagationCombiner, &Navigation::RegisterButtonPress,
                     m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonPress2%", bool( gadget::Keys, int, int, int ),
                     eventmanager::BooleanPropagationCombiner, &Navigation::RegisterButtonPress,
                     m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonPress3%", bool( gadget::Keys, int, int, int ),
                     eventmanager::BooleanPropagationCombiner, &Navigation::RegisterButtonPress,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_1( "MainWindow.JumpSignal", void( const std::string ), &Navigation::SetCenterPointJumpMode,
                     m_connections, any_SignalType, normal_Priority );

    //eventmanager::EventManager::instance()->RegisterSignal(
    //    new eventmanager::SignalWrapper< ObjectPickedSignal_type >( &m_objectPickedSignal ),
    //    "KeyboardMouse.ObjectPickedSignal" );
}
////////////////////////////////////////////////////////////////////////////////
Navigation::~Navigation()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool Navigation::RegisterButtonPress( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{
    m_currX = xPos;
    m_currY = yPos;

//#if !defined( VPR_OS_Windows )
    m_prevX = m_currX;
    m_prevY = m_currY;
//#endif
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool Navigation::ProcessNavigation( int xPos, int yPos, int zPos, int buttonState )
{
    if( buttonState == 0 || buttonState&gadget::KEY_SHIFT )
    {
        return false;
    }
    m_currX = xPos;
    m_currY = yPos;
    
/*#if defined( VPR_OS_Windows )
    double dx = mouse_evt->getScrollDeltaX();
    double dy = mouse_evt->getScrollDeltaY();
#else*/
    double dx = m_currX - m_prevX;
    double dy = m_currY - m_prevY;
//#endif
    
//#if !defined( VPR_OS_Windows )
    m_prevX = m_currX;
    m_prevY = m_currY;
//#endif
    
    const int& windowWidth = m_sceneManager.GetCurrentGLTransformInfo()->GetWindowWidth();
    const int& windowHeight = m_sceneManager.GetCurrentGLTransformInfo()->GetWindowHeight();
    SetWindowValues( windowWidth, windowHeight );
    
    dx /= m_windowWidth;
    dy /= m_windowHeight;
    
    double mMagnitude = sqrt( dx * dx + dy * dy );

    if( buttonState&gadget::BUTTON1_MASK )
    {
        {
            
            {
                if( ( m_currX > 0.1 * m_windowWidth ) &&
                   ( m_currX < 0.9 * m_windowWidth ) &&
                   ( m_currY > 0.1 * m_windowHeight ) &&
                   ( m_currY < 0.9 * m_windowHeight ) )
                {
                    double angle = mMagnitude * 7.0;
                    Rotate( angle, gmtl::Vec3d( -dy, 0.0, dx ) );
                }
                else
                {
                    Twist( dx, dy );
                }
                
                ProcessNavigation();
            }
        }
        //Mod key shift
        return false;
    }
    
    if( buttonState&gadget::BUTTON2_MASK )
    {
        Pan( dx, dy );
        ProcessNavigation();
        return false;
    }
    
    if( buttonState&gadget::BUTTON3_MASK )
    {
        Zoom( dy );
        ProcessNavigation();
        return false;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Navigation::Twist( double dx, double dy )
{
    double tempX = 1.0 / m_windowWidth;
    double tempY = 1.0 / m_windowHeight;
    double currTheta = atan2( m_currX * tempX - 0.5, m_currY * tempY - 0.5 );
    double prevTheta =
    atan2( m_currX * tempX - dx - 0.5, m_currY * tempY - dy - 0.5 );
    double angle = currTheta - prevTheta;
    
    //Twist about the y-axis
    Rotate( angle, gmtl::Vec3d( 0.0, 1.0, 0.0 ) );
}
////////////////////////////////////////////////////////////////////////////////
void Navigation::Zoom( double dy )
{
    /*
     gmtl::Matrix44d vpwMatrix = m_currentGLTransformInfo->GetVPWMatrix();
     
     double viewlength = mCenterPoint->mData[ 1 ];
     double d = ( viewlength * ( 1 / ( 1 - dy * 2 ) ) ) - viewlength;
     
     gmtl::Point3d yTransform = *mCenterPoint;
     yTransform.mData[ 1 ] += d;
     yTransform = vpwMatrix * yTransform;
     
     gmtl::Point3d position = vpwMatrix * *mCenterPoint;
     position.mData[ 2 ] = yTransform.mData[ 2 ];
     position = gmtl::invert( vpwMatrix ) * position;
     mDeltaTranslation = position - *mCenterPoint;
     
     //Test if center point has breached our specified threshold
     if( position.mData[ 1 ] < *mCenterPointThreshold )
     {
     //Prevent center point from jumping when manipulating a selected object
     scenegraph::DCS* const selectedDCS = DeviceHandler::instance()->GetSelectedDCS();
     if( selectedDCS )
     {
     mDeltaTranslation.set( 0.0, 0.0, 0.0 );
     
     return;
     }
     
     position.mData[ 1 ] = *mCenterPointJump;
     }
     
     *mCenterPoint = position;
     */
    
    double viewlength = m_sceneManager.GetCenterPoint().mData[ 1 ];
    double d = ( viewlength * ( 1 / ( 1 - dy * 2 ) ) ) - viewlength;
    
    mDeltaTranslation.mData[ 1 ] = d;
    m_sceneManager.GetCenterPoint().mData[ 1 ] += d;
    
    //Test if center point has breached our specified threshold
    if( m_sceneManager.GetCenterPoint().mData[ 1 ] < mCenterPointThreshold )
    {
        scenegraph::DCS* const selectedDCS = DeviceHandler::instance()->GetSelectedDCS();
        //Only jump center point for the worldDCS
        if( !selectedDCS )
        {
            m_sceneManager.GetCenterPoint().mData[ 1 ] = mCenterPointJump;
        }
        //Prevent the center point from jumping
        //if we are manipulating a selected object
        else
        {
            mDeltaTranslation.mData[ 1 ] = 0.0;
            m_sceneManager.GetCenterPoint().mData[ 1 ] -= d;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Navigation::Pan( double dx, double dz )
{
    /*
     gmtl::Matrix44d vpwMatrix = m_currentGLTransformInfo->GetVPWMatrix();
     
     //std::cout << "mCenterPoint: " << *mCenterPoint << std::endl;
     gmtl::Point3d position = vpwMatrix * *mCenterPoint;
     //std::cout << "vpwMatrix * *mCenterPoint: " << position << std::endl;
     position.mData[ 0 ] += dx * m_windowWidth;
     position.mData[ 1 ] += dz * m_windowHeight;
     position = gmtl::invert( vpwMatrix ) * position;
     //std::cout << "gmtl::invert( vpwMatrix ) * position: " << position << std::endl;
     
     mDeltaTranslation = position - *mCenterPoint;
     *mCenterPoint = position;
     
     //std::cout << std::endl;
     */
    double fovz = m_sceneManager.GetCurrentGLTransformInfo()->GetFOVZ();
    double d = m_sceneManager.GetCenterPoint().mData[ 1 ];
    double theta = fovz * 0.5 ;
    double b = 2.0 * d * tan( theta );
    double dwx = dx * b;
    double dwz =  dz * b;
    
    if( mAspectRatio > 1.0 )
    {
        dwx *= mAspectRatio;
    }
    
    mDeltaTranslation.mData[ 0 ] = dwx;
    mDeltaTranslation.mData[ 2 ] = dwz;
    
    m_sceneManager.GetCenterPoint().mData[ 0 ] += dwx;
    m_sceneManager.GetCenterPoint().mData[ 2 ] += dwz;
}
////////////////////////////////////////////////////////////////////////////////
void Navigation::Rotate( double angle, gmtl::Vec3d axis )
{
    gmtl::normalize( axis );
    gmtl::AxisAngled axisAngle( angle, axis );
    mDeltaRotation = gmtl::makeRot< gmtl::Quatd >( axisAngle );
}
////////////////////////////////////////////////////////////////////////////////
void Navigation::SetWindowValues( unsigned int w, unsigned int h )
{
    m_windowWidth = w;
    m_windowHeight = h;
    
    mAspectRatio =
    static_cast< double >( m_windowWidth ) / static_cast< double >( m_windowHeight );
}
////////////////////////////////////////////////////////////////////////////////
void Navigation::ProcessNavigation()
{
    gmtl::Matrix44d newTransform;
    gmtl::Matrix44d currentTransform;
    
    scenegraph::DCS* const activeDCS = DeviceHandler::instance()->GetActiveDCS();
    //Get the node where are all the geometry is handled
    osg::Group* const activeSwitchNode = m_sceneManager.GetActiveSwitchNode();
    //Get the node where all the nav matrix's are handled
    scenegraph::DCS* const cameraDCS = m_sceneManager.GetActiveNavSwitchNode();
    
    osg::ref_ptr< scenegraph::CoordinateSystemTransform > coordinateSystemTransform;
    //Test if we are manipulating the camera dcs or a model dcs
    if( activeDCS->GetName() != cameraDCS->GetName() )
    {
        //If local dcs, transform to camera space
        coordinateSystemTransform = new scenegraph::CoordinateSystemTransform(
                                                                              activeSwitchNode, activeDCS, true );
        
        currentTransform = coordinateSystemTransform->GetTransformationMatrix();
    }
    else
    {
        //If manipulating camera, no transformations are needed
        currentTransform = activeDCS->GetMat();
    }
    
    //Translate current transform origin back by the center point position
    gmtl::Matrix44d negCenterPointMatrix =
    gmtl::makeTrans< gmtl::Matrix44d >( -m_sceneManager.GetCenterPoint() );
    newTransform = negCenterPointMatrix * currentTransform;
    
    //Apply the delta transform at this new position
    gmtl::Matrix44d deltaTransform;
    gmtl::setRot( deltaTransform, mDeltaRotation );
    gmtl::setTrans( deltaTransform, mDeltaTranslation );
    newTransform = deltaTransform * newTransform;
    
    //Add back the center point position to the transform
    gmtl::Matrix44d posCenterPointMatrix =
        gmtl::makeTrans< gmtl::Matrix44d >( m_sceneManager.GetCenterPoint() );
    newTransform = posCenterPointMatrix * newTransform;
    
    //Convert matrix back to local space after delta transform has been applied
    if( activeDCS->GetName() != cameraDCS->GetName() )
    {
        //Remove local matrix from currentTransform
        //We are multiplying by a new transformed local matrix
        currentTransform =
        coordinateSystemTransform->GetTransformationMatrix( false );
        
        newTransform = gmtl::invert( currentTransform ) * newTransform;
    }
    
    //Set the activeDCS w/ new transform
    activeDCS->SetMat( newTransform );
    
    //Reset the delta transform
    mDeltaRotation.set( 0.0, 0.0, 0.0, 1.0 );
    mDeltaTranslation.set( 0.0, 0.0, 0.0 );
}
////////////////////////////////////////////////////////////////////////////////
void Navigation::SetCenterPointJumpMode( const std::string& jumpMode )
{
    if( jumpMode == "Small" )
    {
        mCenterPointJump = 10.0;
        mCenterPointThreshold = 0.1;
    }
    else if( jumpMode == "Medium" )
    {
        mCenterPointJump = 100.0;
        mCenterPointThreshold = 1.0;
    }
    else if( jumpMode == "Large" )
    {
        mCenterPointJump = 1000.0;
        mCenterPointThreshold = 10.0;
    }
    else if( jumpMode == "Bounding Box" )
    {
        mCenterPointJump = DeviceHandler::instance()->GetActiveDCS()->getBound().radius();
        mCenterPointThreshold = mCenterPointJump * 0.01;
    }
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
