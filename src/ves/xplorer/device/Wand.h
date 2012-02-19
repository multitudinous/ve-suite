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

#ifndef WAND_H
#define WAND_H

// --- VE-Suite Includes
#include <ves/VEConfig.h>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

#include <ves/xplorer/device/Device.h>

#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/open/xml/CommandPtr.h>

#include <boost/signals2/signal.hpp>

#include <ves/xplorer/eventmanager/InteractionEvent.h>

#include <ves/xplorer/Logging.h>

// --- POCO Includes --- //
#include <Poco/Util/Timer.h>

// --- VR Juggler Includes --- //
#include <gmtl/Vec.h>
#include <gmtl/Matrix.h>
#include <gadget/Type/DigitalData.h>
#include <gadget/Type/PositionInterface.h>
#include <gadget/Type/DigitalInterface.h>
#include <gadget/Event/DigitalEventInterface.h>
#include <gadget/Event/DigitalClickEventInterface.h>
#include <gadget/Event/EventPtr.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/MatrixTransform>
#include <osg/ref_ptr>

#include <osgUtil/IntersectVisitor>
#include <osgUtil/LineSegmentIntersector>

namespace osg
{
class Geode;
class Group;
class Vec4d;
class Vec3d;
}

namespace osgUtil
{
class LineSegmentIntersector;
}

namespace ves
{
namespace xplorer
{

namespace scenegraph
{
class DCS;
}

namespace behavior
{
class WandEvents;
}

namespace device
{
/*!\file Wand.h
 * Wand API
 * \class ves::xplorer::Wand
 * \namespace ves::xplorer
 *
 */
class VE_XPLORER_EXPORTS Wand : public Device
{
public:
    ///Constructor
    Wand();

    ///Destructor
    virtual ~Wand();

    ///
    ///\return
    virtual Wand* AsWand();

    ///Initialize some variables in the class
    virtual void Initialize();

    ///Processes wand events
    virtual void ProcessEvents( ves::open::xml::CommandPtr command );

    ///Set the rotation method
    ///\param input Indicates which rotation method is needed
    void SetHeadRotationFlag( int input );

    ///Process if selection is valid
    void ProcessHit();

    ///Update the events done by wand
    void UpdateObjectHandler();

    ///Find translation difference from last position to current
    //void UpdateDeltaWandPosition();
    ///Get the plane equation constants normal to the wand in world space
    double* GetPlaneEquationConstantsNormalToWand();
    
    ///Set wether when selecting parts if the parts will be turned off
    void SetCADSelectionMode( bool cadSelectionMode );

    ///Return the transform that holds the geode for the wand line
    osg::MatrixTransform& GetWandTransform();
    
    ///Identifies selection chosen by wand
    void UpdateSelectionLine( bool drawLine = true );
        
    ///Draws a beam from the wand to object
    ///\param startPoint The start position
    ///\param endPoint The end position
    virtual void DrawLine( const osg::Vec3d& startPoint, const osg::Vec3d& endPoint );
    
protected:
    ///Set the start and end point
    ///\param startPoint The start point
    ///\param endPoint The end point
    virtual void SetStartEndPoint( osg::Vec3d* startPoint, osg::Vec3d* endPoint );

    ///Make the raw geometry for the wand line
    void MakeWandLine();

    ///Roate about arbitrary axis
    void FreeRotateAboutWand( const bool freeRotate = true );

    ///Ensure that the camera stays above ground.
    void EnsureCameraStaysAboveGround( osg::Quat& world_quat );

    ///Preprocess data for nav
    void PreProcessNav();

    ///Postprocess data for nav
    void PostProcessNav();
    
    ///Update the main components of the wand positional information
    void UpdateForwardAndUp();

    ///Check and see if we should send a wand move event
    bool WandMoveUpdate();

private:
    /// The keyboardmouse device needed for juggler >= 3.1
    //gadget::event::last_event_tag
    //gadget::event::all_events_tag
    typedef gadget::DigitalEventInterface<gadget::event::all_events_tag,
        gadget::event::synchronized_tag> WandClickInterface;
    WandClickInterface m_wandButton0EventInterface;
    WandClickInterface m_wandButton1EventInterface;
    WandClickInterface m_wandButton2EventInterface;
    WandClickInterface m_wandButton3EventInterface;
    WandClickInterface m_wandButton4EventInterface;
    WandClickInterface m_wandButton5EventInterface;
    WandClickInterface m_wandButton6EventInterface;

    /// All keyboardmouse events get delivered here
    void OnWandButton0Event( gadget::DigitalState::State event );
    void OnWandButton1Event( gadget::DigitalState::State event );
    void OnWandButton2Event( gadget::DigitalState::State event );
    void OnWandButton3Event( gadget::DigitalState::State event );
    void OnWandButton4Event( gadget::DigitalState::State event );
    void OnWandButton5Event( gadget::DigitalState::State event );
    void OnWandButton6Event( gadget::DigitalState::State event );

    /// Interface to receive double-click events from gadgeteer
    typedef gadget::DigitalClickEventInterface< 2,
        gadget::event::all_events_tag,
        gadget::event::synchronized_tag > WandDoubleClickInterface;
    WandDoubleClickInterface m_wandButton0DoubleClickEventInterface;
    WandDoubleClickInterface m_wandButton1DoubleClickEventInterface;
    WandDoubleClickInterface m_wandButton2DoubleClickEventInterface;
    WandDoubleClickInterface m_wandButton3DoubleClickEventInterface;
    WandDoubleClickInterface m_wandButton4DoubleClickEventInterface;
    WandDoubleClickInterface m_wandButton5DoubleClickEventInterface;

    void OnWandButton0DoubleClick( gadget::DigitalState::State event );
    void OnWandButton1DoubleClick( gadget::DigitalState::State event );
    void OnWandButton2DoubleClick( gadget::DigitalState::State event );
    void OnWandButton3DoubleClick( gadget::DigitalState::State event );
    void OnWandButton4DoubleClick( gadget::DigitalState::State event );
    void OnWandButton5DoubleClick( gadget::DigitalState::State event );

    /// Required to be able to connect up to signals.
    ves::xplorer::eventmanager::ScopedConnectionList m_connections;

    ///LAte PreFrame slot to generate mouse mve events
    void LatePreFrameUpdate();
    ///Trigger a wand move event
    void OnWandMoveTimer( Poco::Util::TimerTask& task );
    
    gadget::DigitalInterface digital[ 6 ]; ///Array handling button controls on wand
    int buttonData[ 6 ]; ///<do not know what this does
    gadget::DigitalInterface buttonEight;

    int cfdIso_value; ///<Value to translate

    gadget::PositionInterface m_wand; ///<VRJuggler's wand positional interface
    gadget::PositionInterface head; ///<VRJuggler's head positional interface

    gmtl::Matrix44d vjHeadMat; ///<Contains current head position matrix

    double m_dir[ 3 ]; ///<Direction of the wand
    double worldLoc[ 3 ]; ///<Location of the objects with respect to the virtual space
    double cursorLoc[ 3 ]; ///<Location of the cursor with respect to the virtual space
    double objLoc[ 3 ]; ///<Location with respect to data set (the actual location to interact with data
    double cursorLen; ///<Cursor length

    double translationStepSize; ///<Size of translation step
    double rotationStepSize; ///<Size of rotation step
    ///Constants for the plane normal to the wand in world space
    double m_planeConstants[ 4 ];
    int rotationFlag; ///<Rotation flag

    double deltaTrans[ 3 ]; ///<Stores difference in translation from last position to to current

    osg::ref_ptr< osg::Geode > selectedGeometry; ///<Geometry currently selected
    double m_distance; ///<Used for scaling
    osg::Vec3d LastWandPosition; ///<Stores last wand position

    ///do not know what this does
    osg::ref_ptr< osgUtil::LineSegmentIntersector > m_beamLineSegment;
    ///Transform to manipulate the selection line
    osg::ref_ptr< osg::MatrixTransform > m_wandPAT;
    
    ///See if a button has been pushed
    bool m_buttonPushed;
    ///Quat used every frame to store and rotational increments
    osg::Quat m_rotIncrement;
    ///Array to hold work translation
    double m_worldTrans[ 3 ];
    ///Control the cad selection setting
    bool m_cadSelectionMode;
    ///Keep track fo the cad files that the user has unselected with the wand
    std::vector< osg::Node* > m_unselectedCADFiles;
    ///The quat for the wand
    osg::Quat m_worldQuat;
    ///Selected DCS
    ves::xplorer::scenegraph::DCS* m_activeDCS;
    
    /// ButtonPress signal type
    /// Params are: button, x, y, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal< void ( gadget::Keys, int, int, int ) > WandButtonPressSignal_type;
    
    /// ButtonRelease signal type
    typedef boost::signals2::signal< void ( gadget::Keys, int, int, int ) > WandButtonReleaseSignal_type;

    /// ButtonRelease signal type
    typedef boost::signals2::signal< void ( gadget::Keys, int, int, int ) > WandButtonOnSignal_type;

    /// MouseDoubleClick signal
    /// Params are: button, x, y, z, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal< void ( gadget::Keys, int, int, int, int ) > WandDoubleClickSignal_type;
    
    /// Map to hold ButtonPress signals
    typedef std::map< std::string, WandButtonPressSignal_type* > WandButtonPressSignalMapType;
    WandButtonPressSignalMapType m_wandButtonPressSignalMap;
    
    /// Map to hold ButtonRelease signals
    typedef std::map< std::string, WandButtonReleaseSignal_type* > WandButtonReleaseSignalMapType;
    WandButtonReleaseSignalMapType m_wandButtonReleaseSignalMap;
    
    typedef std::map< std::string, WandButtonOnSignal_type* > WandButtonOnSignalMapType;
    WandButtonOnSignalMapType m_wandButtonOnSignalMap;
    
    typedef std::map< std::string, WandDoubleClickSignal_type* > WandDoubleClickSignalMapType;
    WandDoubleClickSignalMapType m_wandDoubleClickSignalMap;
    
    /// signal for generating the start and end point for selection and other
    ///interaction tools.
    /// Params are: start point and end point
    typedef boost::signals2::signal< void ( osg::Vec3d, osg::Vec3d ) > StartEndPointSignal_type;
    StartEndPointSignal_type m_startEndPointSignal;
    
    /// MouseMove signal
    /// Params are: x, y, z, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal< void ( int, int, int, int ) > WandMoveSignal_type;
    WandMoveSignal_type m_wandMove;
    
    ///The signature to tell others the game pad is active
    ves::util::BoolSignal_type m_updateData;

    ///Hide show ui signal type
    ves::util::VoidSignal_type m_hideShowUI;

    ///Wand event management
    ves::xplorer::behavior::WandEvents* m_wandEvents;
    ///Selection ray start point
    osg::Vec3d m_startPoint;
    ///Selection ray end point
    osg::Vec3d m_endPoint;
    ///Timer to control when wand move events are sent
    Poco::Util::Timer m_wandMoveTimer;
    ///Trigger wand move
    bool m_triggerWandMove;
    ///We are shuting down
    bool m_shutdown;
    ///Store the previous wand matrix to see if it is moving at all
    gmtl::Matrix44d m_previousWandMat;
    ///Logger reference
    Poco::Logger& m_logger;
    ///Actual stream for this class
    ves::xplorer::LogStreamPtr m_logStream;
    ///Wand move button state
    unsigned int m_buttonMoveState;
    ///Trigger periodic wand move
    bool m_periodicWandMove;
};
} //end device
} //end xplorer
} //end ves

#endif //WAND_H
