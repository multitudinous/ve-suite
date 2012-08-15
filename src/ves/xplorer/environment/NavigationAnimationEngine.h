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
#ifndef VES_XPLORER_NAVIGATION_ANIMATION_ENGINE_H
#define VES_XPLORER_NAVIGATION_ANIMATION_ENGINE_H

// --- VE-Suite Includes --- //
#include <ves/open/xml/CommandPtr.h>

#include <ves/xplorer/GlobalBase.h>

#include <ves/xplorer/event/EventHandler.h>

#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

// --- vrJuggler Includes --- //
#include <gmtl/Vec.h>
#include <gmtl/Quat.h>

#include <vpr/Util/Timer.h>
#include <vpr/Util/Singleton.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

// --- C/C++ Libraries --- //
#include <vector>
#include <map>
#include <utility>

namespace ves
{
namespace xplorer
{

class cfdQuatCam;

namespace scenegraph
{
class DCS;
}

/*!\file NavigationAnimationEngine.h
 * NavigationAnimationEngine API
 */
/*!\class ves::xplorer::NavigationAnimationEngine
 *
 */
class VE_XPLORER_EXPORTS NavigationAnimationEngine : public GlobalBase
{
private:
    ///Constructor
    NavigationAnimationEngine();

    ///Destructor
    virtual ~NavigationAnimationEngine();

    ///Do not know what this is
    ///\param DeviceHandler
    vprSingletonHeader( NavigationAnimationEngine );

public:
    ///In future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand();

    ///Process the command that was set by conductor
    virtual void ProcessCommand();

    ///Set the DCS
    ///\param newDCS The new DCS
    void SetDCS( ves::xplorer::scenegraph::DCS* newDCS );

    ///If a quat is active this will move the cam to the next location
    void PreFrameUpdate();

    ///Set the quaternions and position for the animation
    void SetAnimationEndPoints( gmtl::Vec3d navToPoint, gmtl::Quatd rotationPoint,
                                bool setCenterPoint = false,
                                ves::xplorer::scenegraph::DCS* centerPointDCS = NULL );

    ///Set the quaternions and position for the animation
    void SetAnimationPoints( std::vector < std::pair < gmtl::Vec3d,
                             gmtl::Quatd > > animationPoints );

    ///Do we have an active animation
    bool IsActive();

    ///Set the speed of travel for movement. We default to 10 ft/s.
    void SetAnimationSpeed( double travelSpeed );

    ///Increment the animation speed
    void IncrementAnimationSpeed( double increment );

    void StopAnimation();

    void SetAnimationLoopingOn( bool flag );

private:
    ///Update the gui with the new data
    void UpdateViewGUIPointData();

    ///
    double t;
    ///
    double movementIntervalCalc;
    ///
    double m_movementSpeed;
    ///
    double m_deltaTime;
    ///
    vpr::Timer* m_frameTimer;
    ///
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > _worldDCS;
    ///
    gmtl::Vec3d mEndVec;
    ///
    gmtl::Quatd mEndQuat;
    ///
    bool mBeginAnim;
    ///
    bool mSetCenterPoint;
    ///
    ves::xplorer::scenegraph::DCS* mCenterPointDCS;
    ///The last angle
    double m_lastAngle;

    std::vector < std::pair < gmtl::Vec3d, gmtl::Quatd > > m_animationPoints;

    size_t m_animationPointIndex;

    bool m_loopAnimation;

    ves::util::VoidSignal_type m_flythroughBeginSignal;
    ves::util::VoidSignal_type m_flythroughEndSignal;
};
} //end xplorer
} //end ves

#endif //CFD_QUAT_CAM_HANDLER_H
