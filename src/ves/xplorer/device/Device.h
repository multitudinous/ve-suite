/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

#ifndef DEVICE_H
#define DEVICE_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/GlobalBase.h>

#include <ves/xplorer/scenegraph/DCS.h>

// --- vrJugglerIncludes --- //
#include <gmtl/Point.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
class Vec3d;
}

namespace ves
{

namespace open
{
namespace xml
{
class Command;
}
}

namespace xplorer
{
/*!\file Device.h
 * Device API
 */
/*!\class VE_XPlorer::Device
 *
 */
class VE_XPLORER_EXPORTS Device : public GlobalBase
{
public:
    ///Constructor
    Device();

    ///Destructor
    virtual ~Device();

    ///Definition to update the position in scene
    virtual void UpdateNavigation();

    ///Definition to update the current object selected
    virtual void UpdateSelection();

    ///New function for new VECommand structure
    ///\param veCommand Sets the Command used for navigation
    virtual void SetVECommand( ves::open::xml::CommandPtr command );

    ///Do not know what this is
    virtual void UpdateCommand();

    ///Check if the head collides with the rest of the objects in the scene
    ///\param headPositionInWorld The head position in world coordinates
    bool CheckCollisionsWithHead( osg::Vec3 headPositionInWorld );

    ///Get the active coordinate system
    ves::xplorer::scenegraph::DCS* GetActiveDCS();

    ///Get the active coordinate system
    ves::xplorer::scenegraph::DCS* GetSelectedDCS();

    ///Set the active coordinate system
    ///\param dcs The current active coordinate system
    void SetActiveDCS( ves::xplorer::scenegraph::DCS* activeDCS );

    ///Set the center point
    ///\param cp The center point
    void SetCenterPoint( gmtl::Point3d* centerPoint );

    ///Set the center point delta jump
    void SetCenterPointJump( double* jump );

    ///Set the center point threshold
    void SetCenterPointThreshold( double* threshold );

    ///Set the reset position for the world
    void SetResetWorldPosition( osg::Quat& quat, std::vector< double >& pos );

    ///Set the selected dcs
    ///\param dcs The current selected dcs
    void SetSelectedDCS( ves::xplorer::scenegraph::DCS* selectedDCS );

protected:
    ///Process the selection of a piece of geometry
    virtual void ProcessSelection();

    ///Definition to set the start and end point
    ///\param startPoint The start point
    ///\param endPoint
    virtual void SetStartEndPoint(
        osg::Vec3d* startPoint, osg::Vec3d* endPoint );

    ///Set the start and end point
    ///\param startPoint The start point
    ///\param endPoint The end point
    virtual void DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint );

    ///Triggers a center point jump after this distance has been breached
    double* mCenterPointThreshold;
    ///The distance the center point jumps along the +y axis
    double* mCenterPointJump;

    ///The reset position for the world
    std::vector< double > mResetPosition;

    ///The point about which rotation occurs
    gmtl::Point3d* mCenterPoint;

    ///The reset axis for the world
    osg::Quat mResetAxis;

    ///The current active DCS
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mActiveDCS;
    ///The current selected DCS
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mSelectedDCS;
    
};
} //end xplorer
} //end ves

#endif //DEVICE_H
