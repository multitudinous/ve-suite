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
#ifndef DEVICE_H
#define DEVICE_H
/*!\file Device.h
Device API
*/
/*!\class VE_XPlorer::Device
* 
*/
// --- VE-Suite Stuff --- //
#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"

#include "VE_Xplorer/SceneGraph/DCS.h"

// --- VR Juggler Stuff --- //
#include <gmtl/Point.h>

// --- OSG Stuff --- //
#include <osg/ref_ptr>

namespace osg 
{
    class Vec3f;
}

namespace VE_XML
{
    class Command;
}

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS Device : public cfdGlobalBase
{
public:
    ///Constructor
    Device();

    ///Destructor
    virtual ~Device(){;}

    ///Definition to update the position in scene
    virtual void UpdateNavigation();

    ///Definition to update the current object selected
    virtual void UpdateSelection();

    ///New function for new VECommand structure
    ///\param veCommand Sets the Command used for navigation
    virtual void SetVECommand( VE_XML::Command* command = 0 );

    ///Do not know what this is
    virtual void UpdateCommand();

    ///Do not know what this is
    ///\param _cfdCommandArray 
    virtual bool CheckCommandId( VE_Xplorer::cfdCommandArray* _cfdCommandArray = 0 );

    ///Get the active coordinate system
    VE_SceneGraph::DCS* GetActiveDCS();

    ///Set the active coordinate system
    ///\param dcs The current active coordinate system
    void SetActiveDCS( VE_SceneGraph::DCS* dcs );

   ///Set the device mode
   ///\param mode Do not know what this does
   //void SetDeviceMode( std::string* mode );

    ///Set the center point
    ///\param cp The center point
    void SetCenterPoint( gmtl::Point3f* cp );

    ///Sets the center point threshold
    void SetCenterPointThreshold( float* threshold );

    ///Sets the center point delta jump
    void SetCenterPointJump( float* jump );

protected:
    ///Process the selection of a piece of geometry 
    virtual void ProcessSelection();

    ///Definition to set the start and end point
    ///\param startPoint The start point
    ///\param endPoint
    virtual void SetStartEndPoint( osg::Vec3f* startPoint, osg::Vec3f* endPoint );

    ///Set the start and end point
    ///\param startPoint The start point
    ///\param endPoint The end point
    virtual void DrawLine( osg::Vec3f startPoint, osg::Vec3f endPoint );

    osg::ref_ptr< VE_SceneGraph::DCS > activeDCS;///<Active DCS for the devices to operate on
    //std::string* device_mode;///<The device mode
    gmtl::Point3f* center_point;///<The point about which rotation occurs
    float* m_threshold;///<
    float* m_jump;///<
};
}

#endif //DEVICE_H
