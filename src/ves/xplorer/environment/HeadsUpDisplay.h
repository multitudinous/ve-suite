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

#ifndef VES_XPLORER_HEADS_UP_DISPLAY_H
#define VES_XPLORER_HEADS_UP_DISPLAY_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/Switch.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
    class Camera;
}

namespace osgText
{
class Text;
}

// --- C/C++ Libraries --- //
#include <vector>

namespace ves
{
namespace xplorer
{

// --- VE-Suite Forward Declarations --- //
namespace scenegraph
{
class Switch;
class CADEntity;
}

/*!\file HeadsUpDisplay.h
 * HeadsUpDisplay API
 */

/*!\class ves::xplorer::HeadsUpDisplay
 *
 */
class VE_XPLORER_EXPORTS HeadsUpDisplay
{
public:
    ///Constructor
    ///\param windowResolution Resolution of the xplorer window in pixels
    HeadsUpDisplay( std::pair< unsigned int, unsigned int > windowResolution );

    ///Destructor
    ~HeadsUpDisplay();

    ///Do not know what this is
    void LatePreFrame();

    ///Get the window resolution for the xplorer window in pixels
    std::pair< unsigned int, unsigned int > GetWindowResolution();

    ///Get the camera for the heads up display
    osg::Camera* GetCamera();

    ///Set flag to display frame rate
    ///\param val Bool to determine is frame rate is displayed
    void SetFrameRateFlag( bool val );

    ///Set flag to display world coordinate system
    ///\param val Bool to determine is coordinate system is displayed
    void SetCoordSysFlag( bool val );

    ///Set the text color of the frame rate and world coordinate system
    ///\param color A double pointer containing color properties
    void SetTextColor( std::vector< double > color );

private:
    void Initialize();

    ///<The resolution of the xplorer window in pixels
    std::pair< unsigned int, unsigned int > mWindowResolution;

    ///<The camera for the heads up display
    osg::ref_ptr< osg::Camera > mCamera;

    ///<Geode that contains the framerate text
    osg::ref_ptr< osg::Geode > mFramerateTextGeode;

    ///<Geometry for world coordinate system representation
    ves::xplorer::scenegraph::CADEntity* mGeometryWCS;

    ///<Text for the frame rate
    osg::ref_ptr< osgText::Text > mFramerateText;
    ///<Text for the x world coordinate system
    osg::ref_ptr< osgText::Text > mWCSxText;
    ///<Text for the y world coordinate system
    osg::ref_ptr< osgText::Text > mWCSyText;
    ///<Text for the z world coordinate system
    osg::ref_ptr< osgText::Text > mWCSzText;
};
}
}

#endif //VES_XPLORER_HEADS_UP_DISPLAY_H
