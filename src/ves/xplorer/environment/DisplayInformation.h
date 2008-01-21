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
#ifndef DISPLAY_INFORMATION_H
#define DISPLAY_INFORMATION_H
/*!\file DisplayInformation.h
DisplayInformation API
*/
/*!\class ves::xplorer::DisplayInformation
*
*/
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/Switch.h>

#ifdef _OSG
#include <osg/ref_ptr>
#include <osg/CameraNode>
#include <osgText/Text>
#endif

//C/C++ Libraries
#include <vector>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class Switch;
class CADEntity;
}
}
}

namespace ves
{
namespace xplorer
{
class VE_XPLORER_EXPORTS DisplayInformation
{
public:
    ///Constructor
    DisplayInformation();

    ///Destructor
    ~DisplayInformation();

    ///Do not know what this is
    void LatePreFrame();

    ///Set flag to display frame rate
    ///\param val Bool to determine is frame rate is displayed
    void SetFrameRateFlag( bool val );

    ///Set flag to display world coordinate system
    ///\param val Bool to determine is coordinate system is displayed
    void SetCoordSysFlag( bool val );

    ///Set the text color of the frame rate and world coordinate system
    ///\param color A vector containing color properties
    void SetTextColor( std::vector< double > color );

    ///Set position of the frame rate and world coordinate system
    ///\param width Width of the screen
    ///\param height Height of the screen
    void SetDisplayPositions( unsigned int width, unsigned int height );

private:
    ///Initialize the framerate display
    void InitFrameRateDisplay();

    ///Initialize the world coordinate system display
    void InitCoordSysDisplay();

    osg::ref_ptr< ves::xplorer::scenegraph::Switch > display_switch; ///<Allows switching between different display options

    osg::ref_ptr< osg::CameraNode > framerate;
    osg::ref_ptr< osg::CameraNode > wcs;

    osg::ref_ptr< osgText::Text > framerate_text; ///<Text for the frame rate
    osg::ref_ptr< osgText::Text > wcs_x_text; ///<Text for the x world coordinate system
    osg::ref_ptr< osgText::Text > wcs_y_text; ///<Text for the y world coordinate system
    osg::ref_ptr< osgText::Text > wcs_z_text; ///<Text for the z world coordinate system

    ves::xplorer::scenegraph::CADEntity* wcs_model; ///<Geometry for world coordinate system
};
}
}

#endif //DISPLAY_INFORMATION
