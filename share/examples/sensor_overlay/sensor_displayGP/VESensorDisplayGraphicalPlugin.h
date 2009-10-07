/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#ifndef VE_SENSOR_DISPLAY_GRAPHICAL_PLUGIN_H
#define VE_SENSOR_DISPLAY_GRAPHICAL_PLUGIN_H

// --- My Includes --- //
#include <osg/MatrixTransform>
#include <osg/AnimationPath>
#include <osg/ShapeDrawable>
#include <osg/Sequence>

#include <osgText/Text>

#include <osgDB/ReadFile>

#include <osgSim/ColorRange>
#include <osg/Vec3d>

#include <osg/Billboard>

#include <osg/BlendFunc>

class Shaders;

namespace display
{
    class DigitalGauge;
}

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>
#include <osg/Switch>

namespace ves
{
namespace xplorer
{

namespace device
{
class KeyboardMouse;
}

namespace scenegraph
{
class DCS;
}

} //end xplorer
} //end ves

// --- C/C++ Libraries --- //
#include <map>

class VE_USER_PLUGIN_EXPORTS VESensorDisplayGraphicalPlugin : public ves::xplorer::plugin::PluginBase
{
public:
    VESensorDisplayGraphicalPlugin();
    virtual ~VESensorDisplayGraphicalPlugin();

    virtual void InitializeNode( osg::Group* veworldDCS );
    virtual void PreFrameUpdate(); 
	
	osg::Drawable* VESensorDisplayGraphicalPlugin::createQuad(osg::StateSet* bbState);
	void VESensorDisplayGraphicalPlugin::setTexture();
	//void VESensorDisplayGraphicalPlugin::FileExists(std::string strFilename);
	
       
private:
    ves::xplorer::device::KeyboardMouse* m_keyboard;

 
	
	osg::Drawable* quadDrawable;
	osg::Drawable* quad1Drawable;
	osg::Drawable* quad2Drawable;
	osg::Drawable* quad3Drawable;
	
	osg::Texture2D* texture;
	osg::Texture2D* texture1;
	osg::Texture2D* texture2;
	osg::Texture2D* texture3;
	
	osg::StateSet* billBoardStateSet;
	osg::StateSet* billBoardStateSet1;
	osg::StateSet* billBoardStateSet2;
	osg::StateSet* billBoardStateSet3;
	int iterator;
	


};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( VESensorDisplayGraphicalPlugin )

#endif //VE_SENSOR_DISPLAY_GRAPHICAL_PLUGIN_H
