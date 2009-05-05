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

#ifndef VE_ANIMATION_GRAPHICAL_PLUGIN_H
#define VE_ANIMATION_GRAPHICAL_PLUGIN_H

// --- My Includes --- //
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

class VE_USER_PLUGIN_EXPORTS VEAnimationGraphicalPlugin : public ves::xplorer::plugin::PluginBase
{
public:
    VEAnimationGraphicalPlugin();
    virtual ~VEAnimationGraphicalPlugin();

    virtual void InitializeNode( osg::Group* veworldDCS );
    virtual void PreFrameUpdate(); 
       
private:
    ves::xplorer::device::KeyboardMouse* m_keyboard;
/*
    osg::ref_ptr< osg::Node > _fermentorGeometry;
    osg::ref_ptr< osg::Node > _impellerGeometry;
    osg::ref_ptr< osg::Node > _tankGeometry;

    osg::ref_ptr< osg::MatrixTransform > _roomGeometry;
    osg::ref_ptr< osg::MatrixTransform > fermentorGroup;

    osg::ref_ptr< osg::MatrixTransform > transform_ferm;
    osg::ref_ptr< osg::MatrixTransform > transform_imp;
    osg::ref_ptr< osg::MatrixTransform > transform_tank;
*/
    osg::ref_ptr< osg::Node > m_idleGeometry;
    osg::ref_ptr< osg::Node > m_openGeometry;
    osg::ref_ptr< osg::Node > m_closeGeometry;
    osg::ref_ptr< osg::Switch > m_valueAnimation;
    int mButton;


};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( VEAnimationGraphicalPlugin )

#endif //VE_ANIMATION_GRAPHICAL_PLUGIN_H
