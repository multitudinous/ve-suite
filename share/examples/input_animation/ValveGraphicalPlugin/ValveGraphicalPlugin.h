/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef VALVE_GRAPHICAL_PLUGIN_H
#define VALVE_GRAPHICAL_PLUGIN_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>

#include <ves/open/xml/CommandPtr.h>

#include <osg/Switch>

namespace ves
{
namespace xplorer
{

namespace device
{
class KeyboardMouse;
}
} //end xplorer
} //end ves


namespace opcgp
{
class VE_USER_PLUGIN_EXPORTS ValveGraphicalPlugin : public ves::xplorer::plugin::PluginBase
{
public:
    ///Constructor
    ValveGraphicalPlugin();
    ///Destructor
    virtual ~ValveGraphicalPlugin();

    ///Initialize the scenegraph with graphics models
    virtual void InitializeNode( osg::Group* veworldDCS );
    ///Check if anything needs updated graphically
    virtual void PreFrameUpdate();
    ///Process the current command in the queue from the dynsim unit
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );
    virtual void SetCurrentCommands( std::vector< ves::open::xml::CommandPtr > const& commands );
private:
    void FindPartNodeAndHighlightNode();
    ///Keyboard helper pointer
    ves::xplorer::device::KeyboardMouse* m_keyboard;

    ///panel geometry
    osg::ref_ptr< osg::Node > m_startButtonGeometry;
    osg::ref_ptr< osg::Node > m_stopButtonGeometry;
    osg::ref_ptr< osg::Node > m_panelGeometry;

    ///valve geometry
    osg::ref_ptr< osg::Node > m_handwheelGeometry;
    osg::ref_ptr< osg::Node > m_stemGeometry;
    osg::ref_ptr< osg::Node > m_valveGeometry;

    ///Pump geometry
    osg::ref_ptr< osg::Node > m_pumpGeometry;
    osg::ref_ptr< osg::Uniform > m_highlightColor;

    ///other vars
    osg::ref_ptr< osg::Switch > m_valueAnimation;
    int mButton;

	osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_rotationDCS;
	//osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_startTransDCS;
	//osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_stopTransDCS;
	osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_stemTransDCS;
	osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_valveDCS;
	//osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_switchDCS;
    double m_valveHeight;
    double m_switchOnOff;
    bool m_valveOnOff;
};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( ValveGraphicalPlugin )
    
}
#endif //VE_ANIMATION_GRAPHICAL_PLUGIN_H
