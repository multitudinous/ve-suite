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
#pragma once

#include "UIPluginInterface.h"

/*!\file UIPluginFactory.h
 UIPluginFactory
 */
/*!\class UIPluginFactory
 * This class is part of VE-Suite's UIPlugin architecture. Plugins are divided
 * into two pieces: A plugin factory (this the "Qt Plugin") and a plugin object.
 * The factory (this class) provides meta-information about the plugin object
 * and allows multiple instances of the plugin object to be created. (Qt's plugin
 * architecture does not support this out-of-the box: Qt plugins are singletons,
 * so a factory approach is required to allow multiple instances). The second
 * major part of the plugin architecture is the plugin object, that is, the
 * underlying class that the the factory creates instances of. The plugin object
 * in turn has ports and interface widgets which are unique to each instance of
 * the plugin object.
 *
 * To implement a plugin, you must have two separate classes: one that inherits
 * from QObject and UIPluginFactory, and one that inherits from UIPluginInterface.
 */
namespace ves
{
namespace conductor
{


class UIPluginFactory
{
public:
    ///Default destructor for plugins
    virtual ~UIPluginFactory(){};

    /// Creates a new instance of the underlying Plugin class
    virtual ves::conductor::UIPluginInterface* CreateInstance() = 0;

    // The following methods returns meta information that is common
    // to all instances of the plugin. This allows this information to be
    // queried without having to instantiate the underlying plugin class.

    ///Returns the name of the factory -- each instance of the plugin may have
    ///a different name, but the factory will only have one name.
    virtual std::string GetFactoryName() = 0;

    ///Returns the version number of the module
    virtual double GetVersion() = 0;

    ///Returns the description of the module, This should be a short description
    virtual std::string GetDescription() = 0;

    ///Returns the URL of the online help
    virtual std::string GetHelpURL() = 0;

    ///Gets the image to use as an icon for this plugin
    virtual QIcon GetIcon( ) = 0;

    virtual osg::Geode* GetGeometry() = 0;
};

}
}

Q_DECLARE_INTERFACE( ves::conductor::UIPluginFactory, "vesuite.Plugin.UIPluginFactory/1.0" );
