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

#include <QtCore/QObject>
#include "UIPluginFactory.h"

namespace ves
{
namespace conductor
{


class WarrantyToolPluginFactory: public QObject, public UIPluginFactory
{
    Q_OBJECT
    Q_INTERFACES( ves::conductor::UIPluginFactory )
public:
    //virtual ~DefaultPluginFactory();

    virtual UIPluginInterface* CreateInstance();

    virtual std::string GetFactoryName();

    ///Returns the version number of the module
    virtual double GetVersion();

    ///Returns the description of the module, This should be a short description
    virtual std::string GetDescription();

    ///Returns the URL of the online help
    virtual std::string GetHelpURL();

    ///Gets the image to use as an icon for this plugin
    virtual QIcon GetIcon( );

    virtual osg::Geode* GetGeometry();
};

}
}
