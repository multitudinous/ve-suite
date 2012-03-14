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
#include "HyperLabICEPluginFactory.h"
#include "HyperLabICEPlugin.h"

namespace ves
{
namespace conductor
{

UIPluginInterface* HyperLabICEPluginFactory::CreateInstance()
{
    return new HyperLabICEPlugin;
}

std::string HyperLabICEPluginFactory::GetFactoryClassName()
{
    return std::string( "HyperLabICEPlugin" );
}

std::string HyperLabICEPluginFactory::GetFactoryDisplayName()
{
    return std::string( "Hyper Lab ICE Plugin" );
}

double HyperLabICEPluginFactory::GetVersion()
{
    return 1.0;
}


std::string HyperLabICEPluginFactory::GetDescription()
{
    return std::string( "A generic tool to spary paint with a simple paint model." );
}


std::string HyperLabICEPluginFactory::GetHelpURL()
{
    return std::string( "" );
}


QIcon HyperLabICEPluginFactory::GetIcon( )
{
    QIcon result(":/Icons/TractorIcon.png");
    return result;
}

osg::Geode* HyperLabICEPluginFactory::GetGeometry()
{
    // We don't have custom geometry yet, so just return a null ptr.
    return 0;
}

}
}

Q_EXPORT_PLUGIN2( HyperLabICEPluginFactory, ves::conductor::HyperLabICEPluginFactory );
