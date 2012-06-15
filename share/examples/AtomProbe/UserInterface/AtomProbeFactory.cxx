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
 * Date modified: $Date: 2011-10-07 16:20:34 -0500 (Fri, 07 Oct 2011) $
 * Version:       $Rev: 16404 $
 * Author:        $Author: tjordan $
 * Id:            $Id: AtomProbeFactory.cxx 16404 2011-10-07 21:20:34Z tjordan $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include "AtomProbeFactory.h"
#include "AtomProbe.h"

namespace ves
{
namespace conductor
{

UIPluginInterface* AtomProbeFactory::CreateInstance()
{
    return new AtomProbe;
}

std::string AtomProbeFactory::GetFactoryClassName()
{
    return std::string( "AtomProbe" );
}

std::string AtomProbeFactory::GetFactoryDisplayName()
{
    return std::string( "AtomProbe Plugin" );
}

double AtomProbeFactory::GetVersion()
{
    return 1.0;
}


std::string AtomProbeFactory::GetDescription()
{
    return std::string( "Multiscale 2 plugin." );
}


std::string AtomProbeFactory::GetHelpURL()
{
    return std::string( "http://www.google.com/search?hl=en&source=hp&q=ve+suite" );
}


QIcon AtomProbeFactory::GetIcon( )
{
    QIcon result(":/Tank.png");
    return result;
}

osg::Geode* AtomProbeFactory::GetGeometry()
{
    // We don't have custom geometry yet, so just return a null ptr.
    return 0;
}

}
}

Q_EXPORT_PLUGIN2( AtomProbeFactory, ves::conductor::AtomProbeFactory );