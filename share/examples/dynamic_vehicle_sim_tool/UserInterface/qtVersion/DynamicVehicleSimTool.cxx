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
//#include <ves/conductor/qt/DynamicVehicleSimTool/DynamicVehicleSimTool.h>
//#include <ves/conductor/qt/DynamicVehicleSimTool/DynamicVehicleSimTool_UIDialog.h>
#include "DynamicVehicleSimTool.h"
#include "DynamicVehicleSimTool_UIDialog.h"

#include <osg/Geode>

namespace ves
{
namespace conductor
{


DynamicVehicleSimTool::DynamicVehicleSimTool():
    m_UIWidget(0),
    m_name("DynamicVehicleSimToolUI")
{
    m_UIWidget = new DynamicVehicleSimTool_UIDialog( 0, this );
    m_pluginName = "DynamicVehicleSimToolUI";
}

void DynamicVehicleSimTool::SetName( const std::string& pluginName )
{
    m_name = pluginName;
}


std::string DynamicVehicleSimTool::GetName()
{
    return m_name;
}


QWidget* DynamicVehicleSimTool::GetUIWidget( QWidget* )
{
    return m_UIWidget;
}

void DynamicVehicleSimTool::DeleteWidgets()
{
    delete m_UIWidget;
    m_UIWidget = 0;
}

}
}
