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

#include "HyperLabICEPlugin.h"
#include "HyperLabICEPlugin_UIDialog.h"

namespace ves
{
namespace conductor
{


HyperLabICEPlugin::HyperLabICEPlugin():
    m_UIWidget(0),
    m_ResultWidget(0),
    m_FinancialDataWidget(0),
    m_InputVariablesWidget(0),
    m_ResultsVariablesWidget(0),
    m_name("HyperLabICEPlugin")
{
    m_UIWidget = new HyperLabICEPlugin_UIDialog( 0 );
}

void HyperLabICEPlugin::SetName( const std::string& pluginName )
{
    m_name = pluginName;
}


std::string HyperLabICEPlugin::GetName()
{
    return m_name;
}


QWidget* HyperLabICEPlugin::GetUIWidget( QWidget* )
{
    return m_UIWidget;
}


QWidget* HyperLabICEPlugin::GetResultWidget( QWidget* )
{
    return m_ResultWidget;
}


QWidget* HyperLabICEPlugin::GetFinancialDataWidget()
{
    return m_FinancialDataWidget;
}


QWidget* HyperLabICEPlugin::ViewInputVariables( QWidget* )
{
    return m_InputVariablesWidget;
}

QWidget* HyperLabICEPlugin::ViewResultsVariables( QWidget* )
{
    return m_ResultsVariablesWidget;
}

void HyperLabICEPlugin::DeleteWidgets()
{
    delete m_UIWidget;
    m_UIWidget = m_ResultWidget = m_FinancialDataWidget =
                 m_InputVariablesWidget = m_ResultsVariablesWidget = 0;
}

}
}
