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
 * Id:            $Id: Multiscale2.cxx 16404 2011-10-07 21:20:34Z tjordan $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
//#include <ves/conductor/qt/Multiscale2/Multiscale2.h>
//#include <ves/conductor/qt/Multiscale2/Multiscale2_UIDialog.h>
#include "Multiscale2.h"
#include "Multiscale2_UIDialog.h"

#include <osg/Geode>

namespace ves
{
namespace conductor
{


Multiscale2::Multiscale2():
    m_UIWidget(0),
    m_ResultWidget(0),
    m_FinancialDataWidget(0),
    m_InputVariablesWidget(0),
    m_ResultsVariablesWidget(0),
    m_name("Multiscale2 Plugin")
{
    m_UIWidget = new Multiscale2_UIDialog( 0 );
}

void Multiscale2::SetName( const std::string& pluginName )
{
    m_name = pluginName;
}


std::string Multiscale2::GetName()
{
    return m_name;
}


QWidget* Multiscale2::GetUIWidget( QWidget* )
{
    return m_UIWidget;
}


QWidget* Multiscale2::GetResultWidget( QWidget* )
{
    return m_ResultWidget;
}


QWidget* Multiscale2::GetFinancialDataWidget()
{
    return m_FinancialDataWidget;
}


QWidget* Multiscale2::ViewInputVariables( QWidget* )
{
    return m_InputVariablesWidget;
}

QWidget* Multiscale2::ViewResultsVariables( QWidget* )
{
    return m_ResultsVariablesWidget;
}

void Multiscale2::DeleteWidgets()
{
    delete m_UIWidget;
    delete m_ResultWidget;
    delete m_FinancialDataWidget;
    delete m_InputVariablesWidget;
    delete m_ResultsVariablesWidget;
    m_UIWidget = m_ResultWidget = m_FinancialDataWidget =
                 m_InputVariablesWidget = m_ResultsVariablesWidget = 0;
}

}
}
