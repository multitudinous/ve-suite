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
#pragma once

//QT_NO_KEYWORDS

#include <QtCore/QObject>
#include "UIPluginInterface.h"
#include <QtCore/qplugin.h>
#include <ves/conductor/qt/plugin/UIPluginBase.h>

namespace ves
{
namespace conductor
{


class PaintingPlugin : public ves::conductor::UIPluginBase,
                      public ves::conductor::UIPluginInterface
{
    Q_OBJECT

public:
    PaintingPlugin();

    //virtual ~PaintingPlugin();

    ///Sets the name of the module
    virtual void SetName( const std::string& pluginName );

    ///Returns the name of the module
    virtual std::string GetName();

    ///Returns the UI dialog of the module
    virtual QWidget* GetUIWidget( QWidget* parent = 0 );

    ///Returns the Result dialog of the module
    virtual QWidget* GetResultWidget( QWidget* parent = 0 );

    ///Returns the PortData dialog of the module
    //virtual QWidget* PortData(wxWindow* parent,  Interface *intf);

    ///Returns the FinancialData dialog of the module
    virtual QWidget* GetFinancialDataWidget();

    /// Launch custom input dialogs
    virtual QWidget* ViewInputVariables( QWidget* parent = 0 );
    virtual QWidget* ViewResultsVariables( QWidget* parent = 0 );

    virtual void DeleteWidgets();

    // EPRI TAG
    //FinancialDialog* financial_dlg;

private:
    QWidget* m_UIWidget;
    QWidget* m_ResultWidget;
    QWidget* m_FinancialDataWidget;
    QWidget* m_InputVariablesWidget;
    QWidget* m_ResultsVariablesWidget;

    std::string m_name;
};

}
}
