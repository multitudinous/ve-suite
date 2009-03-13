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
#ifndef SUMMARYRESULTDIALOG_H
#define SUMMARYRESULTDIALOG_H
/*!\file SummaryResultDialog.h
SummaryResultDialog API
*/
/*!\class SummaryResultDialog
*
*/
#include <ves/conductor/UIDialog.h>
#include <ves/conductor/TexTable.h>
#include <ves/VEConfig.h>

#include <ves/open/xml/CommandPtr.h>

class wxButton;
class wxNotebook;
class wxBoxSizer;
class wxStaticText;

#include <vector>

namespace ves
{
namespace conductor
{
class VE_GUIPLUGINS_EXPORTS SummaryResultDialog : public UIDialog
{
public:
    SummaryResultDialog( wxWindow* parent,
        const wxString& title,
        wxSize tabsize,
        const std::vector< ves::open::xml::CommandPtr > command);

    virtual ~SummaryResultDialog();
 
    void TabTitle( const wxString& title );
    void NewTab( const wxString& title = wxT( "Results" ) );
    void Set2Cols( const std::vector<wxString>& col1, const std::vector<wxString>& col2 );
    void GetDataTables( ves::open::xml::CommandPtr inputCommand, 
        std::vector< wxString >& tagNames, std::vector< wxString >& values );
    TexTable* syngas;

    ves::open::xml::CommandPtr mCommand;


private:
    wxNotebook* tabs;
    wxButton* ok;
    wxSize tsize;
    int first_tab;


    //DECLARE_EVENT_TABLE()
};
}
}
#endif
