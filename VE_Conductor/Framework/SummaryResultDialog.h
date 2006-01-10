/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: SummaryResultDialog.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef SUMMARYRESULTDIALOG_H
#define SUMMARYRESULTDIALOG_H
#include "VE_Conductor/Framework/UIDialog.h"
#include "VE_Conductor/Framework/TexTable.h"
#include "VE_Installer/include/VEConfig.h"

class wxButton;
class wxNotebook;
class wxBoxSizer;
class wxStaticText;

#include <vector>

class VE_GUIPLUGINS_EXPORTS SummaryResultDialog : public UIDialog
{
public:
   SummaryResultDialog(wxWindow*parent, const wxString& title=wxT("Result Dialog"), wxSize tabsize= wxSize(477, 300));
   ~SummaryResultDialog();

   void TabTitle( const wxString& title );
   void NewTab( const wxString& title = wxT("Results") );
   void Set2Cols( const std::vector<wxString>& col1, const std::vector<wxString>& col2);
   TexTable* syngas;

private:
   wxNotebook* tabs;
   wxButton* ok;
   wxSize tsize;
   int first_tab;

   DECLARE_EVENT_TABLE()
};
#endif
