/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef PORTDIALOG_H
#define PORTDIALOG_H
/*!\file PortDialog.h
PortDialog API
*/
/*!\class PortDialog
* 
*/

#include <wx/dialog.h>

class wxTextCtrl;
class wxButton;
class wxBoxSizer;
class wxStaticText;

#include <vector>
#include "VE_Conductor/GUIPlugin/ListTable.h"

#include "VE_Installer/include/VEConfig.h"

class VE_GUIPLUGINS_EXPORTS PortDialog : public wxDialog
{
 public:
  PortDialog(const wxString& title);
  virtual ~PortDialog();

  void Set3Cols(const std::vector<wxString>& col1, const std::vector<wxString>& col2, const std::vector<wxString>& col3);
  ListTable *syngas;
  wxTextCtrl *temp;
  wxTextCtrl *pres;
  wxTextCtrl *flrt;
  wxButton *ok;
  void SetVal(const wxString &var, const wxString &val);
  //DECLARE_EVENT_TABLE()
};

#endif


