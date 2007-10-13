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
#ifndef TEXTRESULTDIALOG_H
#define TEXTRESULTDIALOG_H
/*!\file TextResultDialog.h
TextResultDialog API
*/
/*!\class TextResultDialog
* 
*/

#include <ves/conductor/UIDialog.h>

#include <ves/VEConfig.h>
#include <vector>

class wxButton;
//class wxString;

namespace ves
{
namespace conductor
{
class TexTable;
class VE_GUIPLUGINS_EXPORTS TextResultDialog : public UIDialog
{
 public:
  TextResultDialog(wxWindow*parent, const wxString& title=wxT("Result Dialog"), wxSize tabsize= wxSize(477, 300));
  virtual ~TextResultDialog();

  void Set2Cols(const std::vector<wxString>& col1, const std::vector<wxString>& col2);
  TexTable *syngas;
  wxButton *ok;

  //DECLARE_EVENT_TABLE()
};
}
}
#endif


