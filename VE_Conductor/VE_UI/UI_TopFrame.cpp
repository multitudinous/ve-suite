/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/VE_UI/UI_TopFrame.h"
#include "VE_Conductor/VE_UI/UI_Frame.h"

////////////////////////////////////////////////////
UI_TopFrame::UI_TopFrame(const wxString& title,
             const wxPoint& pos,
             const wxSize& size,
             long style)
: wxFrame((wxWindow *) NULL, -1, title, pos, size, style)
{
     _uiFrame = new UI_Frame(this,wxID_HIGHEST);

     wxBoxSizer* _topframeSizer = new wxBoxSizer(wxHORIZONTAL);
     _topframeSizer->Add(_uiFrame,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
     _topframeSizer->Layout();
     SetSizer(_topframeSizer);
 
     SetAutoLayout(true); 
     _topframeSizer->Fit(this);  
}

UI_TopFrame::~UI_TopFrame()
{
}
