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
// ListTable.h: interface for the ListTable class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LISTTABLE_H__745B1AA2_D560_41C1_AC10_748693B1A8EF__INCLUDED_)
#define AFX_LISTTABLE_H__745B1AA2_D560_41C1_AC10_748693B1A8EF__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <wx/wx.h>
#include <wx/listctrl.h>
#include <vector>

class ListTable : public wxListCtrl  
{
public:
	ListTable(wxWindow *parent=NULL,
               const wxWindowID id=-1,
               const wxPoint& pos=wxDefaultPosition,
               const wxSize& size=wxDefaultSize);
	virtual ~ListTable();

	void SetTitle(std::vector<wxString> titles);
	void AddRow(std::vector<wxString> vals);
	void DelRow(int i);

protected:
	wxListItemAttr m_attr;
	int num_of_row;

};

#endif // !defined(AFX_LISTTABLE_H__745B1AA2_D560_41C1_AC10_748693B1A8EF__INCLUDED_)
