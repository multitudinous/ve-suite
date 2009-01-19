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
// ListTable.cpp: implementation of the ListTable class.
//
//////////////////////////////////////////////////////////////////////

#include <ves/conductor/ListTable.h>

#include <wx/window.h>
using namespace ves::conductor;


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

ListTable::ListTable( wxWindow *parent,
                      const wxWindowID id,
                      const wxPoint& pos,
                      const wxSize& size )
        : wxListCtrl( parent, id, pos, size, wxLC_REPORT | wxSUNKEN_BORDER ),
        m_attr( *wxBLUE, *wxLIGHT_GREY, wxNullFont )
{}

ListTable::~ListTable()
{}

void ListTable::SetTitle( std::vector<wxString> titles )
{
    int i;
    int len;

    ClearAll();
    len = titles.size();
    for( i = 0; i < len; i++ )
        InsertColumn( i, titles[i] ); // , wxLIST_FORMAT_LEFT, 140);
    num_of_row = 0;

}

void ListTable::AddRow( std::vector<wxString> vals )
{
    if( vals.size() > 0 )
        InsertItem( num_of_row, vals[0] );//, 0);

    for( unsigned int i = 1; i < vals.size(); i++ )
        SetItem( num_of_row, i, vals[i] );

    num_of_row++;
}

void ListTable::DelRow( int i )
{
    DeleteItem( i );
}
