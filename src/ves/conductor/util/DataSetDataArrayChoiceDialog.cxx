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
#include <ves/conductor/util/DataSetDataArrayChoiceDialog.h>

#include <iostream>

////////////////////////////////////////////////////////////////////////////////
DataSetDataArrayChoiceDialog::DataSetDataArrayChoiceDialog( wxWindow* parent )
    :
    DataArrayChoiceDialog( parent )
{
    CentreOnParent();
}
////////////////////////////////////////////////////////////////////////////////
void DataSetDataArrayChoiceDialog::SetDataArrays( std::vector< std::string > activeArrays )
{
    m_activeArrays = activeArrays;
    //Add new strings to dialog
    wxArrayString stingList;
    for( size_t i = 0; i < m_activeArrays.size(); ++i )
    {
        stingList.Add( wxString( m_activeArrays.at( i ).c_str(), wxConvUTF8 ) );
    }
    m_checkList1->InsertItems( stingList, 0 );
    Refresh( true );
}
////////////////////////////////////////////////////////////////////////////////
std::vector< std::string > DataSetDataArrayChoiceDialog::GetUserActiveArrays()
{
    unsigned int numSelections = m_checkList1->GetCount();
    for( size_t i = 0; i < numSelections; ++i )
    {
        if( m_checkList1->IsChecked( i ) )
        {
            m_usersDataArrays.push_back(
                static_cast< const char* >(
                    wxConvCurrent->cWX2MB( m_checkList1->GetString( i ).c_str() ) ) );
            //std::cout << m_usersDataArrays.back() << std::endl;
        }
    }
    return m_usersDataArrays;
}
////////////////////////////////////////////////////////////////////////////////
