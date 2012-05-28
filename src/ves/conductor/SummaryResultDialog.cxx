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
#include <ves/conductor/SummaryResultDialog.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/OneDStringArray.h>

#include <boost/lexical_cast.hpp>

#include <iostream>

#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/sizer.h>
#include <wx/button.h>
#include <wx/notebook.h>
using namespace ves::conductor;



//BEGIN_EVENT_TABLE(SummaryResultDialog, wxDialog)
//EVT_BUTTON(wxID_OK, SummaryResultDialog::OnOK)
//END_EVENT_TABLE()
////////////////////////////////////////////////////////////////////////////////
SummaryResultDialog::SummaryResultDialog( wxWindow* parent,
        const wxString& title,
        wxSize tabsize,
        const std::vector <
        ves::open::xml::CommandPtr > command )
    : UIDialog( ( wxWindow* )parent, -1, title )
{
    wxSize syn;
    wxBoxSizer* toptop = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* left_margin = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* top_sizer = new wxBoxSizer( wxVERTICAL );
    wxBoxSizer* right_margin = new wxBoxSizer( wxHORIZONTAL );

    //sz2.Set(250, 300);
    left_margin->Add( 10, 10 );
    right_margin->Add( 10, 10 );
    toptop->Add( left_margin, 0, wxALIGN_LEFT );
    toptop->Add( top_sizer, 1,  wxALIGN_CENTER_HORIZONTAL | wxEXPAND );
    toptop->Add( right_margin, 0, wxALIGN_RIGHT );

    tabs = new wxNotebook( this, -1, wxDefaultPosition, tabsize, wxNB_TOP );
    first_tab = FALSE;
    tsize = tabsize;
    NewTab();
    first_tab = TRUE;

    //SummaryResultDialog::SetAffirmativeId(wxID_OK);
    ok = new wxButton( this, wxID_OK, _( "OK" ) );

    top_sizer->Add( 10, 10, 0 );
    //top_sizer->Add(new wxStaticText(this, -1, "Summary Data   ", wxDefaultPosition, wxDefaultSize), 0, wxALIGN_CENTER_HORIZONTAL);
    //top_sizer->Add(10, 5, 0);
    //  top_sizer->Add(syngas, 0, wxALIGN_CENTER_HORIZONTAL);
    top_sizer->Add( tabs, 1, wxALIGN_CENTER_HORIZONTAL | wxEXPAND );
    top_sizer->Add( 10, 5, 0 );
    top_sizer->Add( ok, 0, wxALIGN_CENTER_HORIZONTAL );
    top_sizer->Add( 10, 10, 0 ); //the bottom margin

    SetSizer( toptop );
    SetAutoLayout( TRUE );
    toptop->Fit( this );

    size_t numInputs = command.size();
    for( size_t i = 0; i < numInputs; ++i )
    {
        mTagNames.clear();
        mValues.clear();
        ves::open::xml::CommandPtr inputCommand = command.at( i );
        GetDataTables( inputCommand );
        std::string inputParamter = inputCommand->GetCommandName();
        NewTab( wxString( inputParamter.c_str(), wxConvUTF8 ) );
        Set2Cols( mTagNames, mValues );
    }
}
////////////////////////////////////////////////////////////////////////////////
SummaryResultDialog::~SummaryResultDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SummaryResultDialog::Set2Cols( const std::vector<wxString>& col1,
                                    const std::vector<wxString>& col2 )
{
    if( syngas == NULL )
    {
        return;
    }

    std::vector< wxString > row;
    row.resize( 2 );
    //  syngas->Clear();
    for( unsigned int i = 0; i < col1.size(); i++ )
    {
        row[0] = col1[i];
        row[1] = col2[i];

        syngas->AddRow( row );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SummaryResultDialog::TabTitle( const wxString& title )
{
    size_t i = tabs->GetPageCount();

    if( i < 1 )
    {
        return;
    }

    tabs->SetPageText( i - 1, title );
}
////////////////////////////////////////////////////////////////////////////////
void SummaryResultDialog::NewTab( const wxString& title )
{
    if( first_tab )
    {
        first_tab = FALSE;
        TabTitle( title );
    }
    else
    {
        wxPanel* panel = new wxPanel( tabs );
        wxBoxSizer* sizerPanel = new wxBoxSizer( wxVERTICAL );
        syngas = new TexTable( panel, -1, wxDefaultPosition, tsize );
        sizerPanel->Add( syngas, 1, wxEXPAND );
        panel->SetSizer( sizerPanel );
        tabs->AddPage( panel, title );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SummaryResultDialog::GetDataTables( ves::open::xml::CommandPtr inputCommand )
{
    size_t numDVP = inputCommand->GetNumberOfDataValuePairs();
    for( size_t i = 0; i < numDVP ; ++i )
    {
        ves::open::xml::DataValuePairPtr tempDVP =
            inputCommand->GetDataValuePair( i );
        std::string dataType = tempDVP->GetDataType();
        mDataName = tempDVP->GetDataName();
        mStringData = "empty";

        if( dataType == std::string( "XMLOBJECT" ) )
        {
            if( inputCommand->GetDataValuePair( i )->
                    GetDataXMLObject()->GetObjectType() == "Command" )
            {
                ves::open::xml::CommandPtr tempCommand =
                    boost::dynamic_pointer_cast< ves::open::xml::Command >(
                        inputCommand->GetDataValuePair( i )->GetDataXMLObject() );
                size_t numDVPs = tempCommand->GetNumberOfDataValuePairs();
                for( size_t j = 0; j < numDVPs; ++j )
                {
                    tempDVP = tempCommand->GetDataValuePair( j );
                    mDataName = tempDVP->GetDataName();
                    GetDataValue( tempDVP );
                }
            }
            else if( inputCommand->GetDataValuePair( i )->
                     GetDataXMLObject()->GetObjectType() == "OneDStringArray" )
            {
                std::vector< std::string > tempStringArray =
                    boost::dynamic_pointer_cast <
                    ves::open::xml::OneDStringArray > ( inputCommand->
                                                        GetDataValuePair( 0 )->GetDataXMLObject() )->GetArray();

                for( size_t j = 0; j < tempStringArray.size(); ++j )
                {
                    mValues.push_back( wxString(
                                           tempStringArray.at( j ).c_str(), wxConvUTF8 ) );
                    mTagNames.push_back( wxString( mDataName.c_str(), wxConvUTF8 ) );
                }
            }
        }
        else
        {
            GetDataValue( tempDVP );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void SummaryResultDialog::GetDataValue( ves::open::xml::DataValuePairPtr tempDVP )
{
    std::string dataType = tempDVP->GetDataType();

    if( dataType == std::string( "FLOAT" ) )
    {
        double doubleData;
        tempDVP->GetData( doubleData );
        mStringData =  boost::lexical_cast<std::string>( doubleData );
    }
    else if( dataType == std::string( "UNSIGNED INT" ) )
    {
        unsigned int intData;
        tempDVP->GetData( intData );
        mStringData = boost::lexical_cast<std::string>( intData );
    }
    else if( dataType == std::string( "LONG" ) )
    {
        long longData;
        tempDVP->GetData( longData );
        mStringData = boost::lexical_cast<std::string>( longData ) ;
    }
    else if( dataType == std::string( "STRING" ) )
    {
        tempDVP->GetData( mStringData );
    }
    else if( dataType == std::string( "XMLOBJECT" ) )
    {
        std::vector< std::string > tempStringArray =
            boost::dynamic_pointer_cast <
            ves::open::xml::OneDStringArray > (
                tempDVP->GetDataXMLObject() )->GetArray();

        for( size_t i = 0; i < tempStringArray.size(); ++i )
        {
            mValues.push_back( wxString(
                                   tempStringArray.at( i ).c_str(), wxConvUTF8 ) );
            mTagNames.push_back( wxString( mDataName.c_str(), wxConvUTF8 ) );
        }
    }

    if( dataType != std::string( "XMLOBJECT" ) )
    {
        mTagNames.push_back( wxString( mDataName.c_str(), wxConvUTF8 ) );
        mValues.push_back( wxString( mStringData.c_str(), wxConvUTF8 ) );
    }
}