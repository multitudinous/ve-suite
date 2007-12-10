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
#include <ves/conductor/FinancialDialog.h>

#include <wx/button.h>
#include <wx/radiobut.h>
#include <wx/textctrl.h>
#include <wx/listbox.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>

using namespace ves::conductor;

BEGIN_EVENT_TABLE( FinancialDialog, wxDialog )
    EVT_RADIOBUTTON( RADIO_FDA, FinancialDialog::OnChange )
    EVT_RADIOBUTTON( RADIO_FDB, FinancialDialog::OnChange )
END_EVENT_TABLE()

FinancialDialog::FinancialDialog( wxWindow *parent, wxWindowID id )
        : wxDialog(( wxWindow * )parent, id, wxT( "Financial" ), wxDefaultPosition, wxDefaultSize )
{
    wxSize entry_size( 100, 17 );
    wxSize tag_size( 250, 15 );
    int i;

    _use_data = 0;

    _cc00_d = 0.0; // PFC
    _cc01_d = 15.0; // General facility cost
    _cc02_d = 10.0; // eng., home off fees
    _cc03_d = 15.0; // project contingency
    _cc04_d = 5.0; // process contingency
    _cc05_d = 0.5; // royalties
    _cc06_d = 20.0; // interest charges (AFUDC)
    _cc07_d = 4.0; // preproduction cost
    _cc08_d = 0.6; // inventory

    _om00_d = 0.0; // O & M Variable Costs
    _om01_d = 3.5; // Total Maint. Costs
    _om02_d = 1.65; // Operating Labor
    _om03_d = 30.0; // Adminstrative Labor

    wxBoxSizer* toptop = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* left_margin = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* top_sizer = new wxBoxSizer( wxVERTICAL );
    wxBoxSizer* right_margin = new wxBoxSizer( wxHORIZONTAL );
    left_margin->Add( 5, 10 );
    right_margin->Add( 5, 10 );
    toptop->Add( left_margin, 0, wxALIGN_LEFT );
    toptop->Add( top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL );
    toptop->Add( right_margin, 0, wxALIGN_RIGHT );

    wxStaticBox *cc_lbl = new wxStaticBox( this, -1, _( "Capital Costs" ) );
    wxStaticBoxSizer *cc_sizer = new wxStaticBoxSizer( cc_lbl, wxVERTICAL );

    wxStaticBox *om_lbl = new wxStaticBox( this, -1, _( "O and M Costs" ) );
    wxStaticBoxSizer *om_sizer = new wxStaticBoxSizer( om_lbl, wxVERTICAL );

    wxBoxSizer *ok_row = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer *tech_row = new wxBoxSizer( wxHORIZONTAL );

    top_sizer->Add( 5, 10 );
    top_sizer->Add( tech_row, 0, wxALIGN_CENTER_HORIZONTAL );
    top_sizer->Add( 5, 10 );
    top_sizer->Add( cc_sizer, 0, wxALIGN_CENTER_HORIZONTAL );
    top_sizer->Add( 5, 10 );
    top_sizer->Add( om_sizer, 0, wxALIGN_CENTER_HORIZONTAL );
    top_sizer->Add( 5, 10 );
    top_sizer->Add( ok_row, 0, wxALIGN_CENTER_HORIZONTAL );
    top_sizer->Add( 5, 10 );

    // Capital Costs

    wxBoxSizer *cc_var_sizer[9];
    wxStaticText *cc_var_lbl[9];
    wxStaticText *cc_var_unt[9];

    cc_sizer->Add( 5, 10 );
    for( i = 0; i < 9; i++ )
    {
        cc_var_sizer[i] = new wxBoxSizer( wxHORIZONTAL );

        cc_sizer->Add( cc_var_sizer[i], 0, wxALIGN_CENTER_HORIZONTAL );
    }
    cc_sizer->Add( 5, 10 );

    // O & M Costs

    wxBoxSizer *om_var_sizer[4];
    wxStaticText *om_var_lbl[4];
    wxStaticText *om_var_unt[4];

    om_sizer->Add( 5, 10 );
    for( i = 0; i < 4; i++ )
    {
        om_var_sizer[i] = new wxBoxSizer( wxHORIZONTAL );

        om_sizer->Add( om_var_sizer[i], 0, wxALIGN_CENTER_HORIZONTAL );
    }
    om_sizer->Add( 5, 10 );

    // Use Data

    specify_a = new wxRadioButton( this, RADIO_FDA, _T( "Apply Data " ), wxDefaultPosition, wxDefaultSize, wxRB_GROUP );

    specify_a->SetValue( false );

    specify_b = new wxRadioButton( this, RADIO_FDB, _T( "Ignore Data " ) );

    specify_b->SetValue( true );

    tech_row->Add( specify_a, 0, wxALIGN_CENTER_HORIZONTAL );
    tech_row->Add( specify_b, 0, wxALIGN_CENTER_HORIZONTAL );

    // Capital Costs

    cc_var_lbl[0] = new wxStaticText( this, -1, _T( "Plant Facility Cost" ), wxDefaultPosition, tag_size );
    _cc00 = new wxTextCtrl( this, CC00, _T( "0.01" ), wxDefaultPosition, entry_size );
    cc_var_unt[0] = new wxStaticText( this, -1, _T( "(M$)" ), wxDefaultPosition, wxSize( 50, 17 ) );
    cc_var_sizer[0]->Add( cc_var_lbl[0], 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[0]->Add( _cc00, 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[0]->Add( cc_var_unt[0], 0, wxALIGN_CENTER_HORIZONTAL );

    cc_var_lbl[1] = new wxStaticText( this, -1, _T( "General Facility Cost" ), wxDefaultPosition, tag_size );
    _cc01 = new wxTextCtrl( this, CC01, _T( "0.01" ), wxDefaultPosition, entry_size );
    cc_var_unt[1] = new wxStaticText( this, -1, _T( "(%PFC)" ), wxDefaultPosition,  wxSize( 50, 17 ) );
    cc_var_sizer[1]->Add( cc_var_lbl[1], 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[1]->Add( _cc01, 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[1]->Add( cc_var_unt[1], 0, wxALIGN_CENTER_HORIZONTAL );

    cc_var_lbl[2] = new wxStaticText( this, -1, _T( "Eng. & Home Office Fees" ), wxDefaultPosition, tag_size );
    _cc02 = new wxTextCtrl( this, CC02, _T( "0.01" ), wxDefaultPosition, entry_size );
    cc_var_unt[2] = new wxStaticText( this, -1, _T( "(%PFC)" ), wxDefaultPosition,  wxSize( 50, 17 ) );
    cc_var_sizer[2]->Add( cc_var_lbl[2], 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[2]->Add( _cc02, 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[2]->Add( cc_var_unt[2], 0, wxALIGN_CENTER_HORIZONTAL );

    cc_var_lbl[3] = new wxStaticText( this, -1, _T( "Project Contingency Cost" ), wxDefaultPosition, tag_size );
    _cc03 = new wxTextCtrl( this, CC03, _T( "0.01" ), wxDefaultPosition, entry_size );
    cc_var_unt[3] = new wxStaticText( this, -1, _T( "(%PFC)" ), wxDefaultPosition, wxSize( 50, 17 ) );
    cc_var_sizer[3]->Add( cc_var_lbl[3], 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[3]->Add( _cc03, 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[3]->Add( cc_var_unt[3], 0, wxALIGN_CENTER_HORIZONTAL );

    cc_var_lbl[4] = new wxStaticText( this, -1, _T( "Process Contingency Cost" ), wxDefaultPosition, tag_size );
    _cc04 = new wxTextCtrl( this, CC04, _T( "0.01" ), wxDefaultPosition, entry_size );
    cc_var_unt[4] = new wxStaticText( this, -1, _T( "(%PFC)" ), wxDefaultPosition, wxSize( 50, 17 ) );
    cc_var_sizer[4]->Add( cc_var_lbl[4], 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[4]->Add( _cc04, 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[4]->Add( cc_var_unt[4], 0, wxALIGN_CENTER_HORIZONTAL );

    cc_var_lbl[5] = new wxStaticText( this, -1, _T( "PrePaid Royalties" ), wxDefaultPosition, tag_size );
    _cc05 = new wxTextCtrl( this, CC05, _T( "0.01" ), wxDefaultPosition, entry_size );
    cc_var_unt[5] = new wxStaticText( this, -1, _T( "(%PFC)" ), wxDefaultPosition, wxSize( 50, 17 ) );
    cc_var_sizer[5]->Add( cc_var_lbl[5], 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[5]->Add( _cc05, 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[5]->Add( cc_var_unt[5], 0, wxALIGN_CENTER_HORIZONTAL );

    cc_var_lbl[6] = new wxStaticText( this, -1, _T( "Interest Charges (AFUDC)" ), wxDefaultPosition, tag_size );
    _cc06 = new wxTextCtrl( this, CC06, _T( "0.01" ), wxDefaultPosition, entry_size );
    cc_var_unt[6] = new wxStaticText( this, -1, _T( "(M$)" ), wxDefaultPosition, wxSize( 50, 17 ) );
    cc_var_sizer[6]->Add( cc_var_lbl[6], 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[6]->Add( _cc06, 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[6]->Add( cc_var_unt[6], 0, wxALIGN_CENTER_HORIZONTAL );

    cc_var_lbl[7] = new wxStaticText( this, -1, _T( "PreProduction (Start) Cost" ), wxDefaultPosition, tag_size );
    _cc07 = new wxTextCtrl( this, CC07, _T( "0.01" ), wxDefaultPosition, entry_size );
    cc_var_unt[7] = new wxStaticText( this, -1, _T( "(M$)" ), wxDefaultPosition, wxSize( 50, 17 ) );
    cc_var_sizer[7]->Add( cc_var_lbl[7], 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[7]->Add( _cc07, 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[7]->Add( cc_var_unt[7], 0, wxALIGN_CENTER_HORIZONTAL );

    cc_var_lbl[8] = new wxStaticText( this, -1, _T( "Inventory (Working) Capital" ), wxDefaultPosition, tag_size );
    _cc08 = new wxTextCtrl( this, CC08, _T( "0.01" ), wxDefaultPosition, entry_size );
    cc_var_unt[8] = new wxStaticText( this, -1, _T( "(M$)" ), wxDefaultPosition, wxSize( 50, 17 ) );
    cc_var_sizer[8]->Add( cc_var_lbl[8], 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[8]->Add( _cc08, 0, wxALIGN_CENTER_HORIZONTAL );
    cc_var_sizer[8]->Add( cc_var_unt[8], 0, wxALIGN_CENTER_HORIZONTAL );

    // O & M Costs

    om_var_lbl[0] = new wxStaticText( this, -1, _T( "O and M Variable Costs" ), wxDefaultPosition, tag_size );
    _om00 = new wxTextCtrl( this, OM00, _T( "0.01" ), wxDefaultPosition, entry_size );
    om_var_unt[0] = new wxStaticText( this, -1, _T( "(M$)" ), wxDefaultPosition, wxSize( 50, 17 ) );
    om_var_sizer[0]->Add( om_var_lbl[0], 0, wxALIGN_CENTER_HORIZONTAL );
    om_var_sizer[0]->Add( _om00, 0, wxALIGN_CENTER_HORIZONTAL );
    om_var_sizer[0]->Add( om_var_unt[0], 0, wxALIGN_CENTER_HORIZONTAL );

    om_var_lbl[1] = new wxStaticText( this, -1, _T( "Total Maintenance Cost" ), wxDefaultPosition, tag_size );
    _om01 = new wxTextCtrl( this, OM01, _T( "0.01" ), wxDefaultPosition, entry_size );
    om_var_unt[1] = new wxStaticText( this, -1, _T( "(%TPC)" ), wxDefaultPosition, wxSize( 50, 17 ) );
    om_var_sizer[1]->Add( om_var_lbl[1], 0, wxALIGN_CENTER_HORIZONTAL );
    om_var_sizer[1]->Add( _om01, 0, wxALIGN_CENTER_HORIZONTAL );
    om_var_sizer[1]->Add( om_var_unt[1], 0, wxALIGN_CENTER_HORIZONTAL );

    om_var_lbl[2] = new wxStaticText( this, -1, _T( "Operating Labor" ), wxDefaultPosition, tag_size );
    _om02 = new wxTextCtrl( this, OM02, _T( "0.01" ), wxDefaultPosition, entry_size );
    om_var_unt[2] = new wxStaticText( this, -1, _T( "(M$)" ), wxDefaultPosition, wxSize( 50, 17 ) );
    om_var_sizer[2]->Add( om_var_lbl[2], 0, wxALIGN_CENTER_HORIZONTAL );
    om_var_sizer[2]->Add( _om02, 0, wxALIGN_CENTER_HORIZONTAL );
    om_var_sizer[2]->Add( om_var_unt[2], 0, wxALIGN_CENTER_HORIZONTAL );

    om_var_lbl[3] = new wxStaticText( this, -1, _T( "Adminstrative" ), wxDefaultPosition, tag_size );
    _om03 = new wxTextCtrl( this, OM03, _T( "0.01" ), wxDefaultPosition, entry_size );
    om_var_unt[3] = new wxStaticText( this, -1, _T( "(%Total Labor)" ), wxDefaultPosition, wxSize( 50, 17 ) );
    om_var_sizer[3]->Add( om_var_lbl[3], 0, wxALIGN_CENTER_HORIZONTAL );
    om_var_sizer[3]->Add( _om03, 0, wxALIGN_CENTER_HORIZONTAL );
    om_var_sizer[3]->Add( om_var_unt[3], 0, wxALIGN_CENTER_HORIZONTAL );

    // Calculate row
    ok_b = new wxButton( this, wxID_OK, _( "OK" ) );
    ok_row->Add( ok_b, 0, wxALIGN_CENTER_HORIZONTAL );

    this->SetAutoLayout( true );
    this->SetSizer( toptop );

    toptop->Fit( this );

}

FinancialDialog::~FinancialDialog()
{};

void FinancialDialog::OnChange( wxCommandEvent &event )
{
    event.GetInt();
    _use_data = specify_a->GetValue();
}

bool FinancialDialog::TransferDataToWindow()
{
    double2entry( _cc00, &_cc00_d );
    double2entry( _cc01, &_cc01_d );
    double2entry( _cc02, &_cc02_d );
    double2entry( _cc03, &_cc03_d );
    double2entry( _cc04, &_cc04_d );
    double2entry( _cc05, &_cc05_d );
    double2entry( _cc06, &_cc06_d );
    double2entry( _cc07, &_cc07_d );
    double2entry( _cc08, &_cc08_d );

    double2entry( _om00, &_om00_d );
    double2entry( _om01, &_om01_d );
    double2entry( _om02, &_om02_d );
    double2entry( _om03, &_om03_d );

    if( _use_data )
    {
        specify_a->SetValue( true );
        specify_b->SetValue( false );
    }
    else
    {
        specify_a->SetValue( false );
        specify_b->SetValue( true );
    }

    wxCommandEvent event;
    OnChange( event );

    return true;
}

bool FinancialDialog::TransferDataFromWindow()
{
    entry2double( _cc00, &_cc00_d );
    entry2double( _cc01, &_cc01_d );
    entry2double( _cc02, &_cc02_d );
    entry2double( _cc03, &_cc03_d );
    entry2double( _cc04, &_cc04_d );
    entry2double( _cc05, &_cc05_d );
    entry2double( _cc06, &_cc06_d );
    entry2double( _cc07, &_cc07_d );
    entry2double( _cc08, &_cc08_d );

    entry2double( _om00, &_om00_d );
    entry2double( _om01, &_om01_d );
    entry2double( _om02, &_om02_d );
    entry2double( _om03, &_om03_d );

    if( specify_a->GetValue() ) _use_data = 1;
    else                       _use_data = 0;

    return true;
}

void FinancialDialog::double2entry( wxTextCtrl* entry, double * value )
{
    wxString txt;
    txt << ( *value );
    entry->SetValue( txt );
}

void FinancialDialog::entry2double( wxTextCtrl* entry, double * value )
{
    wxString txt;
    txt = entry->GetValue();
    ( *value ) = atof( ConvertUnicode( txt.c_str() ).c_str() );
}
