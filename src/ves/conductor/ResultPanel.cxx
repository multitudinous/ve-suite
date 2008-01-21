/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/conductor/ResultPanel.h>
#include <wx/combobox.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/sizer.h>
#include <wx/button.h>
#include <wx/intl.h>
using namespace ves::conductor;


ResultPanel_Dialog::ResultPanel_Dialog( wxWindow* parent, int id )
        : wxDialog(( wxWindow * ) parent, id, _( "Result Panel" ),
                   wxDefaultPosition,
                   wxDefaultSize,
                   ( wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMAXIMIZE_BOX | wxMINIMIZE_BOX ) & ~ wxSTAY_ON_TOP
                  )
{
    wxBoxSizer* toptop = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* left_margin = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* top_sizer = new wxBoxSizer( wxVERTICAL );
    wxBoxSizer* right_margin = new wxBoxSizer( wxHORIZONTAL );
    left_margin->Add( 5, 10 );
    right_margin->Add( 5, 10 );

    toptop->Add( left_margin, 0, wxALIGN_LEFT );
    toptop->Add( top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL );
    toptop->Add( right_margin, 0, wxALIGN_RIGHT );

    wxStaticBox* cmn_box = new wxStaticBox( this, -1, _( "Overall Plant" ) );
    wxStaticBoxSizer* cmn_row = new wxStaticBoxSizer( cmn_box, wxVERTICAL );
    wxStaticBox* tex_box = new wxStaticBox( this, -1, _( "Gasifier Area" ) );
    wxStaticBoxSizer* tex_row = new wxStaticBoxSizer( tex_box, wxVERTICAL );
    wxStaticBox* selx_box = new wxStaticBox( this, -1, _( "CO2 Capture" ) );
    wxStaticBoxSizer* selx_row = new wxStaticBoxSizer( selx_box, wxVERTICAL );
    wxStaticBox* finance_box = new wxStaticBox( this, -1, _( "Financial Result" ) );
    wxStaticBoxSizer* finance_row = new wxStaticBoxSizer( finance_box, wxVERTICAL );


    wxBoxSizer* cmn_first_row = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* cmn_second_row = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* cmn_third_row = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* tex_first_row = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* tex_second_row = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* tex_third_row = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* selx_first_row = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* selx_second_row = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* selx_third_row = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* finance_first_row = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* finance_second_row = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* ok_row = new wxBoxSizer( wxHORIZONTAL );

    wxSize sz( 200, 20 ); // the size for the lable
    top_sizer->Add( 10, 10, 0 );
    top_sizer->Add( cmn_row, 0 );
    top_sizer->Add( 10, 5, 0 );
    top_sizer->Add( tex_row, 0 );
    top_sizer->Add( 10, 5, 0 );
    top_sizer->Add( selx_row, 0 );
    top_sizer->Add( 10, 5, 0 );
    top_sizer->Add( finance_row, 0 );
    top_sizer->Add( 10, 5, 0 );
    top_sizer->Add( ok_row, 0, wxALIGN_CENTER_HORIZONTAL );
    top_sizer->Add( 10, 5, 0 );

    cmn_row->Add( 10, 8 );
    cmn_row->Add( cmn_first_row, 0 );
    cmn_row->Add( 10, 3, 0 );
    cmn_row->Add( cmn_second_row, 0 );
    cmn_row->Add( 10, 3, 0 );
    cmn_row->Add( cmn_third_row, 0 );
    cmn_row->Add( 10, 3, 0 );

    tex_row->Add( 10, 8 );
    tex_row->Add( tex_first_row, 0 );
    tex_row->Add( 10, 3, 0 );
    tex_row->Add( tex_second_row, 0 );
    tex_row->Add( 10, 3, 0 );
    tex_row->Add( tex_third_row, 0 );
    tex_row->Add( 10, 3, 0 );

    selx_row->Add( 10, 8 );
    selx_row->Add( selx_first_row, 0 );
    selx_row->Add( 10, 3, 0 );
    selx_row->Add( selx_second_row, 0 );
    selx_row->Add( 10, 3, 0 );
    selx_row->Add( selx_third_row, 0 );
    selx_row->Add( 10, 3, 0 );

    finance_row->Add( 10, 8 );
    finance_row->Add( finance_first_row, 0 );
    finance_row->Add( 10, 3, 0 );
    finance_row->Add( finance_second_row, 0 );
    finance_row->Add( 10, 3, 0 );

    wxStaticText * label0 = new wxStaticText( this, -1, _( "    Nominal Plant Output" ), wxDefaultPosition, sz );
    mw_gross = new wxTextCtrl( this, MW_GROSS, wxT( "9000" ), wxDefaultPosition, wxSize( 80, 20 ) );
    wxStaticText * label1 = new wxStaticText( this, -1, _( " (MW)" ), wxDefaultPosition, wxSize( 80, 20 ), wxTE_READONLY );

    cmn_first_row->Add( label0 );
    cmn_first_row->Add( mw_gross );
    cmn_first_row->Add( label1 );

    wxStaticText * label2 = new wxStaticText( this, -1, _( "    Net Electrical Output" ), wxDefaultPosition, sz );
    mw_net = new wxTextCtrl( this, MW_NET, wxT( "7000" ), wxDefaultPosition, wxSize( 80, 20 ) );
    wxStaticText * label3 = new wxStaticText( this, -1, _( " (MW)" ), wxDefaultPosition, wxSize( 80, 20 ), wxTE_READONLY );

    cmn_second_row->Add( label2 );
    cmn_second_row->Add( mw_net );
    cmn_second_row->Add( label3 );

    wxStaticText * label4 = new wxStaticText( this, -1, _( "    Net Plant Efficiency" ), wxDefaultPosition, sz );
    net_eff = new wxTextCtrl( this, NET_EFF, wxT( "67" ), wxDefaultPosition, wxSize( 80, 20 ) );
    wxStaticText * label5 = new wxStaticText( this, -1, _( " (%)" ), wxDefaultPosition, wxSize( 80, 20 ), wxTE_READONLY );

    cmn_third_row->Add( label4 );
    cmn_third_row->Add( net_eff );
    cmn_third_row->Add( label5 );

    wxStaticText * label6 = new wxStaticText( this, -1, _( "    Coal In" ), wxDefaultPosition, sz );
    coal_in = new wxTextCtrl( this, COAL_IN, wxT( "9000" ), wxDefaultPosition, wxSize( 80, 20 ) );
    wxStaticText * label7 = new wxStaticText( this, -1, _( " (tons/hr)" ), wxDefaultPosition, wxSize( 80, 20 ), wxTE_READONLY );

    tex_first_row->Add( label6 );
    tex_first_row->Add( coal_in );
    tex_first_row->Add( label7 );

    wxStaticText * label8 = new wxStaticText( this, -1, _( "    Water In" ), wxDefaultPosition, sz );
    water_in = new wxTextCtrl( this, WATER_IN, wxT( "9000" ), wxDefaultPosition, wxSize( 80, 20 ) );
    wxStaticText * label9 = new wxStaticText( this, -1, _( " (tons/hr)" ), wxDefaultPosition, wxSize( 80, 20 ), wxTE_READONLY );

    tex_second_row->Add( label8 );
    tex_second_row->Add( water_in );
    tex_second_row->Add( label9 );

    wxStaticText * label10 = new wxStaticText( this, -1, _( "    Oxidant In" ), wxDefaultPosition, sz );
    oxid_in = new wxTextCtrl( this, OXID_IN, wxT( "9000" ), wxDefaultPosition, wxSize( 80, 20 ) );
    wxStaticText * label11 = new wxStaticText( this, -1, _( " (tons/hr)" ), wxDefaultPosition, wxSize( 80, 20 ), wxTE_READONLY );

    tex_third_row->Add( label10 );
    tex_third_row->Add( oxid_in );
    tex_third_row->Add( label11 );

    wxStaticText * label12 = new wxStaticText( this, -1, _( "    CO2 In" ), wxDefaultPosition, sz );
    co2_in = new wxTextCtrl( this, CO2_IN, wxT( "9000" ), wxDefaultPosition, wxSize( 80, 20 ) );
    wxStaticText * label13 = new wxStaticText( this, -1, _( " (tons/hr)" ), wxDefaultPosition, wxSize( 80, 20 ), wxTE_READONLY );

    selx_first_row->Add( label12 );
    selx_first_row->Add( co2_in );
    selx_first_row->Add( label13 );

    wxStaticText * label14 = new wxStaticText( this, -1, _( "    CO2 Out" ), wxDefaultPosition, sz );
    co2_out = new wxTextCtrl( this, CO2_OUT, wxT( "9000" ), wxDefaultPosition, wxSize( 80, 20 ) );
    wxStaticText * label15 = new wxStaticText( this, -1, _( " (tons/hr)" ), wxDefaultPosition, wxSize( 80, 20 ), wxTE_READONLY );

    selx_second_row->Add( label14 );
    selx_second_row->Add( co2_out );
    selx_second_row->Add( label15 );

    wxStaticText * label16 = new wxStaticText( this, -1, _( "    CO2 Captured" ), wxDefaultPosition, sz );
    co2_cap = new wxTextCtrl( this, CO2_CAP, wxT( "9000" ), wxDefaultPosition, wxSize( 80, 20 ) );
    wxStaticText * label17 = new wxStaticText( this, -1, _( " (tons/hr)" ), wxDefaultPosition, wxSize( 80, 20 ), wxTE_READONLY );

    selx_third_row->Add( label16 );
    selx_third_row->Add( co2_cap );
    selx_third_row->Add( label17 );

    wxStaticText * label18 = new wxStaticText( this, -1, _( "    Capital Cost" ), wxDefaultPosition, sz );
    capital_cst = new wxTextCtrl( this, CAPITAL_CST, wxT( "9000" ), wxDefaultPosition, wxSize( 80, 20 ) );
    wxStaticText * label19 = new wxStaticText( this, -1, _( " ($/kWnet)" ), wxDefaultPosition, wxSize( 80, 20 ), wxTE_READONLY );

    finance_first_row->Add( label18 );
    finance_first_row->Add( capital_cst );
    finance_first_row->Add( label19 );

    wxStaticText * label20 = new wxStaticText( this, -1, _( "    Electricity Cost)" ), wxDefaultPosition, sz );
    elec_cst = new wxTextCtrl( this, ELEC_CST, wxT( "9000" ), wxDefaultPosition, wxSize( 80, 20 ) );
    wxStaticText * label21 = new wxStaticText( this, -1, _( " ($/MWh)" ), wxDefaultPosition, wxSize( 80, 20 ), wxTE_READONLY );

    finance_second_row->Add( label20 );
    finance_second_row->Add( elec_cst );
    finance_second_row->Add( label21 );


    //ok_row->Add(new wxButton(this, wxID_OK, wxString("OK",wxConvUTF8), wxDefaultPosition, wxALIGN_CENTER_HORIZONTAL);
    //ok_row->Add(new wxButton(this, wxID_CANCEL, wxString("Cancel",wxConvUTF8), wxDefaultPosition, wxALIGN_CENTER_HORIZONTAL);

    SetSizer( toptop );
    SetAutoLayout( TRUE );
    toptop->Fit( this );

}

ResultPanel_Dialog::~ResultPanel_Dialog()
{}

bool ResultPanel_Dialog::TransferDataToWindow()
{
    wxString txt0, txt1, txt2, txt3, txt4;
    wxString txt5, txt6, txt7, txt8, txt9, txt10;

    txt0 << mw_gross_;
    txt1 << mw_net_;
    txt2 << net_eff_;
    txt3 << coal_in_;
    txt4 << water_in_;
    txt5 << oxid_in_;
    txt6 << co2_in_;
    txt7 << co2_out_;
    txt8 << co2_cap_;
    txt9 << capital_cst_;
    txt10 << elec_cst_;

    mw_gross->SetValue( txt0 );
    mw_net->SetValue( txt1 );
    net_eff->SetValue( txt2 );
    coal_in->SetValue( txt3 );
    water_in->SetValue( txt4 );
    oxid_in->SetValue( txt5 );
    co2_in->SetValue( txt6 );
    co2_out->SetValue( txt7 );
    co2_cap->SetValue( txt8 );
    capital_cst->SetValue( txt9 );
    elec_cst->SetValue( txt10 );
    return true;
}
