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

///////////////////////////////////////////////////////////////////////////////
//
//  Dialog for WMS server properties.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/conductor/MinervaWmsDialog.h>

#include <wx/string.h>
#include <wx/stattext.h>
#include <wx/gdicmn.h>
#include <wx/font.h>
#include <wx/colour.h>
#include <wx/settings.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>
#include <wx/radiobut.h>
#include <wx/statbox.h>
#include <wx/checkbox.h>
#include <wx/button.h>

const wxString WINDOW_TITLE ( wxT( "Add WMS Server" ) );


MinervaWmsDialog::MinervaWmsDialog ( wxWindow *parent, wxWindowID id ) 
    : 
    BaseClass ( parent, id, WINDOW_TITLE, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE ),
    _serverTextCtrl ( 0x0 ),
    _layersTextCtrl ( 0x0 ),
    _stylesTextCtrl ( 0x0 ),
    _jpegRadioBtn ( 0x0 ),
    _pngRadioBtn ( 0x0 ),
    _tiffRadioBtn ( 0x0 ),
    _transparentCheckBox ( 0x0 ),
    _sdbSizer ( 0x0 ),
    _sdbSizerOK ( 0x0 ),
    _sdbSizerCancel ( 0x0 )
{
  this->SetSizeHints( wxDefaultSize, wxDefaultSize );
    
    wxBoxSizer* bSizer2;
    bSizer2 = new wxBoxSizer( wxVERTICAL );
    
    wxGridSizer* gSizer3;
    gSizer3 = new wxGridSizer( 2, 2, 0, 0 );
    
    wxStaticText* serverStaticText = new wxStaticText( this, wxID_ANY, wxT("Server"), wxDefaultPosition, wxDefaultSize, 0 );
    serverStaticText->Wrap( -1 );
    gSizer3->Add ( serverStaticText, 0, wxALL, 5 );
    
    _serverTextCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
    gSizer3->Add( _serverTextCtrl, 0, wxALL, 5 );
    
    wxStaticText* layersStaticText = new wxStaticText( this, wxID_ANY, wxT("Layers"), wxDefaultPosition, wxDefaultSize, 0 );
    layersStaticText->Wrap( -1 );
    gSizer3->Add( layersStaticText, 0, wxALL, 5 );
    
    _layersTextCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
    gSizer3->Add( _layersTextCtrl, 0, wxALL, 5 );
    
    wxStaticText* stylesStaticText = new wxStaticText( this, wxID_ANY, wxT("Styles"), wxDefaultPosition, wxDefaultSize, 0 );
    stylesStaticText->Wrap( -1 );
    gSizer3->Add( stylesStaticText, 0, wxALL, 5 );
    
    _stylesTextCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
    gSizer3->Add( _stylesTextCtrl, 0, wxALL, 5 );
    
    bSizer2->Add( gSizer3, 1, wxEXPAND, 5 );
    
    wxStaticBoxSizer* sbSizer3;
    sbSizer3 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Image Format") ), wxVERTICAL );
    
    wxBoxSizer* bSizer3;
    bSizer3 = new wxBoxSizer( wxVERTICAL );
    
    _jpegRadioBtn = new wxRadioButton( this, wxID_ANY, wxT("JPEG"), wxDefaultPosition, wxDefaultSize, 0 );
    bSizer3->Add( _jpegRadioBtn, 0, wxALL, 5 );
    
    _pngRadioBtn = new wxRadioButton( this, wxID_ANY, wxT("PNG"), wxDefaultPosition, wxDefaultSize, 0 );
    bSizer3->Add( _pngRadioBtn, 0, wxALL, 5 );
    
    _tiffRadioBtn = new wxRadioButton( this, wxID_ANY, wxT("TIFF"), wxDefaultPosition, wxDefaultSize, 0 );
    bSizer3->Add ( _tiffRadioBtn, 0, wxALL, 5 );
    
    sbSizer3->Add( bSizer3, 1, wxEXPAND, 5 );
    
    bSizer2->Add( sbSizer3, 1, wxEXPAND, 5 );
    
    _transparentCheckBox = new wxCheckBox( this, wxID_ANY, wxT("Transparent"), wxDefaultPosition, wxDefaultSize, 0 );
    
    bSizer2->Add( _transparentCheckBox, 0, wxALL, 5 );
    
    _sdbSizer = new wxStdDialogButtonSizer;
    _sdbSizerOK = new wxButton( this, wxID_OK );
    _sdbSizer->AddButton( _sdbSizerOK );
    _sdbSizerCancel = new wxButton( this, wxID_CANCEL );
    _sdbSizer->AddButton( _sdbSizerCancel );
    _sdbSizer->Realize();
    bSizer2->Add( _sdbSizer, 1, wxEXPAND, 5 );
    
    this->SetSizer( bSizer2 );
    this->Layout();
    bSizer2->Fit( this );
}
///////////////////////////////////////////////////////////////////////////////
MinervaWmsDialog::~MinervaWmsDialog()
{
}
///////////////////////////////////////////////////////////////////////////////
std::string MinervaWmsDialog::server() 
{
  return ConvertUnicode( _serverTextCtrl->GetLineText ( 0 ).c_str() );
}
///////////////////////////////////////////////////////////////////////////////
std::string MinervaWmsDialog::layers() 
{
  return ConvertUnicode( _layersTextCtrl->GetLineText ( 0 ).c_str() );
}
///////////////////////////////////////////////////////////////////////////////
std::string MinervaWmsDialog::styles() 
{
  return ConvertUnicode( _stylesTextCtrl->GetLineText ( 0 ).c_str() );
}
///////////////////////////////////////////////////////////////////////////////
std::string MinervaWmsDialog::format() const
{
  if ( _jpegRadioBtn->GetValue() )
    return "image/jpeg";
  if ( _pngRadioBtn->GetValue() )
    return "image/png";
  if ( _tiffRadioBtn->GetValue() )
    return "image/tif";
  return "image/jpeg";
}
