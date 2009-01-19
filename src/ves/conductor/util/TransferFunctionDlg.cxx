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
#include <ves/conductor/util/TransferFunctionDlg.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <wx/statbox.h>
#include <wx/combobox.h>
#include <wx/checkbox.h>
#include <wx/button.h>
#include <wx/slider.h>
#include <wx/choicdlg.h>
#include <wx/filename.h>
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( TransferFunctionDialog, wxDialog )
    EVT_COMBOBOX( AVAILABLE_SHADER_MANAGERS, TransferFunctionDialog::_updateActiveScalarShaderManager )
    EVT_CHECKBOX( PHONG_ENABLE_CHECK, TransferFunctionDialog::_onEnablePhongLighting )
END_EVENT_TABLE()
////////////////////////////////////////////////////////////////
TransferFunctionDialog::TransferFunctionDialog( wxWindow* parent, int id, std::string title )
        : BaseDialog( parent, id, title )
{
    _buildGUI();
    //SetSize(dynamic_cast<AppFrame*>(wxTheApp->GetTopWindow())->GetAppropriateSubDialogSize());
}
///////////////////////
///Destructor        //
///////////////////////
TransferFunctionDialog::~TransferFunctionDialog()
{}
///////////////////////////
void TransferFunctionDialog::_buildGUI()
{
    wxStaticBox* scalarToolsGroup = new wxStaticBox( this, -1, wxT( "Transfer Functions" ) );
    wxStaticBoxSizer* mainSizer = new wxStaticBoxSizer( scalarToolsGroup, wxVERTICAL );
    wxStaticBox* shaderManagerBox = new wxStaticBox( this, -1, _T( "Base Look-Up Table" ) );
    wxStaticBoxSizer* smSizer = new wxStaticBoxSizer( shaderManagerBox, wxVERTICAL );

    _shaderManagerSelection = new wxComboBox( this, AVAILABLE_SHADER_MANAGERS,
                                              _( "" ), wxDefaultPosition,
                                              wxSize( 150, wxDefaultCoord ) );
    _shaderManagerSelection->Append( _T( "BLUE_RED_LINEAR_SHADER" ) );
    _shaderManagerSelection->Append( _T( "GREY_SCALE_SHADER" ) );
    _shaderManagerSelection->Append( _T( "CUSTOM" ) );
    _shaderManagerSelection->SetSelection( 0 );
    smSizer->Add( _shaderManagerSelection, 0, wxALIGN_CENTER | wxEXPAND );

    //wxBoxSizer* enablePhongSizer = new wxBoxSizer(wxHORIZONTAL);
    _phongShadingCheck = new wxCheckBox( this, PHONG_ENABLE_CHECK, _T( "Enable Phong Shading" ) );
    smSizer->Add( _phongShadingCheck, 0, wxALIGN_CENTER );

    wxBoxSizer* buttonRowSizer = new wxBoxSizer( wxHORIZONTAL );
    _addOKButton( buttonRowSizer );

    mainSizer->Add( smSizer, 1, wxEXPAND | wxALIGN_CENTER_HORIZONTAL );
    mainSizer->Add( buttonRowSizer, 0, wxALIGN_CENTER );


    //set this flag and let wx handle alignment
    SetAutoLayout( true );

    //assign the group to the panel
    SetSizer( mainSizer );
    mainSizer->Fit( this );
}
/////////////////////////////////////////////////////////////////////////////////
void TransferFunctionDialog::_updateActiveScalarShaderManager( wxCommandEvent& command )
{
    ClearInstructions();

    _commandName = "TB_SET_ACTIVE_SHADER_MANAGER";

    ves::open::xml::DataValuePairPtr name( new ves::open::xml::DataValuePair() );
    name->SetData( "Active Shader Manager", ConvertUnicode( _shaderManagerSelection->GetValue().GetData() ) );
    _instructions.push_back( name );

    _sendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////
void TransferFunctionDialog::_onEnablePhongLighting( wxCommandEvent& command )
{

    ClearInstructions();
    _commandName = "TB_PHONG_SHADING_ENABLE";

    ves::open::xml::DataValuePairPtr phongValue( new ves::open::xml::DataValuePair() );
    phongValue->SetData( "Phong Shading State", ( _phongShadingCheck->GetValue() ) ? "On" : "Off" );
    _instructions.push_back( phongValue );

    _sendCommandsToXplorer();
    ClearInstructions();
}
