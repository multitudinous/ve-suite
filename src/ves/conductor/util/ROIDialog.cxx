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
#include <ves/conductor/util/ROIDialog.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <wx/statbox.h>
using namespace ves::conductor::util;
////////////////////////////////////////////////////////////////
ROIDialog::ROIDialog( wxWindow* parent, int id, std::string title )
        : BaseDialog( parent, id, title )
{
    _xBounds = 0;
    _yBounds = 0;
    _zBounds = 0;
    _buildGUI();
    wxSize displaySize = ::wxGetDisplaySize();
    wxRect dialogPosition( displaySize.GetWidth() - 427, 440, 427, displaySize.GetHeight() - 350 );
    this->SetSize( dialogPosition );
}
///////////////////////
///Destructor        //
///////////////////////
ROIDialog::~ROIDialog()
{
    /*if(_xBounds)
    {
       _xBounds->Destroy();
       _xBounds = 0;
    }
    if(_yBounds)
    {
       _yBounds->Destroy();
       _yBounds = 0;
    }
    if(_zBounds)
    {
       _zBounds->Destroy();
       _zBounds = 0;
    }*/
}
///////////////////////////
void ROIDialog::_buildGUI()
{
    wxStaticBox* dualSliderGroup = new wxStaticBox( this, -1, wxT( "Volume Clipping Planes" ) );
    wxStaticBoxSizer* mainSizer = new wxStaticBoxSizer( dualSliderGroup, wxVERTICAL );

    _createDualSliders();
    wxBoxSizer* xdualSizer = new wxBoxSizer( wxHORIZONTAL );
    xdualSizer->Add( _xBounds, 1, wxALIGN_CENTER | wxEXPAND );

    wxBoxSizer* ydualSizer = new wxBoxSizer( wxHORIZONTAL );
    ydualSizer->Add( _yBounds, 1, wxALIGN_CENTER | wxEXPAND );

    wxBoxSizer* zdualSizer = new wxBoxSizer( wxHORIZONTAL );
    zdualSizer->Add( _zBounds, 1, wxALIGN_CENTER | wxEXPAND );

    mainSizer->Add( xdualSizer, 3, wxALIGN_CENTER | wxEXPAND );
    mainSizer->Add( ydualSizer, 3, wxALIGN_CENTER | wxEXPAND );
    mainSizer->Add( zdualSizer, 3, wxALIGN_CENTER | wxEXPAND );

    wxBoxSizer* buttonRowSizer = new wxBoxSizer( wxHORIZONTAL );
    _addOKButton( buttonRowSizer );

    mainSizer->Add( buttonRowSizer, 0, wxALIGN_CENTER );
    _xBounds->Raise();
    _yBounds->Raise();
    _zBounds->Raise();
    //set this flag and let wx handle alignment
    SetAutoLayout( true );

    //assign the group to the panel
    SetSizer( mainSizer );
    mainSizer->Fit( this );
}
////////////////////////////////////
void ROIDialog::_createDualSliders()
{
    _xBounds = new DualSlider( this, -1, 1, 0, 100, 0, 100, wxDefaultPosition, wxDefaultSize,
                               wxSL_HORIZONTAL | wxSL_AUTOTICKS | wxSL_LABELS, _( "X Bounds" ) );
    ROIMinSliderCallback* minX = new ROIMinSliderCallback( this, "X" );
    ROIMaxSliderCallback* maxX = new ROIMaxSliderCallback( this, "X" );
    ROIBothMoveCallback* bothX = new ROIBothMoveCallback( this, "X" );

    _xBounds->SetMinSliderCallback( minX );
    _xBounds->SetMaxSliderCallback( maxX );
    _xBounds->SetBothSliderUpdateCallback( bothX );

    _yBounds = new DualSlider( this, -1, 1, 0, 100, 0, 100, wxDefaultPosition, wxDefaultSize,
                               wxSL_HORIZONTAL | wxSL_AUTOTICKS | wxSL_LABELS, _( "Y Bounds" ) );

    ROIMinSliderCallback* minY = new ROIMinSliderCallback( this, "Y" );
    ROIMaxSliderCallback* maxY = new ROIMaxSliderCallback( this, "Y" );
    ROIBothMoveCallback* bothY = new ROIBothMoveCallback( this, "Y" );

    _yBounds->SetMinSliderCallback( minY );
    _yBounds->SetMaxSliderCallback( maxY );
    _yBounds->SetBothSliderUpdateCallback( bothY );

    _zBounds = new DualSlider( this, -1, 1, 0, 100, 0, 100, wxDefaultPosition, wxDefaultSize,
                               wxSL_HORIZONTAL | wxSL_AUTOTICKS | wxSL_LABELS, _( "Z Bounds" ) );

    ROIMinSliderCallback* minZ = new ROIMinSliderCallback( this, "Z" );
    ROIMaxSliderCallback* maxZ = new ROIMaxSliderCallback( this, "Z" );
    ROIBothMoveCallback* bothZ = new ROIBothMoveCallback( this, "Z" );

    _zBounds->SetMinSliderCallback( minZ );
    _zBounds->SetMaxSliderCallback( maxZ );
    _zBounds->SetBothSliderUpdateCallback( bothZ );
}
///////////////////////////////////////////////////////
void ROIDialog::ROIMinSliderCallback::SliderOperation()
{
    _roidlg->SetCommandName( "TB_ROI_UPDATE" );

    ves::open::xml::DataValuePairPtr coordinate( new ves::open::xml::DataValuePair() );
    coordinate->SetDataType( "STRING" );
    coordinate->SetDataName( std::string( "Coordinate" ) );
    coordinate->SetDataString( _direction );
    _roidlg->AddInstruction( coordinate );

    ves::open::xml::DataValuePairPtr direction( new ves::open::xml::DataValuePair() );
    direction->SetDataType( "STRING" );
    direction->SetDataName( std::string( "Direction" ) );
    direction->SetDataString( "Positive" );
    _roidlg->AddInstruction( direction );

    ves::open::xml::DataValuePairPtr value( new ves::open::xml::DataValuePair() );
    value->SetData( "ROI Value", static_cast<double>( _dualSlider->GetMinSliderValue() ) / 100.0 );
    _roidlg->AddInstruction( value );

    _roidlg->SendCommands();
    _roidlg->ClearInstructions();
}
///////////////////////////////////////////////////////
void ROIDialog::ROIMaxSliderCallback::SliderOperation()
{
    _roidlg->SetCommandName( "TB_ROI_UPDATE" );
    ves::open::xml::DataValuePairPtr coordinate( new ves::open::xml::DataValuePair() );
    coordinate->SetDataType( "STRING" );
    coordinate->SetDataName( std::string( "Coordinate" ) );
    coordinate->SetDataString( _direction );
    _roidlg->AddInstruction( coordinate );

    ves::open::xml::DataValuePairPtr direction( new ves::open::xml::DataValuePair() );
    direction->SetDataType( "STRING" );
    direction->SetDataName( std::string( "Direction" ) );
    direction->SetDataString( "Negative" );
    _roidlg->AddInstruction( direction );

    ves::open::xml::DataValuePairPtr value( new ves::open::xml::DataValuePair() );
    value->SetData( "ROI Value", static_cast<double>( _dualSlider->GetMaxSliderValue() ) / 100.0 );
    _roidlg->AddInstruction( value );

    _roidlg->SendCommands();
    _roidlg->ClearInstructions();
}
//////////////////////////////////////////////////////
void ROIDialog::ROIBothMoveCallback::SliderOperation()
{
    _roidlg->SetCommandName( "TB_ROI_UPDATE" );

    ves::open::xml::DataValuePairPtr coordinate( new ves::open::xml::DataValuePair() );
    coordinate->SetDataType( "STRING" );
    coordinate->SetDataName( std::string( "Coordinate" ) );
    coordinate->SetDataString( _direction );
    _roidlg->AddInstruction( coordinate );

    ves::open::xml::DataValuePairPtr direction( new ves::open::xml::DataValuePair() );
    direction->SetDataType( "STRING" );
    direction->SetDataName( std::string( "Direction" ) );
    direction->SetDataString( "Both" );
    _roidlg->AddInstruction( direction );


    ves::open::xml::DataValuePairPtr minvalue( new ves::open::xml::DataValuePair() );
    minvalue->SetData( "ROI Min Value", static_cast<double>( _dualSlider->GetMinSliderValue() ) / 100.0 );
    _roidlg->AddInstruction( minvalue );

    ves::open::xml::DataValuePairPtr maxvalue( new ves::open::xml::DataValuePair() );
    maxvalue->SetData( "ROI Max Value", static_cast<double>( _dualSlider->GetMaxSliderValue() ) / 100.0 );
    _roidlg->AddInstruction( maxvalue );

    _roidlg->SendCommands();
    _roidlg->ClearInstructions();
}
////////////////////////////////////////////////////
void ROIDialog::SetCommandName( std::string newName )
{
    _commandName = newName;
}
///////////////////////////////////////////////////////////////////
void ROIDialog::AddInstruction( ves::open::xml::DataValuePairPtr newInstruct )
{
    _instructions.push_back( newInstruct );
}
//////////////////////////////
void ROIDialog::SendCommands()
{
    _sendCommandsToXplorer();
}
////////////////////////////////////////////
wxSizer* ROIDialog::_buildSpecificWidgets()
{
    wxStaticBox* dualSliderGroup = new wxStaticBox( this, -1, wxT( "Volume Clipping Planes" ) );
    wxStaticBoxSizer* mainSizer = new wxStaticBoxSizer( dualSliderGroup, wxVERTICAL );

    _createDualSliders();
    wxBoxSizer* xdualSizer = new wxBoxSizer( wxHORIZONTAL );
    xdualSizer->Add( _xBounds, 1, wxALIGN_CENTER | wxEXPAND );

    wxBoxSizer* ydualSizer = new wxBoxSizer( wxHORIZONTAL );
    ydualSizer->Add( _yBounds, 1, wxALIGN_CENTER | wxEXPAND );

    wxBoxSizer* zdualSizer = new wxBoxSizer( wxHORIZONTAL );
    zdualSizer->Add( _zBounds, 1, wxALIGN_CENTER | wxEXPAND );

    mainSizer->Add( xdualSizer, 1, wxALIGN_CENTER | wxEXPAND );
    mainSizer->Add( ydualSizer, 1, wxALIGN_CENTER | wxEXPAND );
    mainSizer->Add( zdualSizer, 1, wxALIGN_CENTER | wxEXPAND );

    return mainSizer;
}

