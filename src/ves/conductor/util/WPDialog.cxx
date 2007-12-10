/*************** <auto-copyright.pl BEGIN do not edit this line> *************
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
 *************** <auto-copyright.pl END do not edit this line> **************/
#include <ves/conductor/util/WPDialog.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <wx/statbox.h>

using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( WPDialog, BaseDialog )
    EVT_SPINCTRL( WPDialog::DIMENSION_SPINNER_ID, WPDialog::_updateDimensions )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////////
WPDialog::WPDialog( wxWindow* parent, int id, std::string title )
        :
        BaseDialog( parent, id, title )
{
    _xBounds = 0;
    _yBounds = 0;
    _zBounds = 0;
    numXPointsSpinner = 0;
    numYPointsSpinner = 0;
    numZPointsSpinner = 0;

    _buildGUI();
    /*
    wxSize displaySize = ::wxGetDisplaySize();
     wxRect dialogPosition( displaySize.GetWidth()-427, 440, 427, displaySize.GetHeight()-480 );
    this->SetSize( dialogPosition );
    */
    /*wxSize displaySize = ::wxGetDisplaySize();
    wxRect dialogPosition( displaySize.GetWidth()-427, 440, 427, displaySize.GetHeight()-480 );
    this->SetSize( dialogPosition );*/
    GetSizer()->Fit( this );
    GetSizer()->SetSizeHints( this );
    Centre();
}
////////////////////////////////////////////////////////////////////////////////////
WPDialog::~WPDialog()
{
    /*for ( size_t i = 0; i < seedPointDVP.size(); ++i )
    {
       delete seedPointDVP.at( i );
    }*/
    seedPointDVP.clear();
}
////////////////////////////////////////////////////////////////////////////////////
void WPDialog::_buildGUI()
{
    wxStaticBox* dualSliderGroup = new wxStaticBox( this, -1, wxT( "Bounding Box Controls" ) );
    wxStaticBoxSizer* mainSizer = new wxStaticBoxSizer( dualSliderGroup, wxVERTICAL );

    _createDualSliders();
    wxBoxSizer* xdualSizer = new wxBoxSizer( wxHORIZONTAL );
    xdualSizer->Add( _xBounds, 1, wxALIGN_CENTER | wxEXPAND );

    wxBoxSizer* ydualSizer = new wxBoxSizer( wxHORIZONTAL );
    ydualSizer->Add( _yBounds, 1, wxALIGN_CENTER | wxEXPAND );

    wxBoxSizer* zdualSizer = new wxBoxSizer( wxHORIZONTAL );
    zdualSizer->Add( _zBounds, 1, wxALIGN_CENTER | wxEXPAND );

    wxBoxSizer* spinnerXRowSizer = new wxBoxSizer( wxVERTICAL );
    numXPointsSpinner = new wxSpinCtrl( static_cast< wxWindow* >( this ), WPDialog::DIMENSION_SPINNER_ID,
                                        wxEmptyString,
                                        wxDefaultPosition, wxDefaultSize,
                                        wxSP_ARROW_KEYS, 1, 100, 4 );
    numXPointsSpinner->SetValue( 4 );
    wxStaticText* _spinnerXLabel = new wxStaticText( this, -1, wxT( "X-Plane" ) );
    //wxStaticBox* XspinnerGroup = new wxStaticBox(this, -1, wxT("X-Plane"));
    //wxStaticBoxSizer* spinnerXRowSizer = new wxStaticBoxSizer(XspinnerGroup,wxVERTICAL);

    spinnerXRowSizer->Add( _spinnerXLabel, 0, wxALIGN_CENTER );
    spinnerXRowSizer->Add( numXPointsSpinner, 0, wxALIGN_CENTER_HORIZONTAL | wxBOTTOM | wxTOP, 10 );

    wxBoxSizer* spinnerYRowSizer = new wxBoxSizer( wxVERTICAL );
    numYPointsSpinner = new wxSpinCtrl( static_cast< wxWindow* >( this ), WPDialog::DIMENSION_SPINNER_ID,
                                        wxEmptyString,
                                        wxDefaultPosition, wxDefaultSize,
                                        wxSP_ARROW_KEYS, 1, 100, 4 );
    numYPointsSpinner->SetValue( 4 );
    wxStaticText* _spinnerYLabel = new wxStaticText( this, -1, wxT( "Y-Plane" ) );

    spinnerYRowSizer->Add( _spinnerYLabel, 0, wxALIGN_CENTER );
    spinnerYRowSizer->Add( numYPointsSpinner, 0, wxALIGN_CENTER_HORIZONTAL | wxBOTTOM | wxTOP, 10 );

    wxBoxSizer* spinnerZRowSizer = new wxBoxSizer( wxVERTICAL );
    numZPointsSpinner = new wxSpinCtrl( static_cast< wxWindow* >( this ), WPDialog::DIMENSION_SPINNER_ID,
                                        wxEmptyString,
                                        wxDefaultPosition, wxDefaultSize,
                                        wxSP_ARROW_KEYS, 1, 100, 4 );
    numZPointsSpinner->SetValue( 1 );
    wxStaticText* _spinnerZLabel = new wxStaticText( this, -1, wxT( "Z-Plane" ) );

    spinnerZRowSizer->Add( _spinnerZLabel, 0, wxALIGN_CENTER );
    spinnerZRowSizer->Add( numZPointsSpinner, 0, wxALIGN_CENTER_HORIZONTAL | wxBOTTOM | wxTOP, 10 );

    wxStaticBox* seedPointGroup = new wxStaticBox( this, -1, wxT( "Seed Points" ) );
    wxStaticBoxSizer* spinnerRowSizer = new wxStaticBoxSizer( seedPointGroup, wxHORIZONTAL );

    spinnerRowSizer->Add( spinnerXRowSizer, 0, wxALIGN_LEFT, 5 );
    spinnerRowSizer->Add( spinnerYRowSizer, 0, wxALIGN_LEFT, 5 );
    spinnerRowSizer->Add( spinnerZRowSizer, 0, wxALIGN_LEFT, 5 );

    wxButton* _closeButton = new wxButton( this, wxID_OK, _T( "Close" ), wxDefaultPosition, wxDefaultSize, 0 );

    mainSizer->Add( xdualSizer, 1, wxALIGN_CENTER | wxEXPAND );
    mainSizer->Add( ydualSizer, 1, wxALIGN_CENTER | wxEXPAND );
    mainSizer->Add( zdualSizer, 1, wxALIGN_CENTER | wxEXPAND );
    mainSizer->Add( spinnerRowSizer, 1, wxALIGN_CENTER | wxEXPAND );

    mainSizer->Add( _closeButton, 0, wxALIGN_CENTER | wxALL, 5 );
    _xBounds->Raise();
    _yBounds->Raise();
    _zBounds->Raise();

    numZPointsSpinner->Raise();
    numYPointsSpinner->Raise();
    numXPointsSpinner->Raise();
    SetAutoLayout( true );

    SetSizer( mainSizer );
    //mainSizer->Fit(dynamic_cast<BaseDialog*>(this));
}
//////////////////////////////////////////////////////////////////////////////
void WPDialog::GetDimensions( std::vector<long>& dimensions )
{
    if( numXPointsSpinner && numYPointsSpinner && numZPointsSpinner )
    {
        dimensions.clear();
        dimensions.push_back( numXPointsSpinner->GetValue() );
        dimensions.push_back( numYPointsSpinner->GetValue() );
        dimensions.push_back( numZPointsSpinner->GetValue() );
    }
}
////////////////////////////////////////////////////////////////////////
void WPDialog::GetBounds( std::vector<double>& bounds )
{
    //this is a percentage
    if( _xBounds && _yBounds && _zBounds )
    {
        bounds.clear();
        bounds.push_back(
            static_cast<double>( _xBounds->GetMinSliderValue() ) / 100.0 );
        bounds.push_back(
            static_cast<double>( _xBounds->GetMaxSliderValue() ) / 100.0 );
        bounds.push_back(
            static_cast<double>( _yBounds->GetMinSliderValue() ) / 100.0 );
        bounds.push_back(
            static_cast<double>( _yBounds->GetMaxSliderValue() ) / 100.0 );
        bounds.push_back(
            static_cast<double>( _zBounds->GetMinSliderValue() ) / 100.0 );
        bounds.push_back(
            static_cast<double>( _zBounds->GetMaxSliderValue() ) / 100.0 );
    }
}
////////////////////////////////////////////////////
void WPDialog::_updateDimensions( wxSpinEvent& event )
{
    SetCommand( "Seed Points Dimensions" );
    std::vector<long> dimensions;
    dimensions.push_back( numXPointsSpinner->GetValue() );
    dimensions.push_back( numYPointsSpinner->GetValue() );
    dimensions.push_back( numZPointsSpinner->GetValue() );

    ves::open::xml::DataValuePair* value = new ves::open::xml::DataValuePair;
    value->SetData( "Dimensions", dimensions );
    AddInstruction( value );
    SendCommands();
    ClearInstructions();
}
///////////////////////////////////
void WPDialog::_createDualSliders()
{
//X Slider
    _xBounds = new DualSlider( this, -1, 1, 0, 100, 0, 100, wxDefaultPosition,
                               wxDefaultSize, wxSL_HORIZONTAL | wxSL_AUTOTICKS | wxSL_LABELS,
                               wxString( _T( "X Bounds" ) ) );
    WPMinSliderCallback* minX = new WPMinSliderCallback( this, "X" );
    WPMaxSliderCallback* maxX = new WPMaxSliderCallback( this, "X" );
    WPBothMoveCallback* bothX = new WPBothMoveCallback( this, "X" );

    _xBounds->SetMinSliderCallback( minX );
    _xBounds->SetMaxSliderCallback( maxX );
    _xBounds->SetBothSliderUpdateCallback( bothX );

//Y slider
    _yBounds = new DualSlider( this, -1, 1, 0, 100, 0, 100, wxDefaultPosition,
                               wxDefaultSize, wxSL_HORIZONTAL | wxSL_AUTOTICKS | wxSL_LABELS,
                               wxString( _T( "Y Bounds" ) ) );
    WPMinSliderCallback* minY = new WPMinSliderCallback( this, "Y" );
    WPMaxSliderCallback* maxY = new WPMaxSliderCallback( this, "Y" );
    WPBothMoveCallback* bothY = new WPBothMoveCallback( this, "Y" );

    _yBounds->SetMinSliderCallback( minY );
    _yBounds->SetMaxSliderCallback( maxY );
    _yBounds->SetBothSliderUpdateCallback( bothY );

//Z slider

    _zBounds = new DualSlider( this, -1, 1, 0, 100, 0, 100, wxDefaultPosition,
                               wxDefaultSize, wxSL_HORIZONTAL | wxSL_AUTOTICKS | wxSL_LABELS,
                               wxString( _T( "Z Bounds" ) ) );
    WPMinSliderCallback* minZ = new WPMinSliderCallback( this, "Z" );
    WPMaxSliderCallback* maxZ = new WPMaxSliderCallback( this, "Z" );
    WPBothMoveCallback* bothZ = new WPBothMoveCallback( this, "Z" );

    _zBounds->SetMinSliderCallback( minZ );
    _zBounds->SetMaxSliderCallback( maxZ );
    _zBounds->SetBothSliderUpdateCallback( bothZ );

}
////////////////////////////////////////////////////////////////////////////////////
void WPDialog::WPMinSliderCallback::SliderOperation()
{
    //what does TP stand for?
    _wpdlg->SetCommandName( "Seed Points Bounds" );

    ves::open::xml::DataValuePair* coordinate = new ves::open::xml::DataValuePair();
    coordinate->SetDataType( "STRING" );
    coordinate->SetDataName( std::string( "Coordinate" ) );
    coordinate->SetDataString( _direction );
    _wpdlg->AddInstruction( coordinate );

    ves::open::xml::DataValuePair* direction = new ves::open::xml::DataValuePair();
    direction->SetData( "MinMax", "Min" );
    _wpdlg->AddInstruction( direction );

    ves::open::xml::DataValuePair* value = new ves::open::xml::DataValuePair;
    value->SetData( "Value",
                    static_cast<double>( _dualSlider->GetMinSliderValue() ) / 100.0 );
    _wpdlg->AddInstruction( value );

    _wpdlg->SendCommands();
    _wpdlg->ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////////
void WPDialog::WPMaxSliderCallback::SliderOperation()
{
    _wpdlg->SetCommand( "Seed Points Bounds" );

    ves::open::xml::DataValuePair* coordinate = new ves::open::xml::DataValuePair();
    coordinate->SetData( std::string( "Coordinate" ), _direction );
    _wpdlg->AddInstruction( coordinate );

    ves::open::xml::DataValuePair* direction = new ves::open::xml::DataValuePair();
    direction->SetData( std::string( "MinMax" ), "Max" );
    _wpdlg->AddInstruction( direction );

    ves::open::xml::DataValuePair* value = new ves::open::xml::DataValuePair;
    value->SetData( "Value",
                    static_cast<double>( _dualSlider->GetMaxSliderValue() ) / 100.0 );
    _wpdlg->AddInstruction( value );

    _wpdlg->SendCommands();
    _wpdlg->ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////////
void WPDialog::WPBothMoveCallback::SliderOperation()
{
    _wpdlg->SetCommand( "Seed Points Bounds" );

    ves::open::xml::DataValuePair* coordinate = new ves::open::xml::DataValuePair();
    coordinate->SetData( std::string( "Coordinate" ), _direction );
    _wpdlg->AddInstruction( coordinate );

    ves::open::xml::DataValuePair* direction = new ves::open::xml::DataValuePair();
    direction->SetData( std::string( "MinMax" ), "Both" );
    _wpdlg->AddInstruction( direction );

    ves::open::xml::DataValuePair* minvalue = new ves::open::xml::DataValuePair;
    minvalue->SetData( "Min Value",
                       static_cast<double>( _dualSlider->GetMinSliderValue() ) / 100.0 );


    ves::open::xml::DataValuePair* maxvalue = new ves::open::xml::DataValuePair;
    maxvalue->SetData( "Max Value",
                       static_cast<double>( _dualSlider->GetMaxSliderValue() ) / 100.0 );

    _wpdlg->AddInstruction( minvalue );
    _wpdlg->AddInstruction( maxvalue );

    _wpdlg->SendCommands();
    _wpdlg->ClearInstructions();
}
//////////////////////////////////////////////////
void WPDialog::SetCommandName( std::string newName )
{
    _commandName = newName;
}
////////////////////////////////////////////////////////////////////
void WPDialog::AddInstruction( ves::open::xml::DataValuePair* newInstruction )
{
    _instructions.push_back( newInstruction );
}
////////////////////////////////////////////////////////////////////
void WPDialog::SendCommands()
{
    _sendCommandsToXplorer();
}
////////////////////////////////////////////////////////////////////
wxSizer* WPDialog::_buildSpecificWidgets()
{
    wxStaticBox* dualSliderGroup = new wxStaticBox( this, -1, _( "Volume Clipping Planes" ) );
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
////////////////////////////////////////////////////////////////////////////////
void WPDialog::SetVectorDVP( void )
{
    /*   _zBounds->GetMinSliderValue();
       _yBounds->GetMinSliderValue();
       _xBounds->GetMinSliderValue();

       _zBounds->GetMaxSliderValue();
       _yBounds->GetMaxSliderValue();
       _xBounds->GetMaxSliderValue();

       seedPointDVP*/
}
////////////////////////////////////////////////////////////////////////////////
std::vector< ves::open::xml::DataValuePair* > WPDialog::GetSeedPointDVPVector( void )
{
    return seedPointDVP;
}
////////////////////////////////////////////////////////////////////////////////
bool WPDialog::TransferDataFromWindow( void )
{
    seedPointDVP.clear();

    ves::open::xml::DataValuePair* tempDVP;
    ////////////////
    tempDVP = new ves::open::xml::DataValuePair();
    tempDVP->SetData( "Min_Z_BB",
                      static_cast<double>( _zBounds->GetMinSliderValue() ) / 100.0 );
    seedPointDVP.push_back( tempDVP );
    ////////////////
    tempDVP = new ves::open::xml::DataValuePair();
    tempDVP->SetData( "Min_Y_BB",
                      static_cast<double>( _yBounds->GetMinSliderValue() ) / 100.0 );
    seedPointDVP.push_back( tempDVP );
    ////////////////
    tempDVP = new ves::open::xml::DataValuePair();
    tempDVP->SetData( "Min_X_BB",
                      static_cast<double>( _xBounds->GetMinSliderValue() ) / 100.0 );
    seedPointDVP.push_back( tempDVP );
    ////////////////
    tempDVP = new ves::open::xml::DataValuePair();
    tempDVP->SetData( "Max_Z_BB",
                      static_cast<double>( _zBounds->GetMaxSliderValue() ) / 100.0 );
    seedPointDVP.push_back( tempDVP );
    ////////////////
    tempDVP = new ves::open::xml::DataValuePair();
    tempDVP->SetData( "Max_Y_BB",
                      static_cast<double>( _yBounds->GetMaxSliderValue() ) / 100.0 );
    seedPointDVP.push_back( tempDVP );
    ////////////////
    tempDVP = new ves::open::xml::DataValuePair();
    tempDVP->SetData( "Max_X_BB",
                      static_cast<double>( _xBounds->GetMaxSliderValue() ) / 100.0 );
    seedPointDVP.push_back( tempDVP );
    ////////////////
    tempDVP = new ves::open::xml::DataValuePair();
    tempDVP->SetData( "Num_X_Points",
                      static_cast< unsigned int>( numXPointsSpinner->GetValue() ) );
    seedPointDVP.push_back( tempDVP );
    ////////////////
    tempDVP = new ves::open::xml::DataValuePair();
    tempDVP->SetData( "Num_Y_Points",
                      static_cast< unsigned int>( numYPointsSpinner->GetValue() ) );
    seedPointDVP.push_back( tempDVP );
    ////////////////
    tempDVP = new ves::open::xml::DataValuePair();
    tempDVP->SetData( "Num_Z_Points",
                      static_cast< unsigned int>( numZPointsSpinner->GetValue() ) );
    seedPointDVP.push_back( tempDVP );
    return true;
}

