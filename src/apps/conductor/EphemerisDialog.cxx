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
#include <ves/conductor/util/CORBAServiceList.h>
#include "EphemerisDialog.h"
#include "ConductorAppEnums.h"

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/OneDIntArray.h>
#include <ves/conductor/UserPreferencesDataBuffer.h>
#include <wx/string.h>
#include <wx/config.h>
#include <wx/choicdlg.h>
#include <cmath>

using namespace ves::open::xml;
//Do not add custom headers
//wxDev-C++ designer will remove them
////Header Include Start
////Header Include End

//----------------------------------------------------------------------------
// EphemerisDialog
//----------------------------------------------------------------------------
//Add Custom Events only in the appropriate block.
//Code added in other places will be removed by wxDev-C++
////Event Table Start
BEGIN_EVENT_TABLE( EphemerisDialog, wxDialog )
    ////Manual Code Start
    ////Manual Code End

    //EVT_CLOSE( EphemerisDialog::OnClose )
    EVT_CHOICE( EPHEMERISDIALOG_AMPM, EphemerisDialog::OnAmPmSelected )
    EVT_CHOICE( EPHEMERISDIALOG_LATHEMISPHERE, EphemerisDialog::OnLatitudeDirection )
    EVT_SPINCTRL( EPHEMERISDIALOG_LATDEGREES, EphemerisDialog::OnChangeLatitudeDegrees )
    EVT_SPINCTRL( EPHEMERISDIALOG_LATITUDEMINUTES, EphemerisDialog::OnChangeLatitudeMinutes )

    EVT_SPINCTRL( EPHEMERISDIALOG_LONGITUDEMINUTES, EphemerisDialog::OnChangeLongitudeMinutes )
    EVT_SPINCTRL( EPHEMERISDIALOG_LONGITUDEDEGREE, EphemerisDialog::OnChangeLongitudeDegrees )
    EVT_CHOICE( EPHEMERISDIALOG_LONHEMISPHERE, EphemerisDialog::OnLongitudeDirection )
    EVT_TEXT( EPHEMERISDIALOG_HOUR, EphemerisDialog::OnHourTextUpdated )
    EVT_TEXT( EPHEMERISDIALOG_MINUTES, EphemerisDialog::OnMinuteTextUpdated )

    EVT_CALENDAR_DAY( EPHEMERISDIALOG_CALENDAR, EphemerisDialog::OnCalendarDay )
    EVT_CHECKBOX( EPHEMERISDIALOG_AUTO_DATE_TIME, EphemerisDialog::OnAutoDateTime )
    EVT_BUTTON( EPHEMERISDIALOG_LOAD_LOCATION_BUTTON, EphemerisDialog::OnLoadLocationInformation )
    EVT_BUTTON( EPHEMERISDIALOG_SAVE_LOCATION_BUTTON, EphemerisDialog::OnSaveLocationInformation )
    EVT_FILEPICKER_CHANGED( EPHEMERISDIALOG_LOAD_HEIGHT_MAP, EphemerisDialog::OnLoadHeightMap )
    EVT_CHECKBOX( EPHEMERISDIALOG_TOGGLE_EPHEMERIS, EphemerisDialog::OnToggleDisplay)
END_EVENT_TABLE()
////Event Table End
/////////////////////////////////////////////////////////////////
EphemerisDialog::EphemerisDialog( wxWindow *parent, wxWindowID id,
                                  const wxString &title,
                                  const wxPoint &position,
                                  const wxSize& size, long style )
        : wxDialog( parent, id, title, position, size, style )
{
    CreateGUIControls();
}
///////////////////////////////////
EphemerisDialog::~EphemerisDialog()
{
    WriteLocationInformation();
}
/////////////////////////////////////////
void EphemerisDialog::CreateGUIControls()
{
    //Do not add custom code between
    //GUI Items Creation Start and GUI Items Creation End.
    //wxDev-C++ designer will remove them.
    //Add the custom code before or after the blocks
    ////GUI Items Creation Start

    m_mainSizer = new wxBoxSizer( wxVERTICAL );
    this->SetSizer( m_mainSizer );
    //this->SetAutoLayout(true);

    m_dataEntryPages = new wxNotebook( this, EPHEMERISDIALOG_DATAENTRYPAGES, wxDefaultPosition, wxDefaultSize );
    m_dataEntryPages->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_mainSizer->Add( m_dataEntryPages, 1, wxALIGN_CENTER | wxALL, 5 );

    m_dateTime = new wxPanel( m_dataEntryPages, EPHEMERISDIALOG_DATETIME, wxDefaultPosition, wxDefaultSize );
    m_dateTime->SetToolTip( wxT( "Set the date and time" ) );
    m_dateTime->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_dataEntryPages->AddPage( m_dateTime, wxT( "Date and Time" ) );

    m_latitudeLongitude = new wxPanel( m_dataEntryPages, EPHEMERISDIALOG_LATITUDELONGITUDE, wxDefaultPosition, wxDefaultSize );
    m_latitudeLongitude->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_dataEntryPages->AddPage( m_latitudeLongitude, wxT( "Latitude and Longitude" ) );

    m_dateTimeSizer = new wxBoxSizer( wxVERTICAL );
    m_dateTime->SetSizer( m_dateTimeSizer );
    m_dateTime->SetAutoLayout( true );

    wxStaticBox* m_dateSizer_StaticBoxObj = new wxStaticBox( m_dateTime, wxID_ANY, wxT( "Date" ) );
    m_dateSizer = new wxStaticBoxSizer( m_dateSizer_StaticBoxObj, wxHORIZONTAL );
    m_dateTimeSizer->Add( m_dateSizer, 1, wxALIGN_CENTER | wxALL, 5 );

    wxDateTime dt;
    dt.SetToCurrent();
    m_calendar = new wxCalendarCtrl( m_dateTime, EPHEMERISDIALOG_CALENDAR, dt,
                                     wxDefaultPosition, wxDefaultSize,
                                     wxCAL_SUNDAY_FIRST | wxCAL_SHOW_HOLIDAYS | wxCAL_SHOW_SURROUNDING_WEEKS | wxCAL_SEQUENTIAL_MONTH_SELECTION );
    m_calendar->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_dateSizer->Add( m_calendar, 1, wxALIGN_CENTER | wxALL, 5 );

    wxStaticBox* m_timeSizer_StaticBoxObj = new wxStaticBox( m_dateTime, wxID_ANY, wxT( "Time" ) );
    m_timeSizer = new wxStaticBoxSizer( m_timeSizer_StaticBoxObj, wxHORIZONTAL );
    m_dateTimeSizer->Add( m_timeSizer, 0, wxALIGN_CENTER | wxALL, 5 );

    wxString hourString;
    hourString << dt.GetHour();
    m_hour = new wxSpinCtrl( m_dateTime, EPHEMERISDIALOG_HOUR,
                             hourString, wxDefaultPosition,
                             wxDefaultSize, wxSP_ARROW_KEYS | wxSP_WRAP, 0, 23, 12 );
    m_hour->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_timeSizer->Add( m_hour, 1, wxALIGN_CENTER | wxALL, 5 );

    m_hourColon = new wxStaticText( m_dateTime, EPHEMERISDIALOG_HOURCOLON, wxT( ":" ), wxDefaultPosition, wxDefaultSize, 0 );
    m_hourColon->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_timeSizer->Add( m_hourColon, 0, wxALIGN_CENTER | wxALL, 5 );

    wxString minutesString;
    minutesString << dt.GetMinute();
    m_minutes = new wxSpinCtrl( m_dateTime, EPHEMERISDIALOG_MINUTES,
                                minutesString, wxDefaultPosition,
                                wxDefaultSize,
                                wxSP_ARROW_KEYS | wxSP_WRAP, 0, 59, 0 );
    m_minutes->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_timeSizer->Add( m_minutes, 1, wxALIGN_CENTER | wxALL, 5 );
    m_lastMinute = m_minutes->GetValue();


    m_latLongSizer = new wxBoxSizer( wxVERTICAL );
    m_latitudeLongitude->SetSizer( m_latLongSizer );
    m_latitudeLongitude->SetAutoLayout( true );

    wxStaticBox* m_latitudeSizer_StaticBoxObj = new wxStaticBox( m_latitudeLongitude, wxID_ANY, wxT( "Latitude" ) );
    m_latitudeSizer = new wxStaticBoxSizer( m_latitudeSizer_StaticBoxObj, wxHORIZONTAL );
    m_latLongSizer->Add( m_latitudeSizer, 1, wxALIGN_CENTER | wxALL, 5 );

    m_latDegrees = new wxSpinCtrl( m_latitudeLongitude, EPHEMERISDIALOG_LATDEGREES, wxT( "0" ), wxDefaultPosition, wxDefaultSize,
                                   wxSP_ARROW_KEYS | wxSP_WRAP, 0, 90, 0 );
    m_latDegrees->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_latitudeSizer->Add( m_latDegrees, 1, wxALIGN_CENTER | wxALL, 5 );

    m_degreeSymbol = new wxStaticText( m_latitudeLongitude, EPHEMERISDIALOG_DEGREESYMBOL, wxT( "o" ), wxDefaultPosition, wxDefaultSize );
    m_degreeSymbol->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_latitudeSizer->Add( m_degreeSymbol, 0, wxALIGN_CENTER | wxALL, 5 );

    m_latitudeMinutes = new wxSpinCtrl( m_latitudeLongitude, EPHEMERISDIALOG_LATITUDEMINUTES, wxT( "0" ),
                                        wxDefaultPosition, wxDefaultSize, wxSP_WRAP | wxSP_ARROW_KEYS,
                                        0, 60, 0 );
    m_latitudeMinutes->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_latitudeSizer->Add( m_latitudeMinutes, 1, wxALIGN_CENTER | wxALL, 5 );

    m_minutesSymbol = new wxStaticText( m_latitudeLongitude, EPHEMERISDIALOG_MINUTESSYMBOL, wxT( "\"" ),
                                        wxDefaultPosition, wxDefaultSize );
    m_minutesSymbol->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_latitudeSizer->Add( m_minutesSymbol, 0, wxALIGN_CENTER | wxALL, 5 );

    /* North
    South
    */
    wxArrayString arrayStringFor_m_latHemisphere;
    arrayStringFor_m_latHemisphere.Add( wxT( "North" ) );
    arrayStringFor_m_latHemisphere.Add( wxT( "South" ) );
    m_latHemisphere = new wxChoice( m_latitudeLongitude, EPHEMERISDIALOG_LATHEMISPHERE,
                                    wxDefaultPosition, wxDefaultSize,
                                    arrayStringFor_m_latHemisphere );
    m_latHemisphere->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_latHemisphere->SetSelection( 0 );
    m_latitudeSizer->Add( m_latHemisphere, 1, wxALIGN_CENTER | wxALL, 5 );

    wxStaticBox* m_longitudeSizer_StaticBoxObj = new wxStaticBox( m_latitudeLongitude, wxID_ANY, wxT( "Longitude" ) );
    m_longitudeSizer = new wxStaticBoxSizer( m_longitudeSizer_StaticBoxObj, wxHORIZONTAL );
    m_latLongSizer->Add( m_longitudeSizer, 1, wxALIGN_CENTER | wxALL, 5 );

    m_longitudeDegree = new wxSpinCtrl( m_latitudeLongitude, EPHEMERISDIALOG_LONGITUDEDEGREE, wxT( "0" ),
                                        wxDefaultPosition, wxDefaultSize,
                                        wxSP_WRAP, 0, 180, 0 );
    m_longitudeDegree->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_longitudeSizer->Add( m_longitudeDegree, 1, wxALIGN_CENTER | wxALL, 5 );

    m_degreeLonSymbol = new wxStaticText( m_latitudeLongitude, EPHEMERISDIALOG_DEGREELONSYMBOL, wxT( "o" ),
                                          wxDefaultPosition, wxDefaultSize );
    m_degreeLonSymbol->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_longitudeSizer->Add( m_degreeLonSymbol, 0, wxALIGN_CENTER | wxALL, 5 );

    m_longitudeMinutes = new wxSpinCtrl( m_latitudeLongitude, EPHEMERISDIALOG_LONGITUDEMINUTES, wxT( "0" ),
                                         wxDefaultPosition, wxDefaultSize,
                                         wxSP_WRAP, 0, 60, 0 );
    m_longitudeMinutes->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_longitudeSizer->Add( m_longitudeMinutes, 1, wxALIGN_CENTER | wxALL, 5 );

    m_lonMinutesSymbol = new wxStaticText( m_latitudeLongitude, EPHEMERISDIALOG_LONMINUTESSYMBOL, wxT( "\"" ),
                                           wxDefaultPosition, wxDefaultSize );

    m_lonMinutesSymbol->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_longitudeSizer->Add( m_lonMinutesSymbol, 0, wxALIGN_CENTER | wxALL, 5 );

    wxStaticBox* locationOptions = new wxStaticBox(m_latitudeLongitude, wxID_ANY, wxT("Locations"));
    wxStaticBoxSizer* locationButtonSizer = new wxStaticBoxSizer(locationOptions, wxHORIZONTAL);
    m_latLongSizer->Add(locationButtonSizer, 1, wxALIGN_CENTER | wxALL, 5);
    m_saveLocationButton = new wxButton(m_latitudeLongitude, 
                                            EPHEMERISDIALOG_SAVE_LOCATION_BUTTON,
                                            wxT("Save..."));
    locationButtonSizer->Add(m_saveLocationButton, 1, wxALIGN_CENTER | wxALL, 5);
    m_loadLocationButton = new wxButton(m_latitudeLongitude, 
                                            EPHEMERISDIALOG_LOAD_LOCATION_BUTTON,
                                            wxT("Load..."));
    locationButtonSizer->Add(m_loadLocationButton, 1, wxALIGN_CENTER | wxALL, 5);
    wxStaticBox* heightFieldFile = new wxStaticBox(m_latitudeLongitude, wxID_ANY, wxT("Terrain Map"));
    wxStaticBoxSizer* heightFieldSizer = new wxStaticBoxSizer(heightFieldFile, wxHORIZONTAL);
    m_latLongSizer->Add(heightFieldSizer, 1, wxALIGN_CENTER | wxALL, 5);
    m_heightMapSelector = new wxFilePickerCtrl( m_latitudeLongitude, 
                                                      EPHEMERISDIALOG_LOAD_HEIGHT_MAP,
                                                      ::wxGetCwd(),
                                                      wxT("Height Field"),
                                                      wxT("Image Files (*.bmp;*.BMP;.gif;*.GIF;*.jpg;*.JPG;*.jpeg;*.JPEG;*.png;*.PNG;*.rgb;*.RGB)|*.bmp;*.BMP;.gif;*.GIF;*.jpg;*.JPG;*.jpeg;*.JPEG;*.png;*.PNG;*.rgb;*.RGB"));
        heightFieldSizer->Add(m_heightMapSelector, 1, wxALIGN_CENTER | wxALL, 5);
    wxArrayString arrayStringFor_m_lonHemisphere;
    arrayStringFor_m_lonHemisphere.Add( wxT( "East" ) );
    arrayStringFor_m_lonHemisphere.Add( wxT( "West" ) );
    m_lonHemisphere = new wxChoice( m_latitudeLongitude, EPHEMERISDIALOG_LONHEMISPHERE,
                                    wxDefaultPosition, wxDefaultSize,
                                    arrayStringFor_m_lonHemisphere );
    m_lonHemisphere->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_lonHemisphere->SetSelection( 0 );
    m_longitudeSizer->Add( m_lonHemisphere, 1, wxALIGN_CENTER | wxALL, 5 );

    m_buttonsSizer = new wxBoxSizer( wxHORIZONTAL );
    m_mainSizer->Add( m_buttonsSizer, 0, wxALIGN_CENTER | wxALL, 5 );

    m_ephemerisToggleCheck = new wxCheckBox( this, EPHEMERISDIALOG_TOGGLE_EPHEMERIS,
                                             wxT( "Display Ephemeris Data" ) );

    m_ephemerisToggleCheck->SetFont( wxFont( 9, wxSWISS, wxNORMAL, 
                                             wxNORMAL, false,
                                             wxT( "Segoe UI" ) ) );
    m_ephemerisToggleCheck->SetValue(true);
    m_buttonsSizer->Add( m_ephemerisToggleCheck, 0, wxALIGN_CENTER | wxALL, 5 );

    m_cancel = new wxButton( this, wxID_CANCEL, wxT( "Cancel" ),
                             wxDefaultPosition, wxDefaultSize );

    m_cancel->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );

    m_buttonsSizer->Add( m_cancel, 0, wxALIGN_CENTER | wxALL, 5 );

    m_close = new wxButton( this, wxID_OK, wxT( "OK" ),
                            wxDefaultPosition, wxDefaultSize );
    m_close->SetFont( wxFont( 9, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Segoe UI" ) ) );
    m_buttonsSizer->Add( m_close, 0, wxALIGN_CENTER | wxALL, 5 );

    /*wxArrayString arrayStringFor_m_amPm;
    arrayStringFor_m_amPm.Add(wxT("AM"));
    arrayStringFor_m_amPm.Add(wxT("PM"));
    m_amPm = new wxChoice(m_dateTime, EPHEMERISDIALOG_AMPM,
                                 wxDefaultPosition, wxDefaultSize,
                                 arrayStringFor_m_amPm);
    m_amPm->SetToolTip(wxT("AM/PM"));
    m_amPm->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
    m_amPm->SetSelection(0);
    m_timeSizer->Add(m_amPm,2,wxALIGN_CENTER | wxALL,5);
           m_amPm->Enable(false);*/
    m_autoDateTime = new wxCheckBox( m_dateTime, EPHEMERISDIALOG_AUTO_DATE_TIME,
                                     wxT( "Auto Date/Time" ) );
    m_timeSizer->Add( m_autoDateTime, 2, wxALIGN_CENTER | wxALL, 5 );

    SetTitle( wxT( "Ephemeris Data" ) );
    SetIcon( wxNullIcon );

    GetSizer()->Layout();
    GetSizer()->Fit( this );
    GetSizer()->SetSizeHints( this );
    Center();
    SetAutoLayout( true );

    ////GUI Items Creation End
    m_date = CommandPtr( new Command() );
    m_date->SetCommandName( "Ephemeris Date" );

    //m_date->AddDataValuePair(new ves::open::xml::DataValuePair("Day",
    //                                                        m_calendar->GetDate().Get);

    m_time = CommandPtr( new ves::open::xml::Command() );

    m_latitudeDecimalValue = DataValuePairPtr( new ves::open::xml::DataValuePair() );
    m_latitudeDecimalValue->SetData( "Latitude", 0.0 );

    m_latitudeDirectionValue = DataValuePairPtr( new ves::open::xml::DataValuePair());
    m_latitudeDirectionValue->SetData( "Latitude Direction", "North" );

    m_longitudeDecimalValue = DataValuePairPtr( new ves::open::xml::DataValuePair());
    m_longitudeDecimalValue->SetData( "Longitude", 0.0 );

    m_longitudeDirectionValue = DataValuePairPtr( new ves::open::xml::DataValuePair());
    m_longitudeDirectionValue->SetData( "Longitude Direction", "East" );

    m_dateAndTimeInfo = DataValuePairPtr( new ves::open::xml::DataValuePair());
    UpdateDateAndTimeInfo();
    ReadLocationInformation();
    m_heightMapInfo = DataValuePairPtr( new ves::open::xml::DataValuePair());
    m_heightMapInfo->SetData( "Height Map", std::string( "Default" ) );
}
//////////////////////////////////////////////////////
/*void EphemerisDialog::OnClose( wxCloseEvent& event )
{
    Destroy();
}*/
//////////////////////////////////////////////////////////////
void EphemerisDialog::OnToggleDisplay( wxCommandEvent& event )
{
    //Create the command and data value pairs
    ves::open::xml::DataValuePairPtr DVP( new DataValuePair() );
    ves::open::xml::CommandPtr command( new Command() );
    bool value = false;
    value = m_ephemerisToggleCheck->IsChecked();
    unsigned int displayValue = 0;
    if( value )
    {
        displayValue = 1;
    }

    DVP->SetData( std::string( "Display Ephemeris Data" ), displayValue );
    command->SetCommandName( std::string( "Ephemeris Toggle" ) );
    command->AddDataValuePair( DVP );
    ves::conductor::UserPreferencesDataBuffer::instance()->SetCommand(
        command->GetCommandName(), command );
    ves::conductor::util::CORBAServiceList::instance()->
        SendCommandStringToXplorer( command );
}
//////////////////////////////////////////////////////////////////
void EphemerisDialog::SetLatitudeAndLongitudeOnGUI( double latitude,
                                                    double longitude )
{
    m_lonHemisphere->SetSelection( 0 );
    m_latHemisphere->SetSelection( 0 );

    if( latitude < 0 )
    {
        m_latHemisphere->SetSelection( 1 );
        latitude *= -1;
    }
    if( longitude < 0 )
    {
        m_lonHemisphere->SetSelection( 1 );
        longitude *= -1;
    }
    double latitudeDegrees = 0.0;
    double longitudeDegrees = 0.0;
    double latitudeMinutes = 0.0;
    double longitudeMinutes = 0.0;
    latitudeMinutes = 60 * modf( latitude, &latitudeDegrees );
    longitudeMinutes = 60 * modf( longitude, &longitudeDegrees );

    //std::cout<<"Latitude:"<<latitude<<std::endl;
    //std::cout<<"Latitude degrees:"<<latitudeDegrees<<std::endl;
    //std::cout<<"Longitude:"<<longitude<<std::endl;
    //std::cout<<"Longitude degrees:"<<longitudeDegrees<<std::endl;
    //std::cout<<"Latitude minutes:"<<latitudeMinutes<<std::endl;
    //std::cout<<"Longitude minutes:"<<longitudeMinutes<<std::endl;

    m_latDegrees->SetValue( static_cast<int>( latitudeDegrees ) );
    m_latitudeMinutes->SetValue( static_cast<int>( latitudeMinutes ) );
    m_longitudeDegree->SetValue( static_cast<int>( longitudeDegrees ) );
    m_longitudeMinutes->SetValue( static_cast<int>( longitudeMinutes ) );
    UpdateLatitudeInfo();
    UpdateLongitudeInfo();
}
///////////////////////////////////////////////////////////////
double
EphemerisDialog::ConvertSexagesimalComponentToDecimal( int minutesOrSeconds,
                                                       bool isMinutes )
{
    return ( isMinutes ) ? double( minutesOrSeconds ) / 60.0 : double( minutesOrSeconds ) / 3600.0;
}
///////////////////////////////////////////////////////////////////
double EphemerisDialog::ConvertSexagesimalToDecimal( int degree,
                                                     int minutes,
                                                     int seconds )
{
    double degreeConversion = degree;
    double minConversion = double( minutes ) / 60.0;
    double secondsConversion = double( seconds ) / 3600.0;
    return degreeConversion + minConversion + secondsConversion;
}
//////////////////////////////////////////////////////////////////
void EphemerisDialog::OnChangeLatitudeMinutes( wxSpinEvent& event )
{
    UpdateLatitudeInfo();
}
//////////////////////////////////////////////////////////////////
void EphemerisDialog::OnChangeLatitudeDegrees( wxSpinEvent& event )
{
    UpdateLatitudeInfo();
}
///////////////////////////////////////////////////////////////////
void EphemerisDialog::OnChangeLongitudeMinutes( wxSpinEvent& event )
{
    UpdateLongitudeInfo();
}
///////////////////////////////////////////////////////////////////
void EphemerisDialog::OnChangeLongitudeDegrees( wxSpinEvent& event )
{
    UpdateLongitudeInfo();
}
////////////////////////////////////////////////////////////
void EphemerisDialog::OnAmPmSelected( wxCommandEvent& event )
{
    UpdateDateAndTimeInfo();
    UpdateEphemerisData();
}
////////////////////////////////////////////////////////////
void EphemerisDialog::OnLatitudeDirection( wxCommandEvent& event )
{
    UpdateLatitudeInfo();
}
////////////////////////////////////////////////////////////
void EphemerisDialog::OnLongitudeDirection( wxCommandEvent& event )
{
    UpdateLongitudeInfo();
}
///////////////////////////////////////////////////////////
void EphemerisDialog::OnCalendarDay( wxCalendarEvent& event )
{
    UpdateDateAndTimeInfo();
    UpdateEphemerisData();
}
////////////////////////////////////////////////////////////////
void EphemerisDialog::OnMinuteTextUpdated( wxCommandEvent& event )
{
    int currentMinute =  m_minutes->GetValue();
    int currentHour =  m_hour->GetValue();
    int hourChange = 0;
    if( currentMinute == 0 && m_lastMinute == 59 )
    {
        hourChange = 1;
    }
    else if( currentMinute == 59 && m_lastMinute == 0 )
    {
        hourChange = -1;
    }
    m_lastMinute = currentMinute;

    EnsureHour( hourChange );
    UpdateDateAndTimeInfo();
    UpdateEphemerisData();
}
///////////////////////////////////////////////////////////////
void EphemerisDialog::OnHourTextUpdated( wxCommandEvent& event )
{
    UpdateDateAndTimeInfo();
    UpdateEphemerisData();
}
////////////////////////////////////////////////////////////
void EphemerisDialog::OnChangeTimeOfDay( wxTimerEvent& event )
{
    UpdateDateAndTimeInfo();
    UpdateEphemerisData();
}
/////////////////////////////////////////////////////////
void EphemerisDialog::OnAutoDateTime( wxCommandEvent& event )
{
    ToggleCalendarAndTimerState( !m_autoDateTime->IsChecked() );
    UpdateAutoDateTime( m_autoDateTime->IsChecked() );
}
////////////////////////////////////////////////
void EphemerisDialog::EnsureHour( int hourChange )
{
    int currentHour =  m_hour->GetValue();
    if( currentHour == 23 && hourChange > 0 )
    {
        m_hour->SetValue( 0 );
    }
    else if( currentHour == 0 && hourChange < 0 )
    {
        m_hour->SetValue( 23 );
    }
    else
    {
        m_hour->SetValue( currentHour + hourChange );
    }


}
/////////////////////////////////////////////
void EphemerisDialog::UpdateDateAndTimeInfo()
{
    ves::open::xml::OneDIntArrayPtr dateAndTime( 
        new ves::open::xml::OneDIntArray() );
    wxDateTime dt = m_calendar->GetDate();

    dateAndTime->AddElementToArray( dt.GetYear() );
    dateAndTime->AddElementToArray( dt.GetMonth() );
    dateAndTime->AddElementToArray( dt.GetDay() );
    dateAndTime->AddElementToArray( m_hour->GetValue() );
    dateAndTime->AddElementToArray( m_minutes->GetValue() );
    m_dateAndTimeInfo->SetData( "Date and Time Info",
                                dateAndTime );

}
///////////////////////////////////////////
void EphemerisDialog::UpdateLongitudeInfo()
{
    m_longitudeDecimalValue->SetData( "Longitude",
                                      ConvertSexagesimalToDecimal(
                                          m_longitudeDegree->GetValue(),
                                          m_longitudeMinutes->GetValue(),
                                          /*m_longitudeDegree->GetValue()*/0 ) );
    m_longitudeDirectionValue->SetData( "Longitude Direction",
                                        ConvertUnicode( m_lonHemisphere->GetStringSelection().c_str() ) );
    UpdateEphemerisData();
}
//////////////////////////////////////////
void EphemerisDialog::UpdateLatitudeInfo()
{
    m_latitudeDecimalValue->SetData( "Latitude",
                                     ConvertSexagesimalToDecimal(
                                         m_latDegrees->GetValue(),
                                         m_latitudeMinutes->GetValue(),
                                         /*m_longitudeDegree->GetValue()*/0 ) );
    m_latitudeDirectionValue->SetData( "Latitude Direction",
                                       ConvertUnicode( m_latHemisphere->GetStringSelection().c_str() ) );
    UpdateEphemerisData();
}
///////////////////////////////////////////
void EphemerisDialog::UpdateEphemerisData()
{
    CommandPtr ephemerisData( new Command() );
    ephemerisData->SetCommandName( "Ephemeris Data" );
    ephemerisData->AddDataValuePair( m_latitudeDecimalValue );
    ephemerisData->AddDataValuePair( m_latitudeDirectionValue );
    ephemerisData->AddDataValuePair( m_longitudeDecimalValue );
    ephemerisData->AddDataValuePair( m_longitudeDirectionValue );
    ephemerisData->AddDataValuePair( m_dateAndTimeInfo );
    ves::conductor::UserPreferencesDataBuffer::instance()
    ->SetCommand( ephemerisData->GetCommandName(),
                  ephemerisData ) ;
    ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( ephemerisData );
}
//////////////////////////////////////////////////////////////
void EphemerisDialog::UpdateAutoDateTime( bool useAutoDateTime )
{
    CommandPtr ephemerisAutoDateTime( new Command() );
    ephemerisAutoDateTime->SetCommandName( "Ephemeris Auto Date and Time" );
    DataValuePairPtr autoDateTime( new ves::open::xml::DataValuePair() );
    autoDateTime->SetData( "Auto Date Time", static_cast<long int>(( useAutoDateTime ) ? 1 : 0 ) );
    ephemerisAutoDateTime->AddDataValuePair( autoDateTime );

    ves::conductor::UserPreferencesDataBuffer::instance()
    ->SetCommand( ephemerisAutoDateTime->GetCommandName(),
                  ephemerisAutoDateTime );
    ves::conductor::util::CORBAServiceList::instance()
    ->SendCommandStringToXplorer( ephemerisAutoDateTime );

    if( !useAutoDateTime )
    {
        UpdateEphemerisData();
    }
}
/////////////////////////////////////////////////////////////
void EphemerisDialog::ToggleCalendarAndTimerState( bool onOff )
{
    m_calendar->Enable( onOff );
    m_hour->Enable( onOff );
    m_minutes->Enable( onOff );
}
//////////////////////////////////////////////////////////////////////
void EphemerisDialog::OnLoadLocationInformation( wxCommandEvent& event )
{
    if( m_storedLocations.size() == 0 )
    {
        return;
    }
    
    wxArrayString locations;
    for( std::map<std::string, ves::open::xml::CommandPtr>::iterator iter = 
        m_storedLocations.begin(); iter != m_storedLocations.end(); ++iter )
    {
        locations.Add( wxString( iter->first.c_str(), wxConvUTF8 ) );
    }

    wxSingleChoiceDialog selectedLocation( this, wxT( "Select location to load." ),
                                           wxT( "Load Location" ),
                                           locations );
    selectedLocation.CentreOnParent();
    if( selectedLocation.ShowModal() == wxID_OK )
    {
        std::string location =
            ConvertUnicode( selectedLocation.GetStringSelection().GetData() );
            
        std::map<std::string, ves::open::xml::CommandPtr>::iterator 
            foundLocation = m_storedLocations.find( location );
            
        if( foundLocation == m_storedLocations.end() )
        {
            return;
        }

        double latitude = 90.89;
        double longitude = 89.;
        std::string eastWest = "East";
        std::string northSouth = "North";
        std::string heightMap = "Default";
        unsigned int index = 0;
        foundLocation->second->GetDataValuePair( "Latitude" )->GetData( latitude );
        foundLocation->second->GetDataValuePair( "Latitude Direction" )->GetData( northSouth );
        if( northSouth == "South" )
        {
            latitude = -1 * latitude;
        }
        
        foundLocation->second->GetDataValuePair( "Longitude" )->GetData( longitude );
        foundLocation->second->GetDataValuePair( "Longitude Direction" )->GetData( eastWest );
        if( eastWest == "West" )
        {
            longitude = -1 * longitude;
        }
        
        SetLatitudeAndLongitudeOnGUI( latitude, longitude );
        foundLocation->second->GetDataValuePair( "Height Map" )->GetData( heightMap );
        m_heightMapSelector->SetPath( wxString( heightMap.c_str(), wxConvUTF8 ) );
        wxFileDirPickerEvent emptyEvent;
        OnLoadHeightMap(emptyEvent);
    }
}
//////////////////////////////////////////////////////////////////////
void EphemerisDialog::OnSaveLocationInformation( wxCommandEvent& event )
{
    //pop a text dialog to enter the name of the new assembly
    wxString defaultName;
    int numLocations = m_storedLocations.size();
    defaultName = wxString( wxT( "Location" ) ) + wxString::Format( wxT( "%d" ), numLocations );
    wxTextEntryDialog locationNameDlg( this,
                                       _( "New Location Name" ),
                                       _( "Enter name for new location:" ),
                                       _( "Location" ), wxOK );
    locationNameDlg.CentreOnParent();
    locationNameDlg.ShowModal();
    CommandPtr ephemerisData( new Command() );
    ephemerisData->SetCommandName( ConvertUnicode( locationNameDlg.GetValue().GetData() ) );
    ephemerisData->AddDataValuePair( DataValuePairPtr( new ves::open::xml::DataValuePair( *m_latitudeDecimalValue ) ) );
    ephemerisData->AddDataValuePair( DataValuePairPtr( new ves::open::xml::DataValuePair( *m_latitudeDirectionValue ) ) );
    ephemerisData->AddDataValuePair( DataValuePairPtr( new ves::open::xml::DataValuePair( *m_longitudeDecimalValue ) ) );
    ephemerisData->AddDataValuePair( DataValuePairPtr( new ves::open::xml::DataValuePair( *m_longitudeDirectionValue ))  );
    ephemerisData->AddDataValuePair( DataValuePairPtr( new ves::open::xml::DataValuePair( *m_heightMapInfo ) ) );
    // ephemerisData->AddDataValuePair(new ves::open::xml::DataValuePair(*m_dateAndTimeInfo));
    m_storedLocations[ephemerisData->GetCommandName()] = ephemerisData;
}
///////////////////////////////////////////////
void EphemerisDialog::ReadLocationInformation()
{
    m_storedLocations.clear();
    wxConfig* config = dynamic_cast<wxConfig*>( wxConfig::Get() );

    int numLocations = 0;
    wxString oldPath = config->GetPath();

    wxString key = _T( "EphemerisLocation_" ) +
                   wxString::Format( wxT( "%d" ), numLocations );
    while( config->HasGroup( key ) )
    {
        wxString name( key + _T( "None" ) + wxString::Format( wxT( "%d" ) ), numLocations );
        wxString heightField( _T( "Default" ) );
        wxString latitudeDirection( m_latHemisphere->GetStringSelection() );
        wxString longitudeDirection( m_lonHemisphere->GetStringSelection() );
        wxString heightMap( m_heightMapSelector->GetPath() );
        double latitude = 0.0;
        double longitude = 0.0;

        config->Read( key + _T( "/" ) + _T( "Name" ) , &name );
        config->Read( key + _T( "/" ) + _T( "Latitude" ), &latitude );
        config->Read( key + _T( "/" ) + _T( "Longitude" ), &longitude ) ;
        config->Read( key + _T( "/" ) + _T( "Longitude Direction" ), &longitudeDirection ) ;
        config->Read( key + _T( "/" ) + _T( "Latitude Direction" ), &latitudeDirection ) ;
        config->Read( key + _T( "/" ) + _T( "Height Map" ), &heightMap ) ;

        CommandPtr locationData( new Command() );
        locationData->SetCommandName( ConvertUnicode( name.c_str() ) );
        ves::open::xml::DataValuePairPtr latitudeDecimalValue( new ves::open::xml::DataValuePair() );
        latitudeDecimalValue->SetData( "Latitude", latitude );
        locationData->AddDataValuePair( latitudeDecimalValue );

        ves::open::xml::DataValuePairPtr latitudeDirectionValue( new ves::open::xml::DataValuePair() );
        latitudeDirectionValue->SetData( "Latitude Direction",
                                         ConvertUnicode( latitudeDirection.c_str() ) );
        locationData->AddDataValuePair( latitudeDirectionValue );

        ves::open::xml::DataValuePairPtr longitudeDecimalValue(
            new ves::open::xml::DataValuePair() );
        longitudeDecimalValue->SetData( "Longitude", longitude );
        locationData->AddDataValuePair( longitudeDecimalValue );

        ves::open::xml::DataValuePairPtr longitudeDirectionValue(
            new ves::open::xml::DataValuePair() );
        longitudeDirectionValue->SetData( "Longitude Direction",
                                          ConvertUnicode( longitudeDirection.c_str() ) );
        locationData->AddDataValuePair( longitudeDirectionValue );

        ves::open::xml::DataValuePairPtr heightMapData( 
            new ves::open::xml::DataValuePair() );
        heightMapData->SetData( "Height Map",
                                ConvertUnicode( heightMap.c_str() ) );
        locationData->AddDataValuePair( heightMapData );
        m_storedLocations[locationData->GetCommandName()] = locationData;

        numLocations++;
        key = _T( "EphemerisLocation_" ) +
              wxString::Format( wxT( "%d" ), numLocations );
    }
    config->SetPath( oldPath );
}
////////////////////////////////////////////////
void EphemerisDialog::WriteLocationInformation()
{
    wxConfig* config  = dynamic_cast<wxConfig*>( wxConfig::Get() );
    int numLocations = 0;
    for( std::map< std::string, ves::open::xml::CommandPtr >::iterator iter = 
        m_storedLocations.begin(); iter != m_storedLocations.end(); ++iter )
    {
        wxString key = _T( "EphemerisLocation_" ) +
            wxString::Format( wxT( "%d" ), numLocations );
        config->Write( key + _T( "/" ) + _T( "Name" ),
            wxString( iter->second->GetCommandName().c_str(), wxConvUTF8 ) );
        _writeLocation( iter->second, key );
        numLocations++;
    }
}
////////////////////////////////////////////////////////////////////////////
void EphemerisDialog::_writeLocation( ves::open::xml::CommandPtr location,
                                      wxString keySection )
{
    wxConfig* config  = dynamic_cast<wxConfig*>( wxConfig::Get() );
    size_t nDvps = location->GetNumberOfDataValuePairs();
    for( size_t i = 0; i < nDvps; ++i )
    {
        ves::open::xml::DataValuePairPtr locationInfo = 
            location->GetDataValuePair( i );
        if (( locationInfo->GetDataName() == std::string( "Latitude" ) ) ||
                ( locationInfo->GetDataName() == std::string( "Longitude" ) ) )
        {
            config->Write( keySection +
                           _T( "/" ) +
                           wxString( locationInfo->GetDataName().c_str(),
                                     wxConvUTF8 ),
                           locationInfo->GetDataValue() );
        }
        else
        {
            config->Write( keySection +
                           _T( "/" ) +
                           wxString( locationInfo->GetDataName().c_str(),
                                     wxConvUTF8 ),
                           wxString( locationInfo->GetDataString().c_str(),
                                     wxConvUTF8 ) );
        }
    }
}
//////////////////////////////////////////////////////////////////
void EphemerisDialog::OnLoadHeightMap( wxFileDirPickerEvent& event )
{
    m_heightMapInfo->SetData( "Height Map", 
        ConvertUnicode( m_heightMapSelector->GetPath().c_str() ) );
    CommandPtr ephemerisHeightMapInfo( new Command() );
    ephemerisHeightMapInfo->SetCommandName( "Ephemeris Height Map" );

    DataValuePairPtr heightMapInfo( new ves::open::xml::DataValuePair() );
    heightMapInfo->SetData( "Height Map", 
        ConvertUnicode( m_heightMapSelector->GetPath().c_str() ) );
    ephemerisHeightMapInfo->AddDataValuePair( heightMapInfo );

    ves::conductor::UserPreferencesDataBuffer::instance()->SetCommand( 
        ephemerisHeightMapInfo->GetCommandName(), ephemerisHeightMapInfo );
    ves::conductor::util::CORBAServiceList::instance()->
        SendCommandStringToXplorer( ephemerisHeightMapInfo );
}
