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
#ifndef __EPHEMERISDIALOG_h__
#define __EPHEMERISDIALOG_h__

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#include <wx/dialog.h>
#else
#include <wx/wxprec.h>
#endif

//Do not add custom headers between
//Header Include Start and Header Include End.
//wxDev-C++ designer will remove them. Add custom headers after the block.
////Header Include Start
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/stattext.h>
#include <wx/spinctrl.h>
#include <wx/calctrl.h>
#include <wx/panel.h>
#include <wx/notebook.h>
#include <wx/sizer.h>
#include <wx/filepicker.h>

////Header Include End
#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <string>
#include <map>

#include <ves/VEConfig.h>

////Dialog Style Start
#undef EphemerisDialog_STYLE
#define EphemerisDialog_STYLE wxCAPTION | wxRESIZE_BORDER | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxCLOSE_BOX
////Dialog Style End

class VE_GUIPLUGINS_EXPORTS EphemerisDialog : public wxDialog
{
private:
    DECLARE_EVENT_TABLE();

public:
    EphemerisDialog( wxWindow *parent, wxWindowID id = 1, 
        const wxString &title = wxT( "Ephemeris Data" ), 
        const wxPoint& pos = wxDefaultPosition, 
        const wxSize& size = wxDefaultSize, 
        long style = EphemerisDialog_STYLE );
    virtual ~EphemerisDialog();
    void OnChangeLongitudeMinutes( wxSpinEvent& event );
    void OnChangeLongitudeDegrees( wxSpinEvent& event );
    void OnChangeLatitudeMinutes( wxSpinEvent& event );
    void OnChangeLatitudeDegrees( wxSpinEvent& event );
    void OnLatitudeDirection( wxCommandEvent& event );
    void OnLongitudeDirection( wxCommandEvent& event );

    void OnChangeTimeOfDay( wxTimerEvent& event );
    void OnAmPmSelected( wxCommandEvent& event );
    void OnHourTextUpdated( wxCommandEvent& event );
    void OnCalendarDay( wxCalendarEvent& event );
    void OnMinuteTextUpdated( wxCommandEvent& event );
    void OnAutoDateTime( wxCommandEvent& event );
    void OnSaveLocationInformation( wxCommandEvent& event );
    void OnLoadLocationInformation( wxCommandEvent& event );
    void OnLoadHeightMap( wxFileDirPickerEvent& event );
    void OnToggleDisplay( wxCommandEvent& event );

    ///Convert the input sexagesimal value to decimal values
    ///\param degree Degree of Latitude or Longitude
    ///\param minutes Minutes of Latitude or Longitude
    ///\param seconds Seconds of Latitude or Longitude
    double ConvertSexagesimalToDecimal( int degree,
                                        int minutes,
                                        int seconds );


    ///Convert the input sexagesimal value to decimal values
    ///\param minSecs Minutes or seconds of Latitude or Longitude
    ///\param isMinutes Converts minutes or seconds
    double ConvertSexagesimalComponentToDecimal( int minSecs,
                                                 bool isMinutes = true );

    ///Update the longitude info
    void UpdateLongitudeInfo();
    ///Update the latitude info
    void UpdateLatitudeInfo();
    ///Send the ephemeris data to Xplorer
    void UpdateEphemerisData();

    ///Update the date and time information
    void UpdateDateAndTimeInfo();

    ///Make sure the hour is correct
    ///\param hourChange Negative or Positive hour change
    void EnsureHour( int hourChange );

    ///Update the Auto Date And Time of osgEphemerisModel
    ///\param useAutoDateTime Flag triggering auto date/time usage
    void UpdateAutoDateTime( bool useAutoDateTime );

    ///Toggle on/off controls for date and time setting
    void ToggleCalendarAndTimerState( bool onOff );

    ///Load the location selected
    //void LoadLocationInformation();

    ///Read the location saved location information
    void ReadLocationInformation();

    ///Write the location data to wxconfig
    void WriteLocationInformation();

    ///Use decimal values to set the GUI
    ///\param latitude The decimal latitude
    ///\param longitude The decimal ongitude
    void SetLatitudeAndLongitudeOnGUI( double latitude, double longitude );

private:
    ///Write the location information to config block
    ///\param location The location information
    ///\param  key The block to write the location information
    void _writeLocation( ves::open::xml::CommandPtr location, wxString key );
    //Do not add custom control declarations between
    //GUI Control Declaration Start and GUI Control Declaration End.
    //wxDev-C++ will remove them. Add custom code after the block.
    ////GUI Control Declaration Start
    wxChoice* m_amPm;
    wxButton* m_close;
    wxButton* m_cancel;
    wxBoxSizer* m_buttonsSizer;
    wxChoice* m_lonHemisphere;
    wxStaticText* m_lonMinutesSymbol;
    wxSpinCtrl* m_longitudeMinutes;
    wxStaticText* m_degreeLonSymbol;
    wxSpinCtrl* m_longitudeDegree;
    wxStaticBoxSizer* m_longitudeSizer;
    wxChoice* m_latHemisphere;
    wxStaticText* m_minutesSymbol;
    wxSpinCtrl* m_latitudeMinutes;
    wxStaticText* m_degreeSymbol;
    wxSpinCtrl* m_latDegrees;
    wxStaticBoxSizer* m_latitudeSizer;
    wxBoxSizer* m_latLongSizer;
    wxSpinCtrl* m_minutes;
    wxStaticText* m_hourColon;
    wxSpinCtrl* m_hour;
    wxStaticBoxSizer* m_timeSizer;
    wxCalendarCtrl* m_calendar;
    wxStaticBoxSizer* m_dateSizer;
    wxBoxSizer* m_dateTimeSizer;
    wxPanel* m_latitudeLongitude;
    wxPanel* m_dateTime;
    wxNotebook* m_dataEntryPages;
    wxBoxSizer* m_mainSizer;
    ////GUI Control Declaration End
    wxCheckBox* m_autoDateTime;
    wxButton* m_saveLocationButton;
    wxButton* m_loadLocationButton;
    wxFilePickerCtrl* m_heightMapSelector;
    wxCheckBox* m_ephemerisToggleCheck;

    ves::open::xml::CommandPtr m_date;
    ves::open::xml::CommandPtr m_time;
    ves::open::xml::DataValuePairPtr m_latitudeDecimalValue;
    ves::open::xml::DataValuePairPtr m_latitudeDirectionValue;
    ves::open::xml::DataValuePairPtr m_longitudeDecimalValue;
    ves::open::xml::DataValuePairPtr m_longitudeDirectionValue;
    ves::open::xml::DataValuePairPtr m_dateAndTimeInfo;
    ves::open::xml::DataValuePairPtr m_heightMapInfo;

    ves::open::xml::CommandPtr m_ephemerisData;
    int m_lastMinute;///<The last value of the minutes;

    std::map<std::string, ves::open::xml::CommandPtr> m_storedLocations;///<The saved locations

private:
    //void OnClose( wxCloseEvent& event );
    void CreateGUIControls();

    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }
};

#endif
