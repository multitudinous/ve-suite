/////////////////////////////////////////////////////////////////////////////
// Name:        streamlines.cpp
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Fri 21 Apr 2006 10:45:04 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////


////@begin includes
////@end includes

#include "VE_Conductor/Framework/streamlines.h"
#include "VE_Conductor/Framework/vistab.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"
#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/radiobox.h>
#include <wx/radiobut.h>
#include <wx/slider.h>
#include <wx/icon.h>
#include <wx/msgdlg.h>
#include <iostream>


BEGIN_EVENT_TABLE( Streamlines, wxDialog )
////@begin Streamlines event table entries 
   EVT_RADIOBOX      (CURSOR_RBOX,                 Streamlines::_onCursorSelect)
   EVT_RADIOBOX      (DIRECTION_RBOX,              Streamlines::_onDirection)
   EVT_RADIOBOX      (INTEGRATION_DIR_RBOX,        Streamlines::_onIntegrateDir)
   EVT_COMMAND_SCROLL(NUMBER_PTS_SLIDER,           Streamlines::_onPointsSlider)
   EVT_COMMAND_SCROLL(PLANE_SIZE_SLIDER,           Streamlines::_onSizeSlider)
   EVT_BUTTON        (ADVANCED_STREAMLINE_BUTTON,  Streamlines::_onAdvanced)
   EVT_BUTTON        (COMPUTE_STREAMLINE_BUTTON,  Streamlines::_onCompute)
////@end Streamlines event table entries
END_EVENT_TABLE()

Streamlines::Streamlines( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
    Create(parent, id, caption, pos, size, style);
    wxSize displaySize = ::wxGetDisplaySize();
    wxRect dialogPosition( displaySize.GetWidth()-427, 440, 427, displaySize.GetHeight()-480 );
    this->SetSize( dialogPosition );
}
///////////////////////////////////////////////////////////
bool Streamlines::Create( wxWindow* parent, wxWindowID id, 
                         const wxString& caption, 
                         const wxPoint& pos, 
                         const wxSize& size, long style )
{
////@begin Streamlines member initialisation
   _cursorRBox = 0;
   _directionRBox = 0;
   _integrationRBox = 0;
   _sizeSlider = 0;
   _nPointsSlider = 0;
   _streamlineDirection = "x";
   _integrationDirection = "forward";
   _cursorType = "none";
   _streamSize = .5;
   _nPointsPerPlane = 2;
   itemButton13 = 0;
   itemButton14 = 0;
////@end Streamlines member initialisation

////@begin Streamlines creation
    SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);
    Centre();
////@end Streamlines creation
    return true;
}

/*!
 * Control creation for Streamlines
 */

void Streamlines::CreateControls()
{    
////@begin Streamlines content construction
    // Generated by DialogBlocks, Fri 21 Apr 2006 11:01:46 CDT (unregistered)

    Streamlines* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Streamline Controls"));
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer(itemStaticBoxSizer3Static, wxVERTICAL);
    itemBoxSizer2->Add(itemStaticBoxSizer3, 0, wxGROW|wxALL, 5);

    wxBoxSizer* itemBoxSizer4 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer4, 0, wxGROW|wxALL, 5);

    wxString itemRadioBox5Strings[] = {
        _T("none"),
        _T("point"),
        _T("line"),
        _T("plane")
    };
    _cursorRBox = new wxRadioBox( itemDialog1, CURSOR_RBOX, _T("Cursor Selection"), wxDefaultPosition, wxDefaultSize, 4, itemRadioBox5Strings, 1, wxRA_SPECIFY_COLS );
    itemBoxSizer4->Add(_cursorRBox, 0, wxALIGN_TOP|wxALL, 5);

    wxString itemRadioBox6Strings[] = {
        _T("x"),
        _T("y"),
        _T("z")
    };
    _directionRBox = new wxRadioBox( itemDialog1, DIRECTION_RBOX, _T("Direction"), wxDefaultPosition, wxDefaultSize, 3, itemRadioBox6Strings, 1, wxRA_SPECIFY_COLS );
    itemBoxSizer4->Add(_directionRBox, 0, wxALIGN_TOP|wxALL, 5);

    wxString itemRadioBox7Strings[] = {
        _T("backward"),
        _T("forward"),
        _T("both directions")
    };
    _integrationRBox = new wxRadioBox( itemDialog1, INTEGRATION_DIR_RBOX, _T("Integration Direction"), wxDefaultPosition, wxDefaultSize, 3, itemRadioBox7Strings, 1, wxRA_SPECIFY_COLS );
    itemBoxSizer4->Add(_integrationRBox, 0, wxALIGN_TOP|wxALL, 5);

    wxStaticText* itemStaticText8 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Size(%)"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText8, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    _nPointsSlider = new wxSlider( itemDialog1, NUMBER_PTS_SLIDER, 50, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(_nPointsSlider, 0, wxGROW|wxLEFT|wxRIGHT|wxBOTTOM, 5);

    wxStaticText* itemStaticText10 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Number of Point (Per Plane Direction)"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText10, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    _sizeSlider = new wxSlider( itemDialog1, PLANE_SIZE_SLIDER, 2, 2, 20, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(_sizeSlider, 0, wxGROW|wxLEFT|wxRIGHT|wxBOTTOM, 5);

    wxBoxSizer* itemBoxSizer12 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer12, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxButton* itemButton13 = new wxButton( itemDialog1, COMPUTE_STREAMLINE_BUTTON, _T("Compute Streamline"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer12->Add(itemButton13, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxButton* itemButton14 = new wxButton( itemDialog1, ADVANCED_STREAMLINE_BUTTON, _T("Advanced..."), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer12->Add(itemButton14, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

////@end Streamlines content construction
}

/*!
 * Should we show tooltips?
 */

bool Streamlines::ShowToolTips()
{
    return true;
}

/*!
 * Get bitmap resources
 */

wxBitmap Streamlines::GetBitmapResource( const wxString& name )
{
    // Bitmap retrieval
////@begin Streamlines bitmap retrieval
    wxUnusedVar(name);
    return wxNullBitmap;
////@end Streamlines bitmap retrieval
}

/*!
 * Get icon resources
 */

wxIcon Streamlines::GetIconResource( const wxString& name )
{
    // Icon retrieval
////@begin Streamlines icon retrieval
    wxUnusedVar(name);
    return wxNullIcon;
////@end Streamlines icon retrieval

}
////////////////////////////////////////
void Streamlines::_updateAdvancedSettings()
{
   _advancedSettings.clear();

   /*VE_XML::DataValuePair* contourOpacity = new VE_XML::DataValuePair();
   contourOpacity->SetData("Contour Opacity",_lastOpacity);
   _advancedSettings.push_back(contourOpacity);

   VE_XML::DataValuePair* warpedScale = new VE_XML::DataValuePair();
   warpedScale->SetData("Warped Contour Scale",_lastWarpedScale);
   _advancedSettings.push_back(warpedScale);

   VE_XML::DataValuePair* LODSetting = new VE_XML::DataValuePair();
   LODSetting->SetData("Contour LOD",_lastLOD);
   _advancedSettings.push_back(LODSetting);*/
}
////////////////////////////////////////////////
void Streamlines::_updateStreamlineInformation()
{
   _streamlineInformation.clear();
   VE_XML::DataValuePair* streamlineDirection = new VE_XML::DataValuePair();
   streamlineDirection->SetDataType("STRING");
   streamlineDirection->SetDataName(std::string("Streamline Orientation"));
   streamlineDirection->SetDataString(_streamlineDirection);

   _streamlineInformation.push_back(streamlineDirection);
   
   VE_XML::DataValuePair* cursorSelection = new VE_XML::DataValuePair();
   cursorSelection->SetDataType("STRING");
   cursorSelection->SetDataName(std::string("Cursor Selection"));
   cursorSelection->SetDataString(_cursorType);

   _streamlineInformation.push_back(cursorSelection);

   VE_XML::DataValuePair* integrationDirection = new VE_XML::DataValuePair();
   integrationDirection->SetDataType("STRING");
   integrationDirection->SetDataName(std::string("Integtration Direction"));
   integrationDirection->SetDataString(_integrationDirection);

   _streamlineInformation.push_back(integrationDirection);

   VE_XML::DataValuePair* streamSize = new VE_XML::DataValuePair();
   streamSize->SetData("Size",_streamSize);

   _streamlineInformation.push_back(streamSize);

   VE_XML::DataValuePair* nPointsPerPlane = new VE_XML::DataValuePair();
   nPointsPerPlane->SetDataName("Number Of Points Per Plane");
   nPointsPerPlane->SetDataType("UNSIGNED INT");
   nPointsPerPlane->SetDataValue(_nPointsPerPlane);

   _streamlineInformation.push_back(nPointsPerPlane);
}
//////////////////////////////////////////////////////////
void Streamlines::_onAdvanced( wxCommandEvent& WXUNUSED(event) )
{
   AdvancedStreamlines adStreamline(this,                
                     SYMBOL_ADVANCEDSTREAMLINES_IDNAME, 
                     SYMBOL_ADVANCEDSTREAMLINES_TITLE,
                     SYMBOL_ADVANCEDSTREAMLINES_POSITION,
                     SYMBOL_ADVANCEDSTREAMLINES_SIZE, 
                     SYMBOL_ADVANCEDSTREAMLINES_STYLE );
   adStreamline.ShowModal();
std::cout<<"ADVANCEDSTREAMLINES WORKING"<<std::endl;
}
////////////////////////////////////////////////////////
void Streamlines::_onCompute(wxCommandEvent& WXUNUSED(event))
{
   _updateStreamlineInformation();
   _updateAdvancedSettings();

   VE_XML::Command* newCommand = new VE_XML::Command();
   newCommand->SetCommandName("UPDATE_STREAMLINE_SETTINGS");

   for(size_t i =0; i < _streamlineInformation.size(); i++)
   {
      newCommand->AddDataValuePair(_streamlineInformation.at(i));
   }

   //The advanced settings command
   VE_XML::Command* advancedSettings = new VE_XML::Command();
   advancedSettings->SetCommandName("ADVANCED_STREAMLINE_SETTINGS");
   for(size_t i =0; i < _advancedSettings.size(); i++)
   {
      advancedSettings->AddDataValuePair(_advancedSettings.at(i));
   }
   //dvp representing the advanced settings within the contours information
   VE_XML::DataValuePair* advancedStreamlineSettings = new VE_XML::DataValuePair();
   advancedStreamlineSettings->SetData("Advanced Streamline Settings",advancedSettings);
   newCommand->AddDataValuePair(advancedStreamlineSettings);
   
   try
   {
      dynamic_cast<Vistab*>(GetParent())->SendUpdatedSettingsToXplorer(newCommand);
   }
   catch(...)
   {
      {
         wxMessageBox( "Invalid Parent","Communication Failure", 
            wxOK | wxICON_INFORMATION );
         if(newCommand)
         {
            delete newCommand;
         }
      }
   }
}
//////////////////////////////////////////////////////////
void Streamlines::_onCursorSelect(wxCommandEvent& WXUNUSED(event))
{
   _cursorType = _cursorRBox->GetStringSelection();
}
//////////////////////////////////////////////////////////
void Streamlines::_onDirection(wxCommandEvent& WXUNUSED(event))
{
   _streamlineDirection = _directionRBox->GetStringSelection();
}
//////////////////////////////////////////////////////////
void Streamlines::_onIntegrateDir(wxCommandEvent& WXUNUSED(event))
{
   _integrationDirection = _integrationRBox->GetStringSelection();
}
///////////////////////////////////////////////////////////////
void Streamlines::_onSizeSlider(wxScrollEvent& WXUNUSED(event))
{
   _streamSize = static_cast<double>(_sizeSlider->GetValue()/100.0);
}
//////////////////////////////////////////////////////////
void Streamlines::_onPointsSlider(wxScrollEvent& WXUNUSED(event))
{
   _nPointsPerPlane = _nPointsSlider->GetValue();
}
