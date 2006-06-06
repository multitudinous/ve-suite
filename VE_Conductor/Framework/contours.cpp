/////////////////////////////////////////////////////////////////////////////
// Name:        contours.cpp
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Thu 20 Apr 2006 19:49:24 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////


#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"

#include "VE_Conductor/Framework/contours.h"
#include "VE_Conductor/Framework/vistab.h"
#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/radiobox.h>
#include <wx/radiobut.h>
#include <wx/slider.h>
#include <wx/icon.h>
#include <wx/msgdlg.h>

#include <iostream>

BEGIN_EVENT_TABLE( Contours, wxDialog )
////@begin Contours event table entries
   EVT_RADIOBOX      (CONTOUR_DIR_RBOX,            Contours::_onDirection)
   EVT_RADIOBOX      (CONTOUR_TYPE_RBOX,           Contours::_onContourType)
   EVT_RADIOBUTTON   (MULTIPLE_PRECONTOUR_RBUTTON, Contours::_onMultiplePlanes)
   EVT_CHECKBOX      (MULTIPLE_PRECONTOUR_CHK,     Contours::_onCyclePlanes)
   EVT_RADIOBUTTON   (SINGLE_PRECONTOUR_RBUTTON,   Contours::_onSinglePlane)
   EVT_CHECKBOX      (SINGLE_PRECONTOUR_CHK,       Contours::_onPrecomputedPlane)
   EVT_SLIDER        (CONTOUR_PLANE_SLIDER,        Contours::_onPlane)
   EVT_BUTTON        (ADD_CONTOUR_PLANE_BUTTON,    Contours::_onAddPlane)
   EVT_BUTTON        (ADVANCED_CONTOUR_BUTTON,     Contours::_onAdvanced)
////@end Contours event table entries
END_EVENT_TABLE()
//////////////////////////////////////////////////////////////////////
Contours::Contours( wxWindow* parent, wxWindowID id, 
                  const wxString& caption, const wxPoint& pos, 
                  const wxSize& size, long style,std::string type )
{
   Create(parent, id, caption, pos, size, style);
   wxSize displaySize = ::wxGetDisplaySize();
   wxRect dialogPosition( displaySize.GetWidth()-427, 440, 427, displaySize.GetHeight()-480 );
   this->SetSize( dialogPosition );
   SetDataType(type);
}
/////////////////////////////////////////////////////////////////////////////////
bool Contours::Create(wxWindow* parent, wxWindowID id, const wxString& caption,
                    const wxPoint& pos, const wxSize& size, 
                    long style )
{
   _directionRBox = 0;
   _contourTypeRBox = 0;
   _allPrecomputedRButton = 0;
   _cyclePrecomputedCBox = 0;
   _singlePlaneRButton = 0;
   _nearestPrecomputedCBox = 0;
   _planePositonSlider = 0;
   itemButton16 = 0;
   itemButton17 = 0;
   _planeDirection = "x";
   _planeType = "Graduated";
   _numberOfPlanesOption = "Single";
   _planeOption = "";
   _planePosition = 0.;
   _lastLOD = 1.0;
   _lastWarpedScale = .5;
   _lastOpacity = 1.0;
   _lastVectorScale = 0.0;
   _lastVectorRatio = 0.0;
   _lastScaleByMagnitude = false;
   _lastVectorThreshold.push_back(0.0);
   _lastVectorThreshold.push_back(1.0);

   SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
   wxDialog::Create( parent, id, caption, pos, size, style );

   CreateControls();
   GetSizer()->Fit(this);
   GetSizer()->SetSizeHints(this);
   Centre();
   return true;
}
///////////////////////////////
void Contours::CreateControls()
{    
    Contours* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Contour Controls"));
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer(itemStaticBoxSizer3Static, wxVERTICAL);
    itemBoxSizer2->Add(itemStaticBoxSizer3, 0, wxGROW|wxALL, 5);

    wxBoxSizer* itemBoxSizer4 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer4, 0, wxGROW|wxALL, 5);

    wxString itemRadioBox5Strings[] = {
        _T("x"),
        _T("y"),
        _T("z"),
        _T("By Wand")
    };
    
    _directionRBox = new wxRadioBox( itemDialog1, CONTOUR_DIR_RBOX, _T("Direction"), wxDefaultPosition, wxDefaultSize, 4, itemRadioBox5Strings, 1, wxRA_SPECIFY_COLS );
    itemBoxSizer4->Add(_directionRBox, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxString itemRadioBox6Strings[] = {
        _T("Graduated"),
        _T("Banded"),
        _T("Lined")
    };
   
    _contourTypeRBox = new wxRadioBox( itemDialog1, CONTOUR_TYPE_RBOX, _T("Contour Type"), wxDefaultPosition, wxDefaultSize, 3, itemRadioBox6Strings, 1, wxRA_SPECIFY_COLS );
    itemBoxSizer4->Add(_contourTypeRBox, 0, wxALIGN_TOP|wxALL, 5);

    wxStaticBox* itemStaticBoxSizer7Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Multiple Planes"));
    wxStaticBoxSizer* itemStaticBoxSizer7 = new wxStaticBoxSizer(itemStaticBoxSizer7Static, wxVERTICAL);
    itemStaticBoxSizer3->Add(itemStaticBoxSizer7, 0, wxGROW|wxALL, 5);

    _allPrecomputedRButton = new wxRadioButton( itemDialog1, MULTIPLE_PRECONTOUR_RBUTTON, _T("All Precomputed Surfaces"), wxDefaultPosition, wxDefaultSize, 0 );
    _allPrecomputedRButton->SetValue(false);
    itemStaticBoxSizer7->Add(_allPrecomputedRButton, 0, wxALIGN_LEFT|wxALL, 5);

    _cyclePrecomputedCBox = new wxCheckBox( itemDialog1, MULTIPLE_PRECONTOUR_CHK, _T("Cycle Precomputed Surfaces"), wxDefaultPosition, wxDefaultSize, 0 );
    _cyclePrecomputedCBox->SetValue(false);
    itemStaticBoxSizer7->Add(_cyclePrecomputedCBox, 0, wxALIGN_LEFT|wxALL, 5);

    wxStaticBox* itemStaticBoxSizer10Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Single Plane"));
    wxStaticBoxSizer* itemStaticBoxSizer10 = new wxStaticBoxSizer(itemStaticBoxSizer10Static, wxVERTICAL);
    itemStaticBoxSizer3->Add(itemStaticBoxSizer10, 0, wxGROW|wxALL, 5);

    _singlePlaneRButton = new wxRadioButton( itemDialog1, SINGLE_PRECONTOUR_RBUTTON, _T("Specify a Single Plane"), wxDefaultPosition, wxDefaultSize, 0 );
    _singlePlaneRButton->SetValue(false);
    itemStaticBoxSizer10->Add(_singlePlaneRButton, 0, wxALIGN_LEFT|wxALL, 5);

    _nearestPrecomputedCBox = new wxCheckBox( itemDialog1, SINGLE_PRECONTOUR_CHK, _T("Use Nearest Precomputed Plane"), wxDefaultPosition, wxDefaultSize, 0 );
    _nearestPrecomputedCBox->SetValue(false);
    itemStaticBoxSizer10->Add(_nearestPrecomputedCBox, 0, wxALIGN_LEFT|wxALL, 5);

    wxStaticText* itemStaticText13 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Plane"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText13, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    _planePositonSlider = new wxSlider( itemDialog1, CONTOUR_PLANE_SLIDER, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(_planePositonSlider, 0, wxGROW|wxLEFT|wxRIGHT|wxBOTTOM, 5);

    wxBoxSizer* itemBoxSizer15 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer15, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxButton* itemButton16 = new wxButton( itemDialog1, ADD_CONTOUR_PLANE_BUTTON, _T("Add Plane"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer15->Add(itemButton16, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxButton* itemButton17 = new wxButton( itemDialog1, ADVANCED_CONTOUR_BUTTON, _T("Advanced..."), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer15->Add(itemButton17, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

////@end Contours content construction
}
////////////////////
Contours::~Contours()
{
   _advancedSettings.clear();
   _contourInformation.clear();
}
////////////////////////////////////////////
void Contours::SetDataType(std::string type)
{
   _dataType = type;
   
   if(_dataType == "SCALAR")
   {
      SetTitle("Scalar Contour");
   }
   else if(_dataType == "VECTOR")
   {
      SetTitle("Vector Contour");
   }
}
/////////////////////////////
bool Contours::ShowToolTips()
{
    return true;
}
/////////////////////////////////////////////////////////////
wxBitmap Contours::GetBitmapResource( const wxString& name )
{
   wxUnusedVar(name);
   return wxNullBitmap;
}
/////////////////////////////////////////////////////////
wxIcon Contours::GetIconResource( const wxString& name )
{
   wxUnusedVar(name);
   return wxNullIcon;
}
/////////////////////////////////////////////////////////////////
void Contours::_onAdvanced( wxCommandEvent& WXUNUSED(event) )
{
   if(_dataType == "SCALAR")
   {
      AdvancedContours adContour(this,                
                  SYMBOL_ADVANCEDCONTOURS_IDNAME, 
                  SYMBOL_ADVANCEDCONTOURS_TITLE,
                  SYMBOL_ADVANCEDCONTOURS_POSITION,
                  SYMBOL_ADVANCEDCONTOURS_SIZE, 
                  SYMBOL_ADVANCEDCONTOURS_STYLE );
      adContour.SetLOD(_lastLOD);
      adContour.SetOpacity(_lastOpacity);
      adContour.SetWarpedScale(_lastWarpedScale);
      int status = adContour.ShowModal(); 
 
      if( status == wxID_OK||
          status == wxID_CLOSE||
          status == wxID_CANCEL)
       {
          _lastLOD = adContour.GetLOD();
          _lastOpacity = adContour.GetOpacity();
          _lastWarpedScale = adContour.GetWarpedScale();
       }
   }
   else if(_dataType == "VECTOR")
   {
      AdvancedVectors adVector(this,                
                  SYMBOL_ADVANCEDVECTORS_IDNAME, 
                  SYMBOL_ADVANCEDVECTORS_TITLE,
                  SYMBOL_ADVANCEDVECTORS_POSITION,
                  SYMBOL_ADVANCEDVECTORS_SIZE, 
                  SYMBOL_ADVANCEDVECTORS_STYLE );

      adVector.SetScaleByMagFlag(_lastScaleByMagnitude);
      adVector.SetVectorRatio(_lastVectorRatio);
      adVector.SetVectorScale(_lastVectorScale);
      adVector.SetVectorThreshold(_lastVectorThreshold);
      
      int status = adVector.ShowModal();
      if( status == wxID_OK||
          status == wxID_CLOSE||
          status == wxID_CANCEL)
       {
          _lastScaleByMagnitude = adVector.GetScaleByMagFlag();
          _lastVectorRatio = adVector.GetVectorRatio();
          _lastVectorScale = adVector.GetVectorScale();
          adVector.GetVectorThreshold(_lastVectorThreshold);
       }
   }
   else
   {
      wxMessageBox( _dataType.c_str(),"Unknown Data Type!", 
            wxOK | wxICON_INFORMATION );
   }
}
/////////////////////////////////////////////////////
void Contours::_onDirection( wxCommandEvent& event )
{
   _planeDirection = _directionRBox->GetStringSelection();
}
///////////////////////////////////////////////////////
void Contours::_onContourType( wxCommandEvent& event )
{
   _planeType = _contourTypeRBox->GetStringSelection();
}
//////////////////////////////////////////////////////////
void Contours::_onMultiplePlanes( wxCommandEvent& event )
{   
   _cyclePrecomputedCBox->SetValue(false);
   _cyclePrecomputedCBox->Enable(true);
   _nearestPrecomputedCBox->SetValue(false);
   _nearestPrecomputedCBox->Enable(false);
   _planeOption.clear();
   _numberOfPlanesOption = "Multiple";

}
//////////////////////////////////////////////////////
void Contours::_onCyclePlanes( wxCommandEvent& event )
{
   _planeOption = _cyclePrecomputedCBox->GetLabel();
}
///////////////////////////////////////////////////////
void Contours::_onSinglePlane( wxCommandEvent& event )
{
   _cyclePrecomputedCBox->SetValue(false);
   _cyclePrecomputedCBox->Enable(false);

   _nearestPrecomputedCBox->Enable(true);
   _nearestPrecomputedCBox->SetValue(false);
   _planeOption.clear();
   _numberOfPlanesOption = "Single";
}
////////////////////////////////////////////////////////////
void Contours::_onPrecomputedPlane( wxCommandEvent& event )
{
   _planeOption = _nearestPrecomputedCBox->GetLabel();
}
/////////////////////////////////////////////////
void Contours::_onPlane( wxCommandEvent& event )
{
   _planePosition = static_cast<double>(_planePositonSlider->GetValue());  
}
////////////////////////////////////////
void Contours::_updateAdvancedSettings()
{
   _advancedSettings.clear();

   if(_dataType == "SCALARS")
   {
      VE_XML::DataValuePair* contourOpacity = new VE_XML::DataValuePair();
      contourOpacity->SetData("Contour Opacity",_lastOpacity);
      _advancedSettings.push_back(contourOpacity);

      VE_XML::DataValuePair* warpedScale = new VE_XML::DataValuePair();
      warpedScale->SetData("Warped Contour Scale",_lastWarpedScale);
      _advancedSettings.push_back(warpedScale);

      VE_XML::DataValuePair* LODSetting = new VE_XML::DataValuePair();
      LODSetting->SetData("Contour LOD",_lastLOD);
      _advancedSettings.push_back(LODSetting);
   }
   else if(_dataType == "VECTOR")
   {
      VE_XML::DataValuePair* vectorThreshold = new VE_XML::DataValuePair();
      vectorThreshold->SetData("Vector Threshold",_lastVectorThreshold);
      _advancedSettings.push_back(vectorThreshold);
   
      VE_XML::DataValuePair* vectorScale = new VE_XML::DataValuePair();
      vectorScale->SetData("Vector Scale",_lastVectorScale);
      _advancedSettings.push_back(vectorScale);

      VE_XML::DataValuePair* vectorRatio = new VE_XML::DataValuePair();
      vectorRatio->SetData("Vector Ratio",_lastVectorRatio);
      _advancedSettings.push_back(vectorRatio);

      VE_XML::DataValuePair* scaleByMagFlag = new VE_XML::DataValuePair();
      scaleByMagFlag->SetDataName("Scale By Magnitude");
      scaleByMagFlag->SetDataType("UNSIGNED INT");
      if(_lastScaleByMagnitude)
      {
         scaleByMagFlag->SetDataValue(static_cast<unsigned int>(1));
      }
      else
      {
         scaleByMagFlag->SetDataValue(static_cast<unsigned int>(0));
      }
      _advancedSettings.push_back(scaleByMagFlag);
   }
   else
   {
      wxMessageBox( _dataType.c_str(),"Unknown Data Type", 
            wxOK | wxICON_INFORMATION );
   }
}
//////////////////////////////////////////
void Contours::_updateContourInformation()
{
   _contourInformation.clear();
   VE_XML::DataValuePair* contourDirection = new VE_XML::DataValuePair();
   contourDirection->SetDataType("STRING");
   contourDirection->SetDataName(std::string("Direction"));
   contourDirection->SetDataString(_planeDirection);

   _contourInformation.push_back(contourDirection);
   
   VE_XML::DataValuePair* contourType = new VE_XML::DataValuePair();
   contourType->SetDataType("STRING");
   contourType->SetDataName(std::string("Type"));
   contourType->SetDataString(_planeType);

   _contourInformation.push_back(contourType);

   VE_XML::DataValuePair* numberOfPlanes = new VE_XML::DataValuePair();
   numberOfPlanes->SetDataType("STRING");
   numberOfPlanes->SetDataName(std::string("Number of Planes"));
   numberOfPlanes->SetDataString(_numberOfPlanesOption);

   _contourInformation.push_back(numberOfPlanes);

   VE_XML::DataValuePair* planePosition = new VE_XML::DataValuePair();
   planePosition->SetData("Position",_planePosition);

   _contourInformation.push_back(planePosition);

   if(!_planeOption.empty())
   {
      VE_XML::DataValuePair* planeOption = new VE_XML::DataValuePair();
      planeOption->SetDataType("STRING");
      planeOption->SetDataName(std::string("Plane Option"));
      planeOption->SetDataString(_planeOption);

      _contourInformation.push_back(planeOption);
   }

}
////////////////////////////////////////////////////
void Contours::_onAddPlane( wxCommandEvent& event )
{
   _updateContourInformation();
   _updateAdvancedSettings();

   VE_XML::Command* newCommand = new VE_XML::Command();
   newCommand->SetCommandName("UPDATE_CONTOUR_SETTINGS");
   
   for(size_t i =0; i < _contourInformation.size(); i++)
   {
      newCommand->AddDataValuePair(_contourInformation.at(i));
   }

   //The advanced settings command
   VE_XML::Command* advancedSettings = new VE_XML::Command();
   advancedSettings->SetCommandName("ADVANCED_CONTOUR_SETTINGS");
   for(size_t i =0; i < _advancedSettings.size(); i++)
   {
      advancedSettings->AddDataValuePair(_advancedSettings.at(i));
   }
   std::string typeName = (_dataType == "SCALAR")?"Advanced Scalar Settings":"Advanced Vector Settings";

   //dvp representing the advanced settings within the contours information
   VE_XML::DataValuePair* advancedContourSettings = new VE_XML::DataValuePair();
   advancedContourSettings->SetData(typeName,advancedSettings);
   newCommand->AddDataValuePair(advancedContourSettings);
   
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
            newCommand = 0;
         }
      }
   }
}

