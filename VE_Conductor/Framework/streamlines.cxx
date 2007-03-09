/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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

#include "VE_Conductor/Framework/streamlines.h"
#include "VE_Conductor/Framework/vistab.h"
#include "VE_Conductor/Utilities/WPDialog.h"

#include "VE_Conductor/GUIPlugin/CORBAServiceList.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"
#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/radiobox.h>
#include <wx/radiobut.h>
#include <wx/slider.h>
#include <wx/icon.h>
#include <wx/msgdlg.h>
#include <wx/button.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <iostream>

using namespace VE_Conductor::GUI_Utilities;

BEGIN_EVENT_TABLE( Streamlines, wxDialog )
////@begin Streamlines event table entries 
   EVT_RADIOBOX      (CURSOR_RBOX,                 Streamlines::_onCursorSelect)
   EVT_RADIOBOX      (DIRECTION_RBOX,              Streamlines::_onDirection)
   EVT_RADIOBOX      (INTEGRATION_DIR_RBOX,        Streamlines::_onIntegrateDir)
   EVT_COMMAND_SCROLL(NUMBER_PTS_SLIDER,           Streamlines::_onPointsSlider)
   EVT_COMMAND_SCROLL(PLANE_SIZE_SLIDER,           Streamlines::_onSizeSlider)
   EVT_BUTTON        (ADVANCED_STREAMLINE_BUTTON,  Streamlines::_onAdvanced)
   EVT_BUTTON        (COMPUTE_STREAMLINE_BUTTON,   Streamlines::_onCompute)
   EVT_BUTTON        (SET_SEED_POINTS_BUTTON,      Streamlines::SetSeedPoints)
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
   seedPointDialog = 0;
   
   _lastIntegrationStepSize = 1000.0;
   _lastPropagationSize = 1.0;
   _lastLineDiameter = 0.0;
   _lastSphereArrowParticleSize = 1.0;
   _lastStep = 1.0;
   _lastSeedPtFlag = false;
   _lastStreamArrow = false;
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
////////////////////////////////////////////////////////////////////////////////
Streamlines::~Streamlines( void )
{
   for ( size_t i = 0; i < seedPointInformation.size(); ++i )
   {
      delete seedPointInformation.at( i );
   }
   seedPointInformation.clear();   
}
//////////////////////////////////////////
void Streamlines::CreateControls()
{    
    Streamlines* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Streamline Controls"));
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer(itemStaticBoxSizer3Static, wxVERTICAL);
    itemBoxSizer2->Add(itemStaticBoxSizer3, 0, wxGROW|wxALL, 5);

    wxBoxSizer* itemBoxSizer4 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer4, 0, wxGROW|wxALL, 5);

    /*wxString itemRadioBox5Strings[] = {
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
    itemBoxSizer4->Add(_directionRBox, 0, wxALIGN_TOP|wxALL, 5);*/

    wxString itemRadioBox7Strings[] = {
        _T("backward"),
        _T("forward"),
        _T("both directions")
    };
    _integrationRBox = new wxRadioBox( itemDialog1, INTEGRATION_DIR_RBOX, _T("Integration Direction"), wxDefaultPosition, wxDefaultSize, 3, itemRadioBox7Strings, 1, wxRA_SPECIFY_COLS );
    itemBoxSizer4->Add(_integrationRBox, 0, wxALIGN_TOP|wxALL, 5);

    wxStaticText* itemStaticText8 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Size(%)"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText8, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    _sizeSlider = new wxSlider( itemDialog1, PLANE_SIZE_SLIDER, 50, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(_sizeSlider, 0, wxGROW|wxLEFT|wxRIGHT|wxBOTTOM, 5);

    wxStaticText* itemStaticText10 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Number of Point (Per Plane Direction)"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText10, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    _nPointsSlider = new wxSlider( itemDialog1, NUMBER_PTS_SLIDER, 2, 2, 20, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(_nPointsSlider, 0, wxGROW|wxLEFT|wxRIGHT|wxBOTTOM, 5);

    wxBoxSizer* itemBoxSizer12 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer12, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxButton* itemButton13 = new wxButton( itemDialog1, COMPUTE_STREAMLINE_BUTTON, _T("Compute Streamline"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer12->Add(itemButton13, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxButton* seedPointsbutton = new wxButton( itemDialog1, SET_SEED_POINTS_BUTTON, _T("Seed Points"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer12->Add(seedPointsbutton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxButton* itemButton14 = new wxButton( itemDialog1, ADVANCED_STREAMLINE_BUTTON, _T("Advanced..."), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer12->Add(itemButton14, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxButton* _closeButton = new wxButton( itemDialog1, wxID_OK, _T("Close"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer12->Add(_closeButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

////@end Streamlines content construction
}
/////////////////////////////////////////
bool Streamlines::ShowToolTips()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////////
wxBitmap Streamlines::GetBitmapResource( const wxString& name )
{
     wxUnusedVar(name);
    return wxNullBitmap;
}
//////////////////////////////////////////////////////////////////////////////
wxIcon Streamlines::GetIconResource( const wxString& name )
{
	wxUnusedVar(name);
    return wxNullIcon;
}
///////////////////////////////////////////////////////
void Streamlines::_updateAdvancedSettings()
{
   _advancedSettings.clear();

   VE_XML::DataValuePair* propagationTime = new VE_XML::DataValuePair();
   propagationTime->SetData("Propagation Time",_lastPropagationSize);
   _advancedSettings.push_back(propagationTime);

   VE_XML::DataValuePair* integrationStep = new VE_XML::DataValuePair();
   integrationStep->SetData("Integration Step Size",_lastIntegrationStepSize);
   _advancedSettings.push_back(integrationStep);
  
   VE_XML::DataValuePair* lineDiameter = new VE_XML::DataValuePair();
   lineDiameter->SetData("Diameter",_lastLineDiameter);
   _advancedSettings.push_back(lineDiameter);
   
   VE_XML::DataValuePair* sphereArrowParticles = new VE_XML::DataValuePair();
   sphereArrowParticles->SetData("Sphere/Arrow/Particle Size",_lastSphereArrowParticleSize);
   _advancedSettings.push_back(sphereArrowParticles);

   VE_XML::DataValuePair* stepSize = new VE_XML::DataValuePair();
   stepSize->SetData("Step",_lastStep);
   _advancedSettings.push_back(stepSize);

   VE_XML::DataValuePair* seedPtFlag = new VE_XML::DataValuePair();
   seedPtFlag->SetDataName("Use Last Seed Pt");
   seedPtFlag->SetDataType("UNSIGNED INT");
   if(_lastSeedPtFlag)
   {
      seedPtFlag->SetDataValue(static_cast<unsigned int>(1));
   }
   else
   {
      seedPtFlag->SetDataValue(static_cast<unsigned int>(0));
   }
   _advancedSettings.push_back(seedPtFlag);

   VE_XML::DataValuePair* streamArrow = new VE_XML::DataValuePair();
   streamArrow->SetDataName("Use Stream Arrows");
   streamArrow->SetDataType("UNSIGNED INT");
   if(_lastStreamArrow)
   {
      streamArrow->SetDataValue(static_cast<unsigned int>(1));
   }
   else
   {
      streamArrow->SetDataValue(static_cast<unsigned int>(0));
   }
   _advancedSettings.push_back(streamArrow);

}
////////////////////////////////////////////////////////////
void Streamlines::_updateStreamlineInformation()
{
   _streamlineInformation.clear();
   VE_XML::DataValuePair* streamlineDirection = new VE_XML::DataValuePair();
   streamlineDirection->SetDataType("STRING");
   streamlineDirection->SetDataName(std::string("Cursor Direction"));
   streamlineDirection->SetDataString(_streamlineDirection);

   _streamlineInformation.push_back(streamlineDirection);
   
   VE_XML::DataValuePair* cursorSelection = new VE_XML::DataValuePair();
   cursorSelection->SetDataType("STRING");
   cursorSelection->SetDataName(std::string("Cursor Type"));
   cursorSelection->SetDataString(_cursorType);

   _streamlineInformation.push_back(cursorSelection);

   VE_XML::DataValuePair* integrationDirection = new VE_XML::DataValuePair();
   integrationDirection->SetDataType("STRING");
   integrationDirection->SetDataName(std::string("Integration Direction"));
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
   /*int displayWidth, displayHeight = 0;
   ::wxDisplaySize(&displayWidth,&displayHeight);
  
   wxRect bbox = GetRect();

   int width,height = 0;
   GetSize(&width,&height);
   adStreamline.SetSize(wxRect( 2*displayWidth/3, bbox.GetBottomRight().y, 
                        width, height));*/
   adStreamline.SetSize(GetRect());
   adStreamline.SetIntegrationStepSize(_lastIntegrationStepSize);
   adStreamline.SetPropagationSize(_lastPropagationSize);
   adStreamline.SetLineDiameter( _lastLineDiameter );
   adStreamline.SetSphereArrowParticleSize(_lastSphereArrowParticleSize);
   adStreamline.SetStep(_lastStep);
   adStreamline.SetUseLastSeedPt(_lastSeedPtFlag);
   adStreamline.SetStreamArrow(_lastStreamArrow);

   int error = adStreamline.ShowModal(); 
   if( error == wxID_OK||
       error == wxID_CLOSE||
       error == wxID_CANCEL)
    {
       _lastIntegrationStepSize = adStreamline.GetIntegrationStepSize();
       _lastPropagationSize = adStreamline.GetPropagationSize();
       _lastLineDiameter = adStreamline.GetLineDiameter();
       _lastSphereArrowParticleSize = adStreamline.GetSphereArrowParticleSize();
       _lastStep = adStreamline.GetStep();
       _lastSeedPtFlag = adStreamline.GetUseLastSeedPoint();
       _lastStreamArrow = adStreamline.GetStreamArrow();
    }
}
////////////////////////////////////////////////////////
void Streamlines::_onCompute(wxCommandEvent& WXUNUSED(event))
{
   _updateStreamlineInformation();
   _updateAdvancedSettings();

    VE_XML::Command* veCommand = new VE_XML::Command();
    veCommand->SetCommandName( std::string("Display Seed Points") );
   VE_XML::DataValuePair* seedPointDVP = new VE_XML::DataValuePair();
   seedPointDVP->SetData("OnOff",static_cast<unsigned int>(0));
   veCommand->AddDataValuePair(seedPointDVP);
   VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
   delete veCommand;   
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
   //Add the dvp's for the seed point info
   //VE_XML::Command* seedPointSettings = new VE_XML::Command();
   //seedPointSettings->SetCommandName("Set_Seed_Point_Settings");
   for(size_t i =0; i < seedPointInformation.size(); i++)
   {
      advancedSettings->AddDataValuePair( seedPointInformation.at(i) );
   }

   //dvp representing the advanced settings within the contours information
   VE_XML::DataValuePair* advancedStreamlineSettings = new VE_XML::DataValuePair();
   advancedStreamlineSettings->SetData("Advanced Streamline Settings",advancedSettings);
   newCommand->AddDataValuePair(advancedStreamlineSettings);

   //dvp representing the advanced settings within the contours information
   //VE_XML::DataValuePair* seedPoint = new VE_XML::DataValuePair();
   //seedPoint->SetData( "Seed_Point_Settings", seedPointSettings );
   //newCommand->AddDataValuePair( seedPoint );
   
   try
   {
      dynamic_cast<Vistab*>(GetParent())->SendUpdatedSettingsToXplorer(newCommand);
   }
   catch(...)
   {
      wxMessageBox( _("Invalid Parent"),_("Communication Failure"), 
         wxOK | wxICON_INFORMATION );
   }

   if(newCommand)
   {
      delete newCommand;
   }
}
////////////////////////////////////////////////////////////////////////////////
void Streamlines::_onCursorSelect(wxCommandEvent& WXUNUSED(event))
{
   _cursorType = ConvertUnicode( _cursorRBox->GetStringSelection() );
}
////////////////////////////////////////////////////////////////////////////////
void Streamlines::_onDirection(wxCommandEvent& WXUNUSED(event))
{
   _streamlineDirection = ConvertUnicode( _directionRBox->GetStringSelection() );
}
////////////////////////////////////////////////////////////////////////////////
void Streamlines::_onIntegrateDir(wxCommandEvent& WXUNUSED(event))
{
   _integrationDirection = ConvertUnicode( _integrationRBox->GetStringSelection() );
}
////////////////////////////////////////////////////////////////////////////////
void Streamlines::_onSizeSlider(wxScrollEvent& WXUNUSED(event))
{
   _streamSize = static_cast<double>(_sizeSlider->GetValue());
}
////////////////////////////////////////////////////////////////////////////////
void Streamlines::_onPointsSlider(wxScrollEvent& WXUNUSED(event))
{
   _nPointsPerPlane = _nPointsSlider->GetValue();
}
////////////////////////////////////////////////////////////////////////////////
void Streamlines::SetSeedPoints( wxCommandEvent& WXUNUSED(event) )
{
   //Clear the old dvps if there were any
   for ( size_t i = 0; i < seedPointInformation.size(); ++i )
   {
      delete seedPointInformation.at( i );
   }
   seedPointInformation.clear();
   if(!seedPointDialog)
   {
      seedPointDialog = new WPDialog( static_cast< wxWindow* >( this ), 0, "Seed Point Controls" );
   }
   //display the seed points
   VE_XML::Command* newCommand = new VE_XML::Command();
   try
   {
	  newCommand->SetCommandName("Display Seed Points");
	  VE_XML::DataValuePair* seedPointDVP = new VE_XML::DataValuePair();
	  seedPointDVP->SetData("OnOff",static_cast<unsigned int>(1));
	  newCommand->AddDataValuePair(seedPointDVP);
	  VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( newCommand );
	  delete newCommand;

     
   }
   catch(...)
   {
	   wxMessageBox( _("Invalid command!"),_(newCommand->GetCommandName().c_str()), 
      wxOK | wxICON_INFORMATION );
	  delete newCommand;
   }
   VE_XML::Command* boundsCommand = new VE_XML::Command();
   try
   {
	  boundsCommand->SetCommandName("Seed Points Bounds");
     std::vector<double> seedPointBounds;
     seedPointDialog->GetBounds(seedPointBounds);

     VE_XML::DataValuePair* coordinate = new VE_XML::DataValuePair();
     coordinate->SetData("Coordinate","All Bounds");
     boundsCommand->AddDataValuePair(coordinate);

     VE_XML::DataValuePair* seedPointBoundsDVP = new VE_XML::DataValuePair();
     seedPointBoundsDVP->SetData("Bounds",seedPointBounds);
	  boundsCommand->AddDataValuePair(seedPointBoundsDVP);
	  VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( boundsCommand );
	  delete boundsCommand;

   }
   catch(...)
   {
	   wxMessageBox( _("Invalid command!"),_(boundsCommand->GetCommandName().c_str()), 
      wxOK | wxICON_INFORMATION );
	  delete newCommand;
   }
   VE_XML::Command* dimensionsCommand = new VE_XML::Command();
   try
   {
	  dimensionsCommand->SetCommandName("Seed Points Dimensions");
     std::vector<long> seedPointDims;
     seedPointDialog->GetDimensions(seedPointDims);

     VE_XML::DataValuePair* dimensions = new VE_XML::DataValuePair();
     dimensions->SetData("Dimensions",seedPointDims);
     dimensionsCommand->AddDataValuePair(dimensions);

     VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( dimensionsCommand );
	  delete dimensionsCommand;

   }
   catch(...)
   {
	   wxMessageBox( _("Invalid command!"),_(dimensionsCommand->GetCommandName().c_str()), 
      wxOK | wxICON_INFORMATION );
	  delete dimensionsCommand;
   }
   if(seedPointDialog->ShowModal())
   {
      seedPointInformation = seedPointDialog->GetSeedPointDVPVector();
   }
}
