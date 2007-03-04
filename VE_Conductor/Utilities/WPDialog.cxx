#include "VE_Conductor/Utilities/WPDialog.h"
//#include "VE_Conductor/Utilities/basic.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/XMLReaderWriter.h"

#include <wx/statbox.h>
#include <wx/spinctrl.h>

using namespace VE_Conductor::GUI_Utilities;

WPDialog::WPDialog(wxWindow* parent, int id, std::string title):
BaseDialog(parent, id, title)
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
   wxSize displaySize = ::wxGetDisplaySize();
   wxRect dialogPosition( displaySize.GetWidth()-427, 440, 427, displaySize.GetHeight()-480 );
   this->SetSize( dialogPosition ); 
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
   wxStaticBox* dualSliderGroup = new wxStaticBox(this, -1, wxT("Bounding Box Controls"));
   wxStaticBoxSizer* mainSizer = new wxStaticBoxSizer(dualSliderGroup,wxVERTICAL);
   
   _createDualSliders();
   wxBoxSizer* xdualSizer = new wxBoxSizer (wxHORIZONTAL);
   xdualSizer->Add(_xBounds,1,wxALIGN_CENTER|wxEXPAND);
   
   wxBoxSizer* ydualSizer = new wxBoxSizer (wxHORIZONTAL);
   ydualSizer->Add(_yBounds,1,wxALIGN_CENTER|wxEXPAND);
   
   wxBoxSizer* zdualSizer = new wxBoxSizer (wxHORIZONTAL);
   zdualSizer->Add(_zBounds,1,wxALIGN_CENTER|wxEXPAND); 
   
   numXPointsSpinner = new wxSpinCtrl( static_cast< wxWindow* >( this ), -1, 
                                       wxEmptyString, 
                                       wxDefaultPosition, wxDefaultSize, 
                                       wxSP_ARROW_KEYS, 1, 100, 4 );
   numYPointsSpinner = new wxSpinCtrl( static_cast< wxWindow* >( this ), -1, 
                                       wxEmptyString, 
                                       wxDefaultPosition, wxDefaultSize, 
                                       wxSP_ARROW_KEYS, 1, 100, 4 );
   numZPointsSpinner = new wxSpinCtrl( static_cast< wxWindow* >( this ), -1, 
                                       wxEmptyString, 
                                       wxDefaultPosition, wxDefaultSize, 
                                       wxSP_ARROW_KEYS, 1, 100, 4 );

   wxBoxSizer* spinnerRowSizer = new wxBoxSizer(wxHORIZONTAL);
   spinnerRowSizer->Add( numXPointsSpinner, 0, wxALIGN_LEFT, 5 );
   spinnerRowSizer->Add( numYPointsSpinner, 0, wxALIGN_LEFT, 5 );
   spinnerRowSizer->Add( numZPointsSpinner, 0, wxALIGN_LEFT, 5 );
   
   wxBoxSizer* buttonRowSizer = new wxBoxSizer(wxHORIZONTAL);
   _addOKButton(buttonRowSizer);
   
   mainSizer->Add(xdualSizer,1,wxALIGN_CENTER|wxEXPAND);  
   mainSizer->Add(ydualSizer,1,wxALIGN_CENTER|wxEXPAND);   
   mainSizer->Add(zdualSizer,1,wxALIGN_CENTER|wxEXPAND);   
   mainSizer->Add(spinnerRowSizer,1,wxALIGN_CENTER|wxEXPAND);   
   
   mainSizer->Add(buttonRowSizer,1,wxALIGN_CENTER|wxEXPAND);
   
   _xBounds->Raise();
   _yBounds->Raise();
   _zBounds->Raise();
   
   SetAutoLayout(true);
   
   SetSizer(mainSizer);
   mainSizer->Fit(dynamic_cast<BaseDialog*>(this));
}
////////////////////////////////////////////////////////////////////////////////////
void WPDialog::_createDualSliders()
{
//X Slider
  _xBounds = new DualSlider(this, -1, 1,0,100, 0, 100, wxDefaultPosition,
	  wxDefaultSize,wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS,
	  wxString( _T("X Bounds") ) );
   WPMinSliderCallback* minX = new WPMinSliderCallback(this,"X");
   WPMaxSliderCallback* maxX = new WPMaxSliderCallback(this,"X");
   WPBothMoveCallback* bothX = new WPBothMoveCallback(this,"X");
   
   _xBounds->SetMinSliderCallback(minX);
   _xBounds->SetMaxSliderCallback(maxX);
   _xBounds->SetBothSliderUpdateCallback(bothX);
   
//Y slider   
     _yBounds = new DualSlider(this, -1, 1,0,100, 0, 100, wxDefaultPosition,
	  wxDefaultSize,wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS,
	  wxString( _T("Y Bounds") ));
   WPMinSliderCallback* minY = new WPMinSliderCallback(this,"Y");
   WPMaxSliderCallback* maxY = new WPMaxSliderCallback(this,"Y");
   WPBothMoveCallback* bothY = new WPBothMoveCallback(this,"Y");
   
   _yBounds->SetMinSliderCallback(minY);
   _yBounds->SetMaxSliderCallback(maxY);
   _yBounds->SetBothSliderUpdateCallback(bothY);
   
//Z slider

  _zBounds = new DualSlider(this, -1, 1,0,100, 0, 100, wxDefaultPosition,
	  wxDefaultSize,wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS,
	  wxString( _T("Z Bounds") ) );
   WPMinSliderCallback* minZ = new WPMinSliderCallback(this,"Z");
   WPMaxSliderCallback* maxZ = new WPMaxSliderCallback(this,"Z");
   WPBothMoveCallback* bothZ = new WPBothMoveCallback(this,"Z");
   
   _zBounds->SetMinSliderCallback(minZ);
   _zBounds->SetMaxSliderCallback(maxZ);
   _zBounds->SetBothSliderUpdateCallback(bothZ);

}     
////////////////////////////////////////////////////////////////////////////////////
void WPDialog::WPMinSliderCallback::SliderOperation()
{
   //what does TP stand for?
   _wpdlg->SetCommandName("Min WP Update");

   VE_XML::DataValuePair* coordinate = new VE_XML::DataValuePair();
   coordinate->SetDataType("STRING");
   coordinate->SetDataName(std::string("Coordinate"));
   coordinate->SetDataString(_direction);
   _wpdlg->AddInstruction(coordinate);
   
   VE_XML::DataValuePair* direction = new VE_XML::DataValuePair();
   direction->SetDataType("STRING");
   direction->SetDataName(std::string("Direction"));
   direction->SetDataString("Positive");
   _wpdlg->AddInstruction(direction);
   
   VE_XML::DataValuePair* value = new VE_XML::DataValuePair;
   value->SetData("WP Value",
	   static_cast<double>(_dualSlider->GetMinSliderValue())/100.0);
   _wpdlg->AddInstruction(value);
   
   _wpdlg->SendCommands();
   _wpdlg->ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////////
void WPDialog::WPMaxSliderCallback::SliderOperation()
{
//what does TP stand for?
   _wpdlg->SetCommand("Max WP Update");

   VE_XML::DataValuePair* coordinate = new VE_XML::DataValuePair();
   coordinate->SetDataType("STRING");
   coordinate->SetDataName(std::string("Coordinate"));
   coordinate->SetDataString(_direction);
   _wpdlg->AddInstruction(coordinate);
   
   VE_XML::DataValuePair* direction = new VE_XML::DataValuePair();
   direction->SetDataType("STRING");
   direction->SetDataName(std::string("Direction"));
   direction->SetDataString("Negative");
   _wpdlg->AddInstruction(direction);
   
   VE_XML::DataValuePair* value = new VE_XML::DataValuePair;
   value->SetData("WP Value",
	   static_cast<double>(_dualSlider->GetMaxSliderValue())/100.0);
   _wpdlg->AddInstruction(value);
   
   _wpdlg->SendCommands();
   _wpdlg->ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////////
void WPDialog::WPBothMoveCallback::SliderOperation()
{
//what does TP stand for?
   _wpdlg->SetCommand("Both WP Update");

   VE_XML::DataValuePair* coordinate = new VE_XML::DataValuePair();
   coordinate->SetDataType("STRING");
   coordinate->SetDataName(std::string("Coordinate"));
   coordinate->SetDataString(_direction);
   _wpdlg->AddInstruction(coordinate);
   
   VE_XML::DataValuePair* direction = new VE_XML::DataValuePair();
   direction->SetDataType("STRING");
   direction->SetDataName(std::string("Direction"));
   direction->SetDataString("Both");
   _wpdlg->AddInstruction(direction);
   
   VE_XML::DataValuePair* minvalue = new VE_XML::DataValuePair;
   minvalue->SetData("WP Value",
	   static_cast<double>(_dualSlider->GetMinSliderValue())/100.0); 
 
 
   VE_XML::DataValuePair* maxvalue = new VE_XML::DataValuePair;
   maxvalue->SetData("WP Value",
	   static_cast<double>(_dualSlider->GetMaxSliderValue())/100.0);
   
   
   
   _wpdlg->AddInstruction(minvalue);
   _wpdlg->AddInstruction(maxvalue);
   
   _wpdlg->SendCommands();
   _wpdlg->ClearInstructions();
}
//////////////////////////////////////////////////
void WPDialog::SetCommandName(std::string newName)
{
   _commandName = newName;
}
////////////////////////////////////////////////////////////////////
void WPDialog::AddInstruction(VE_XML::DataValuePair* newInstruction)
{
   _instructions.push_back(newInstruction);
}
////////////////////////////////////////////////////////////////////
void WPDialog::SendCommands()
{
   _sendCommandsToXplorer();
}
////////////////////////////////////////////////////////////////////
wxSizer* WPDialog::_buildSpecificWidgets()
{
   wxStaticBox* dualSliderGroup = new wxStaticBox(this, -1, _("Volume Clipping Planes"));
   wxStaticBoxSizer* mainSizer = new wxStaticBoxSizer(dualSliderGroup,wxVERTICAL);

   _createDualSliders();
   wxBoxSizer* xdualSizer = new wxBoxSizer(wxHORIZONTAL);
   xdualSizer->Add(_xBounds,1,wxALIGN_CENTER|wxEXPAND);

   wxBoxSizer* ydualSizer = new wxBoxSizer(wxHORIZONTAL);
   ydualSizer->Add(_yBounds,1,wxALIGN_CENTER|wxEXPAND);

   wxBoxSizer* zdualSizer = new wxBoxSizer(wxHORIZONTAL);
   zdualSizer->Add(_zBounds,1,wxALIGN_CENTER|wxEXPAND);

   mainSizer->Add(xdualSizer,1,wxALIGN_CENTER|wxEXPAND);
   mainSizer->Add(ydualSizer,1,wxALIGN_CENTER|wxEXPAND);
   mainSizer->Add(zdualSizer,1,wxALIGN_CENTER|wxEXPAND);

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
std::vector< VE_XML::DataValuePair* > WPDialog::GetSeedPointDVPVector( void )
{
   return seedPointDVP;
}
////////////////////////////////////////////////////////////////////////////////
bool WPDialog::TransferDataFromWindow( void )
{
   VE_XML::DataValuePair* tempDVP;
   ////////////////
   tempDVP = new VE_XML::DataValuePair();
   tempDVP->SetData("Min_Z_BB",
                    static_cast<double>( _zBounds->GetMinSliderValue() )/100.0 ); 
   seedPointDVP.push_back( tempDVP );
   ////////////////
   tempDVP = new VE_XML::DataValuePair();
   tempDVP->SetData("Min_Y_BB",
                    static_cast<double>( _yBounds->GetMinSliderValue() )/100.0 ); 
   seedPointDVP.push_back( tempDVP );
   ////////////////
   tempDVP = new VE_XML::DataValuePair();
   tempDVP->SetData("Min_X_BB",
                    static_cast<double>( _xBounds->GetMinSliderValue() )/100.0 ); 
   seedPointDVP.push_back( tempDVP );
   ////////////////
   tempDVP = new VE_XML::DataValuePair();
   tempDVP->SetData("Max_Z_BB",
                    static_cast<double>( _zBounds->GetMaxSliderValue() )/100.0 ); 
   seedPointDVP.push_back( tempDVP );
   ////////////////
   tempDVP = new VE_XML::DataValuePair();
   tempDVP->SetData("Max_Y_BB",
                    static_cast<double>( _yBounds->GetMaxSliderValue() )/100.0 ); 
   seedPointDVP.push_back( tempDVP );
   ////////////////
   tempDVP = new VE_XML::DataValuePair();
   tempDVP->SetData("Max_X_BB",
                    static_cast<double>( _xBounds->GetMaxSliderValue() )/100.0 ); 
   seedPointDVP.push_back( tempDVP );
   ////////////////
   tempDVP = new VE_XML::DataValuePair();
   tempDVP->SetData("Num_X_Points", 
                    static_cast< unsigned int>( numXPointsSpinner->GetValue() ) ); 
   seedPointDVP.push_back( tempDVP );
   ////////////////
   tempDVP = new VE_XML::DataValuePair();
   tempDVP->SetData("Num_Y_Points", 
                    static_cast< unsigned int>( numYPointsSpinner->GetValue() ) ); 
   seedPointDVP.push_back( tempDVP );
   ////////////////
   tempDVP = new VE_XML::DataValuePair();
   tempDVP->SetData("Num_Z_Points", 
                    static_cast< unsigned int>( numZPointsSpinner->GetValue() ) ); 
   seedPointDVP.push_back( tempDVP );
   return true;
}
   
