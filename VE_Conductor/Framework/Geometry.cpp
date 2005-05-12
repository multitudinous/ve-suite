#include "Geometry.h"
//#include "UI_Tabs.h"
#include "cfdEnum.h"
#include "interface.h"

#include <iostream>
#include <string>
#include <cmath>

BEGIN_EVENT_TABLE(Geometry, wxDialog)
   EVT_COMMAND_SCROLL(GEOMETRY_CONFIG_OPACITY_SLIDER, Geometry::ChangeOpacity)
   EVT_COMMAND_SCROLL(GEOMETRY_CONFIG_LOD_SLIDER, Geometry::_onGeometry)
   EVT_CHECKLISTBOX(GEOMETRY_CONFIG_CBOX,Geometry::_onUpdate)
   //EVT_RADIOBOX(GEOMETRY_CONFIG_RBOX,Geometry::ChangeOpacity)
   EVT_BUTTON(GEOMETRY_CONFIG_UPDATE_BUTTON,Geometry::_onUpdate)
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
Geometry::Geometry(wxWindow *parent, wxWindowID id)
:wxDialog(parent, id, wxString("Geometry"))//, wxDefaultPosition,wxDefaultSize)
{
   
   _geometryRBox = 0;
   _geometryCBox = 0;
   _updateButton = 0;
   geomOpacitySlider = 0;
   geomLODSlider = 0;
   _parent = parent;

   _buildPage();
}
//////////////////////////////
//build the geometry tab    //
//////////////////////////////
void Geometry::_buildPage()
{

   //the radio box
   int numGeoms = 0;//((UI_Tabs *)_parent)->num_geo;
   wxString* defaultName = 0;
   wxString* opacitytName = 0;

   if ( numGeoms > 0 )
   {  
  /*    defaultName = new wxString[ numGeoms ];
      opacitytName = new wxString[ numGeoms ];
      for(CORBA::ULong i = 0; i < (unsigned int)numGeoms; i++)
      {  
         defaultName[ i ] = ((UI_Tabs*)_parent)->geoNameArray[ i ];
         std::cout << "Geometry Name " << i << " : " << defaultName[ i ] << std::endl;
         opacitytName[ i ] = wxString::Format("%s %i", "File", (int)(i+1) );
      }*/
   }
   else
   {
      numGeoms = 1;
      defaultName = new wxString[ numGeoms ];
      defaultName[ 0 ] = wxT("No Geometry Files");
      opacitytName = new wxString[ numGeoms ];
      opacitytName[ 0 ] = wxT( "0" );
   }

   _geometryRBox = new wxRadioBox(  this, GEOMETRY_CONFIG_RBOX, wxT("Opacity Control"),
                                    wxDefaultPosition, wxDefaultSize, 
                                    numGeoms, opacitytName,
                                    1, wxRA_SPECIFY_COLS);

   wxStaticBox* geomFiles = new wxStaticBox(this,-1, wxT("Geometry Files"));
   wxStaticBoxSizer* geomFilesGroup = new wxStaticBoxSizer(geomFiles,wxVERTICAL);   
   _geometryCBox = new wxCheckListBox( this, GEOMETRY_CONFIG_CBOX,  
                                    wxDefaultPosition, wxDefaultSize, 
                                    numGeoms, defaultName );
   geomFilesGroup->Add(_geometryCBox,1,wxALIGN_LEFT|wxEXPAND);
   
   wxBoxSizer* radioAndCheckBoxes = new wxBoxSizer( wxHORIZONTAL );
   radioAndCheckBoxes->Add(geomFilesGroup,1,wxALIGN_LEFT|wxEXPAND|wxALL, 5);
   radioAndCheckBoxes->Add(_geometryRBox,0,wxALIGN_RIGHT|wxEXPAND|wxALL, 5);

   // Used to initialize all the checkboxes on
   
   for(int j = 0; j < numGeoms; j++)
   {
      _geometryCBox->Check( j );
   }

   /*if ( ((UI_Tabs *)_parent)->num_geo == 0 )
   {
      _geometryCBox->Enable( false );
   }
*/
   // slider info
   //the labels for the sliders
   wxStaticText* opacityLabel = new wxStaticText(this, -1, wxT("Geometry Opacity"));
   wxStaticText* opacityLabelLeft = new wxStaticText(this, -1, wxT("Transparent"));
   wxStaticText* opacityLabelRight = new wxStaticText(this, -1, wxT("Opaque"));
   
   wxStaticText* lodLabel = new wxStaticText(this, -1, wxT("Geometry LOD Control"));
   wxStaticText* lodLabelLeft = new wxStaticText(this, -1, wxT("Lower"));
   wxStaticText* lodLabelRight = new wxStaticText(this, -1, wxT("Higher")); 

   //opacity slider
   geomOpacitySlider = new wxSlider(this, GEOMETRY_CONFIG_OPACITY_SLIDER,100,0,100,
                                       wxDefaultPosition, wxDefaultSize,
                                       wxSL_HORIZONTAL|
                                       wxSL_AUTOTICKS|
                                       wxSL_LABELS );

   //lod slider
   geomLODSlider = new wxSlider(this, GEOMETRY_CONFIG_LOD_SLIDER,1000,0,1000,
                                       wxDefaultPosition, wxDefaultSize,
                                       wxSL_HORIZONTAL|
                                       wxSL_AUTOTICKS|
                                       wxSL_LABELS );

   //two sizers to group the sliders and their lables
   wxBoxSizer* opacityGroup = new wxBoxSizer( wxVERTICAL );
   wxBoxSizer* opacityLabelBottom = new wxBoxSizer( wxHORIZONTAL );

   wxBoxSizer* lodGroup = new wxBoxSizer( wxVERTICAL );
   wxBoxSizer* lodLabelBottom = new wxBoxSizer( wxHORIZONTAL );

   opacityGroup->Add(opacityLabel,0,wxALIGN_LEFT);
   opacityGroup->Add(geomOpacitySlider,1,wxALIGN_LEFT|wxEXPAND);
   opacityLabelBottom->Add(opacityLabelLeft,6,wxALIGN_LEFT);
   opacityLabelBottom->Add(opacityLabelRight,0,wxALIGN_RIGHT);
   opacityGroup->Add(opacityLabelBottom,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND|wxALL, 5);

   lodGroup->Add(lodLabel,0,wxALIGN_LEFT);
   lodGroup->Add(geomLODSlider,1,wxALIGN_LEFT|wxEXPAND);
   lodLabelBottom->Add(lodLabelLeft,6,wxALIGN_LEFT);
   lodLabelBottom->Add(lodLabelRight,0,wxALIGN_RIGHT);
   lodGroup->Add(lodLabelBottom,0,wxALIGN_LEFT|wxEXPAND|wxALL);

   //the update button
   //_updateButton = new wxButton(this,GEOMETRY_UPDATE_BUTTON,wxT("Update"));

   wxStaticBox* geomControls = new wxStaticBox(this,-1, wxT("Geometry Controls"));
   wxStaticBoxSizer* geomControlsGroup = new wxStaticBoxSizer(geomControls,wxVERTICAL);
   //the panel sizer
   //geometryPanelGroup->Add(_geometryRBox,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   geomControlsGroup->Add(radioAndCheckBoxes,4,wxEXPAND|wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
   geomControlsGroup->Add(opacityGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND|wxALL, 5);
   geomControlsGroup->Add(lodGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND|wxALL, 5);

   wxBoxSizer* geometryPanelGroup = new wxBoxSizer(wxVERTICAL);
   geometryPanelGroup->Add(geomControlsGroup,1,wxEXPAND|wxALL, 5);
   // Calculate row
   wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);
   ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
   ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
   
   geometryPanelGroup->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(geometryPanelGroup);
  
   geometryPanelGroup->Fit(this);

   // Send lod info back to ve-xplorer
   /*((UI_Tabs *)_parent)->cSc = geomLODSlider->GetValue();
   ((UI_Tabs *)_parent)->cId = CHANGE_LOD_SCALE;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();*/
}

void Geometry::UnPack( Interface* intf )
{
  std::vector<std::string> vars;
  
  std::map<std::string, long *>::iterator iteri;
  std::map<std::string, double *>::iterator iterd;
  std::map<std::string, std::string *>::iterator iters;
  std::map<std::string, std::vector<long> *>::iterator itervi;
  std::map<std::string, std::vector<double> *>::iterator itervd;
  std::map<std::string, std::vector<std::string> *>::iterator itervs;
  
  unsigned int i;
  long temp;
/*
  mod_pack = *intf;
  vars = mod_pack.getInts();
  for (i=0; i<vars.size(); i++)
    {
      iteri =_int.find(vars[i]);
      if (iteri!=_int.end())
	mod_pack.getVal(vars[i], *(iteri->second));
      else if (vars[i]=="XPOS")
	{
	  mod_pack.getVal("XPOS", temp);
	  //	  printf("xpos %ld\n", temp);
	  pos.x = temp;
	}
      else if (vars[i]=="YPOS")
	{
	  //	  printf("ypos %ld\n", temp);
	  mod_pack.getVal("YPOS", temp);
	  pos.y = temp;
	}
    }

  vars = mod_pack.getDoubles();
  for (i=0; i<vars.size(); i++)
    {
      iterd =_double.find(vars[i]);
      if (iterd!=_double.end())
	mod_pack.getVal(vars[i], *(iterd->second));
    }  
  
  vars = mod_pack.getStrings();
  for (i=0; i<vars.size(); i++)
    {
      iters =_string.find(vars[i]);
      if (iters!=_string.end())
	mod_pack.getVal(vars[i], *(iters->second));
    }

  vars = mod_pack.getInts1D();
  for (i=0; i<vars.size(); i++)
    {
      itervi =_int1D.find(vars[i]);
      if (itervi!=_int1D.end())
	mod_pack.getVal(vars[i], *(itervi->second));
    }

  vars = mod_pack.getDoubles1D();
  for (i=0; i<vars.size(); i++)
    {
      itervd =_double1D.find(vars[i]);
      if (itervd!=_double1D.end())
	mod_pack.getVal(vars[i], *(itervd->second));
    }

  vars = mod_pack.getStrings1D();
  for (i=0; i<vars.size(); i++)
    {
      itervs =_string1D.find(vars[i]);
      if (itervs!=_string1D.end())
	mod_pack.getVal(vars[i], *(itervs->second));
    }*/
}

Interface* Geometry::Pack( void )
{
   mod_pack._type = 2; //Module
   mod_pack._category = 1; // normal modules
   mod_pack._id = id;

   std::map<std::string, long *>::iterator iteri;
   std::map<std::string, double *>::iterator iterd;
   std::map<std::string, std::vector<long> *>::iterator itervi;
   std::map<std::string, std::vector<double> *>::iterator itervd;
   std::map<std::string, std::vector<std::string> *>::iterator itervs;


   for(iteri=_int.begin(); iteri!=_int.end(); iteri++)
      mod_pack.setVal(iteri->first, *(iteri->second));

   for(iterd=_double.begin(); iterd!=_double.end(); iterd++)
      mod_pack.setVal(iterd->first, *(iterd->second));

   for(itervi=_int1D.begin(); itervi!=_int1D.end(); itervi++)
      mod_pack.setVal(itervi->first, *(itervi->second));

   for(itervd=_double1D.begin(); itervd!=_double1D.end(); itervd++)
      mod_pack.setVal(itervd->first, *(itervd->second));

   for(itervs=_string1D.begin(); itervs!=_string1D.end(); itervs++)
   {
	   std::vector<std::string> * y;
	   std::string x;
	   x=itervs->first;
	   y=itervs->second;
      mod_pack.setVal(itervs->first, *(itervs->second));
   }
   return &mod_pack;
}

//////////////////
//event handling//
//////////////////
//////////////////////////////////////////////////
void Geometry::_onGeometry( wxScrollEvent& event )
{
   event.GetInt();
/*   ((UI_Tabs *)_parent)->cSc = geomLODSlider->GetValue();
   ((UI_Tabs *)_parent)->cId = CHANGE_LOD_SCALE;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();*/
}

//////////////////////////////////////////////////
void Geometry::ChangeOpacity( wxScrollEvent& event )
{
   event.GetInt();
/*   ((UI_Tabs *)_parent)->cPre_state = 0;
   ((UI_Tabs *)_parent)->cSc = _geometryRBox->GetSelection();
   ((UI_Tabs *)_parent)->cMin = geomOpacitySlider->GetValue();
   ((UI_Tabs *)_parent)->cId = UPDATE_GEOMETRY;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();*/
}

//////////////////////////////////////////////////
void Geometry::_onUpdate(wxCommandEvent& event)
{
   event.GetInt();
/*   ((UI_Tabs *)_parent)->cGeo_state = 0;
   ((UI_Tabs *)_parent)->cPre_state = 1;
   for(int i = 0; i < ((UI_Tabs *)_parent)->num_geo; i++)
   {
      if ( _geometryCBox->IsChecked( i ) )
         ((UI_Tabs *)_parent)->cGeo_state += (int)pow( 2.0f, (float)i );
   }
   std::cout << " Geometry::_onUpdate : " << 
         ((UI_Tabs *)_parent)->cGeo_state << std::endl;
   ((UI_Tabs *)_parent)->cId  = UPDATE_GEOMETRY;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();*/
}

