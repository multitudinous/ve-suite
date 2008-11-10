#include "OPPD_UI_constants.h"

OPPD_UI_constants::OPPD_UI_constants(wxNotebook* parent)
:wxPanel(parent)
{
	_buildPage();
}

OPPD_UI_constants::~OPPD_UI_constants()
{
}

void OPPD_UI_constants::_buildPage( void )
{
	_compcap = new wxStaticText(this, -1, wxT("Oil Capacity: 7.5 gallons"));
	_comptype = new wxStaticText(this, -1, wxT("Oil Type: Lube Oil"));
	_compspillarea = new wxStaticText(this, -1, wxT("Estimated Oil Spill Area: 80 sq ft"));

	wxStaticBox* compBox = new wxStaticBox(this, -1, "Air Compressor Assumptions");
    wxStaticBoxSizer* compGroup = new wxStaticBoxSizer(compBox,wxVERTICAL);
	compGroup->Add(_compcap,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	compGroup->Add(_comptype,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	compGroup->Add(_compspillarea,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	_fwpumpcap = new wxStaticText(this, -1, wxT("Oil Capacity: 5.5 gallons"));
	_fwpumptype = new wxStaticText(this, -1, wxT("Oil Type: Lube Oil"));
	_fwpumpspillarea = new wxStaticText(this, -1, wxT("Estimated Oil Spill Area: 48 sq ft"));

	wxStaticBox* fwpumpBox = new wxStaticBox(this, -1, "Auxilary Feedwater Pump Assumptions");
    wxStaticBoxSizer* fwpumpGroup = new wxStaticBoxSizer(fwpumpBox,wxVERTICAL);
	fwpumpGroup->Add(_fwpumpcap,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	fwpumpGroup->Add(_fwpumptype,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	fwpumpGroup->Add(_fwpumpspillarea,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	_roomlength = new wxStaticText(this, -1, wxT("Length: 220 ft"));
	_roomheight = new wxStaticText(this, -1, wxT("Height: 21.3 ft"));
	_roomwidth = new wxStaticText(this, -1, wxT("Width: 26 ft"));

	wxStaticBox* roomdimBox = new wxStaticBox(this, -1, "Room 19 Dimensions");
    wxStaticBoxSizer* roomdimGroup = new wxStaticBoxSizer(roomdimBox,wxVERTICAL);
	roomdimGroup->Add(_roomlength,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	roomdimGroup->Add(_roomheight,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	roomdimGroup->Add(_roomwidth,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	wxBoxSizer* pageLayout = new wxBoxSizer(wxVERTICAL);
    pageLayout->Add(compGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	pageLayout->Add(fwpumpGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	pageLayout->Add(roomdimGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	//set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(pageLayout);
   pageLayout->Fit(this);  

}