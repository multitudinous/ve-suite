#include "ASU_UI.h"
#include "wx/combobox.h"

IMPLEMENT_DYNAMIC_CLASS(ASU_UI_Dialog, UIDialog);

ASU_UI_Dialog::ASU_UI_Dialog(wxWindow* parent, int id, long *num_idx_spare_trains, double *o2_purity_d)
  : UIDialog((wxWindow *) parent, id, "ASU"),
    num_spare_trains_(num_idx_spare_trains),
    o2_purity_d_(o2_purity_d)
{

  wxBoxSizer* toptop = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* left_margin = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* right_margin = new wxBoxSizer(wxHORIZONTAL);
  left_margin->Add(5, 10);
  right_margin->Add(5, 10);
  toptop->Add(left_margin, 0, wxALIGN_LEFT);
  toptop->Add(top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL);
  toptop->Add(right_margin, 0, wxALIGN_RIGHT);

  wxBoxSizer* first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* third_row = new wxBoxSizer(wxHORIZONTAL);

  wxSize sz(200, 20); // the size for the label
  top_sizer->Add(10, 10, 1);
  top_sizer->Add(first_row, 1, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(second_row, 1);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(third_row, 1, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);


  // oxygen purity
  wxStaticText * label3 = new wxStaticText(this, -1, "Oxygen Purity (%)", wxDefaultPosition, sz);
  o2_purity = new wxTextCtrl(this, OXYGEN_PURITY, "95", wxDefaultPosition, wxSize(60, 20));
  o2_purity->Enable(true);

  first_row->Add( label3 );
  first_row->Add( o2_purity );

  // number of spare trains (spare asu's)
  wxString nums[] = { wxT("0"), wxT("1"), wxT("2") };

  wxStaticText * label2 = new wxStaticText(this, -1, "Number of Spare Trains", wxDefaultPosition, sz);
  num_spare_trains = new wxComboBox(this,  NUM_SPARE_TRAINS, wxT("0"), wxDefaultPosition, wxSize(60,20), 3,  nums, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
  
  second_row->Add(label2);
  second_row->Add(num_spare_trains);

  // buttons
  third_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  third_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);
  
}

//////////////////////////////////////////////////////////////////////////////////
ASU_UI_Dialog::~ASU_UI_Dialog()
{
	
}

///////////////////////////////////////////////////////////////////////////////////
bool ASU_UI_Dialog::TransferDataFromWindow()
{
	wxString txt2, txt3;
	
	txt2 = num_spare_trains->GetValue();
	txt3 = o2_purity->GetValue();
	
	(*num_spare_trains_)=atoi(txt2.c_str());
	
	(*o2_purity_d_)=atof(txt3.c_str());
	
	if ((*o2_purity_d_)>100.0 || (*o2_purity_d_)<0.0)
    {
		wxMessageBox("Oxygen Purity should be between 0 and 100%", "Error");
		return false;
    } 
	return true;
}

////////////////////////////////////////////////////////////////////////////////////
bool ASU_UI_Dialog::TransferDataToWindow()
{
	wxString txt2, txt3;
	
	txt2<<(*num_spare_trains_);
	txt3<<(*o2_purity_d_);
	
	num_spare_trains->SetValue(txt2);
	o2_purity->SetValue(txt3);
	return true;
}
