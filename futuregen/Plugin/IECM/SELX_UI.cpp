#include "SELX_UI.h"
#include "wx/combobox.h"

IMPLEMENT_DYNAMIC_CLASS(SELX_UI_Dialog, UIDialog);

SELX_UI_Dialog::SELX_UI_Dialog(wxWindow* parent, int id, long *num_idx_nspares, double *co2_removal_d)
: UIDialog((wxWindow *) parent, id, "CO2 Capture"),
num_spares_(num_idx_nspares),
co2_removal_d_(co2_removal_d)
{
    
    wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
    wxBoxSizer* first_row = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer* second_row = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer* third_row = new wxBoxSizer(wxHORIZONTAL);
    
    wxSize sz(210, 20); // the size for the label
    top_sizer->Add(10, 10, 1);
    top_sizer->Add(first_row, 1); //, wxALIGN_CENTER_HORIZONTAL);
    top_sizer->Add(10, 5, 0);
    top_sizer->Add(second_row, 1); //,wxALIGN_CENTER_HORIZONTAL );
    top_sizer->Add(10, 5, 0);
    top_sizer->Add(third_row, 1, wxALIGN_CENTER_HORIZONTAL);
    top_sizer->Add(10, 5, 0);
    
	// CO2 Capture
    wxStaticText* label5 = new wxStaticText(this, -1, "    CO2 Capture (%)", wxDefaultPosition, sz);
    co2_removal = new wxTextCtrl(this, CO2_REMOVAL, "90", wxDefaultPosition, wxSize(60, 20));
    
    first_row->Add(label5);
    first_row->Add(co2_removal);
    
    // number of spare trains (spare CO2 units)
	wxString nums[] = { wxT("0"), wxT("1"), wxT("2") };
	
	wxStaticText * label2 = new wxStaticText(this, -1, "    Number of Spare Absorbers", wxDefaultPosition, sz);
	num_spares = new wxComboBox(this,  NUM_SPARES, wxT("0"), wxDefaultPosition, wxSize(60,20), 3,  nums, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
	wxStaticText * label4 = new wxStaticText(this, -1, "    ", wxDefaultPosition, wxSize(20, 20));

	second_row->Add(label2);
	second_row->Add(num_spares);
	second_row->Add(label4);
    
	// buttons
    third_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
    third_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
    
    SetSizer(top_sizer);
    SetAutoLayout(TRUE);
    top_sizer->Fit(this);
    
}

////////////////////////////////////////////////////////////////////////
SELX_UI_Dialog::~SELX_UI_Dialog()
{
    
}

////////////////////////////////////////////////////////////////////////
bool SELX_UI_Dialog::TransferDataFromWindow()
{
    wxString txt2, txt3;
    
    txt2 = num_spares->GetValue();
    txt3 = co2_removal->GetValue();
    
    (*num_spares_)=atoi(txt2.c_str());
    
    (*co2_removal_d_)=atof(txt3.c_str());
    
    if ((*co2_removal_d_)>100.0 || (*co2_removal_d_)<0.0)
    {
        wxMessageBox("CO2 capture percentage should be between 0 and 100%", "Error");
        return false;
    }
    
    return true;
}

/////////////////////////////////////////////////////////////////////////
bool SELX_UI_Dialog::TransferDataToWindow()
{
    wxString txt2, txt3;
    
    txt2<<(*num_spares_);
    txt3<<(*co2_removal_d_);
    
    num_spares->SetValue(txt2);
    co2_removal->SetValue(txt3);
    return true;
}
