#include "Gasi_UI.h"
#include "wx/combobox.h"

IMPLEMENT_DYNAMIC_CLASS(GASI_UI_Dialog, UIDialog);

BEGIN_EVENT_TABLE(GASI_UI_Dialog, UIDialog)
END_EVENT_TABLE()


GASI_UI_Dialog::GASI_UI_Dialog(wxWindow* parent, int id, long *tex_idx_in_ntrains, long* tex_idx_in_nspares, double *tex_idx_in_temp, double *wc_ratio)
: UIDialog((wxWindow *) parent, id, "Gasifier"),
tex_ntrains_(tex_idx_in_ntrains),
tex_nspares_(tex_idx_in_nspares),
tex_temp_(tex_idx_in_temp),
wc_ratio_(wc_ratio)
{
	wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
	wxBoxSizer* first_row = new wxBoxSizer(wxHORIZONTAL);
	wxBoxSizer* second_row = new wxBoxSizer(wxHORIZONTAL);
	wxBoxSizer* third_row = new wxBoxSizer(wxHORIZONTAL);
	wxBoxSizer* fourth_row = new wxBoxSizer(wxHORIZONTAL);
	wxBoxSizer* fifth_row = new wxBoxSizer(wxHORIZONTAL);
	wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);
	
	wxSize sz(200, 20); // the size for the lable
	top_sizer->Add(10, 10, 1);
	//  top_sizer->Add(coal_row, 1);
	// top_sizer->Add(10, 5, 0);
	top_sizer->Add(first_row, 1);
	top_sizer->Add(10, 5, 0);
	top_sizer->Add(second_row, 1);
	top_sizer->Add(10, 5, 0);
	top_sizer->Add(third_row, 1);
	top_sizer->Add(10, 5, 0);
	top_sizer->Add(fourth_row, 1);
	top_sizer->Add(10, 5, 0);
	top_sizer->Add(fifth_row, 1);
	top_sizer->Add(10, 5, 0);
	top_sizer->Add(ok_row, 1, wxALIGN_CENTER_HORIZONTAL);
	top_sizer->Add(10, 5, 0);
	
	
	// temperature - row 1
	wxString temps[] = { wxT("2250"), wxT("2350"), wxT("2450"), wxT("2550"), wxT("2650") };
	
	wxStaticText * label1 = new wxStaticText(this, -1, "    Gasifier Temperature (deg.F)", wxDefaultPosition, sz);
	gasi_temp = new wxComboBox(this,  GASI_TEMP, wxT("2350"), wxDefaultPosition, wxSize(60, 20), 5, temps, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
	wxStaticText * label2 = new wxStaticText(this, -1, " ", wxDefaultPosition, wxSize(25, 20));
	
	first_row->Add(label1);
	first_row->Add(gasi_temp);
	first_row->Add(label2);
	gasi_temp->Enable(true);
	
	// Pressure - row 2                                    
	wxStaticText * label6 = new wxStaticText(this, -1, "    Pressure (psia)", wxDefaultPosition, sz);
	pressure = new wxTextCtrl(this, PRESSURE, "615.0", wxDefaultPosition, wxSize(60, 20));
	pressure->Enable(false);
	
	second_row->Add( label6 );
	second_row->Add( pressure );
	
	// water to carbon
	wxString wcs[] = { wxT("0.45"), wxT("0.50"), wxT("0.55") };
	
	wxStaticText * label4 = new wxStaticText(this, -1, "    Water To Carbon mol(H2O) / C", wxDefaultPosition, sz);
	steam_input = new wxComboBox(this, STEAM_INPUT, wxT("0.50"), wxDefaultPosition, wxSize(60, 20), 3,  wcs, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
	
	third_row->Add( label4 );
	third_row->Add( steam_input );
	
	
	// number of trains - this is specified by the number of turbines in the power block - cannot be independently specified
	wxString ntrains[] = { wxT("1"), wxT("2"), wxT("3") };
	wxStaticText * label3 = new wxStaticText(this, -1, "    Number of Operating Trains", wxDefaultPosition, sz);
	tex_ntrains = new wxComboBox(this,  TEX_NTRAINS, wxT("2"), wxDefaultPosition, wxSize(60, 20), 3,  ntrains, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
	
	fourth_row->Add(label3);
	fourth_row->Add(tex_ntrains);
	
	
	// number of spare gasifiers
	wxString nspares[] = { wxT("0"), wxT("1"), wxT("2") };
	wxStaticText * label5 = new wxStaticText(this, -1, "    Number of Spare Trains", wxDefaultPosition, sz);
	tex_nspares = new wxComboBox(this,  TEX_NSPARES, wxT("1"), wxDefaultPosition, wxSize(60, 20), 3,  nspares, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
	
	fifth_row->Add(label5);
	fifth_row->Add(tex_nspares);
	
	
	// buttons
	ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
	ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
	
	SetSizer(top_sizer);
	SetAutoLayout(TRUE);
	top_sizer->Fit(this);
	
}

///////////////////////////////////////////////////////////////////////////
GASI_UI_Dialog::~GASI_UI_Dialog()
{
}

///////////////////////////////////////////////////////////////////////////
bool GASI_UI_Dialog::TransferDataFromWindow()
{
	wxString txt1, txt2, txt3, txt4;
	
	txt1=tex_ntrains->GetValue();
	txt2=tex_nspares->GetValue();
	txt3=gasi_temp->GetValue();
	txt4=steam_input->GetValue();
	
	(*tex_ntrains_) = atoi(txt1.c_str());
	(*tex_nspares_) = atoi(txt2.c_str());
	(*tex_temp_) = atoi(txt3.c_str());
	(*wc_ratio_) = atof(txt4.c_str());
	
	return true;
}

///////////////////////////////////////////////////////////////////////////
bool GASI_UI_Dialog::TransferDataToWindow()
{
	wxString txt1, txt2, txt3, txt4;
	
	txt1<<(*tex_ntrains_);
	txt2<<(*tex_nspares_);
	txt3<<(*tex_temp_);
	
	if((*wc_ratio_)==0.45)txt4="0.45";
	else if((*wc_ratio_)==0.50)txt4="0.50";
	else txt4="0.55";
	
	tex_ntrains->SetValue(txt1);
	tex_nspares->SetValue(txt2);
	gasi_temp->SetValue(txt3);
	steam_input->SetValue(txt4);
	
	return true;
}


