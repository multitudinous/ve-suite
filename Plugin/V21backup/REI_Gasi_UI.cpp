#include "REI_Gasi_UI.h"

IMPLEMENT_DYNAMIC_CLASS(REI_Gasi_UI, UIDialog)

  
BEGIN_EVENT_TABLE(REI_Gasi_UI, UIDialog)
//    EVT_TEXT(SLURRY_TEMP, REI_Gasi_UI::Calc)
//    EVT_TEXT(SLURRY_FLRT, REI_Gasi_UI::Calc)
//    EVT_TEXT(SLURRY_COAL, REI_Gasi_UI::Calc)
//    EVT_TEXT(SLURRY_CHAR, REI_Gasi_UI::Calc)
//    EVT_TEXT(SLURRY_WATER, REI_Gasi_UI::Calc)
EVT_TEXT(WC_RATIO, REI_Gasi_UI::OnChange)
//EVT_TEXT(OC_RATIO
END_EVENT_TABLE()

REI_Gasi_UI:: REI_Gasi_UI(wxWindow* parent, int id)
  : UIDialog((wxWindow *) parent, id, "REI Gasifier"),
    s_temp(422.0),
    s_flrt(0.0),
    i_temp(422.0),
    i_angle(0.0),
    d_wc(0.45),
    d_oc(0.4270)
{
  wxBoxSizer *top_sizer = new wxBoxSizer(wxHORIZONTAL);
 
  wxBoxSizer *left_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer *right_sizer = new wxBoxSizer(wxVERTICAL);
  
  wxSize sz(185, 17);

  top_sizer->Add(5, 5);
  top_sizer->Add(left_sizer, 0, wxALIGN_LEFT);
  top_sizer->Add(5, 5);
  top_sizer->Add(right_sizer, 0, wxALIGN_RIGHT);
  top_sizer->Add(5, 5);
  wxStaticBox *steam_box = new wxStaticBox(this, -1, "Steam");
  wxStaticBoxSizer *steam_sizer = new wxStaticBoxSizer(steam_box, wxVERTICAL);
  
  //  wxStaticBox *slurry_box = new wxStaticBox(this, -1, "Slurry");
  //  wxStaticBoxSizer *slurry_sizer = new wxStaticBoxSizer(slurry_box, wxVERTICAL);

  wxStaticBox *ratio_box = new wxStaticBox(this, -1, "Ratio");
  wxStaticBoxSizer *ratio_sizer = new wxStaticBoxSizer(ratio_box, wxVERTICAL);

  wxBoxSizer *ok_row = new wxBoxSizer(wxHORIZONTAL);

  wxStaticBox *inject_box = new wxStaticBox(this, -1, "Injection");
  wxStaticBoxSizer *inject_sizer = new wxStaticBoxSizer(inject_box, wxVERTICAL);

    
  wxBoxSizer* steam_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* steam_second_row = new wxBoxSizer(wxHORIZONTAL);
  /*  
      wxBoxSizer* slurry_first_row = new wxBoxSizer(wxHORIZONTAL);
      wxBoxSizer* slurry_second_row = new wxBoxSizer(wxHORIZONTAL);
      wxBoxSizer* slurry_third_row = new wxBoxSizer(wxHORIZONTAL);
      wxBoxSizer* slurry_forth_row = new wxBoxSizer(wxHORIZONTAL);
      wxBoxSizer* slurry_fifth_row = new wxBoxSizer(wxHORIZONTAL);
  */
  wxBoxSizer* ratio_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* ratio_second_row = new wxBoxSizer(wxHORIZONTAL);

  steam_sizer->Add(steam_first_row);
  steam_sizer->Add(10, 3, 0);
  steam_sizer->Add(steam_second_row);
  steam_sizer->Add(10, 3, 0);
  /*
    slurry_sizer->Add(slurry_first_row);
    slurry_sizer->Add(10, 3, 0);
    slurry_sizer->Add(slurry_second_row);
    slurry_sizer->Add(10, 3, 0);
    slurry_sizer->Add(slurry_third_row);
    slurry_sizer->Add(10, 3, 0);
    slurry_sizer->Add(slurry_forth_row);
    slurry_sizer->Add(10, 3, 0);
    slurry_sizer->Add(slurry_fifth_row);
    slurry_sizer->Add(10, 3, 0);
  */
  ratio_sizer->Add(ratio_first_row);
  ratio_sizer->Add(10, 3, 0);
  ratio_sizer->Add(ratio_second_row);
  ratio_sizer->Add(10, 3, 0);
  
  
  ok_b = new wxButton(this, wxID_OK, "OK");
  cancel_b = new wxButton(this, wxID_CANCEL, "Cancel");

  ok_row->Add(ok_b, 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(cancel_b, 0, wxALIGN_CENTER_HORIZONTAL);

  right_sizer->Add(5, 12);
  right_sizer->Add(steam_sizer, 0, wxALIGN_CENTER_HORIZONTAL); 
  right_sizer->Add(10, 12);
  //right_sizer->Add(slurry_sizer,0, wxALIGN_CENTER_HORIZONTAL);
  //  right_sizer->Add(5, 12);
  right_sizer->Add(ratio_sizer,0, wxALIGN_CENTER_HORIZONTAL);
  right_sizer->Add(5, 12);
  right_sizer->Add(inject_sizer, 0, wxALIGN_CENTER_HORIZONTAL);
  right_sizer->Add(5, 12);
  right_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  right_sizer->Add(5, 12);
  //Picture * pic = new Picture(this);
  
  wxImage* my_img = new wxImage("target.jpg", wxBITMAP_TYPE_JPEG);
  wxBitmap* m_bitmap = new wxBitmap(my_img);
  // pic ->SetBitmap(*m_bitmap);

  wxStaticBitmap *pic = new wxStaticBitmap(this, -1, *m_bitmap);
  wxBoxSizer* inject_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* inject_second_row = new wxBoxSizer(wxHORIZONTAL);
  
  inject_sizer->Add(inject_first_row);
  inject_sizer->Add(10, 3, 0);
  inject_sizer->Add(inject_second_row);
  inject_sizer->Add(10, 3, 0);
  
  left_sizer->Add(5, 12);
  left_sizer->Add(pic, 0, wxALIGN_CENTER_HORIZONTAL);
  
  //Instantiate the steam row
  wxStaticText * label0 = new wxStaticText(this, -1, "    Temperature (K)", wxDefaultPosition, sz);
  steam_temp = new wxTextCtrl(this, STEAM_TEMP, wxT("422.0"), wxDefaultPosition, wxSize(80, 20));
  steam_temp->Enable(false);
  
  steam_first_row->Add(label0);
  steam_first_row->Add(steam_temp);
  
  wxStaticText * label1 = new wxStaticText(this, -1, "    Flowrate (kg/s)", wxDefaultPosition, sz);
  steam_flrt = new wxTextCtrl(this, STEAM_FLRT, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  steam_flrt->Enable(false);

  steam_second_row->Add(label1);
  steam_second_row->Add(steam_flrt);

  /*  
      wxStaticText * label2 = new wxStaticText(this, -1, "    Temperature (K)", wxDefaultPosition, sz);
      slurry_temp = new wxTextCtrl(this, SLURRY_TEMP, wxT("422.0"), wxDefaultPosition, wxSize(80, 20));
      
      slurry_first_row->Add(label2);
      slurry_first_row->Add(slurry_temp);
      
      wxStaticText * label3 = new wxStaticText(this, -1, "    Flowrate (kg/s)", wxDefaultPosition, sz);
      slurry_flrt = new wxTextCtrl(this, SLURRY_FLRT, wxT("43.46"), wxDefaultPosition, wxSize(80, 20));
      
      slurry_second_row->Add(label3);
      slurry_second_row->Add(slurry_flrt);
      
      wxStaticText * label4 = new wxStaticText(this, -1, "    Coal (%)", wxDefaultPosition, sz);
      slurry_coal = new wxTextCtrl(this, SLURRY_COAL, wxT("74.26"), wxDefaultPosition, wxSize(80, 20));
      
      slurry_third_row->Add(label4);
      slurry_third_row->Add(slurry_coal);
      
      wxStaticText * label5 = new wxStaticText(this, -1, "    Re-Cycled Char (%)", wxDefaultPosition, sz);
      slurry_char = new wxTextCtrl(this, SLURRY_CHAR, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
      
      slurry_forth_row->Add(label5);
      slurry_forth_row->Add(slurry_char);
      
      wxStaticText * label6 = new wxStaticText(this, -1, "    Water (%)", wxDefaultPosition, sz);
      slurry_water = new wxTextCtrl(this, SLURRY_WATER, wxT("25.74"), wxDefaultPosition, wxSize(80, 20));
      
      slurry_fifth_row->Add(label6);
      slurry_fifth_row->Add(slurry_water);
  */
  wxStaticText * label7 = new wxStaticText(this, -1, "Water To Carbon mol(H2O) / C", wxDefaultPosition, sz);

  wxString wcs[] = { wxT("0.45"), wxT("0.5"), wxT("0.55") };

  wc_ratio = new wxComboBox(this, WC_RATIO, wxT("0.45"), wxDefaultPosition, wxSize(80, 20), 3,  wcs, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);


  ratio_first_row->Add(label7);
  ratio_first_row->Add(wc_ratio);

  wxString ocs1[] = { wxT("0.4270"), wxT("0.4376"), wxT("0.4426"), wxT("0.4450"), wxT("0.4463") };
 
  wxStaticText * label8 = new wxStaticText(this, -1, "Oxygen To Carbon mol(O2) / C", wxDefaultPosition, sz);
  oc_ratio = new wxComboBox(this, OC_RATIO, wxT("0.4270"), wxDefaultPosition, wxSize(80, 20), 5, ocs1, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);

  //oc_ratio->Enable(false);

  ratio_second_row->Add(label8);
  ratio_second_row->Add(oc_ratio);

  wxStaticText * label9 = new wxStaticText(this, -1, "    Temperature", wxDefaultPosition, sz);
  inject_temp = new wxTextCtrl(this, INJECT_TEMP, wxT("422.0"), wxDefaultPosition, wxSize(80, 20));
  inject_temp->Enable(false);
  
  inject_first_row->Add(label9);
  inject_first_row->Add(inject_temp);

  wxStaticText * label10 = new wxStaticText(this, -1, "    Spray Angle", wxDefaultPosition, sz);
  inject_angle = new wxTextCtrl(this, INJECT_ANGLE, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  inject_angle->Enable(false);

  inject_second_row->Add(label10);
  inject_second_row->Add(inject_angle);

  top_sizer->Fit(this);
  SetAutoLayout(true);
  SetSizer(top_sizer);
}

REI_Gasi_UI::~REI_Gasi_UI()
{

}

void REI_Gasi_UI:: double2entry(wxTextCtrl* entry, double * value)
{
  wxString txt;
  txt<<(*value);
  entry->SetValue(txt);
}

void REI_Gasi_UI::entry2double(wxTextCtrl* entry, double * value)
{
  wxString txt;
  txt=entry->GetValue();
  (*value) = atof(txt.c_str());
}

bool REI_Gasi_UI::TransferDataToWindow()
{
  
  double2entry(steam_temp, &s_temp);
  double2entry(steam_flrt, &s_flrt);
  /*
    double2entry(slurry_temp, &sl_temp);
    double2entry(slurry_flrt, &sl_flrt);
    double2entry(slurry_coal, &sl_coal);
    double2entry(slurry_char, &sl_char);
    double2entry(slurry_water, &sl_water);
  */
  double2entry(inject_temp, &i_temp);
  double2entry(inject_angle, &i_angle);
  //  double2entry(wc_ratio, &d_wc);

  wxString txt;
  txt<<(d_wc);
  wc_ratio->SetValue(txt);

  //double2entry(oc_ratio, &d_oc);

  return true;
}

bool REI_Gasi_UI::TransferDataFromWindow()
{
  entry2double(steam_temp, &s_temp);
  entry2double(steam_flrt, &s_flrt);
  /*
    entry2double(slurry_temp, &sl_temp);
    entry2double(slurry_flrt, &sl_flrt);
    entry2double(slurry_coal, &sl_coal);
    entry2double(slurry_char, &sl_char);
    entry2double(slurry_water, &sl_water);
  */
  entry2double(inject_temp, &i_temp);
  entry2double(inject_angle, &i_angle);
  //entry2double(wc_ratio, &d_wc);
  wxString txt;
  txt=wc_ratio->GetValue();
  d_wc = atof(txt.c_str());
  //entry2double(oc_ratio, &d_oc);
  return true;
}

//void REI_Gasi_UI::Calc(wxCommandEvent& event)
//{
// 
//}
void REI_Gasi_UI::OnChange(wxCommandEvent& event)
{
	wxString txt;
	wxString v1[]={"0.4270", "0.4376", "0.4426", "0.4450", "0.4463"};
	wxString v2[]={"0.4341", "0.4437", "0.4482", "0.4504", "0.4517"};
	wxString v3[]={"0.4412", "0.4499", "0.4540", "0.4560", "0.4572"};
	long lastpos;
	unsigned int i;

	txt = wc_ratio->GetValue();
	
	oc_ratio->Clear();
	if (txt=="0.45")
	{
		for(i=0; i<5; i++)
			oc_ratio->Append(v1[i]);
	} 
	else if (txt=="0.5")
	{
		for(i=0; i<5; i++)
			oc_ratio->Append(v2[i]);
	}
	else
	{
		for(i=0; i<5; i++)
			oc_ratio->Append(v3[i]);
	}

	oc_ratio->SetSelection(0);
}
