#include "GasSplitter_UI.h"

IMPLEMENT_DYNAMIC_CLASS(GasSplitter_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
GasSplitter_UI_Dialog
::GasSplitter_UI_Dialog
(wxWindow* parent, int id,
  double* percent_port1,
  double* percent_port2,
  double* percent_port3,
  double* percent_port4)
: UIDialog((wxWindow *) parent, id, "GasSplitter"),
  p_percent_port1(percent_port1),
  p_percent_port2(percent_port2),
  p_percent_port3(percent_port3),
  p_percent_port4(percent_port4)
{
  wxBoxSizer* toptop= new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* left_margin = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* right_margin = new wxBoxSizer(wxHORIZONTAL);

  left_margin->Add(5, 10);
  right_margin->Add(5, 10);
  toptop->Add(left_margin, 0, wxALIGN_LEFT);
  toptop->Add(top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL);
  toptop->Add(right_margin, 0, wxALIGN_RIGHT);

  wxStaticBox *gui1_box = new wxStaticBox(this, -1, "Percentage Flow");
  
  wxStaticBoxSizer *data1_row = new wxStaticBoxSizer(gui1_box, wxVERTICAL);

  wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 10, 0); //the top margin
  top_sizer->Add(data1_row, 0); 
  top_sizer->Add(10, 5, 0); 
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0); //the bottom margin

  wxBoxSizer *data1_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_forth_row = new wxBoxSizer(wxHORIZONTAL);
  
  data1_row->Add(10, 5, 0);
  data1_row->Add(data1_first_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_second_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_third_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_forth_row, 0);
  data1_row->Add(10, 3, 0);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);

  wxStaticText * label1 = new wxStaticText(this, -1, " Gas Port 0", wxDefaultPosition, wxSize(200, 17));
  t_percent_port1 = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_first_row->Add(label1);
  data1_first_row->Add(t_percent_port1);

  wxStaticText * label2 = new wxStaticText(this, -1, " Gas Port 1", wxDefaultPosition, wxSize(200, 17));
  t_percent_port2 = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_second_row->Add(label2);
  data1_second_row->Add(t_percent_port2);

  wxStaticText * label3 = new wxStaticText(this, -1, " Gas Port 2", wxDefaultPosition, wxSize(200, 17));
  t_percent_port3 = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_third_row->Add(label3);
  data1_third_row->Add(t_percent_port3);

  wxStaticText * label4 = new wxStaticText(this, -1, " Gas Port 3", wxDefaultPosition, wxSize(200, 17));
  t_percent_port4 = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_forth_row->Add(label4);
  data1_forth_row->Add(t_percent_port4);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);  
}

/////////////////////////////////////////////////////
GasSplitter_UI_Dialog
::~GasSplitter_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool GasSplitter_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_percent_port1->GetValue();
  (*p_percent_port1) = atof(txt.c_str());
  
  txt = t_percent_port2->GetValue();
  (*p_percent_port2) = atof(txt.c_str());

  txt = t_percent_port3->GetValue();
  (*p_percent_port3) = atof(txt.c_str());
  
  txt = t_percent_port4->GetValue();
  (*p_percent_port4) = atof(txt.c_str());

  return true;
}

////////////////////////////////////////////////////
bool GasSplitter_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4;

  txt1<<(*p_percent_port1);
  t_percent_port1->SetValue(txt1);

  txt2<<(*p_percent_port2);
  t_percent_port2->SetValue(txt2);

  txt3<<(*p_percent_port3);
  t_percent_port3->SetValue(txt3);

  txt4<<(*p_percent_port4);
  t_percent_port4->SetValue(txt4);

  return true;
}

void GasSplitter_UI_Dialog::Lock(bool l)
{
}

