#include "Gasifier_UI.h"
#include "wx/image.h" //This line have to be under the first line for some unknown reason to pass the compilation

BEGIN_EVENT_TABLE(Gasi2Tabs, wxNotebook)
  EVT_RADIOBUTTON(R_STAGE1, Gasi2Tabs::OnChangeStage)
  EVT_RADIOBUTTON(R_STAGE2, Gasi2Tabs::OnChangeStage)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(Gasifier_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
Gasifier_UI_Dialog
::Gasifier_UI_Dialog
(wxWindow* parent, int id,
  double* steam_temp1,
  double* steam_flrt1,
  double* slurry_temp1,
  double* slurry_flrt1,
  double* coal_percent1,
  double* char_percent1,
  double* steam_temp2,
  double* steam_flrt2,
  double* slurry_temp2,
  double* slurry_flrt2,
  double* coal_percent2,
  double* char_percent2,
  double* steam_temp3,
  double* steam_flrt3,
  double* slurry_temp3,
  double* slurry_flrt3,
  double* coal_percent3,
  double* char_percent3,
  double* size_50,
  double* size_200,
  double* pres_drop,
  string* coal_type,
  long* stage)
: UIDialog((wxWindow *) parent, id, "Gasifier"),
  p_steam_temp1(steam_temp1),
  p_steam_flrt1(steam_flrt1),
  p_slurry_temp1(slurry_temp1),
  p_slurry_flrt1(slurry_flrt1),
  p_coal_percent1(coal_percent1),
  p_char_percent1(char_percent1),
  p_steam_temp2(steam_temp2),
  p_steam_flrt2(steam_flrt2),
  p_slurry_temp2(slurry_temp2),
  p_slurry_flrt2(slurry_flrt2),
  p_coal_percent2(coal_percent2),
  p_char_percent2(char_percent2),
  p_steam_temp3(steam_temp3),
  p_steam_flrt3(steam_flrt3),
  p_slurry_temp3(slurry_temp3),
  p_slurry_flrt3(slurry_flrt3),
  p_coal_percent3(coal_percent3),
  p_char_percent3(char_percent3),
  p_size_50(size_50),
  p_size_200(size_200),
  p_pres_drop(pres_drop),
  p_coal_type(coal_type),
  p_stage(stage)
{

  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
   
  m_tabs = new Gasi2Tabs(this);
  m_tabs->CreateInitialPages();
  m_sizerNotebook = new wxNotebookSizer(m_tabs);
  
  wxBoxSizer *ok_row=new wxBoxSizer(wxHORIZONTAL);
  
  
  top_sizer->Add(m_sizerNotebook, 1, wxEXPAND | wxALL, 4);// wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(5, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0); //the bottom margin

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  SetSizer(top_sizer);
  top_sizer->Layout();
  //SetAutoLayout(TRUE);
  top_sizer->Fit(this);
}

/////////////////////////////////////////////////////
Gasifier_UI_Dialog
::~Gasifier_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool Gasifier_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = m_tabs->t_steam_temp1->GetValue();
  (*p_steam_temp1) = atof(txt.c_str());
  txt = m_tabs->t_steam_flrt1->GetValue();
  (*p_steam_flrt1) = atof(txt.c_str());
  txt = m_tabs->t_slurry_temp1->GetValue();
  (*p_slurry_temp1) = atof(txt.c_str());
  txt = m_tabs->t_slurry_flrt1->GetValue();
  (*p_slurry_flrt1) = atof(txt.c_str());
  txt = m_tabs->t_coal_percent1->GetValue();
  (*p_coal_percent1) = atof(txt.c_str());
  txt = m_tabs->t_char_percent1->GetValue();
  (*p_char_percent1) = atof(txt.c_str());
  txt = m_tabs->t_steam_temp2->GetValue();
  (*p_steam_temp2) = atof(txt.c_str());
  txt = m_tabs->t_steam_flrt2->GetValue();
  (*p_steam_flrt2) = atof(txt.c_str());
  txt = m_tabs->t_slurry_temp2->GetValue();
  (*p_slurry_temp2) = atof(txt.c_str());
  txt = m_tabs->t_slurry_flrt2->GetValue();
  (*p_slurry_flrt2) = atof(txt.c_str());
  txt = m_tabs->t_coal_percent2->GetValue();
  (*p_coal_percent2) = atof(txt.c_str());
  txt = m_tabs->t_char_percent2->GetValue();
  (*p_char_percent2) = atof(txt.c_str());
  txt = m_tabs->t_steam_temp3->GetValue();
  (*p_steam_temp3) = atof(txt.c_str());
  txt = m_tabs->t_steam_flrt3->GetValue();
  (*p_steam_flrt3) = atof(txt.c_str());
  txt = m_tabs->t_slurry_temp3->GetValue();
  (*p_slurry_temp3) = atof(txt.c_str());
  txt = m_tabs->t_slurry_flrt3->GetValue();
  (*p_slurry_flrt3) = atof(txt.c_str());
  txt = m_tabs->t_coal_percent3->GetValue();
  (*p_coal_percent3) = atof(txt.c_str());
  txt = m_tabs->t_char_percent3->GetValue();
  (*p_char_percent3) = atof(txt.c_str());

  txt = m_tabs->t_pres_drop->GetValue();
  (*p_pres_drop) = atof(txt.c_str());

  (*p_stage) = m_tabs->r_stage1->GetValue();

  (*p_coal_type) = m_tabs->c_coal_type->GetValue();
  txt = m_tabs->t_size_50->GetValue();
  (*p_size_50) = atof(txt.c_str());
  txt = m_tabs->t_size_200->GetValue();
  (*p_size_200) = atof(txt.c_str());

  return true;
}

////////////////////////////////////////////////////
bool Gasifier_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4, txt5, txt6, txt7, txt8, txt9, txt10;
  wxString txt11, txt12, txt13, txt14, txt15, txt16, txt17, txt18, txt19, txt20;
  wxString txt21, txt22;

  txt1<<(*p_steam_temp1);
  m_tabs->t_steam_temp1->SetValue(txt1);

  txt2<<(*p_steam_flrt1);
  m_tabs->t_steam_flrt1->SetValue(txt2);

  txt3<<(*p_slurry_temp1);
  m_tabs->t_slurry_temp1->SetValue(txt3);

  txt4<<(*p_slurry_flrt1);
  m_tabs->t_slurry_flrt1->SetValue(txt4);

  txt5<<(*p_coal_percent1);
  m_tabs->t_coal_percent1->SetValue(txt5);

  txt6<<(*p_steam_temp2);
  m_tabs->t_steam_temp2->SetValue(txt6);

  txt7<<(*p_steam_flrt2);
  m_tabs->t_steam_flrt2->SetValue(txt7);

  txt8<<(*p_slurry_temp2);
  m_tabs->t_slurry_temp2->SetValue(txt8);

  txt9<<(*p_slurry_flrt2);
  m_tabs->t_slurry_flrt2->SetValue(txt9);

  txt10<<(*p_coal_percent2);
  m_tabs->t_coal_percent2->SetValue(txt10);

  txt11<<(*p_steam_temp3);
  m_tabs->t_steam_temp3->SetValue(txt11);

  txt12<<(*p_steam_flrt3);
  m_tabs->t_steam_flrt3->SetValue(txt12);

  txt13<<(*p_slurry_temp3);
  m_tabs->t_slurry_temp3->SetValue(txt13);

  txt14<<(*p_slurry_flrt3);
  m_tabs->t_slurry_flrt3->SetValue(txt14);

  txt15<<(*p_coal_percent3);
  m_tabs->t_coal_percent3->SetValue(txt15);

  txt16<<(*p_char_percent1);
  m_tabs->t_char_percent1->SetValue(txt16);

  txt17<<(*p_char_percent2);
  m_tabs->t_char_percent2->SetValue(txt17);

  txt18<<(*p_char_percent3);
  m_tabs->t_char_percent3->SetValue(txt18);

  txt19<<(*p_pres_drop);
  m_tabs->t_pres_drop->SetValue(txt19);

  txt20= p_coal_type->c_str();
  m_tabs->c_coal_type->SetValue(txt20);

  txt21<<(*p_size_50);
  m_tabs->t_size_50->SetValue(txt21);
  
  txt22<<(*p_size_200);
  m_tabs->t_size_200->SetValue(txt22);
  
  wxCommandEvent event;
  if (*p_stage)
    {
      m_tabs->r_stage2->SetValue(false);
      m_tabs->r_stage1->SetValue(true);
    }
  else
    {
      m_tabs->r_stage2->SetValue(true);
      m_tabs->r_stage1->SetValue(false);
    }

  m_tabs->OnChangeStage(event);

    return true;
}

void Gasifier_UI_Dialog::Lock(bool l)
{
}

Gasi2Tabs::Gasi2Tabs(wxWindow *parent, wxWindowID id,
	   const wxPoint& pos , 
	   const wxSize& size , 
	   long style)
  : wxNotebook(parent, id, pos, size, style)
{
}

void Gasi2Tabs::CreateInitialPages()
{
    wxPanel *panel = (wxPanel *) NULL;

    // Create and add some panels to the notebook

    panel = CreateFirstPage();
    AddPage( panel, _T("Flows"), false);

    panel = CreateSecondPage();
    AddPage( panel, _T("Misc"), true);
 
}

wxPanel *Gasi2Tabs::CreateFirstPage()
{
  wxPanel *panel = new wxPanel(this);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  
  wxBoxSizer* stage_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* second_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 5, 0);
  top_sizer->Add(stage_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(first_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(second_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);

  r_stage1 = new wxRadioButton(panel, R_STAGE1, _T(" One-stage Gasifier0D "), wxDefaultPosition, wxSize(150, 20), wxRB_GROUP);
  r_stage2 = new wxRadioButton(panel, R_STAGE2, _T(" Two-stage Gasifier0D "), wxDefaultPosition, wxSize(150, 20));

  stage_row->Add(r_stage1);
  stage_row->Add(r_stage2);

  wxStaticBox *lv1_box = new wxStaticBox(panel, -1, "Fuel Injectors - level 1");
  wxStaticBox *lv2_box = new wxStaticBox(panel, -1, "Fuel Injectors - level 2");
  wxStaticBox *lv3_box = new wxStaticBox(panel, -1, "Fuel Injectors - level 3");

  wxImage* my_img = new wxImage("target.jpg", wxBITMAP_TYPE_JPEG);
  wxBitmap* m_bitmap = new wxBitmap(my_img);
  wxStaticBitmap *pic = new wxStaticBitmap(panel, -1, *m_bitmap);
  wxStaticBoxSizer* lv1_row = new wxStaticBoxSizer(lv1_box, wxVERTICAL);
  wxStaticBoxSizer* lv2_row = new wxStaticBoxSizer(lv2_box, wxVERTICAL);
  wxStaticBoxSizer* lv3_row = new wxStaticBoxSizer(lv3_box, wxVERTICAL);
  first_row->Add(pic, 0);
  first_row->Add(5, 5, 0);
  first_row->Add(lv1_row);
  second_row->Add(lv2_row);
  second_row->Add(5, 5, 0);
  second_row->Add(lv3_row);

  wxStaticBox *steam1_box = new wxStaticBox(panel, -1, "Steam");
  wxStaticBox *slurry1_box = new wxStaticBox(panel, -1, "Slurry");
  wxStaticBoxSizer *steam1_row = new wxStaticBoxSizer(steam1_box, wxVERTICAL);
  wxStaticBoxSizer *slurry1_row = new wxStaticBoxSizer(slurry1_box, wxVERTICAL);

  lv1_row->Add(10, 5, 0);
  lv1_row->Add(steam1_row);
  lv1_row->Add(10, 5, 0);
  lv1_row->Add(slurry1_row);
  lv1_row->Add(10, 5, 0);

  wxStaticBox *steam2_box = new wxStaticBox(panel, -1, "Steam");
  wxStaticBox *slurry2_box = new wxStaticBox(panel, -1, "Slurry");
  wxStaticBoxSizer *steam2_row = new wxStaticBoxSizer(steam2_box, wxVERTICAL);
  wxStaticBoxSizer *slurry2_row = new wxStaticBoxSizer(slurry2_box, wxVERTICAL);

  lv2_row->Add(10, 5, 0);
  lv2_row->Add(steam2_row);
  lv2_row->Add(10, 5, 0);
  lv2_row->Add(slurry2_row);
  lv2_row->Add(10, 5, 0);

  wxStaticBox *steam3_box = new wxStaticBox(panel, -1, "Steam");
  wxStaticBox *slurry3_box = new wxStaticBox(panel, -1, "Slurry");
  wxStaticBoxSizer *steam3_row = new wxStaticBoxSizer(steam3_box, wxVERTICAL);
  wxStaticBoxSizer *slurry3_row = new wxStaticBoxSizer(slurry3_box, wxVERTICAL);

  lv3_row->Add(10, 5, 0);
  lv3_row->Add(steam3_row);
  lv3_row->Add(10, 5, 0);
  lv3_row->Add(slurry3_row);
  lv3_row->Add(10, 5, 0);

  wxBoxSizer* steam1_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* steam1_2row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* steam2_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* steam2_2row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* steam3_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* steam3_2row = new wxBoxSizer(wxHORIZONTAL);
  
  wxBoxSizer* slurry1_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry1_2row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry1_3row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry1_4row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry2_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry2_2row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry2_3row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry2_4row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry3_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry3_2row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry3_3row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry3_4row = new wxBoxSizer(wxHORIZONTAL);
  
  steam1_row->Add(3,3,0);
  steam1_row->Add(steam1_1row);
  steam1_row->Add(3,3,0);
  steam1_row->Add(steam1_2row);
  steam1_row->Add(3,3,0);
  steam2_row->Add(3,3,0);
  steam2_row->Add(steam2_1row);
  steam2_row->Add(3,3,0);
  steam2_row->Add(steam2_2row);
  steam2_row->Add(3,3,0);
  steam3_row->Add(3,3,0);
  steam3_row->Add(steam3_1row);
  steam3_row->Add(3,3,0);
  steam3_row->Add(steam3_2row);
  steam3_row->Add(3,3,0);

  slurry1_row->Add(3,3,0);
  slurry1_row->Add(slurry1_1row);
  slurry1_row->Add(3,3,0);
  slurry1_row->Add(slurry1_2row);
  slurry1_row->Add(3,3,0);
  slurry1_row->Add(slurry1_3row);
  slurry1_row->Add(3,3,0);
  slurry1_row->Add(slurry1_4row);
  slurry1_row->Add(3,3,0);
  slurry2_row->Add(3,3,0);
  slurry2_row->Add(slurry2_1row);
  slurry2_row->Add(3,3,0);
  slurry2_row->Add(slurry2_2row);
  slurry2_row->Add(3,3,0);
  slurry2_row->Add(slurry2_3row);
  slurry2_row->Add(3,3,0);
  slurry2_row->Add(slurry2_4row);
  slurry2_row->Add(3,3,0);
  slurry3_row->Add(3,3,0);
  slurry3_row->Add(slurry3_1row);
  slurry3_row->Add(3,3,0);
  slurry3_row->Add(slurry3_2row);
  slurry3_row->Add(3,3,0);
  slurry3_row->Add(slurry3_3row);
  slurry3_row->Add(3,3,0);
  slurry3_row->Add(slurry3_4row);
  slurry3_row->Add(3,3,0);

  //=================================================
  
  wxStaticText * label0 = new wxStaticText(panel, -1, "Temperature (K)", wxDefaultPosition, wxSize(150, 17));
  t_steam_temp1 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  steam1_1row->Add(label0);
  steam1_1row->Add(t_steam_temp1);
  wxStaticText * label1 = new wxStaticText(panel, -1, "Flowrate (kg/s)", wxDefaultPosition, wxSize(150, 17));
  t_steam_flrt1 = new wxTextCtrl(panel, -1, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  steam1_2row->Add(label1);
  steam1_2row->Add(t_steam_flrt1);

  wxStaticText * label2 = new wxStaticText(panel, -1, "Temperature (K)", wxDefaultPosition, wxSize(150, 17));
  t_slurry_temp1 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  slurry1_1row->Add(label2);
  slurry1_1row->Add(t_slurry_temp1);
  wxStaticText * label3 = new wxStaticText(panel, -1, "Flowrate (kg/s)", wxDefaultPosition, wxSize(150, 17));
  t_slurry_flrt1 = new wxTextCtrl(panel, -1, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  slurry1_2row->Add(label3);
  slurry1_2row->Add(t_slurry_flrt1);
  wxStaticText * label4 = new wxStaticText(panel, -1, "Coal (%) ", wxDefaultPosition, wxSize(150, 17));
  t_coal_percent1 = new wxTextCtrl(panel, -1, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  slurry1_3row->Add(label4);
  slurry1_3row->Add(t_coal_percent1);
  wxStaticText * label4a = new wxStaticText(panel, -1, "Re-Cycled Char (%) ", wxDefaultPosition, wxSize(150, 17));
  t_char_percent1 = new wxTextCtrl(panel, -1, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  slurry1_4row->Add(label4a);
  slurry1_4row->Add(t_char_percent1);
  
  //=============================================
  wxStaticText * label5 = new wxStaticText(panel, -1, "Temperature (K)", wxDefaultPosition, wxSize(150, 17));
  t_steam_temp2 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  steam2_1row->Add(label5);
  steam2_1row->Add(t_steam_temp2);
  wxStaticText * label6 = new wxStaticText(panel, -1, "Flowrate (kg/s)", wxDefaultPosition, wxSize(150, 17));
  t_steam_flrt2 = new wxTextCtrl(panel, -1, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  steam2_2row->Add(label6);
  steam2_2row->Add(t_steam_flrt2);

  wxStaticText * label7 = new wxStaticText(panel, -1, "Temperature (K)", wxDefaultPosition, wxSize(150, 17));
  t_slurry_temp2 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  slurry2_1row->Add(label7);
  slurry2_1row->Add(t_slurry_temp2);
  wxStaticText * label8 = new wxStaticText(panel, -1, "Flowrate (kg/s)", wxDefaultPosition, wxSize(150, 17));
  t_slurry_flrt2 = new wxTextCtrl(panel, -1, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  slurry2_2row->Add(label8);
  slurry2_2row->Add(t_slurry_flrt2);
  wxStaticText * label9 = new wxStaticText(panel, -1, "Coal (%) ", wxDefaultPosition, wxSize(150, 17));
  t_coal_percent2 = new wxTextCtrl(panel, -1, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  slurry2_3row->Add(label9);
  slurry2_3row->Add(t_coal_percent2);
  wxStaticText * label9a = new wxStaticText(panel, -1, "Re-Cycled Char (%) ", wxDefaultPosition, wxSize(150, 17));
  t_char_percent2 = new wxTextCtrl(panel, -1, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  slurry2_4row->Add(label9a);
  slurry2_4row->Add(t_char_percent2);

  //=================================================
  wxStaticText * label10 = new wxStaticText(panel, -1, "Temperature (K)", wxDefaultPosition, wxSize(150, 17));
  t_steam_temp3 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  steam3_1row->Add(label10);
  steam3_1row->Add(t_steam_temp3);
  wxStaticText * label11 = new wxStaticText(panel, -1, "Flowrate (kg/s)", wxDefaultPosition, wxSize(150, 17));
  t_steam_flrt3 = new wxTextCtrl(panel, -1, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  steam3_2row->Add(label11);
  steam3_2row->Add(t_steam_flrt3);

  wxStaticText * label12 = new wxStaticText(panel, -1, "Temperature (K)", wxDefaultPosition, wxSize(150, 17));
  t_slurry_temp3 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  slurry3_1row->Add(label12);
  slurry3_1row->Add(t_slurry_temp3);
  wxStaticText * label13 = new wxStaticText(panel, -1, "Flowrate (kg/s)", wxDefaultPosition, wxSize(150, 17));
  t_slurry_flrt3 = new wxTextCtrl(panel, -1, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  slurry3_2row->Add(label13);
  slurry3_2row->Add(t_slurry_flrt3);
  wxStaticText * label14 = new wxStaticText(panel, -1, "Coal (%) ", wxDefaultPosition, wxSize(150, 17));
  t_coal_percent3 = new wxTextCtrl(panel, -1, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  slurry3_3row->Add(label14);
  slurry3_3row->Add(t_coal_percent3);
  wxStaticText * label14a = new wxStaticText(panel, -1, "Re-Cycled Char (%) ", wxDefaultPosition, wxSize(150, 17));
  t_char_percent3 = new wxTextCtrl(panel, -1, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  slurry3_4row->Add(label14a);
  slurry3_4row->Add(t_char_percent3);

  //top_sizer->Fit(panel);
  //panel->SetAutoLayout(true);
  panel->SetSizer(top_sizer);

  return panel;
}

wxPanel *Gasi2Tabs::CreateSecondPage()
{
  wxPanel *panel = new wxPanel(this);
 
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);

  wxBoxSizer* second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* third_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 5, 0);
  top_sizer->Add(second_row, 0);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(third_row, 0);

  wxStaticBox *misc_box = new wxStaticBox(panel, -1, "Misc");
  wxStaticBox *coal_box = new wxStaticBox(panel, -1, "Coal Info");
   
  wxStaticBoxSizer* misc_row = new wxStaticBoxSizer(misc_box, wxVERTICAL);
  wxStaticBoxSizer* coal_row = new wxStaticBoxSizer(coal_box, wxVERTICAL);
  
  second_row->Add(misc_row, 0);

  third_row->Add(coal_row, 0);

  wxBoxSizer* misc_3row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* coal_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* coal_2row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* coal_3row = new wxBoxSizer(wxHORIZONTAL);

  misc_row->Add(3, 3, 0);
  misc_row->Add(misc_3row);
  misc_row->Add(3, 3, 0);
  
  coal_row->Add(3, 3, 0);
  coal_row->Add(coal_1row);
  coal_row->Add(3, 3, 0);
  coal_row->Add(coal_2row);
  coal_row->Add(3, 3, 0);
  coal_row->Add(coal_3row);
  coal_row->Add(3, 3, 0);
  
  wxStaticText * label13 = new wxStaticText(panel, -1, "Pressure Drop (Pa)", wxDefaultPosition, wxSize(150, 17));
  t_pres_drop = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(120, 20));
  misc_3row->Add(label13);
  misc_3row->Add(t_pres_drop);

  wxStaticText * label14 = new wxStaticText(panel, -1, "Coal Type", wxDefaultPosition, wxSize(150, 17));

  wxString coal_type_val[] = {
	wxT("Pittsburg_#8"), wxT("Illinois_#5"), wxT("Illinois_#6"), wxT("Petcoke"),
	wxT("Pike_County"), wxT("Pocahantas_#3"), wxT("E-Gas_Illinois_#6"), wxT("E-Gas_Utah"), 
	wxT("E-Gas_Wyodak"), wxT("E-Gas_Wyoming"), wxT("E-Gas_AppMS"), wxT("E-Gas_AppLS")
  };
  
  c_coal_type = new wxComboBox(panel, -1, wxT("Illinois_#5"), wxDefaultPosition, wxSize(120, 20), 12, coal_type_val, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
  coal_1row->Add(label14);
  coal_1row->Add(c_coal_type);
  wxStaticText * label15 = new wxStaticText(panel, -1, "Size percent thru 50 ", wxDefaultPosition, wxSize(150, 17));
  t_size_50 = new wxTextCtrl(panel, -1, wxT("99.9"), wxDefaultPosition, wxSize(120, 20));
  coal_2row->Add(label15);
  coal_2row->Add(t_size_50);
  wxStaticText * label16 = new wxStaticText(panel, -1, "Size percent thru 200 ", wxDefaultPosition, wxSize(150, 17));
  t_size_200 = new wxTextCtrl(panel, -1, wxT("90"), wxDefaultPosition, wxSize(120, 20));
  coal_3row->Add(label16);
  coal_3row->Add(t_size_200);
  
  //top_sizer->Fit(panel);
  //panel->SetAutoLayout(true);
  panel->SetSizer(top_sizer);

  return panel;
}

void Gasi2Tabs::OnChangeStage(wxCommandEvent &event)
{
  if (r_stage2->GetValue())
    {
      t_steam_temp2->Enable(true);
      t_steam_flrt2->Enable(true);
      t_slurry_temp2->Enable(true);
      t_slurry_flrt2->Enable(true);
      t_coal_percent2->Enable(true);
      t_char_percent2->Enable(true);
      t_steam_temp3->Enable(true);
      t_steam_flrt3->Enable(true);
      t_slurry_temp3->Enable(true);
      t_slurry_flrt3->Enable(true);
      t_coal_percent3->Enable(true);
      t_char_percent3->Enable(true);
    }
  else
    {
      t_steam_temp2->Enable(false);
      t_steam_flrt2->Enable(false);
      t_slurry_temp2->Enable(false);
      t_slurry_flrt2->Enable(false);
      t_coal_percent2->Enable(false);
      t_char_percent2->Enable(false);
      t_steam_temp3->Enable(false);
      t_steam_flrt3->Enable(false);
      t_slurry_temp3->Enable(false);
      t_slurry_flrt3->Enable(false);
      t_coal_percent3->Enable(false);
      t_char_percent3->Enable(false);
    }

}

