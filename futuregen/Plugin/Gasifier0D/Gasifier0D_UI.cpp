#include "Gasifier0D_UI.h"
#include "wx/image.h" //This line have to be under the first line for some unknown reason to pass the compilation

BEGIN_EVENT_TABLE(GasiTabs, wxNotebook)
  EVT_RADIOBUTTON(R_STAGE1, GasiTabs::OnChangeStage)
  EVT_RADIOBUTTON(R_STAGE2, GasiTabs::OnChangeStage)
  EVT_CHECKBOX(SPEC_GEOM, GasiTabs::OnChangeGeom)
  EVT_CHECKBOX(DES_MODE, GasiTabs::OnChangeMode)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(Gasifier0D_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers

Gasifier0D_UI_Dialog
::Gasifier0D_UI_Dialog
(wxWindow* parent, int id,
  double* steam_temp1,
  double* steam_flrt1,
  double* slurry_temp1,
  double* slurry_flrt1,
  double* coal_percent1,
  double* steam_temp2,
  double* steam_flrt2,
  double* slurry_temp2,
  double* slurry_flrt2,
  double* coal_percent2,
  double* steam_temp3,
  double* steam_flrt3,
  double* slurry_temp3,
  double* slurry_flrt3,
  double* coal_percent3,
  double* geo_diam,
  double* geo_stage1_len,
  double* geo_stage2_len,
  double* geo_stage1_wall,
  double* geo_stage2_wall,
  double* burn_out,
  double* stage1_heatloss,
  double* stage2_heatloss,
  double* LD_ratio,
  double* stage1_emis,
  double* stage2_emis,
  double* backside_temp,
  double* slag_eff,
  double* pres_drop,
  long* stage,
  long* spec_geometry,
  long* des_mode,
  string* coal_type,
  double* size_50,
  double* size_200)
: UIDialog((wxWindow *) parent, id, "Gasifier0D"),
  p_steam_temp1(steam_temp1),
  p_steam_flrt1(steam_flrt1),
  p_slurry_temp1(slurry_temp1),
  p_slurry_flrt1(slurry_flrt1),
  p_coal_percent1(coal_percent1),
  p_steam_temp2(steam_temp2),
  p_steam_flrt2(steam_flrt2),
  p_slurry_temp2(slurry_temp2),
  p_slurry_flrt2(slurry_flrt2),
  p_coal_percent2(coal_percent2),
  p_steam_temp3(steam_temp3),
  p_steam_flrt3(steam_flrt3),
  p_slurry_temp3(slurry_temp3),
  p_slurry_flrt3(slurry_flrt3),
  p_coal_percent3(coal_percent3),
  p_geo_diam(geo_diam),
  p_geo_stage1_len(geo_stage1_len),
  p_geo_stage2_len(geo_stage2_len),
  p_geo_stage1_wall(geo_stage1_wall),
  p_geo_stage2_wall(geo_stage2_wall),
  p_burn_out(burn_out),
  p_stage1_heatloss(stage1_heatloss),
  p_stage2_heatloss(stage2_heatloss),
  p_LD_ratio(LD_ratio),
  p_stage1_emis(stage1_emis),
  p_stage2_emis(stage2_emis),
  p_backside_temp(backside_temp),
  p_slag_eff(slag_eff),
  p_pres_drop(pres_drop),
  p_stage(stage),
  p_spec_geometry(spec_geometry),
  p_des_mode(des_mode),
  p_coal_type(coal_type),
  p_size_50(size_50),
  p_size_200(size_200)
{

  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
   
  m_tabs = new GasiTabs(this);
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
Gasifier0D_UI_Dialog
::~Gasifier0D_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool Gasifier0D_UI_Dialog::TransferDataFromWindow()
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

  txt = m_tabs->t_geo_diam->GetValue();
  (*p_geo_diam) = atof(txt.c_str());
  txt = m_tabs->t_geo_stage1_len->GetValue();
  (*p_geo_stage1_len) = atof(txt.c_str());
  txt = m_tabs->t_geo_stage2_len->GetValue();
  (*p_geo_stage2_len) = atof(txt.c_str());
  txt = m_tabs->t_geo_stage1_wall->GetValue();
  (*p_geo_stage1_wall) = atof(txt.c_str());
  txt = m_tabs->t_geo_stage2_wall->GetValue();
  (*p_geo_stage2_wall) = atof(txt.c_str());
  
  txt = m_tabs->t_burn_out->GetValue();
  (*p_burn_out) = atof(txt.c_str());
  txt = m_tabs->t_stage1_heatloss->GetValue();
  (*p_stage1_heatloss) = atof(txt.c_str());
  txt = m_tabs->t_stage2_heatloss->GetValue();
  (*p_stage2_heatloss) = atof(txt.c_str());
  txt = m_tabs->t_LD_ratio->GetValue();
  (*p_LD_ratio) = atof(txt.c_str());

  txt = m_tabs->t_stage1_emis->GetValue();
  (*p_stage1_emis) = atof(txt.c_str());
  txt = m_tabs->t_stage2_emis->GetValue();
  (*p_stage2_emis) = atof(txt.c_str());
  
  txt = m_tabs->t_backside_temp->GetValue();
  (*p_backside_temp) = atof(txt.c_str());
  txt = m_tabs->t_slag_eff->GetValue();
  (*p_slag_eff) = atof(txt.c_str());
  txt = m_tabs->t_pres_drop->GetValue();
  (*p_pres_drop) = atof(txt.c_str());

  (*p_stage) = m_tabs->r_stage1->GetValue();
  (*p_spec_geometry) = m_tabs->cb_spec_geometry->GetValue();
  (*p_des_mode) = m_tabs->cb_des_mode->GetValue();

  (*p_coal_type) = m_tabs->c_coal_type->GetValue();
  txt = m_tabs->t_size_50->GetValue();
  (*p_size_50) = atof(txt.c_str());
  txt = m_tabs->t_size_200->GetValue();
  (*p_size_200) = atof(txt.c_str());
  return true;

}

////////////////////////////////////////////////////
bool Gasifier0D_UI_Dialog::TransferDataToWindow()
{

  wxString txt1, txt2, txt3, txt4, txt5, txt6, txt7, txt8, txt9, txt10;
  wxString txt11, txt12, txt13, txt14, txt15, txt16, txt17, txt18, txt19, txt20;
  wxString txt21, txt22, txt23, txt24, txt25, txt26, txt27, txt28, txt29, txt30;
  wxString txt31, txt32;

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

  txt16<<(*p_geo_diam);
  m_tabs->t_geo_diam->SetValue(txt16);

  txt17<<(*p_geo_stage1_len);
  m_tabs->t_geo_stage1_len->SetValue(txt17);

  txt18<<(*p_geo_stage2_len);
  m_tabs->t_geo_stage2_len->SetValue(txt18);

  txt19<<(*p_geo_stage1_wall);
  m_tabs->t_geo_stage1_wall->SetValue(txt19);

  txt20<<(*p_geo_stage2_wall);
  m_tabs->t_geo_stage2_wall->SetValue(txt20);

  txt21<<(*p_burn_out);
  m_tabs->t_burn_out->SetValue(txt21);

  txt22<<(*p_stage1_heatloss);
  m_tabs->t_stage1_heatloss->SetValue(txt22);

  txt23<<(*p_stage2_heatloss);
  m_tabs->t_stage2_heatloss->SetValue(txt23);

  txt24<<(*p_LD_ratio);
  m_tabs->t_LD_ratio->SetValue(txt24);

  txt25<<(*p_stage1_emis);
  m_tabs->t_stage1_emis->SetValue(txt25);

  txt26<<(*p_stage2_emis);
  m_tabs->t_stage2_emis->SetValue(txt26);

  txt27<<(*p_backside_temp);
  m_tabs->t_backside_temp->SetValue(txt27);

  txt28<<(*p_slag_eff);
  m_tabs->t_slag_eff->SetValue(txt28);

  txt29<<(*p_pres_drop);
  m_tabs->t_pres_drop->SetValue(txt29);

  txt30= p_coal_type->c_str();
  m_tabs->c_coal_type->SetValue(txt30);

  txt31<<(*p_size_50);
  m_tabs->t_size_50->SetValue(txt31);
  
  txt32<<(*p_size_200);
  m_tabs->t_size_200->SetValue(txt32);
  
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

  if (*p_spec_geometry)
    m_tabs->cb_spec_geometry->SetValue(true);

  if (*p_des_mode)
    m_tabs->cb_des_mode->SetValue(true);

  m_tabs->OnChangeStage(event);
  m_tabs->OnChangeGeom(event);
  m_tabs->OnChangeMode(event);

  return true;
  
}

void Gasifier0D_UI_Dialog::Lock(bool l)
{
}

GasiTabs::GasiTabs(wxWindow *parent, wxWindowID id,
	   const wxPoint& pos , 
	   const wxSize& size , 
	   long style)
  : wxNotebook(parent, id, pos, size, style)
{
}

void GasiTabs::CreateInitialPages()
{
    wxPanel *panel = (wxPanel *) NULL;

    // Create and add some panels to the notebook

    panel = CreateFirstPage();
    AddPage( panel, _T("Flows"), false);

    panel = CreateSecondPage();
    AddPage( panel, _T("Misc"), true);
 
}

wxPanel *GasiTabs::CreateFirstPage()
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
  wxBoxSizer* slurry2_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry2_2row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry2_3row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry3_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry3_2row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* slurry3_3row = new wxBoxSizer(wxHORIZONTAL);
  
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
  slurry2_row->Add(3,3,0);
  slurry2_row->Add(slurry2_1row);
  slurry2_row->Add(3,3,0);
  slurry2_row->Add(slurry2_2row);
  slurry2_row->Add(3,3,0);
  slurry2_row->Add(slurry2_3row);
  slurry2_row->Add(3,3,0);
  slurry3_row->Add(3,3,0);
  slurry3_row->Add(slurry3_1row);
  slurry3_row->Add(3,3,0);
  slurry3_row->Add(slurry3_2row);
  slurry3_row->Add(3,3,0);
  slurry3_row->Add(slurry3_3row);
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

  //top_sizer->Fit(panel);
  //panel->SetAutoLayout(true);
  panel->SetSizer(top_sizer);

  return panel;
}

wxPanel *GasiTabs::CreateSecondPage()
{
  wxPanel *panel = new wxPanel(this);
 
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);

  wxBoxSizer* first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* third_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 5, 0);
  top_sizer->Add(first_row,0);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(second_row, 0);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(third_row, 0);

  wxStaticBox *geom_box = new wxStaticBox(panel, -1, "Geometry Specified");
  wxStaticBox *burnout_box = new wxStaticBox(panel, -1, "Burnout Specified");
  wxStaticBox *emis_box = new wxStaticBox(panel, -1, "Emis");
  wxStaticBox *misc_box = new wxStaticBox(panel, -1, "Misc");
  wxStaticBox *coal_box = new wxStaticBox(panel, -1, "Coal Info");
   
  wxStaticBoxSizer* geom_row = new wxStaticBoxSizer(geom_box, wxVERTICAL);
  wxStaticBoxSizer* burnout_row = new wxStaticBoxSizer(burnout_box, wxVERTICAL);
  wxStaticBoxSizer* emis_row = new wxStaticBoxSizer(emis_box, wxVERTICAL);
  wxStaticBoxSizer* misc_row = new wxStaticBoxSizer(misc_box, wxVERTICAL);
  wxStaticBoxSizer* coal_row = new wxStaticBoxSizer(coal_box, wxVERTICAL);
  
  first_row->Add(geom_row, 0);
  first_row->Add(5, 5, 0);
  first_row->Add(burnout_row, 0);

  second_row->Add(emis_row, 0);
  second_row->Add(5, 5, 0);
  second_row->Add(misc_row, 0);

  third_row->Add(coal_row, 0);

  wxBoxSizer* geom_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* geom_2row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* geom_3row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* geom_4row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* geom_5row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* geom_6row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* geom_7row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* geom_8row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* geom_9row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* burnout_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* burnout_2row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* burnout_3row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* burnout_4row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* burnout_5row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* burnout_6row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* emis_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* emis_2row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* misc_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* misc_2row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* misc_3row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* coal_1row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* coal_2row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* coal_3row = new wxBoxSizer(wxHORIZONTAL);

  geom_row->Add(3, 3, 0);
  geom_row->Add(geom_1row, 0, wxALIGN_CENTER_HORIZONTAL);
  geom_row->Add(3, 3, 0);
  geom_row->Add(geom_2row, 0, wxALIGN_CENTER_HORIZONTAL);
  geom_row->Add(3, 3, 0);
  geom_row->Add(geom_3row);
  geom_row->Add(3, 3, 0);
  geom_row->Add(geom_4row, 0, wxALIGN_CENTER_HORIZONTAL);
  geom_row->Add(3, 3, 0);
  geom_row->Add(geom_5row);
  geom_row->Add(3, 3, 0);
  geom_row->Add(geom_6row);
  geom_row->Add(3, 3, 0);
  geom_row->Add(geom_7row, 0, wxALIGN_CENTER_HORIZONTAL);
  geom_row->Add(3, 3, 0);
  geom_row->Add(geom_8row);
  geom_row->Add(3, 3, 0);
  geom_row->Add(geom_9row);
  geom_row->Add(3, 3, 0);

  burnout_row->Add(3, 3, 0);
  burnout_row->Add(burnout_1row);
  burnout_row->Add(3, 3, 0);
  burnout_row->Add(burnout_2row, 0, wxALIGN_CENTER_HORIZONTAL);
  burnout_row->Add(3, 3, 0);
  burnout_row->Add(burnout_3row);
  burnout_row->Add(3, 3, 0);
  burnout_row->Add(burnout_4row);
  burnout_row->Add(3, 3, 0);
  burnout_row->Add(burnout_5row, 0, wxALIGN_CENTER_HORIZONTAL);
  burnout_row->Add(3, 3, 0);
  burnout_row->Add(burnout_6row);
  burnout_row->Add(3, 3, 0);
  burnout_row->Add(80, 20, 0);
  burnout_row->Add(3, 3, 0);
  burnout_row->Add(80, 20, 0);
  burnout_row->Add(3, 3, 0);
  burnout_row->Add(80, 20, 0);
  burnout_row->Add(3, 3, 0);

  emis_row->Add(3, 3, 0);
  emis_row->Add(emis_1row);
  emis_row->Add(3, 3, 0);
  emis_row->Add(emis_2row);
  emis_row->Add(3, 3, 0);
  emis_row->Add(80, 20, 0);
  emis_row->Add(3, 3, 0);

  misc_row->Add(3, 3, 0);
  misc_row->Add(misc_1row);
  misc_row->Add(3, 3, 0);
  misc_row->Add(misc_2row);
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
  
  cb_spec_geometry = new wxCheckBox(panel, SPEC_GEOM, "Specifiy Geometry", wxDefaultPosition, wxSize(150, 17));
  geom_1row->Add(cb_spec_geometry);
  geom_2row->Add(new wxStaticText(panel, -1, "Dimensions", wxDefaultPosition, wxSize(150, 17)), 0, wxALIGN_CENTER_HORIZONTAL);
  wxStaticText * label0 = new wxStaticText(panel, -1, "Diameter (m)", wxDefaultPosition, wxSize(150, 17));
  t_geo_diam = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  geom_3row->Add(label0);
  geom_3row->Add(t_geo_diam);
  geom_4row->Add(new wxStaticText(panel, -1, "Length (m)", wxDefaultPosition, wxSize(150, 17)), 0, wxALIGN_CENTER_HORIZONTAL);
  wxStaticText * label1 = new wxStaticText(panel, -1, "1st Stage", wxDefaultPosition, wxSize(150, 17));
  t_geo_stage1_len = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  geom_5row->Add(label1);
  geom_5row->Add(t_geo_stage1_len);
  wxStaticText * label2 = new wxStaticText(panel, -1, "2nd Stage", wxDefaultPosition, wxSize(150, 17));
  t_geo_stage2_len = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  geom_6row->Add(label2);
  geom_6row->Add(t_geo_stage2_len);
  geom_7row->Add(new wxStaticText(panel, -1, "Rwall (m2-K/W)", wxDefaultPosition, wxSize(150, 17)), 0, wxALIGN_CENTER_HORIZONTAL);
  wxStaticText * label3 = new wxStaticText(panel, -1, "1st Stage", wxDefaultPosition, wxSize(150, 17));
  t_geo_stage1_wall = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  geom_8row->Add(label3);
  geom_8row->Add(t_geo_stage1_wall);
  wxStaticText * label4 = new wxStaticText(panel, -1, "2nd Stage", wxDefaultPosition, wxSize(150, 17));
  t_geo_stage2_wall = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  geom_9row->Add(label4);
  geom_9row->Add(t_geo_stage2_wall);

  wxStaticText * label5 = new wxStaticText(panel, -1, "Burnout (%)", wxDefaultPosition, wxSize(150, 17));
  t_burn_out = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  burnout_1row->Add(label5);
  burnout_1row->Add(t_burn_out);
  burnout_2row->Add(new wxStaticText(panel, -1, "Heatloss (%)", wxDefaultPosition, wxSize(150, 17)), 0, wxALIGN_CENTER_HORIZONTAL);
  wxStaticText * label6 = new wxStaticText(panel, -1, "1st Stage", wxDefaultPosition, wxSize(150, 17));
  t_stage1_heatloss = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  burnout_3row->Add(label6);
  burnout_3row->Add(t_stage1_heatloss);
  wxStaticText * label7 = new wxStaticText(panel, -1, "2nd Stage", wxDefaultPosition, wxSize(150, 17));
  t_stage2_heatloss = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  burnout_4row->Add(label7);
  burnout_4row->Add(t_stage2_heatloss);
  cb_des_mode = new wxCheckBox(panel, DES_MODE, "Design Mode", wxDefaultPosition, wxSize(150, 17));
  burnout_5row->Add(cb_des_mode);
  wxStaticText * label8 = new wxStaticText(panel, -1, "L/D", wxDefaultPosition, wxSize(150, 17));
  t_LD_ratio = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  burnout_6row->Add(label8);
  burnout_6row->Add(t_LD_ratio);
 
  wxStaticText * label9 = new wxStaticText(panel, -1, "1st Stage", wxDefaultPosition, wxSize(150, 17));
  t_stage1_emis = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  emis_1row->Add(label9);
  emis_1row->Add(t_stage1_emis);
  wxStaticText * label10 = new wxStaticText(panel, -1, "2nd Stage", wxDefaultPosition, wxSize(150, 17));
  t_stage2_emis = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  emis_2row->Add(label10);
  emis_2row->Add(t_stage2_emis);
  
  wxStaticText * label11 = new wxStaticText(panel, -1, "Backside Temp (K)", wxDefaultPosition, wxSize(150, 17));
  t_backside_temp = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  misc_1row->Add(label11);
  misc_1row->Add(t_backside_temp);
  wxStaticText * label12 = new wxStaticText(panel, -1, "Slag Efficiency (%)", wxDefaultPosition, wxSize(150, 17));
  t_slag_eff = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  misc_2row->Add(label12);
  misc_2row->Add(t_slag_eff);
  wxStaticText * label13 = new wxStaticText(panel, -1, "Pressure Drop (Pa)", wxDefaultPosition, wxSize(150, 17));
  t_pres_drop = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
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

void GasiTabs::OnChangeStage(wxCommandEvent &event)
{
  if (r_stage2->GetValue())
    {
      t_steam_temp2->Enable(true);
      t_steam_flrt2->Enable(true);
      t_slurry_temp2->Enable(true);
      t_slurry_flrt2->Enable(true);
      t_coal_percent2->Enable(true);
      t_steam_temp3->Enable(true);
      t_steam_flrt3->Enable(true);
      t_slurry_temp3->Enable(true);
      t_slurry_flrt3->Enable(true);
      t_coal_percent3->Enable(true);
    }
  else
    {
      t_steam_temp2->Enable(false);
      t_steam_flrt2->Enable(false);
      t_slurry_temp2->Enable(false);
      t_slurry_flrt2->Enable(false);
      t_coal_percent2->Enable(false);
      t_steam_temp3->Enable(false);
      t_steam_flrt3->Enable(false);
      t_slurry_temp3->Enable(false);
      t_slurry_flrt3->Enable(false);
      t_coal_percent3->Enable(false);
    }

}

void GasiTabs::OnChangeGeom(wxCommandEvent &event)
{
  if (cb_spec_geometry->GetValue())
    {
      t_geo_diam->Enable(true);
      t_geo_stage1_len->Enable(true);
      t_geo_stage2_len->Enable(true);
      t_geo_stage1_wall->Enable(true);
      t_geo_stage2_wall->Enable(true);
	  t_burn_out->Enable(false);
	  t_stage1_heatloss->Enable(false);
      t_stage2_heatloss->Enable(false);
	  t_LD_ratio->Enable(false);
      cb_des_mode->Enable(false); 
    }
  else
    {
      t_geo_diam->Enable(false);
      t_geo_stage1_len->Enable(false);
      t_geo_stage2_len->Enable(false);
      t_geo_stage1_wall->Enable(false);
      t_geo_stage2_wall->Enable(false);
	  t_burn_out->Enable(true);
	  t_stage1_heatloss->Enable(true);
      t_stage2_heatloss->Enable(true);
      cb_des_mode->Enable(true); 
    }

}
 
void GasiTabs::OnChangeMode(wxCommandEvent &event)
{
  if (cb_des_mode->GetValue())
    t_LD_ratio->Enable(true);
  else
    t_LD_ratio->Enable(false);
}
