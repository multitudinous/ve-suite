#include "SOFC1D_UI.h"
#include "open.xpm"

BEGIN_EVENT_TABLE(SOFC1D_UI_Dialog, UIDialog)
  EVT_BUTTON(BROWSE, SOFC1D_UI_Dialog::OnBrowse)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(SOFC1D_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
SOFC1D_UI_Dialog
::SOFC1D_UI_Dialog
(wxWindow* parent, int id,
  double* a_height,
  double* a_width,
  double* a_space,
  double* a_ecd,
  double* a_tcoeff,
  double* a_thick,
  double* a_presdrop,
  double* c_height,
  double* c_width,
  double* c_space,
  double* c_ecdb,
  double* c_ecdm,
  double* c_tcoeffb,
  double* c_tcoeffm,
  double* c_thick,
  double* c_presdrop,
  double* s_thick,
  double* s_heatcap,
  double* s_density,
  double* e_thick,
  double* e_preexp,
  double* e_exp,
  double* f_preexp,
  double* f_exp,
  double* a_preexp,
  double* a_exp,
  double* i_preexp,
  double* i_exp,
  double* l_heatcap,
  double* l_density,
  double* l_length,
  double* l_width,
  double* stop_time,
  double* loadres,
  string* work_dir,
  long* l_numCells,
  long* ax_nodes)
: UIDialog((wxWindow *) parent, id, "SOFC1D"),
  p_a_height(a_height),
  p_a_width(a_width),
  p_a_space(a_space),
  p_a_ecd(a_ecd),
  p_a_tcoeff(a_tcoeff),
  p_a_thick(a_thick),
  p_a_presdrop(a_presdrop),
  p_c_height(c_height),
  p_c_width(c_width),
  p_c_space(c_space),
  p_c_ecdb(c_ecdb),
  p_c_ecdm(c_ecdm),
  p_c_tcoeffb(c_tcoeffb),
  p_c_tcoeffm(c_tcoeffm),
  p_c_thick(c_thick),
  p_c_presdrop(c_presdrop),
  p_s_thick(s_thick),
  p_s_heatcap(s_heatcap),
  p_s_density(s_density),
  p_e_thick(e_thick),
  p_e_preexp(e_preexp),
  p_e_exp(e_exp),
  p_f_preexp(f_preexp),
  p_f_exp(f_exp),
  p_a_preexp(a_preexp),
  p_a_exp(a_exp),
  p_i_preexp(i_preexp),
  p_i_exp(i_exp),
  p_l_heatcap(l_heatcap),
  p_l_density(l_density),
  p_l_length(l_length),
  p_l_width(l_width),
  p_stop_time(stop_time),
  p_loadres(loadres),
  p_work_dir(work_dir),
  p_l_numCells(l_numCells),
  p_ax_nodes(ax_nodes)
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

  wxBoxSizer* wd_row = new wxBoxSizer(wxHORIZONTAL); //work dir row
  wxBoxSizer* data_row = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 10, 0); //the top margin
  top_sizer->Add(wd_row, 0);
  top_sizer->Add(10, 10, 0);
  top_sizer->Add(data_row, 0); 
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0); //the bottom margin

  wxStaticText * label0 = new wxStaticText(this, -1, " Working Directory ", wxDefaultPosition, wxSize(200, 17));
  t_work_dir = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(338, 20));
  openfileBitmap = new wxBitmap(open_xpm);
  bb_browse = new wxBitmapButton(this, BROWSE, *openfileBitmap);

  wd_row->Add(label0);
  wd_row->Add(t_work_dir);
  wd_row->Add(5, 20);
  wd_row->Add(bb_browse);
  
  wxBoxSizer *data_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_forth_row = new wxBoxSizer(wxHORIZONTAL);

  //  data_row->Add(10, 5, 0);
  data_row->Add(data_first_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_second_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_third_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_forth_row, 0);
  data_row->Add(10, 3, 0);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  wxStaticBox *gui1_box = new wxStaticBox(this, -1, "Anode");
  wxStaticBox *gui2_box = new wxStaticBox(this, -1, "Cathode");

  wxStaticBoxSizer *data_first_l_row = new wxStaticBoxSizer(gui1_box, wxVERTICAL);
  wxStaticBoxSizer *data_first_r_row = new wxStaticBoxSizer(gui2_box, wxVERTICAL);

  data_first_row->Add(data_first_l_row);
  data_first_row->Add(5,5,0);
  data_first_row->Add(data_first_r_row);

  wxStaticBox *gui3_box = new wxStaticBox(this, -1, "Separator");
  wxStaticBox *gui4_box = new wxStaticBox(this, -1, "Electrolyte");

  wxStaticBoxSizer *data_second_l_row = new wxStaticBoxSizer(gui3_box, wxVERTICAL);
  wxStaticBoxSizer *data_second_r_row = new wxStaticBoxSizer(gui4_box, wxVERTICAL);

  data_second_row->Add(data_second_l_row);
  data_second_row->Add(5,5,0);
  data_second_row->Add(data_second_r_row);

  wxStaticBox *gui5_box = new wxStaticBox(this, -1, "Electrodes");
  wxStaticBox *gui6_box = new wxStaticBox(this, -1, "Interconect");

  wxStaticBoxSizer *data_third_l_row = new wxStaticBoxSizer(gui5_box, wxVERTICAL);
  wxStaticBoxSizer *data_third_r_row = new wxStaticBoxSizer(gui6_box, wxVERTICAL);

  data_third_row->Add(data_third_l_row);
  data_third_row->Add(5,5,0);
  data_third_row->Add(data_third_r_row);

  wxStaticBox *gui7_box = new wxStaticBox(this, -1, "Cells");
  wxStaticBox *gui8_box = new wxStaticBox(this, -1, "Operational");

  wxStaticBoxSizer *data_forth_l_row = new wxStaticBoxSizer(gui7_box, wxVERTICAL);
  wxStaticBoxSizer *data_forth_r_row = new wxStaticBoxSizer(gui8_box, wxVERTICAL);

  data_forth_row->Add(data_forth_l_row);
  data_forth_row->Add(5,5,0);
  data_forth_row->Add(data_forth_r_row);

  wxBoxSizer *data_first_l1_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_l2_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_l3_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_l4_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_l5_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_l6_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_l7_row = new wxBoxSizer(wxHORIZONTAL);

  data_first_l_row->Add(10, 5, 0);
  data_first_l_row->Add(data_first_l1_row, 0);
  data_first_l_row->Add(10, 3, 0);
  data_first_l_row->Add(data_first_l2_row, 0);
  data_first_l_row->Add(10, 3, 0);
  data_first_l_row->Add(data_first_l3_row, 0);
  data_first_l_row->Add(10, 3, 0);
  data_first_l_row->Add(data_first_l4_row, 0);
  data_first_l_row->Add(10, 3, 0);
  data_first_l_row->Add(data_first_l5_row, 0);
  data_first_l_row->Add(10, 3, 0);
  data_first_l_row->Add(data_first_l6_row, 0);
  data_first_l_row->Add(10, 3, 0);
  data_first_l_row->Add(data_first_l7_row, 0);
  data_first_l_row->Add(10, 3, 0);
  data_first_l_row->Add(80, 20, 0);
  data_first_l_row->Add(10, 3, 0);
  data_first_l_row->Add(80, 20, 0);
  data_first_l_row->Add(10, 3, 0);

  wxBoxSizer *data_first_r1_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_r2_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_r3_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_r4_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_r5_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_r6_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_r7_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_r8_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_r9_row = new wxBoxSizer(wxHORIZONTAL);

  data_first_r_row->Add(10, 5, 0);
  data_first_r_row->Add(data_first_r1_row, 0);
  data_first_r_row->Add(10, 3, 0);
  data_first_r_row->Add(data_first_r2_row, 0);
  data_first_r_row->Add(10, 3, 0);
  data_first_r_row->Add(data_first_r3_row, 0);
  data_first_r_row->Add(10, 3, 0);
  data_first_r_row->Add(data_first_r4_row, 0);
  data_first_r_row->Add(10, 3, 0);
  data_first_r_row->Add(data_first_r5_row, 0);
  data_first_r_row->Add(10, 3, 0);
  data_first_r_row->Add(data_first_r6_row, 0);
  data_first_r_row->Add(10, 3, 0);
  data_first_r_row->Add(data_first_r7_row, 0);
  data_first_r_row->Add(10, 3, 0);
  data_first_r_row->Add(data_first_r8_row, 0);
  data_first_r_row->Add(10, 3, 0);
  data_first_r_row->Add(data_first_r9_row, 0);
  data_first_r_row->Add(10, 3, 0);
  
  wxBoxSizer *data_second_l1_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_second_l2_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_second_l3_row = new wxBoxSizer(wxHORIZONTAL);

  data_second_l_row->Add(10, 5, 0);
  data_second_l_row->Add(data_second_l1_row, 0);
  data_second_l_row->Add(10, 3, 0);
  data_second_l_row->Add(data_second_l2_row, 0);
  data_second_l_row->Add(10, 3, 0);
  data_second_l_row->Add(data_second_l3_row, 0);
  data_second_l_row->Add(10, 3, 0);

  wxBoxSizer *data_second_r1_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_second_r2_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_second_r3_row = new wxBoxSizer(wxHORIZONTAL);

  data_second_r_row->Add(10, 5, 0);
  data_second_r_row->Add(data_second_r1_row, 0);
  data_second_r_row->Add(10, 3, 0);
  data_second_r_row->Add(data_second_r2_row, 0);
  data_second_r_row->Add(10, 3, 0);
  data_second_r_row->Add(data_second_r3_row, 0);
  data_second_r_row->Add(10, 3, 0);


  wxBoxSizer *data_third_l1_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_third_l2_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_third_l3_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_third_l4_row = new wxBoxSizer(wxHORIZONTAL);

  data_third_l_row->Add(10, 5, 0);
  data_third_l_row->Add(data_third_l1_row, 0);
  data_third_l_row->Add(10, 3, 0);
  data_third_l_row->Add(data_third_l2_row, 0);
  data_third_l_row->Add(10, 3, 0);
  data_third_l_row->Add(data_third_l3_row, 0);
  data_third_l_row->Add(10, 3, 0);
  data_third_l_row->Add(data_third_l4_row, 0);
  data_third_l_row->Add(10, 3, 0);

  wxBoxSizer *data_third_r1_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_third_r2_row = new wxBoxSizer(wxHORIZONTAL);

  data_third_r_row->Add(10, 5, 0);
  data_third_r_row->Add(data_third_r1_row, 0);
  data_third_r_row->Add(10, 3, 0);
  data_third_r_row->Add(data_third_r2_row, 0);
  data_third_r_row->Add(10, 3, 0);
  data_third_r_row->Add(80, 20, 0);
  data_third_r_row->Add(10, 3, 0);
  data_third_r_row->Add(80, 20, 0);
  data_third_r_row->Add(10, 3, 0);

  wxBoxSizer *data_forth_l1_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_forth_l2_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_forth_l3_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_forth_l4_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_forth_l5_row = new wxBoxSizer(wxHORIZONTAL);

  data_forth_l_row->Add(10, 5, 0);
  data_forth_l_row->Add(data_forth_l1_row, 0);
  data_forth_l_row->Add(10, 3, 0);
  data_forth_l_row->Add(data_forth_l2_row, 0);
  data_forth_l_row->Add(10, 3, 0);
  data_forth_l_row->Add(data_forth_l3_row, 0);
  data_forth_l_row->Add(10, 3, 0);
  data_forth_l_row->Add(data_forth_l4_row, 0);
  data_forth_l_row->Add(10, 3, 0);
  data_forth_l_row->Add(data_forth_l5_row, 0);
  data_forth_l_row->Add(10, 3, 0);

  wxBoxSizer *data_forth_r1_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_forth_r2_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_forth_r3_row = new wxBoxSizer(wxHORIZONTAL);

  data_forth_r_row->Add(10, 5, 0);
  data_forth_r_row->Add(data_forth_r1_row, 0);
  data_forth_r_row->Add(10, 3, 0);
  data_forth_r_row->Add(data_forth_r2_row, 0);
  data_forth_r_row->Add(10, 3, 0);
  data_forth_r_row->Add(data_forth_r3_row, 0);
  data_forth_r_row->Add(10, 3, 0);
  data_forth_r_row->Add(80, 20, 0);
  data_forth_r_row->Add(10, 3, 0);
  data_forth_r_row->Add(80, 20, 0);
  data_forth_r_row->Add(10, 3, 0);

  wxStaticText * label_1l1 = new wxStaticText(this, -1, "Channel Height (m) ", wxDefaultPosition, wxSize(200, 17));
  t_a_height = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_l1_row->Add(label_1l1);
  data_first_l1_row->Add(t_a_height);

  wxStaticText * label_1l2 = new wxStaticText(this, -1, "Channel Width (m) ", wxDefaultPosition, wxSize(200, 17));
  t_a_width = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_l2_row->Add(label_1l2);
  data_first_l2_row->Add(t_a_width);

  wxStaticText * label_1l3 = new wxStaticText(this, -1, "Channel Spacing (m) ", wxDefaultPosition, wxSize(200, 17));
  t_a_space = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_l3_row->Add(label_1l3);
  data_first_l3_row->Add(t_a_space);

  wxStaticText * label_1l4 = new wxStaticText(this, -1, "Exg.Cur.Den. (Amp/m2) ", wxDefaultPosition, wxSize(200, 17));
  t_a_ecd = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_l4_row->Add(label_1l4);
  data_first_l4_row->Add(t_a_ecd);

  wxStaticText * label_1l5 = new wxStaticText(this, -1, "Trans. Coeff ", wxDefaultPosition, wxSize(200, 17));
  t_a_tcoeff = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_l5_row->Add(label_1l5);
  data_first_l5_row->Add(t_a_tcoeff);

  wxStaticText * label_1l6 = new wxStaticText(this, -1, "Electrode Thickness (m) ", wxDefaultPosition, wxSize(200, 17));
  t_a_thick = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_l6_row->Add(label_1l6);
  data_first_l6_row->Add(t_a_thick);

  wxStaticText * label_1l7 = new wxStaticText(this, -1, "Pressure Drop (Pa) ", wxDefaultPosition, wxSize(200, 17));
  t_a_presdrop = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_l7_row->Add(label_1l7);
  data_first_l7_row->Add(t_a_presdrop);

  wxStaticText * label_1r1 = new wxStaticText(this, -1, "Channel Height (m) ", wxDefaultPosition, wxSize(200, 17));
  t_c_height = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_r1_row->Add(label_1r1);
  data_first_r1_row->Add(t_c_height);

  wxStaticText * label_1r2 = new wxStaticText(this, -1, "Channel Width (m) ", wxDefaultPosition, wxSize(200, 17));
  t_c_width = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_r2_row->Add(label_1r2);
  data_first_r2_row->Add(t_c_width);

  wxStaticText * label_1r3 = new wxStaticText(this, -1, "Channel Spacing (m) ", wxDefaultPosition, wxSize(200, 17));
  t_c_space = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_r3_row->Add(label_1r3);
  data_first_r3_row->Add(t_c_space);

  wxStaticText * label_1r4 = new wxStaticText(this, -1, "Exg.Cur.Den. 'b' Coeff ", wxDefaultPosition, wxSize(200, 17));
  t_c_ecdb = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_r4_row->Add(label_1r4);
  data_first_r4_row->Add(t_c_ecdb);

  wxStaticText * label_1r5 = new wxStaticText(this, -1, "Exg.Cur.Den. 'm' Coeff ", wxDefaultPosition, wxSize(200, 17));
  t_c_ecdm = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_r5_row->Add(label_1r5);
  data_first_r5_row->Add(t_c_ecdm);

  wxStaticText * label_1r6 = new wxStaticText(this, -1, "Trans. 'B' Coeff ", wxDefaultPosition, wxSize(200, 17));
  t_c_tcoeffb = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_r6_row->Add(label_1r6);
  data_first_r6_row->Add(t_c_tcoeffb);

  wxStaticText * label_1r7 = new wxStaticText(this, -1, "Trans. 'M' Coeff ", wxDefaultPosition, wxSize(200, 17));
  t_c_tcoeffm = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_r7_row->Add(label_1r7);
  data_first_r7_row->Add(t_c_tcoeffm);

  wxStaticText * label_1r8 = new wxStaticText(this, -1, "Electrode Thickness (m) ", wxDefaultPosition, wxSize(200, 17));
  t_c_thick = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_r8_row->Add(label_1r8);
  data_first_r8_row->Add(t_c_thick);

  wxStaticText * label_1r9 = new wxStaticText(this, -1, "Pressure Drop (Pa) ", wxDefaultPosition, wxSize(200, 17));
  t_c_presdrop = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_r9_row->Add(label_1r9);
  data_first_r9_row->Add(t_c_presdrop);


  wxStaticText * label_2l1 = new wxStaticText(this, -1, "Thickness (m) ", wxDefaultPosition, wxSize(200, 17));
  t_s_thick = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_l1_row->Add(label_2l1);
  data_second_l1_row->Add(t_s_thick);

  wxStaticText * label_2l2 = new wxStaticText(this, -1, "Heat Capacity (J/kg-K) ", wxDefaultPosition, wxSize(200, 17));
  t_s_heatcap = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_l2_row->Add(label_2l2);
  data_second_l2_row->Add(t_s_heatcap);

  wxStaticText * label_2l3 = new wxStaticText(this, -1, "Density (kg/m3) ", wxDefaultPosition, wxSize(200, 17));
  t_s_density = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_l3_row->Add(label_2l3);
  data_second_l3_row->Add(t_s_density);

  wxStaticText * label_2r1 = new wxStaticText(this, -1, "Thickness (m) ", wxDefaultPosition, wxSize(200, 17));
  t_e_thick = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_r1_row->Add(label_2r1);
  data_second_r1_row->Add(t_e_thick);

  wxStaticText * label_2r2 = new wxStaticText(this, -1, "Res. PreExpon. (ohm-cm) ", wxDefaultPosition, wxSize(200, 17));
  t_e_preexp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_r2_row->Add(label_2r2);
  data_second_r2_row->Add(t_e_preexp);

  wxStaticText * label_2r3 = new wxStaticText(this, -1, "Rex. Expon. (K) ", wxDefaultPosition, wxSize(200, 17));
  t_e_exp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_r3_row->Add(label_2r3);
  data_second_r3_row->Add(t_e_exp);


 wxStaticText * label_3l1 = new wxStaticText(this, -1, "Fuel Res. PreExpon. (ohm-cm) ", wxDefaultPosition, wxSize(200, 17));
  t_f_preexp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_l1_row->Add(label_3l1);
  data_third_l1_row->Add(t_f_preexp);

  wxStaticText * label_3l2 = new wxStaticText(this, -1, "Fuel Res. Expon. (K) ", wxDefaultPosition, wxSize(200, 17));
  t_f_exp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_l2_row->Add(label_3l2);
  data_third_l2_row->Add(t_f_exp);

  wxStaticText * label_3l3 = new wxStaticText(this, -1, "Air Res. PreExpon. (ohm-cm) ", wxDefaultPosition, wxSize(200, 17));
  t_a_preexp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_l3_row->Add(label_3l3);
  data_third_l3_row->Add(t_a_preexp);

  wxStaticText * label_3l4 = new wxStaticText(this, -1, "Air Res. Expon. (K) ", wxDefaultPosition, wxSize(200, 17));
  t_a_exp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_l4_row->Add(label_3l4);
  data_third_l4_row->Add(t_a_exp);

 wxStaticText * label_3r1 = new wxStaticText(this, -1, "Res. PreExpon. (ohm-cm) ", wxDefaultPosition, wxSize(200, 17));
  t_i_preexp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_r1_row->Add(label_3r1);
  data_third_r1_row->Add(t_i_preexp);

  wxStaticText * label_3r2 = new wxStaticText(this, -1, "Res. Expon. (K) ", wxDefaultPosition, wxSize(200, 17));
  t_i_exp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_r2_row->Add(label_3r2);
  data_third_r2_row->Add(t_i_exp);


  wxStaticText * label_4l1 = new wxStaticText(this, -1, "Number of Cells ", wxDefaultPosition, wxSize(200, 17));
  t_l_numCells = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_l1_row->Add(label_4l1);
  data_forth_l1_row->Add(t_l_numCells);

  wxStaticText * label_4l2 = new wxStaticText(this, -1, "Heat Capacity (J/kg-K) ", wxDefaultPosition, wxSize(200, 17));
  t_l_heatcap = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_l2_row->Add(label_4l2);
  data_forth_l2_row->Add(t_l_heatcap);

  wxStaticText * label_4l3 = new wxStaticText(this, -1, "Density (kg/m3) ", wxDefaultPosition, wxSize(200, 17));
  t_l_density = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_l3_row->Add(label_4l3);
  data_forth_l3_row->Add(t_l_density);

  wxStaticText * label_4l4 = new wxStaticText(this, -1, "Axial Length (m) ", wxDefaultPosition, wxSize(200, 17));
  t_l_length = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_l4_row->Add(label_4l4);
  data_forth_l4_row->Add(t_l_length);

  wxStaticText * label_4l5 = new wxStaticText(this, -1, "Axial Width (m) ", wxDefaultPosition, wxSize(200, 17));
  t_l_width = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_l5_row->Add(label_4l5);
  data_forth_l5_row->Add(t_l_width);

  wxStaticText * label_4r1 = new wxStaticText(this, -1, "Stop Time (sec) ", wxDefaultPosition, wxSize(200, 17));
  t_stop_time = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_r1_row->Add(label_4r1);
  data_forth_r1_row->Add(t_stop_time);

  wxStaticText * label_4r2 = new wxStaticText(this, -1, "Number of Axial Nodes ", wxDefaultPosition, wxSize(200, 17));
  t_ax_nodes = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_r2_row->Add(label_4r2);
  data_forth_r2_row->Add(t_ax_nodes);

  wxStaticText * label_4r3 = new wxStaticText(this, -1, "Load Resistance (ohm) ", wxDefaultPosition, wxSize(200, 17));
  t_loadres = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_r3_row->Add(label_4r3);
  data_forth_r3_row->Add(t_loadres);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);   
}

/////////////////////////////////////////////////////
SOFC1D_UI_Dialog
::~SOFC1D_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool SOFC1D_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_work_dir->GetValue();
  (*p_work_dir) = txt.c_str();

  txt = t_a_height->GetValue();
  (*p_a_height) = atof(txt.c_str());
  txt = t_a_width->GetValue();
  (*p_a_width) = atof(txt.c_str());
  txt = t_a_space->GetValue();
  (*p_a_space) = atof(txt.c_str());
  txt = t_a_ecd->GetValue();
  (*p_a_ecd) = atof(txt.c_str());
  txt = t_a_tcoeff->GetValue();
  (*p_a_tcoeff) = atof(txt.c_str());
  txt = t_a_thick->GetValue();
  (*p_a_thick) = atof(txt.c_str());
  txt = t_a_presdrop->GetValue();
  (*p_a_presdrop) = atof(txt.c_str());

  txt = t_c_height->GetValue();
  (*p_c_height) = atof(txt.c_str());
  txt = t_c_width->GetValue();
  (*p_c_width) = atof(txt.c_str());
  txt = t_c_space->GetValue();
  (*p_c_space) = atof(txt.c_str());
  txt = t_c_ecdb->GetValue();
  (*p_c_ecdb) = atof(txt.c_str());
  txt = t_c_ecdm->GetValue();
  (*p_c_ecdm) = atof(txt.c_str());
  txt = t_c_tcoeffb->GetValue();
  (*p_c_tcoeffb) = atof(txt.c_str());
  txt = t_c_tcoeffm->GetValue();
  (*p_c_tcoeffm) = atof(txt.c_str());
  txt = t_c_thick->GetValue();
  (*p_c_thick) = atof(txt.c_str());
  txt = t_c_presdrop->GetValue();
  (*p_c_presdrop) = atof(txt.c_str());

  txt = t_s_thick->GetValue();
  (*p_s_thick) = atof(txt.c_str());
  txt = t_s_heatcap->GetValue();
  (*p_s_heatcap) = atof(txt.c_str());
  txt = t_s_density->GetValue();
  (*p_s_density) = atof(txt.c_str());

  txt = t_e_thick->GetValue();
  (*p_e_thick) = atof(txt.c_str());
  txt = t_e_preexp->GetValue();
  (*p_e_preexp) = atof(txt.c_str());
  txt = t_e_exp->GetValue();
  (*p_e_exp) = atof(txt.c_str());

  txt = t_f_preexp->GetValue();
  (*p_f_preexp) = atof(txt.c_str());
  txt = t_f_exp->GetValue();
  (*p_f_exp) = atof(txt.c_str());
  txt = t_a_preexp->GetValue();
  (*p_a_preexp) = atof(txt.c_str());
  txt = t_a_exp->GetValue();
  (*p_a_exp) = atof(txt.c_str());

  txt = t_i_preexp->GetValue();
  (*p_i_preexp) = atof(txt.c_str());
  txt = t_i_exp->GetValue();
  (*p_i_exp) = atof(txt.c_str());

  txt = t_l_numCells->GetValue();
  (*p_l_numCells) = atoi(txt.c_str());
  txt = t_l_heatcap->GetValue();
  (*p_l_heatcap) = atof(txt.c_str());
  txt = t_l_density->GetValue();
  (*p_l_density) = atof(txt.c_str());
  txt = t_l_length->GetValue();
  (*p_l_length) = atof(txt.c_str());
  txt = t_l_width->GetValue();
  (*p_l_width) = atof(txt.c_str());

  txt = t_stop_time->GetValue();
  (*p_stop_time) = atof(txt.c_str());
  txt = t_ax_nodes->GetValue();
  (*p_ax_nodes) = atoi(txt.c_str());
  txt = t_loadres->GetValue();
  (*p_loadres) = atof(txt.c_str());

  return true;
}

////////////////////////////////////////////////////
bool SOFC1D_UI_Dialog::TransferDataToWindow()
{
  wxString txt0, txt1, txt2, txt3, txt4, txt5, txt6, txt7, txt8, txt9;
  wxString txt10, txt11, txt12, txt13, txt14, txt15, txt16, txt17, txt18, txt19;
  wxString txt20, txt21, txt22, txt23, txt24, txt25, txt26, txt27, txt28, txt29;
  wxString txt30, txt31, txt32, txt33, txt34, txt35, txt36;
   
  txt0 = p_work_dir->c_str();
  t_work_dir->SetValue(txt0);

  txt1<<(*p_a_height);
  t_a_height->SetValue(txt1);

  txt2<<(*p_a_width);
  t_a_width->SetValue(txt2);

  txt3<<(*p_a_space);
  t_a_space->SetValue(txt3);

  txt4<<(*p_a_ecd);
  t_a_ecd->SetValue(txt4);

  txt5<<(*p_a_tcoeff);
  t_a_tcoeff->SetValue(txt5);

  txt6<<(*p_a_thick);
  t_a_thick->SetValue(txt6);

  txt7<<(*p_a_presdrop);
  t_a_presdrop->SetValue(txt7);

  txt8<<(*p_c_height);
  t_c_height->SetValue(txt8);

  txt9<<(*p_c_width);
  t_c_width->SetValue(txt9);

  txt10<<(*p_c_space);
  t_c_space->SetValue(txt10);

  txt11<<(*p_c_ecdb);
  t_c_ecdb->SetValue(txt11);

  txt12<<(*p_c_ecdm);
  t_c_ecdm->SetValue(txt12);

  txt13<<(*p_c_tcoeffb);
  t_c_tcoeffb->SetValue(txt13);

  txt14<<(*p_c_tcoeffm);
  t_c_tcoeffm->SetValue(txt14);

  txt15<<(*p_c_thick);
  t_c_thick->SetValue(txt15);

  txt16<<(*p_c_presdrop);
  t_c_presdrop->SetValue(txt16);

  txt17<<(*p_s_thick);
  t_s_thick->SetValue(txt17);

  txt18<<(*p_s_heatcap);
  t_s_heatcap->SetValue(txt18);

  txt19<<(*p_s_density);
  t_s_density->SetValue(txt19);

  txt20<<(*p_e_thick);
  t_e_thick->SetValue(txt20);

  txt21<<(*p_e_preexp);
  t_e_preexp->SetValue(txt21);

  txt22<<(*p_e_exp);
  t_e_exp->SetValue(txt22);

  txt23<<(*p_f_preexp);
  t_f_preexp->SetValue(txt23);

  txt24<<(*p_f_exp);
  t_f_exp->SetValue(txt24);

  txt25<<(*p_a_preexp);
  t_a_preexp->SetValue(txt25);

  txt26<<(*p_a_exp);
  t_a_exp->SetValue(txt26);

  txt27<<(*p_i_preexp);
  t_i_preexp->SetValue(txt27);

  txt28<<(*p_i_exp);
  t_i_exp->SetValue(txt28);

  txt29<<(*p_l_numCells);
  t_l_numCells->SetValue(txt29);

  txt30<<(*p_l_heatcap);
  t_l_heatcap->SetValue(txt30);

  txt31<<(*p_l_density);
  t_l_density->SetValue(txt31);

  txt32<<(*p_l_length);
  t_l_length->SetValue(txt32);

  txt33<<(*p_l_width);
  t_l_width->SetValue(txt33);

  txt34<<(*p_stop_time);
  t_stop_time->SetValue(txt34);

  txt35<<(*p_ax_nodes);
  t_ax_nodes->SetValue(txt35);

  txt36<<(*p_loadres);
  t_loadres->SetValue(txt36);


  return true;
}

void SOFC1D_UI_Dialog::Lock(bool l)
{
}

void SOFC1D_UI_Dialog::OnBrowse(wxCommandEvent &event)
{
  wxString f_workdir;

  f_workdir = t_work_dir->GetValue();
  f_workdir = wxDirSelector("Choose a working directory", f_workdir);

  if (f_workdir == _T(""))
    f_workdir = t_work_dir->GetValue();
  t_work_dir->SetValue(f_workdir);
}
