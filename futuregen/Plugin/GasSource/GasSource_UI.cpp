#include "GasSource_UI.h"

BEGIN_EVENT_TABLE(GasSourceTabs, wxNotebook)
  EVT_BUTTON(ADD, GasSourceTabs::OnAdd)
  EVT_BUTTON(DEL, GasSourceTabs::OnDel)
  EVT_BUTTON(P_ADD, GasSourceTabs::OnPAdd)
  EVT_BUTTON(P_DEL, GasSourceTabs::OnPDel)
  EVT_LISTBOX_DCLICK(SPEC_LIST, GasSourceTabs::OnEditSpec)
  EVT_LISTBOX_DCLICK(PART_LIST, GasSourceTabs::OnEditPart)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(GasSource_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
GasSource_UI_Dialog
::GasSource_UI_Dialog
(wxWindow* parent, int id,
  double* temp,
  double* pres,
  double* flow,
  double* p_temp,
  double* p_m,
  double* mps,
  double* sv,
  double* coalcal,
  double* ashcal,
  double* ashph,
  vector<string>* species,
  vector<string>* comp,
  vector<string>* spec_frac,
  vector<string>* particles,
  vector<string>* p_comp,
  vector<string>* p_frac)
: UIDialog((wxWindow *) parent, id, "GasSource"),
  p_temp(temp),
  p_pres(pres),
  p_flow(flow),
  p_p_temp(p_temp),
  p_p_m(p_m),
  p_mps(mps),
  p_sv(sv),
  p_coalcal(coalcal),
  p_ashcal(ashcal),
  p_ashph(ashph),
  p_species(species),
  p_comp(comp),
  p_spec_frac(spec_frac),
  p_particles(particles),
  p_p_comp(p_comp),
  p_p_frac(p_frac)
{
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
   
  m_tabs = new GasSourceTabs(this);
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
  SetAutoLayout(TRUE);
  top_sizer->Fit(this);
}

/////////////////////////////////////////////////////
GasSource_UI_Dialog
::~GasSource_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool GasSource_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;

  txt = m_tabs->t_temp->GetValue();
  *(p_temp) = atof(txt.c_str());
  txt = m_tabs->t_pres->GetValue();
  *(p_pres) = atof(txt.c_str());
  txt = m_tabs->t_flow->GetValue();
  *(p_flow) = atof(txt.c_str());
  txt = m_tabs->t_p_temp->GetValue();
  *(p_p_temp) = atof(txt.c_str());
  txt = m_tabs->t_p_m->GetValue();
  *(p_p_m) = atof(txt.c_str());
  txt = m_tabs->t_mps->GetValue();
  *(p_mps) = atof(txt.c_str());
  txt = m_tabs->t_sv->GetValue();
  *(p_sv) = atof(txt.c_str());
  txt = m_tabs->t_coalcal->GetValue();
  *(p_coalcal) = atof(txt.c_str());
  txt = m_tabs->t_ashcal->GetValue();
  *(p_ashcal) = atof(txt.c_str());
  txt = m_tabs->t_ashph->GetValue();
  *(p_ashph) = atof(txt.c_str());

  int num;
  int i;
  wxString val, var;
  
  num = m_tabs->lb_comp_frac->GetCount();
  p_comp->clear();
  p_spec_frac->clear();
  for (i=0; i<num; i++)
    {
      txt = m_tabs->lb_comp_frac->GetString(i);
      val = m_tabs->ToValue(txt, var);
      p_comp->push_back(var.c_str());
      p_spec_frac->push_back(val.c_str());
    }

  num = m_tabs->lb_p_comp_frac->GetCount();
  p_p_comp->clear();
  p_p_frac->clear();
  for (i=0; i<num; i++)
    {
      txt = m_tabs->lb_p_comp_frac->GetString(i);
      val = m_tabs->ToValue(txt, var);
      p_p_comp->push_back(var.c_str());
      p_p_frac->push_back(val.c_str());
    }
  
  return true;
}

////////////////////////////////////////////////////
bool GasSource_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4, txt5, txt6, txt7, txt8, txt9, txt10;

  txt1<<(*p_temp);
  m_tabs->t_temp->SetValue(txt1);
  txt2<<(*p_pres);
  m_tabs->t_pres->SetValue(txt2);
  txt3<<(*p_flow);
  m_tabs->t_flow->SetValue(txt3);
  txt4<<(*p_p_temp);
  m_tabs->t_p_temp->SetValue(txt4);
  txt5<<(*p_p_m);
  m_tabs->t_p_m->SetValue(txt5);
  txt6<<(*p_mps);
  m_tabs->t_mps->SetValue(txt6);
  txt7<<(*p_sv);
  m_tabs->t_sv->SetValue(txt7);
  txt8<<(*p_coalcal);
  m_tabs->t_coalcal->SetValue(txt8);
  txt9<<(*p_ashcal);
  m_tabs->t_ashcal->SetValue(txt9);
  txt10<<(*p_ashph);
  m_tabs->t_ashph->SetValue(txt10);

  //Here should include the code to read in the species and the paticles
  int i;
  int num;
  wxString var, val;

  m_tabs->lb_species->Clear();
  num = p_species->size();
  for (i=0; i<num; i++)
    m_tabs->lb_species->Append((*p_species)[i].c_str());

  m_tabs->lb_comp_frac->Clear();
  num = p_comp->size();
  for (i=0; i<num; i++)
    {
      var=(*p_comp)[i].c_str();
      val=(*p_spec_frac)[i].c_str();
      m_tabs->lb_comp_frac->Append(m_tabs->ToString(var, val));
    }

  m_tabs->lb_particles->Clear();
  num = p_particles->size();
  for (i=0; i<num; i++)
    m_tabs->lb_particles->Append((*p_particles)[i].c_str());

  m_tabs->lb_p_comp_frac->Clear();
  num = p_p_comp->size();
  for (i=0; i<num; i++)
    {
      var=(*p_p_comp)[i].c_str();
      val=(*p_p_frac)[i].c_str();
      m_tabs->lb_p_comp_frac->Append(m_tabs->ToString(var, val));
    }
  
  return true;
}

void GasSource_UI_Dialog::Lock(bool l)
{
}

void GasSourceTabs::OnAdd(wxCommandEvent& event)
{
  wxArrayInt selections;
  int i;
  map<wxString, wxString> items;
  map<wxString, wxString>::iterator iter;

  wxString cur_val;
  wxString cur_var;
  int num_sels, count;

  items.clear();
  
  count = lb_comp_frac->GetCount();
  for (i=0; i<count; i++)
    {
      cur_val = ToValue(lb_comp_frac->GetString(i), cur_var);
      items.insert(pair<wxString, wxString>(cur_var, cur_val));
    }

  num_sels = lb_species->GetSelections(selections);
  for (i=0; i<num_sels; i++)
    items.insert(pair<wxString, wxString>(lb_species->GetString(selections[i]), _T("0.0")));
  
  lb_comp_frac->Clear();

  for (iter = items.begin(); iter!=items.end(); iter++)
    lb_comp_frac->Append(ToString(iter->first, iter->second));
}

void GasSourceTabs::OnDel(wxCommandEvent& event)
{
  wxArrayInt selections;
  int num_sels, i;
  
  num_sels = lb_comp_frac->GetSelections(selections);
  
  for (i=0; i<num_sels; i++)
    lb_comp_frac->Delete(selections[i]-i);
  
}

void GasSourceTabs::OnPAdd(wxCommandEvent& event)
{
  wxArrayInt selections;
  int i;
  map<wxString, wxString> items;
  map<wxString, wxString>::iterator iter;

  wxString cur_val;
  wxString cur_var;
  int num_sels, count;

  items.clear();
  
  count = lb_p_comp_frac->GetCount();
  for (i=0; i<count; i++)
    {
      cur_val = ToValue(lb_p_comp_frac->GetString(i), cur_var);
      items.insert(pair<wxString, wxString>(cur_var, cur_val));
    }

  num_sels = lb_particles->GetSelections(selections);
  for (i=0; i<num_sels; i++)
    items.insert(pair<wxString, wxString>(lb_particles->GetString(selections[i]), _T("0.0")));
  
  lb_p_comp_frac->Clear();

  for (iter = items.begin(); iter!=items.end(); iter++)
    lb_p_comp_frac->Append(ToString(iter->first, iter->second));
}

void GasSourceTabs::OnPDel(wxCommandEvent& event)
{
  wxArrayInt selections;
  int num_sels, i;

  num_sels = lb_p_comp_frac->GetSelections(selections);
  
  for (i=0; i<num_sels; i++)
    lb_p_comp_frac->Delete(selections[i]-i);

}

GasSourceTabs::GasSourceTabs(wxWindow *parent, wxWindowID id,
	   const wxPoint& pos , 
	   const wxSize& size , 
	   long style)
  : wxNotebook(parent, id, pos, size, style)
{
}

void GasSourceTabs::CreateInitialPages()
{
    wxPanel *panel = (wxPanel *) NULL;

    // Create and add some panels to the notebook

    panel = CreateFirstPage();
    AddPage( panel, _T("Gas"), true);
    
    panel = CreateSecondPage();
    AddPage( panel, _T("Particle"), false);
 
}

wxPanel* GasSourceTabs::CreateFirstPage()
{
  wxPanel *panel = new wxPanel(this);

  wxBoxSizer* toptop= new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* left_margin = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* right_margin = new wxBoxSizer(wxHORIZONTAL);

  left_margin->Add(5, 10);
  right_margin->Add(5, 10);
  toptop->Add(left_margin, 0, wxALIGN_LEFT);
  toptop->Add(top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL);
  toptop->Add(right_margin, 0, wxALIGN_RIGHT);

  wxStaticBox *gui1_box = new wxStaticBox(panel, -1, "Gas Average");
  
  wxStaticBoxSizer *data1_row = new wxStaticBoxSizer(gui1_box, wxVERTICAL);
  wxBoxSizer* data2_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* data3_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 10, 0); //the top margin
  top_sizer->Add(data1_row, 0); 
  top_sizer->Add(10, 5, 0); 
  top_sizer->Add(data2_row, 0);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(data3_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);

  wxBoxSizer *data1_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_third_row = new wxBoxSizer(wxHORIZONTAL);
  
  data1_row->Add(10, 5, 0);
  data1_row->Add(data1_first_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_second_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_third_row, 0);
  data1_row->Add(10, 3, 0);
 
  wxStaticText * label1 = new wxStaticText(panel, -1, " Temperature (K):", wxDefaultPosition, wxSize(200, 17));
  t_temp = new wxTextCtrl(panel, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_first_row->Add(label1);
  data1_first_row->Add(t_temp);

  wxStaticText * label2 = new wxStaticText(panel, -1, " Pressure (Pa):", wxDefaultPosition, wxSize(200, 17));
  t_pres = new wxTextCtrl(panel, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_second_row->Add(label2);
  data1_second_row->Add(t_pres);

  wxStaticText * label3 = new wxStaticText(panel, -1, " Flowrate (kg/s)", wxDefaultPosition, wxSize(200, 17));
  t_flow = new wxTextCtrl(panel, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_third_row->Add(label3);
  data1_third_row->Add(t_flow);

  wxBoxSizer* spec_row = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* comp_row = new wxBoxSizer(wxVERTICAL);

  data2_row->Add(spec_row);
  data2_row->Add(comp_row);

  lb_species = new wxListBox(panel, -1, wxDefaultPosition, wxSize(120, 500), 0, NULL, wxLB_MULTIPLE|wxLB_SORT|wxLB_NEEDED_SB);
  spec_row->Add(new wxStaticText(panel, -1, "    Specie", wxDefaultPosition, wxSize(120, 17)));
  spec_row->Add(lb_species);


  wxBoxSizer* title_row = new wxBoxSizer(wxHORIZONTAL);
  comp_row->Add(title_row);

  title_row->Add(new wxStaticText(panel, -1, "Composition", wxDefaultPosition, wxSize(100, 17)));
  title_row->Add(new wxStaticText(panel, -1, "Mole Fraction", wxDefaultPosition, wxSize(120, 17)));

  lb_comp_frac = new wxListBox(panel, SPEC_LIST, wxDefaultPosition, wxSize(240,500), 0, NULL, wxLB_SINGLE|wxLB_SORT);  
  comp_row->Add(lb_comp_frac);

  b_add = new wxButton(panel, ADD, "Add");
  b_del = new wxButton(panel, DEL, "Del");

  data3_row->Add(b_add, 0, wxALIGN_CENTER_HORIZONTAL);
  data3_row->Add(b_del, 0, wxALIGN_CENTER_HORIZONTAL);

  panel->SetSizer(toptop);
  return panel;

}

wxPanel* GasSourceTabs::CreateSecondPage()
{
  wxPanel *panel = new wxPanel(this);

  wxBoxSizer* toptop= new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* left_margin = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* right_margin = new wxBoxSizer(wxHORIZONTAL);

  left_margin->Add(5, 10);
  right_margin->Add(5, 10);
  toptop->Add(left_margin, 0, wxALIGN_LEFT);
  toptop->Add(top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL);
  toptop->Add(right_margin, 0, wxALIGN_RIGHT);

  wxStaticBox *gui1_box = new wxStaticBox(panel, -1, "Particle Average");
  
  wxStaticBoxSizer *data1_row = new wxStaticBoxSizer(gui1_box, wxVERTICAL);
  wxBoxSizer* data2_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* data3_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 10, 0); //the top margin
  top_sizer->Add(data1_row, 0); 
  top_sizer->Add(10, 5, 0); 
  top_sizer->Add(data2_row, 0);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(data3_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);

  wxBoxSizer *data1_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_forth_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_fifth_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_sixth_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_seventh_row = new wxBoxSizer(wxHORIZONTAL);

  data1_row->Add(10, 5, 0);
  data1_row->Add(data1_first_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_second_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_third_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_forth_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_fifth_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_sixth_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_seventh_row, 0);
  data1_row->Add(10, 3, 0);

  wxStaticText * label1 = new wxStaticText(panel, -1, "Particle Temperature (K):", wxDefaultPosition, wxSize(200, 17));
  t_p_temp = new wxTextCtrl(panel, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_first_row->Add(label1);
  data1_first_row->Add(t_p_temp);

  wxStaticText * label2 = new wxStaticText(panel, -1, "Particle Flowrate (kg/s):", wxDefaultPosition, wxSize(200, 17));
  t_p_m = new wxTextCtrl(panel, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_second_row->Add(label2);
  data1_second_row->Add(t_p_m);

  wxStaticText * label3 = new wxStaticText(panel, -1, "Mean Particle Size (M):", wxDefaultPosition, wxSize(200, 17));
  t_mps = new wxTextCtrl(panel, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_third_row->Add(label3);
  data1_third_row->Add(t_mps);

  wxStaticText * label4 = new wxStaticText(panel, -1, "Size Variance (M^2):", wxDefaultPosition, wxSize(200, 17));
  t_sv = new wxTextCtrl(panel, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_forth_row->Add(label4);
  data1_forth_row->Add(t_sv);

  wxStaticText * label5 = new wxStaticText(panel, -1, "Coal Calcium (wt%):", wxDefaultPosition, wxSize(200, 17));
  t_coalcal = new wxTextCtrl(panel, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_fifth_row->Add(label5);
  data1_fifth_row->Add(t_coalcal);

  wxStaticText * label6 = new wxStaticText(panel, -1, "Ash Calcium (wt%):", wxDefaultPosition, wxSize(200, 17));
  t_ashcal = new wxTextCtrl(panel, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_sixth_row->Add(label6);
  data1_sixth_row->Add(t_ashcal);

  wxStaticText * label7 = new wxStaticText(panel, -1, "Ash pH:", wxDefaultPosition, wxSize(200, 17));
  t_ashph = new wxTextCtrl(panel, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_seventh_row->Add(label7);
  data1_seventh_row->Add(t_ashph);

  wxBoxSizer* spec_row = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* comp_row = new wxBoxSizer(wxVERTICAL);

  data2_row->Add(spec_row);
  data2_row->Add(comp_row);

  lb_particles = new wxListBox(panel, -1, wxDefaultPosition, wxSize(120, 400), 0, NULL, wxLB_MULTIPLE|wxLB_SORT|wxLB_NEEDED_SB);
  spec_row->Add(new wxStaticText(panel, -1, "      Constituent", wxDefaultPosition, wxSize(120, 17)));
  spec_row->Add(lb_particles);


  wxBoxSizer* title_row = new wxBoxSizer(wxHORIZONTAL);
  comp_row->Add(title_row);

  title_row->Add(new wxStaticText(panel, -1, "Composition", wxDefaultPosition, wxSize(100, 17)));
  title_row->Add(new wxStaticText(panel, -1, "Mass Fraction", wxDefaultPosition, wxSize(120, 17)));

  lb_p_comp_frac = new wxListBox(panel, PART_LIST, wxDefaultPosition, wxSize(240,400), 0, NULL, wxLB_SINGLE|wxLB_SORT);  
  comp_row->Add(lb_p_comp_frac);

  b_p_add = new wxButton(panel, P_ADD, "Add");
  b_p_del = new wxButton(panel, P_DEL, "Del");

  data3_row->Add(b_p_add, 0, wxALIGN_CENTER_HORIZONTAL);
  data3_row->Add(b_p_del, 0, wxALIGN_CENTER_HORIZONTAL);

  panel->SetSizer(toptop);
  return panel;
}

void GasSourceTabs::OnEditSpec(wxCommandEvent& event)
{
  wxString var;
  wxString val;
  wxString result;
  int sel;
  EditDialog* e_dlg = new EditDialog(this, -1);

  val = ToValue(lb_comp_frac->GetStringSelection(), var);

  e_dlg->m_var->SetLabel(var);
  e_dlg->m_val->SetValue(val);

  e_dlg->ShowModal();

  val = e_dlg->m_val->GetValue();
  
  result = ToString(var, val);
  sel = lb_comp_frac->GetSelection();
  lb_comp_frac->SetString(sel, result);
  e_dlg->Close(); 
}

void GasSourceTabs::OnEditPart(wxCommandEvent& event)
{
  wxString var;
  wxString val;
  wxString result;
  int sel;

  EditDialog* e_dlg = new EditDialog(this, -1);

  val = ToValue(lb_p_comp_frac->GetStringSelection(), var);

  e_dlg->m_var->SetLabel(var);
  e_dlg->m_val->SetValue(val);

  e_dlg->ShowModal();

  val = e_dlg->m_val->GetValue();

  result = ToString(var, val);
  sel = lb_comp_frac->GetSelection();
  lb_comp_frac->SetString(sel, result);

  e_dlg->Close(); //Value will be passed back by the OnOK calls of the EditDialog
}

wxString GasSourceTabs::ToValue(const wxString inp, wxString &outp)
{
  char var[80];
  char result[80];
  
  sscanf(inp.c_str(), "%s    %s", var, result);
  outp = _T(var);
  return _T(result);
}

wxString GasSourceTabs::ToString(const wxString inp, const wxString val)
{
  char result[1024];

  sprintf(result, "%-16s     %s", inp.c_str(), val.c_str());
  return _T(result);
}

EditDialog::EditDialog(wxWindow *parent, wxWindowID id)
  :wxDialog(parent, id, _T("Edit"), wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE, "dialogBox")
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

  wxBoxSizer* data_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 10, 0); //the top margin
  top_sizer->Add(data_row, 0); 
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0); //the bottom margin
  
  m_var = new wxStaticText(this, -1, " Var name ", wxDefaultPosition, wxSize(80, 17));
  m_val = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_row->Add(m_var, 0, wxALIGN_CENTER_HORIZONTAL);
  data_row->Add(m_val, 0, wxALIGN_CENTER_HORIZONTAL);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  toptop->Fit(this);
  SetAutoLayout(true);
  SetSizer(toptop);
  
}
